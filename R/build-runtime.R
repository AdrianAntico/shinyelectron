#' Embed a portable R runtime into a bundled Electron build
#'
#' Behavior-preserving extraction of the R bundled-embedding block from
#' [build_electron_app()]. ALWAYS installs + copies the interpreter (and resolves
#' symlinks) so the shared `runtime/R` path exists for suite-wide bundled
#' detection; only the package install is gated on a non-empty `packages` set.
#' `packages` is the DIRECT set (as stored in `dependencies.json`); the recursive
#' dependency closure and the `pre_installed` setdiff are resolved here, against
#' the freshly-created `runtime/R/library`.
#'
#' @param output_dir Character. The Electron app output directory.
#' @param packages Character vector. DIRECT R package names (may be empty/NULL).
#' @param repos Character vector. CRAN-like repository URLs.
#' @param version Character. Resolved R version (non-NULL from callers).
#' @param platform Character scalar. Target platform ("win"/"mac"/"linux").
#' @param arch Character scalar. Target architecture ("x64"/"arm64").
#' @param verbose Logical. Whether to display progress.
#' @return Invisibly, the path to the embedded `runtime/R` directory.
#' @keywords internal
embed_r_runtime <- function(output_dir, packages, repos, version,
                            platform, arch, verbose = TRUE) {
  if (verbose) cli::cli_alert_info("Embedding R runtime for bundled strategy...")

  # Resolve the effective version ONCE and pass it to both install_r_portable and
  # r_executable, replacing the two independent NULL-fallbacks that could
  # otherwise make two GitHub API calls that disagree.
  effective_version <- version %||% r_portable_latest_version(platform)

  r_path <- install_r_portable(
    version = effective_version,
    platform = platform,
    arch = arch,
    verbose = verbose
  )

  # Copy runtime into the Electron app
  runtime_dest <- fs::path(output_dir, "runtime", "R")
  copy_dir_contents(r_path, runtime_dest)

  # Resolve symlinks that point outside the package directory.
  # Portable R may contain fontconfig symlinks pointing to system R,
  # which electron-builder refuses to package (security protection).
  runtime_files <- list.files(runtime_dest, recursive = TRUE,
                              full.names = TRUE, all.files = TRUE)
  for (f in runtime_files) {
    if (nzchar(Sys.readlink(f))) {
      abs_target <- normalizePath(f, mustWork = FALSE)
      if (file.exists(abs_target)) {
        file.remove(f)
        file.copy(abs_target, f, copy.date = TRUE)
      } else {
        # Dead symlink -- remove it
        file.remove(f)
      }
    }
  }

  # Install packages using the BUNDLED portable R itself (not the cache,
  # and not system R). This ensures binary packages are linked against
  # matching dylibs AND are installed into the exact library the app
  # will load from at runtime.
  if (length(packages) > 0) {

    # Install into a SIBLING library directory (runtime_dest/library/),
    # NOT into portable-r-*/library/. On macOS, installing into the
    # bundled R's own library triggers hardened-runtime library
    # validation at dyn.load() time, causing segfaults on unsigned
    # CRAN binaries. The sibling-library layout avoids that; the
    # Electron runtime (native-r.js) prepends this path to .libPaths().
    lib_path <- fs::path(runtime_dest, "library")
    fs::dir_create(lib_path, recurse = TRUE)

    # Use the CACHED Rscript, not the bundled copy. The cached binary
    # has its original code signature intact; running the copied one
    # on macOS with --vanilla can interact oddly with hardened-runtime
    # library validation.
    bundled_rscript <- r_executable(
      version = effective_version,
      platform = platform,
      arch = arch
    )

    if (is.null(bundled_rscript) || !fs::file_exists(bundled_rscript)) {
      cli::cli_abort(c(
        "Could not locate the cached portable Rscript",
        "i" = "Try: {.code shinyelectron::install_r_portable(force = TRUE)}"
      ))
    }

    if (verbose) cli::cli_alert_info("Installing packages with bundled R...")

    pkgs <- unlist(packages)
    repos <- unlist(repos)
    github_specs <- pkgs[is_github_r_package(pkgs)]
    github_names <- github_r_package_name(github_specs)
    cran_pkgs <- pkgs[!is_github_r_package(pkgs)]

    # Fetch the available-packages database once (avoids repeated CRAN network
    # calls). A GitHub-only dependency set does not need this index at all.
    all_deps <- list()
    if (length(cran_pkgs) > 0) {
      avail_pkgs <- utils::available.packages(repos = repos)
      all_deps <- tools::package_dependencies(
        cran_pkgs, db = avail_pkgs,
        which = c("Depends", "Imports", "LinkingTo"),
        recursive = TRUE
      )
    }
    all_pkgs <- unique(c(cran_pkgs, unlist(all_deps)))

    # Skip packages already present in the bundled library. Portable-R
    # ships with base + recommended + a few extras; reinstalling them is
    # wasteful and, on Windows, tripped "cannot remove prior installation"
    # errors when antivirus held file handles on freshly-extracted DLLs.
    pre_installed <- list.dirs(lib_path, recursive = FALSE, full.names = FALSE)
    pre_installed <- pre_installed[nzchar(pre_installed)]
    all_pkgs <- setdiff(all_pkgs, pre_installed)
    github_specs <- github_specs[!github_names %in% pre_installed]

    if (length(all_pkgs) == 0 && length(github_specs) == 0) {
      if (verbose) cli::cli_alert_info("All dependencies already present in bundled R library")
    } else {
      if (verbose) {
        install_count <- length(all_pkgs) + length(github_specs)
        cli::cli_alert_info("Installing {install_count} package{?s} into bundled library")
      }

      r_literal <- function(x) {
        paste(deparse(as.character(x), width.cutoff = 500L), collapse = "")
      }
      # type = "binary" is unsupported on Linux (.Platform$pkgType ==
      # "source"); only request it on the platforms that accept it.
      # Keyed on the build HOST pkgType (detect_current_platform()), not the
      # target `platform` argument; they coincide for any successful build.
      type_clause <- if (identical(detect_current_platform(), "linux")) {
        ""
      } else {
        "type = 'binary', "
      }
      # Use the bundled library as both destination AND the first lib on
      # .libPaths. GitHub references are installed with pak; ordinary package
      # names retain the existing install.packages path.
      lib_literal <- r_literal(gsub("\\\\", "/", lib_path))
      repo_literal <- r_literal(repos)
      r_steps <- c(
        sprintf(".libPaths(%s)", lib_literal),
        sprintf("options(repos = %s)", repo_literal)
      )
      if (length(all_pkgs) > 0) {
        r_steps <- c(r_steps, sprintf(
          paste0(
            "install.packages(%s, lib = %s, repos = %s, ",
            "%sdependencies = FALSE, quiet = TRUE)"
          ),
          r_literal(all_pkgs), lib_literal, repo_literal, type_clause
        ))
      }
      if (length(github_specs) > 0) {
        r_steps <- c(
          r_steps,
          sprintf(
            paste0(
              "if (!requireNamespace('pak', quietly = TRUE, lib.loc = %s)) ",
              "install.packages('pak', lib = %s, repos = %s, %squiet = TRUE)"
            ),
            lib_literal, lib_literal, repo_literal, type_clause
          ),
          sprintf(
            paste0(
              "if (!nzchar(Sys.getenv('GITHUB_PAT')) && nzchar(Sys.getenv('GH_TOKEN'))) ",
              "Sys.setenv(GITHUB_PAT = Sys.getenv('GH_TOKEN'))"
            )
          ),
          sprintf(
            "pak::pkg_install(%s, lib = %s, dependencies = NA, upgrade = FALSE, ask = FALSE)",
            r_literal(github_specs), lib_literal
          )
        )
      }
      r_code <- paste(r_steps, collapse = "; ")

      # Pre-session code didn't scrub env or pass --vanilla and worked
      # fine -- the bundled library being a sibling (not the R's own
      # library) means R_LIBS_USER contamination doesn't override our
      # explicit lib_path argument to install.packages.
      result <- processx::run(
        bundled_rscript, c("-e", r_code),
        error_on_status = FALSE,
        echo = verbose,
        timeout = 600
      )

      # Verify every app-direct package is present in the bundled library
      # after install. A post-install check is far easier to diagnose
      # than "no package called 'htmltools'" from a running Shiny server.
      present <- c(pre_installed,
                   list.dirs(lib_path, recursive = FALSE, full.names = FALSE))
      expected_pkgs <- unique(c(cran_pkgs, github_names))
      missing_pkgs <- setdiff(expected_pkgs, present)
      if (length(missing_pkgs) > 0) {
        cli::cli_abort(c(
          "Failed to install bundled R packages: {paste(missing_pkgs, collapse = ', ')}",
          "i" = "install.packages exit code: {result$status}",
          "x" = "stderr: {trimws(result$stderr %||% '')}"
        ))
      }

    }
  }

  if (verbose) cli::cli_alert_success("Embedded R runtime")
  invisible(runtime_dest)
}

#' Embed a portable Python runtime into a bundled Electron build
#'
#' Behavior-preserving extraction of the Python bundled-embedding block from
#' [build_electron_app()]. ALWAYS installs + copies the interpreter so the shared
#' `runtime/Python` path exists for suite-wide bundled detection; only the pip
#' install is gated on a non-empty `packages` set. Warn-only (not abort) on pip
#' failure; the result is not verified, matching the original block. Reproduces
#' the three `output_dir`-derived paths and the unix-only fallback glob so the
#' `native-py.js` `sys.path` expectations hold.
#'
#' @param output_dir Character. The Electron app output directory.
#' @param packages Character vector. Python package specs (may be empty/NULL).
#' @param index_urls Character vector. PyPI-like index URLs.
#' @param version Character. Resolved Python version (non-NULL from callers).
#' @param platform Character scalar. Target platform.
#' @param arch Character scalar. Target architecture.
#' @param verbose Logical. Whether to display progress.
#' @return Invisibly, the path to the embedded `runtime/Python` directory.
#' @keywords internal
embed_python_runtime <- function(output_dir, packages, index_urls, version,
                                 platform, arch, verbose = TRUE) {
  if (verbose) cli::cli_alert_info("Embedding Python runtime for bundled strategy...")

  # Resolve the effective version ONCE and pass it to both install_python_standalone and
  # python_executable.
  effective_version <- version %||% SHINYELECTRON_DEFAULTS$runtime_versions$python$version

  py_path <- install_python_standalone(
    version = effective_version,
    platform = platform,
    arch = arch,
    verbose = verbose
  )

  runtime_dest <- fs::path(output_dir, "runtime", "Python")
  copy_dir_contents(py_path, runtime_dest)

  # Install packages using the BUNDLED Python (not system Python) so
  # C extensions match the bundled Python version's ABI
  bundled_python <- python_executable(effective_version, platform, arch)
  if (is.null(bundled_python)) {
    # Fall back to searching the copied runtime
    bundled_python <- Sys.glob(fs::path(runtime_dest, "*", "python", "bin", "python3"))[1]
  }

  if (!is.null(bundled_python) && length(packages) > 0) {
    index_url <- unlist(index_urls)[1] %||% "https://pypi.org/simple"
    pip_args <- c("-m", "pip", "install", "--only-binary", ":all:",
                 "-i", index_url,
                 "--target", fs::path(runtime_dest, "lib", "python", "site-packages"),
                 unlist(packages))
    if (verbose) {
      cli::cli_alert_info("Installing Python packages using bundled Python...")
    }
    pip_result <- processx::run(
      bundled_python, pip_args,
      echo = verbose, spinner = verbose,
      error_on_status = FALSE, timeout = 600
    )
    if (pip_result$status != 0) {
      cli::cli_warn(c(
        "Failed to install some Python packages",
        "x" = "Error: {pip_result$stderr}"
      ))
    }
  }

  if (verbose) cli::cli_alert_success("Embedded Python runtime")
  invisible(runtime_dest)
}
