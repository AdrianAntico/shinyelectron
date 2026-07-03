# Base and recommended packages that ship with R -- never need to be installed
BASE_R_PACKAGES <- c(
  "base", "compiler", "datasets", "grDevices", "graphics", "grid",
  "methods", "parallel", "splines", "stats", "stats4", "tcltk",
  "tools", "utils",
  "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
  "lattice", "MASS", "Matrix", "mgcv", "nlme", "nnet", "rpart",
  "spatial", "survival"
)

#' Identify a GitHub R package reference
#'
#' Supports pak-style references such as `github::owner/repo`, an optional
#' ref (`@v1.2.0`), and an optional package-name alias
#' (`mypkg=github::owner/repo`).
#' @keywords internal
is_github_r_package <- function(x) {
  grepl("^(?:[^=]+[=])?(?:github::|https?://github[.]com/)", x,
        perl = TRUE)
}

#' Identify a local R package reference
#' @keywords internal
is_local_r_package <- function(x) {
  grepl("^[^=]+[=]local::", x)
}

#' Identify an R package reference installed by pak
#' @keywords internal
is_pak_r_package <- function(x) {
  is_github_r_package(x) | is_local_r_package(x)
}

#' Infer the installed package name from a GitHub package reference
#' @keywords internal
github_r_package_name <- function(x) {
  aliased <- grepl("=", x, fixed = TRUE)
  alias <- ifelse(aliased, sub("=.*$", "", x), NA_character_)
  ref <- sub("^[^=]+=", "", x)
  ref <- sub("^github::", "", ref)
  ref <- sub("^https?://github[.]com/", "", ref)
  ref <- sub("[?#].*$", "", ref)
  ref <- sub("@[^/]*$", "", ref)
  repo <- basename(ref)
  ifelse(aliased, alias, repo)
}

#' Return the installed package name for any supported package declaration
#' @keywords internal
r_package_name <- function(x) {
  result <- x
  pak_ref <- is_pak_r_package(x)
  result[pak_ref] <- ifelse(
    is_local_r_package(x[pak_ref]),
    sub("=.*$", "", x[pak_ref]),
    github_r_package_name(x[pak_ref])
  )
  result
}

#' Resolve local package paths relative to the Shiny application
#' @keywords internal
resolve_local_r_packages <- function(packages, appdir) {
  local <- is_local_r_package(packages)
  if (!any(local)) return(packages)

  packages[local] <- vapply(packages[local], function(spec) {
    package_name <- sub("=.*$", "", spec)
    package_path <- sub("^[^=]+[=]local::", "", spec)
    if (!fs::is_absolute_path(package_path)) {
      package_path <- fs::path(appdir, package_path)
    }
    package_path <- fs::path_abs(package_path)
    if (!fs::file_exists(package_path) && !fs::dir_exists(package_path)) {
      cli::cli_abort(c(
        "Local R package does not exist: {.path {package_path}}",
        "i" = "Local package paths are resolved relative to {.path {appdir}}."
      ))
    }
    paste0(package_name, "=local::", gsub("\\\\", "/", package_path))
  }, character(1))
  packages
}

#' Detect R package dependencies from source files
#'
#' Uses `renv::dependencies()` to scan R source files for package
#' references. This catches `library()`, `require()`,
#' `pkg::func()`, `loadNamespace()`, and other patterns.
#'
#' @param appdir Character string. Path to the app directory.
#' @return Character vector of unique package names (sorted), excluding
#'   base and recommended R packages.
#' @keywords internal
detect_r_dependencies <- function(appdir) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg renv} package is required to detect R dependencies",
      "i" = "Install with: {.code install.packages('renv')}"
    ))
  }

  deps_df <- tryCatch(
    renv::dependencies(appdir, quiet = TRUE),
    error = function(e) {
      cli::cli_warn(c(
        "Failed to detect R dependencies",
        "x" = "Error: {e$message}",
        "i" = "Falling back to empty dependency list"
      ))
      data.frame(Package = character(0))
    }
  )

  packages <- unique(deps_df$Package)
  packages <- setdiff(packages, BASE_R_PACKAGES)
  sort(packages)
}

#' Merge detected R dependencies with config declarations
#'
#' Combines auto-detected packages with user-declared packages from config.
#' When auto_detect is FALSE, only declared packages are used.
#'
#' @param detected Character vector of detected package names.
#' @param config_deps List from config$dependencies.
#' @return List with `packages` (character vector) and `repos` (list).
#' @keywords internal
merge_r_dependencies <- function(detected, config_deps) {
  repos <- config_deps$r$repos %||% SHINYELECTRON_DEFAULTS$dependencies$r$repos

  declared <- unlist(config_deps$r$packages %||% list())
  extra <- unlist(config_deps$extra_packages %||% list())

  pak_specs <- c(declared, extra)
  pak_specs <- pak_specs[is_pak_r_package(pak_specs)]
  detected <- setdiff(detected, r_package_name(pak_specs))

  packages <- if (isTRUE(config_deps$auto_detect %||% TRUE)) {
    sort(unique(c(detected, declared, extra)))
  } else {
    sort(unique(c(declared, extra)))
  }

  list(packages = packages, repos = repos)
}
