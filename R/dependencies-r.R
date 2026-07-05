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
  explicit <- r_dependency_source_package_names(config_deps)

  pak_specs <- c(declared, extra, explicit)
  pak_specs <- pak_specs[is_pak_r_package(pak_specs)]
  detected <- setdiff(detected, r_package_name(pak_specs))

  packages <- if (isTRUE(config_deps$auto_detect %||% TRUE)) {
    sort(unique(c(detected, declared, extra, explicit)))
  } else {
    sort(unique(c(declared, extra, explicit)))
  }

  list(
    packages = packages,
    repos = repos,
    dependency_sources = r_dependency_source_overrides(config_deps)
  )
}

#' Apply explicit R dependency source arguments to config
#' @keywords internal
apply_r_dependency_source_args <- function(config,
                                           GitHub_Packages = NULL,
                                           GitHub_Refs = NULL,
                                           URL_Packages = NULL,
                                           URL_INSTALL_opts = NULL,
                                           Local_Packages = NULL) {
  config$dependencies <- config$dependencies %||% SHINYELECTRON_DEFAULTS$dependencies

  check_named_character <- function(x, arg) {
    if (is.null(x)) return(NULL)
    if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
      cli::cli_abort("{.arg {arg}} must be a named character vector.")
    }
    x
  }
  GitHub_Packages <- check_named_character(GitHub_Packages, "GitHub_Packages")
  GitHub_Refs <- check_named_character(GitHub_Refs, "GitHub_Refs")
  URL_Packages <- check_named_character(URL_Packages, "URL_Packages")
  Local_Packages <- check_named_character(Local_Packages, "Local_Packages")

  if (!is.null(URL_INSTALL_opts) &&
      (!is.list(URL_INSTALL_opts) || is.null(names(URL_INSTALL_opts)))) {
    cli::cli_abort("{.arg URL_INSTALL_opts} must be a named list.")
  }

  if (!is.null(GitHub_Packages)) config$dependencies$GitHub_Packages <- as.list(GitHub_Packages)
  if (!is.null(GitHub_Refs)) config$dependencies$GitHub_Refs <- as.list(GitHub_Refs)
  if (!is.null(URL_Packages)) config$dependencies$URL_Packages <- as.list(URL_Packages)
  if (!is.null(URL_INSTALL_opts)) config$dependencies$URL_INSTALL_opts <- URL_INSTALL_opts
  if (!is.null(Local_Packages)) config$dependencies$Local_Packages <- as.list(Local_Packages)

  config
}

#' Package names declared through explicit dependency source maps
#' @keywords internal
r_dependency_source_package_names <- function(config_deps) {
  unique(c(
    names(config_deps$GitHub_Packages %||% list()),
    names(config_deps$URL_Packages %||% list()),
    names(config_deps$Local_Packages %||% list())
  ))
}

#' Extract per-package R dependency source overrides from config
#'
#' Supports app-level entries such as:
#' dependencies:
#'   AutoPlots:
#'     source: local
#'     path: C:/path/to/AutoPlots
#'     fallback_to_cran: false
#'     force: true
#' @keywords internal
r_dependency_source_overrides <- function(config_deps) {
  overrides <- list()

  add_github <- function(package_name) {
    ref <- if (package_name %in% names(config_deps$GitHub_Refs %||% list())) {
      unname(unlist(config_deps$GitHub_Refs[[package_name]]))
    } else {
      NULL
    }
    overrides[[package_name]] <<- Filter(Negate(is.null), list(
      source = "github",
      repo = unname(unlist(config_deps$GitHub_Packages[[package_name]])),
      ref = ref,
      fallback_to_cran = FALSE,
      force = TRUE
    ))
  }
  add_url <- function(package_name) {
    opts <- NULL
    if (package_name %in% names(config_deps$URL_INSTALL_opts %||% list())) {
      opts <- unlist(config_deps$URL_INSTALL_opts[[package_name]], use.names = FALSE)
    }
    overrides[[package_name]] <<- NULL
    overrides[[package_name]] <<- list(
      source = "url",
      url = unname(unlist(config_deps$URL_Packages[[package_name]])),
      install_opts = as.list(opts %||% character()),
      fallback_to_cran = FALSE,
      force = FALSE
    )
  }
  add_local <- function(package_name) {
    overrides[[package_name]] <<- NULL
    overrides[[package_name]] <<- list(
      source = "local",
      path = unname(unlist(config_deps$Local_Packages[[package_name]])),
      fallback_to_cran = FALSE,
      force = TRUE
    )
  }

  for (package_name in names(config_deps$GitHub_Packages %||% list())) add_github(package_name)
  for (package_name in names(config_deps$URL_Packages %||% list())) add_url(package_name)
  for (package_name in names(config_deps$Local_Packages %||% list())) add_local(package_name)

  known <- c(
    "auto_detect", "r", "python", "electron", "system_packages",
    "extra_packages", "GitHub_Packages", "GitHub_Refs",
    "URL_Packages", "URL_INSTALL_opts", "Local_Packages"
  )
  package_keys <- setdiff(names(config_deps %||% list()), known)
  for (package_name in package_keys) {
    if (package_name %in% names(overrides)) {
      next
    }
    entry <- config_deps[[package_name]]
    if (!is.list(entry) || is.null(entry$source)) {
      next
    }

    source <- tolower(as.character(entry$source))
    if (!source %in% c("local", "github", "url", "cran", "none", "already_installed")) {
      cli::cli_abort(c(
        "Invalid dependency source for {.pkg {package_name}}: {.val {source}}",
        "i" = "Use one of: local, url, github, cran, none, already_installed."
      ))
    }

    overrides[[package_name]] <- list(
      source = source,
      path = entry$path %||% NULL,
      url = entry$url %||% NULL,
      install_opts = as.list(entry$install_opts %||% character()),
      repo = entry$repo %||% entry$github %||% NULL,
      ref = entry$ref %||% NULL,
      fallback_to_cran = isTRUE(entry$fallback_to_cran),
      force = isTRUE(entry$force)
    )
  }

  overrides
}

#' Resolve local dependency source paths relative to the app directory
#' @keywords internal
resolve_r_dependency_source_paths <- function(dependency_sources, appdir) {
  if (is.null(dependency_sources) || !length(dependency_sources)) {
    return(list())
  }

  for (package_name in names(dependency_sources)) {
    entry <- dependency_sources[[package_name]]
    if (!identical(entry$source, "local")) {
      next
    }

    package_path <- entry$path
    if (is.null(package_path) || !nzchar(package_path)) {
      cli::cli_abort(c(
        "{.pkg {package_name}} is configured as a local dependency but no path was supplied.",
        "x" = "{package_name} cannot be installed from CRAN when local source is required."
      ))
    }

    if (!fs::is_absolute_path(package_path)) {
      package_path <- fs::path(appdir, package_path)
    }
    package_path <- fs::path_abs(package_path)

    if (!fs::file_exists(package_path) && !fs::dir_exists(package_path)) {
      msg <- paste0(
        package_name,
        " is configured as a local dependency but the path does not exist: ",
        package_path,
        ". "
      )
      if (identical(package_name, "AutoQuant")) {
        msg <- paste0(msg, "AutoQuant is not available on CRAN and ")
      }
      cli::cli_abort(paste0(msg, "CRAN fallback is disabled."))
    }

    entry$path <- gsub("\\\\", "/", package_path)
    dependency_sources[[package_name]] <- entry
  }

  dependency_sources
}
