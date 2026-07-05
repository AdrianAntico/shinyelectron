// Dependency checker -- checks and installs missing R/Python packages at launch
const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');
const crypto = require('crypto');
const { checkManifestSchema } = require('./utils');

/**
 * Read the dependencies manifest from the app directory.
 * @param {string} appPath - Path to the app directory.
 * @returns {object|null} Parsed manifest or null if not found.
 */
function readManifest(appPath) {
  const manifestPath = path.join(appPath, 'dependencies.json');
  if (!fs.existsSync(manifestPath)) return null;
  try {
    const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
    checkManifestSchema(manifest, 'dependencies');
    return manifest;
  } catch (err) {
    console.warn('Failed to read dependencies.json:', err.message);
    return null;
  }
}

/**
 * Check which R packages are missing.
 * @param {string[]} packages - Package names to check.
 * @param {string} rscript - Path to Rscript executable.
 * @param {string|null} libPath - Library path to check (null = R default).
 * @returns {Promise<string[]>} List of missing package names.
 */
async function checkMissingR(packages, rscript, libPath) {
  if (packages.length === 0) return [];

  // Sanitize package names -- strip anything that isn't alphanumeric, dot, or dash
  const pkgList = packages.map(p => `"${p.replace(/[^a-zA-Z0-9._-]/g, '')}"`).join(',');
  let rCode;
  if (libPath) {
    // Escape backslashes and double quotes in libPath to prevent R code injection
    const safeLibPath = libPath.replace(/\\/g, '/').replace(/"/g, '\\"');
    // Use base R only -- no jsonlite required -- so this works with a bare R
    // (e.g. system strategy with minimal packages).  cat() prints one package
    // name per line; JS splits on newlines to get the missing list.
    rCode = `cat(setdiff(c(${pkgList}), rownames(installed.packages(lib.loc="${safeLibPath}"))), sep="\\n")`;
  } else {
    rCode = `cat(setdiff(c(${pkgList}), rownames(installed.packages())), sep="\\n")`;
  }

  try {
    const result = execFileSync(rscript, ['-e', rCode], {
      encoding: 'utf8',
      timeout: 30000,
      stdio: ['ignore', 'pipe', 'pipe']
    });
    // Split on newlines; filter empty strings produced by a trailing newline
    const lines = result.trim().split(/\r?\n/).filter(l => l.length > 0);
    return lines;
  } catch (err) {
    console.warn('Failed to check R packages:', err.message);
    return packages;
  }
}

/**
 * Check which Python packages are missing.
 * @param {string[]} packages - Package names to check.
 * @param {string} python - Path to Python executable.
 * @returns {Promise<string[]>} List of missing package names.
 */
async function checkMissingPy(packages, python) {
  if (packages.length === 0) return [];

  // Use pip show to check installed packages (handles module-name mismatches
  // like opencv-python→cv2 and scikit-learn→sklearn, which importlib can't)
  const crypto = require('crypto');
  const tmpFile = path.join(os.tmpdir(), `shinyelectron-check-${crypto.randomBytes(8).toString('hex')}.py`);
  const pyScript = `import json, subprocess, sys
pkgs = ${JSON.stringify(packages)}
missing = []
for p in pkgs:
    result = subprocess.run(
        [sys.executable, '-m', 'pip', 'show', p],
        capture_output=True, timeout=10
    )
    if result.returncode != 0:
        missing.append(p)
print(json.dumps(missing))
`;
  fs.writeFileSync(tmpFile, pyScript);

  try {
    const result = execFileSync(python, [tmpFile], {
      encoding: 'utf8',
      timeout: 60000,
      stdio: ['ignore', 'pipe', 'pipe']
    });
    fs.unlinkSync(tmpFile);
    return JSON.parse(result.trim());
  } catch (err) {
    try { fs.unlinkSync(tmpFile); } catch { /* ignore */ }
    console.warn('Failed to check Python packages:', err.message);
    return packages;
  }
}

/**
 * Install missing R packages (binary only).
 * @param {string[]} packages - Packages to install.
 * @param {string[]} repos - CRAN-like repository URLs.
 * @param {string} rscript - Path to Rscript.
 * @param {string|null} libPath - Target library path (null = R default).
 * @param {function} onProgress - Callback: (packageName, index, total) => void
 * @returns {Promise<{success: boolean, error?: string}>}
 */
function installR(packages, repos, rscript, libPath, onProgress, packageSources = {}) {
  return installRWithSources(packages, repos, rscript, libPath, onProgress, packageSources);
}

function rString(value) {
  return `"${String(value || '').replace(/\\/g, '/').replace(/"/g, '\\"')}"`;
}

function rLibSetupCode(libPath) {
  const requestedLib = libPath ? rString(libPath) : 'NULL';
  return [
    `lib <- ${requestedLib}`,
    `if (is.null(lib) || !nzchar(lib)) lib <- Sys.getenv("R_LIBS_USER")`,
    `if (!nzchar(lib)) lib <- file.path(path.expand("~"), "R", paste0(R.version$platform, "-library"), paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = "."))`,
    `dir.create(lib, recursive = TRUE, showWarnings = FALSE)`,
    `.libPaths(unique(c(lib, .libPaths())))`
  ].join('; ');
}

function normalizeSourceOverride(pkg, packageSources) {
  const override = packageSources && packageSources[pkg] ? packageSources[pkg] : null;
  if (!override || typeof override !== 'object') {
    return { source: 'cran', fallback_to_cran: true, force: false };
  }

  const source = String(override.source || 'cran').toLowerCase();
  return {
    source,
    path: override.path || null,
    url: override.url || null,
    install_opts: Array.isArray(override.install_opts) ? override.install_opts : [],
    repo: override.repo || override.github || null,
    ref: override.ref || null,
    fallback_to_cran: override.fallback_to_cran === true,
    force: override.force === true
  };
}

function rValidationCode(safePkg, sourceLabel, commandLabel) {
  const checks = {
    AutoPlots: ['Line', 'Bar', 'CorrMatrix'],
    AutoQuant: [
      'generate_eda_artifacts',
      'generate_model_assessment_artifacts',
      'generate_regression_model_insights_artifacts',
      'generate_binary_classification_model_insights_artifacts'
    ]
  };
  const fnChecks = checks[safePkg] || [];
  const fnCode = fnChecks.map(fn =>
    `if (!exists(${rString(fn)}, envir = asNamespace(pkg), inherits = FALSE)) failed <- c(failed, paste0("missing function: ", ${rString(fn)}))`
  );

  return [
    `failed <- character()`,
    `if (!requireNamespace(pkg, quietly = TRUE)) failed <- c(failed, "requireNamespace failed")`,
    `if (length(failed) == 0) {`,
    `  version <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)`,
    `  path_found <- tryCatch(find.package(pkg), error = function(e) NA_character_)`,
    `  if (is.na(version) || !nzchar(version)) failed <- c(failed, "packageVersion failed")`,
    `  if (is.na(path_found) || !nzchar(path_found)) failed <- c(failed, "find.package failed")`,
    ...fnCode.map(line => `  ${line}`),
    `}`,
    `if (length(failed) > 0) {`,
    `  detail <- c("Package validation failed for ${safePkg}", paste0("source: ", ${rString(sourceLabel)}), paste0("command: ", ${rString(commandLabel)}), paste0(".libPaths(): ", paste(.libPaths(), collapse = " | ")))`,
    `  if (exists("install_error") && length(install_error) && !is.null(install_error)) detail <- c(detail, paste0("install error: ", install_error))`,
    `  detail <- c(detail, paste0("failed checks: ", paste(failed, collapse = "; ")))`,
    `  stop(paste(detail, collapse = "\\n"), call. = FALSE)`,
    `}`,
    `if (exists("install_error") && length(install_error) && !is.null(install_error)) message("${safePkg} install emitted an error/warning, but validation passed: ", install_error)`,
    `message("${safePkg} installed with warnings/output; validation passed.")`,
    `message("Validated ", pkg, " version ", version, " at ", path_found)`,
    `quit(status = 0, save = "no")`
  ].join('; ');
}

function runRScriptCode(rscript, rCode, timeout = 300000) {
  const tmpFile = path.join(os.tmpdir(), `shinyelectron-r-install-${crypto.randomBytes(8).toString('hex')}.R`);
  fs.writeFileSync(tmpFile, rCode, 'utf8');
  try {
    return execFileSync(rscript, [tmpFile], {
      timeout,
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'pipe']
    });
  } finally {
    try { fs.unlinkSync(tmpFile); } catch { /* ignore cleanup errors */ }
  }
}

function installRWithSources(packages, repos, rscript, libPath, onProgress, packageSources) {
  return new Promise((resolve) => {
    const sourceOrder = Object.keys(packageSources || {});
    const packageSet = new Set(packages);
    const orderedSourcePackages = sourceOrder.filter(pkg => packageSet.has(pkg));
    const remainingPackages = packages.filter(pkg => !sourceOrder.includes(pkg));
    packages = [...orderedSourcePackages, ...remainingPackages];

    // Sanitize repo URLs -- only allow URL-safe characters
    const repoStr = repos.map(r => `"${r.replace(/"/g, '')}"`).join(',');
    const libSetup = rLibSetupCode(libPath);
    const libArg = ', lib=lib';

    // Create lib directory if needed
    if (libPath) {
      fs.mkdirSync(libPath, { recursive: true });
    }

    let completed = 0;
    const total = packages.length;

    function installNext() {
      if (completed >= total) {
        resolve({ success: true });
        return;
      }

      const pkg = packages[completed];
      if (onProgress) onProgress(pkg, completed, total);

      // Sanitize package name before interpolating into R code
      const safePkg = pkg.replace(/[^a-zA-Z0-9._-]/g, '');
      const source = normalizeSourceOverride(pkg, packageSources);
      // type="binary" is unsupported on Linux (.Platform$pkgType == "source");
      // let R pick the platform default there to avoid an install error.
      const typeArg = process.platform === 'linux' ? '' : ', type="binary"';
      let rCode;
      let sourceLabel = 'cran';
      let commandLabel = `install.packages("${safePkg}")`;

      if (source.source === 'local') {
        if (!source.path) {
          resolve({
            success: false,
            error: `${pkg} is configured as a local dependency but no path was supplied. CRAN fallback is disabled.`
          });
          return;
        }
        const friendly = pkg === 'AutoQuant'
          ? `${pkg} is configured as a local dependency but the path does not exist: ${source.path}. AutoQuant is not available on CRAN and CRAN fallback is disabled.`
          : `${pkg} is configured as a local dependency but the path does not exist: ${source.path}. CRAN fallback is disabled.`;
        console.log(`[shinyelectron] Installing ${safePkg} from local path: ${source.path}`);
        console.log(`[shinyelectron] ${safePkg} source=local fallback_to_cran=${source.fallback_to_cran}`);
        sourceLabel = `local:${source.path}`;
        commandLabel = `remotes::install_local(path = ${source.path})`;
        rCode = [
          `pkg <- ${rString(safePkg)}`,
          `path <- ${rString(source.path)}`,
          libSetup,
          `if (!file.exists(path) && !dir.exists(path)) stop(${rString(friendly)}, call. = FALSE)`,
          `if (requireNamespace(pkg, quietly = TRUE)) { desc <- utils::packageDescription(pkg); message("Installed ", pkg, " version ", desc$Version, " at ", find.package(pkg)) }`,
          `message("Installing ${safePkg} from local path: ", path)`,
          `message("CRAN fallback ${source.fallback_to_cran ? 'enabled' : 'disabled'}")`,
          `if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos=c(${repoStr}), quiet=TRUE, dependencies=TRUE, lib=lib)`,
          `install_error <- NULL`,
          `tryCatch(remotes::install_local(path = path, dependencies = TRUE, upgrade = "never", force = ${source.force ? 'TRUE' : 'FALSE'}, lib = lib), error = function(e) {`,
          source.fallback_to_cran
            ? `  tryCatch(install.packages(pkg, repos=c(${repoStr})${typeArg}, quiet=TRUE, dependencies=TRUE${libArg}), error = function(e2) install_error <<- paste(conditionMessage(e), conditionMessage(e2), sep = "; "))`
            : `  install_error <<- paste("Failed to install ${safePkg} from local path ${source.path}. CRAN fallback is disabled for this package.", conditionMessage(e))`,
          `})`,
          rValidationCode(safePkg, sourceLabel, commandLabel)
        ].join('; ');
      } else if (source.source === 'github') {
        if (!source.repo) {
          resolve({
            success: false,
            error: `${pkg} is configured as a GitHub dependency but no repo was supplied. CRAN fallback is disabled.`
          });
          return;
        }
        const refArg = source.ref ? `, ref = ${rString(source.ref)}` : '';
        console.log(`[shinyelectron] Installing ${safePkg} from GitHub: ${source.repo}`);
        console.log(`[shinyelectron] ${safePkg} source=github fallback_to_cran=${source.fallback_to_cran}`);
        sourceLabel = `github:${source.repo}${source.ref ? '@' + source.ref : ''}`;
        commandLabel = `remotes::install_github(repo = ${source.repo}${source.ref ? ', ref = ' + source.ref : ''})`;
        rCode = [
          `pkg <- ${rString(safePkg)}`,
          `repo <- ${rString(source.repo)}`,
          libSetup,
          `if (requireNamespace(pkg, quietly = TRUE)) { desc <- utils::packageDescription(pkg); message("Installed ", pkg, " version ", desc$Version, " at ", find.package(pkg)) }`,
          `message("Installing ${safePkg} from GitHub: ", repo)`,
          `message("CRAN fallback ${source.fallback_to_cran ? 'enabled' : 'disabled'}")`,
          `if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos=c(${repoStr}), quiet=TRUE, dependencies=TRUE, lib=lib)`,
          `install_error <- NULL`,
          `tryCatch(remotes::install_github(repo = repo${refArg}, dependencies = TRUE, upgrade = "never", force = ${source.force ? 'TRUE' : 'FALSE'}, lib = lib), error = function(e) {`,
          source.fallback_to_cran
            ? `  tryCatch(install.packages(pkg, repos=c(${repoStr})${typeArg}, quiet=TRUE, dependencies=TRUE${libArg}), error = function(e2) install_error <<- paste(conditionMessage(e), conditionMessage(e2), sep = "; "))`
            : `  install_error <<- paste("Failed to install ${safePkg} from GitHub repo ${source.repo}. CRAN fallback is disabled for this package.", conditionMessage(e))`,
          `})`,
          rValidationCode(safePkg, sourceLabel, commandLabel)
        ].join('; ');
      } else if (source.source === 'url') {
        if (!source.url) {
          resolve({
            success: false,
            error: `${pkg} is listed in URL_Packages but no URL was supplied.`
          });
          return;
        }
        const opts = source.install_opts || [];
        const optsCode = opts.length > 0
          ? `c(${opts.map(rString).join(', ')})`
          : 'NULL';
        console.log(`[shinyelectron] Installing ${safePkg} from URL package: ${source.url}`);
        console.log(`[shinyelectron] ${safePkg} source=url fallback_to_cran=false`);
        sourceLabel = `url:${source.url}`;
        commandLabel = `install.packages(url, repos = NULL, type = "source")`;
        rCode = [
          `pkg <- ${rString(safePkg)}`,
          `url <- ${rString(source.url)}`,
          libSetup,
          `if (requireNamespace(pkg, quietly = TRUE)) { desc <- utils::packageDescription(pkg); message("Installed ", pkg, " version ", desc$Version, " at ", find.package(pkg)) }`,
          `message("Installing ${safePkg} from URL package: ", url)`,
          `message("CRAN fallback disabled")`,
          `install_error <- NULL`,
          `tryCatch(install.packages(url, repos = NULL, type = "source", INSTALL_opts = ${optsCode}${libArg}), error = function(e) install_error <<- paste("${safePkg} is listed in URL_Packages but installation from ${source.url} failed.", conditionMessage(e)))`,
          rValidationCode(safePkg, sourceLabel, commandLabel)
        ].join('; ');
      } else if (source.source === 'none' || source.source === 'already_installed') {
        console.log(`[shinyelectron] Checking ${safePkg}; source=${source.source}; CRAN fallback disabled`);
        sourceLabel = source.source;
        commandLabel = 'already-installed validation';
        rCode = [
          `pkg <- ${rString(safePkg)}`,
          libSetup,
          `if (!requireNamespace("${safePkg}", quietly = TRUE)) stop("${safePkg} is configured as already installed, but it is not available. CRAN fallback is disabled.", call. = FALSE)`,
          rValidationCode(safePkg, sourceLabel, commandLabel)
        ].join('; ');
      } else {
        console.log(`[shinyelectron] Installing ${safePkg} from CRAN repositories`);
        sourceLabel = 'cran';
        commandLabel = `install.packages("${safePkg}")`;
        rCode = [
          `pkg <- ${rString(safePkg)}`,
          libSetup,
          `install_error <- NULL`,
          `tryCatch(install.packages("${safePkg}", repos=c(${repoStr})${typeArg}, quiet=TRUE, dependencies=TRUE${libArg}), error = function(e) install_error <<- conditionMessage(e))`,
          rValidationCode(safePkg, sourceLabel, commandLabel)
        ].join('; ');
      }

      try {
        const output = runRScriptCode(rscript, rCode, 300000);
        if (output && output.trim()) {
          console.log(output.trim());
        }
        completed++;
        installNext();
      } catch (err) {
        const stdout = err.stdout ? err.stdout.toString().trim() : '';
        const stderr = err.stderr ? err.stderr.toString().trim() : '';
        const validationCode = [
          `pkg <- ${rString(safePkg)}`,
          libSetup,
          `install_error <- "Installer process exited non-zero after install output; validation-only retry is checking package availability."`,
          rValidationCode(safePkg, sourceLabel, commandLabel)
        ].join('; ');
        try {
          const validationOutput = runRScriptCode(rscript, validationCode, 60000);
          console.warn(`[shinyelectron] ${safePkg} installer exited non-zero, but post-install validation passed.`);
          if (stdout) console.warn(stdout);
          if (stderr) console.warn(stderr);
          if (validationOutput && validationOutput.trim()) {
            console.log(validationOutput.trim());
          }
          completed++;
          installNext();
          return;
        } catch (validationErr) {
          const validationStdout = validationErr.stdout ? validationErr.stdout.toString().trim() : '';
          const validationStderr = validationErr.stderr ? validationErr.stderr.toString().trim() : '';
          if (validationStdout || validationStderr) {
            console.warn([
              `[shinyelectron] ${safePkg} validation-only retry failed.`,
              validationStdout ? `validation output:\n${validationStdout}` : '',
              validationStderr ? `validation errors:\n${validationStderr}` : ''
            ].filter(Boolean).join('\n\n'));
          }
        }
        const pieces = [
          `source: ${sourceLabel}`,
          `command: ${commandLabel}`,
          stdout ? `install output:\n${stdout}` : '',
          stderr ? `install/validation errors:\n${stderr}` : '',
          err.message ? `process error: ${err.message}` : ''
        ].filter(Boolean);
        const detail = pieces.join('\n\n');
        resolve({ success: false, error: `Failed to install ${pkg}: ${detail}` });
      }
    }

    installNext();
  });
}

/**
 * Install missing Python packages (binary only).
 * @param {string[]} packages - Packages to install.
 * @param {string[]} indexUrls - PyPI index URLs.
 * @param {string} python - Path to Python.
 * @param {string|null} libPath - Target directory (null = default).
 * @param {function} onProgress - Callback: (packageName, index, total) => void
 * @returns {Promise<{success: boolean, error?: string}>}
 */
function installPy(packages, indexUrls, python, libPath, onProgress) {
  return new Promise((resolve) => {
    if (onProgress) onProgress(packages[0], 0, packages.length);

    // Install using the provided Python (which may be a venv Python
    // created by native-py.js, avoiding PEP 668 errors)
    const installCmd = python;
    const args = ['-m', 'pip', 'install', '--only-binary', ':all:'];
    if (indexUrls && indexUrls.length > 0) {
      args.push('-i', indexUrls[0]);
    }
    if (libPath) {
      fs.mkdirSync(libPath, { recursive: true });
      args.push('--target', libPath);
    }
    args.push(...packages);

    try {
      execFileSync(installCmd, args, {
        timeout: 600000,
        stdio: ['ignore', 'pipe', 'pipe']
      });
      resolve({ success: true });
    } catch (err) {
      const stderr = err.stderr ? err.stderr.toString().trim() : '';
      const detail = stderr || err.message;
      resolve({ success: false, error: `Failed to install packages: ${detail}` });
    }
  });
}

/**
 * Check Linux system dependencies from manifest.
 * @param {object} manifest - Dependencies manifest with system_deps field.
 * @returns {string[]} List of missing system packages.
 */
function checkSystemDeps(manifest) {
  if (process.platform !== 'linux') return [];
  if (!manifest.system_deps) return [];

  let depsToCheck = [];
  try {
    const osRelease = fs.readFileSync('/etc/os-release', 'utf8');
    if (/debian|ubuntu/i.test(osRelease)) {
      depsToCheck = manifest.system_deps.debian || [];
    } else if (/fedora|rhel|centos/i.test(osRelease)) {
      depsToCheck = manifest.system_deps.fedora || [];
    }
  } catch { return []; }

  if (depsToCheck.length === 0) return [];

  const missing = [];
  for (const dep of depsToCheck) {
    try {
      execFileSync('dpkg', ['-s', dep], { stdio: 'ignore' });
    } catch {
      try {
        execFileSync('rpm', ['-q', dep], { stdio: 'ignore' });
      } catch {
        missing.push(dep);
      }
    }
  }
  return missing;
}

// --- Preferences ---

const PREFS_BASE = path.join(os.homedir(), '.shinyelectron', 'apps');

/**
 * Read saved preferences for an app.
 * @param {string} appSlug - App slug identifier.
 * @returns {object|null} Preferences or null.
 */
function readPreferences(appSlug) {
  const prefsPath = path.join(PREFS_BASE, appSlug, 'preferences.json');
  if (!fs.existsSync(prefsPath)) return null;
  try {
    return JSON.parse(fs.readFileSync(prefsPath, 'utf8'));
  } catch { return null; }
}

/**
 * Save preferences for an app.
 * @param {string} appSlug - App slug identifier.
 * @param {object} prefs - Preferences to save.
 */
function savePreferences(appSlug, prefs) {
  const prefsDir = path.join(PREFS_BASE, appSlug);
  fs.mkdirSync(prefsDir, { recursive: true });
  fs.writeFileSync(path.join(prefsDir, 'preferences.json'), JSON.stringify(prefs, null, 2));
}

/**
 * Resolve the library path for package installation.
 * @param {string} appSlug - App slug.
 * @param {object} config - Backend config (may have lib_path).
 * @param {object|null} prefs - Saved preferences.
 * @returns {string|null} Resolved library path, or null for system default.
 */
function resolveLibPath(appSlug, config, prefs) {
  if (prefs && prefs.lib_path) {
    if (prefs.lib_path === 'app-local') {
      return path.join(os.homedir(), '.shinyelectron', 'libraries', appSlug);
    }
    if (prefs.lib_path !== 'system') {
      return prefs.lib_path;
    }
    return null;
  }

  if (config && config.lib_path) {
    if (config.lib_path === 'app-local') {
      return path.join(os.homedir(), '.shinyelectron', 'libraries', appSlug);
    }
    if (config.lib_path !== 'system' && config.lib_path !== null) {
      return config.lib_path;
    }
  }

  return null;
}

module.exports = {
  readManifest,
  checkMissingR,
  checkMissingPy,
  installR,
  installPy,
  checkSystemDeps,
  readPreferences,
  savePreferences,
  resolveLibPath
};
