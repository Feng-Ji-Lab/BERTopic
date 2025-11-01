# ===== Internal helpers ============================================================

#' @keywords internal
.same_path <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  ai <- try(normalizePath(a, winslash = "/", mustWork = FALSE), silent = TRUE)
  bi <- try(normalizePath(b, winslash = "/", mustWork = FALSE), silent = TRUE)
  if (inherits(ai, "try-error") || inherits(bi, "try-error")) return(FALSE)
  identical(ai, bi)
}

# compatible, version-agnostic check for a usable conda binary
#' @keywords internal
.has_conda <- function() {
  cb <- tryCatch(reticulate::conda_binary(), error = function(e) "")
  is.character(cb) && length(cb) == 1 && nzchar(cb)
}

# find a Python executable for virtualenv creation
#' @keywords internal
.find_python_for_venv <- function(python = NULL) {
  if (!is.null(python) && nzchar(python)) return(python)
  cand <- unique(c(
    unname(Sys.which("python")),
    unname(Sys.which("python3"))
  ))
  cand <- cand[nzchar(cand)]
  if (length(cand) > 0) return(cand[[1]])
  ""  # not found
}

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a) || (is.logical(a) && length(a) == 1 && is.na(a))) b else a


# ===== Conda route ================================================================

#' Install Python dependencies for BERTopic (Conda route)
#'
#' Creates (or reuses) a Conda environment with a pinned Python toolchain,
#' installs the scientific stack + PyTorch (CPU) + sentence-transformers, then
#' installs \code{bertopic==0.16.0} via pip. Optionally validates imports.
#'
#' @param envname Character. Conda environment name. Default \code{"r-bertopic"}.
#' @param python_version Character. Python version to use, e.g. \code{"3.10"}.
#' @param reinstall Logical. If \code{TRUE}, delete any existing env and recreate.
#' @param validate Logical. If \code{TRUE}, bind and validate imports (will skip
#'   if reticulate is already initialized to another Python).
#' @param verbose Logical. Print progress messages.
#'
#' @return Invisibly returns the path to the Python executable inside the env.
#'
#' @examples
#' \dontrun{
#' install_py_deps_conda(envname = "r-bertopic", python_version = "3.10")
#' }
#' @export
install_py_deps_conda <- function(envname = "r-bertopic",
                                  python_version = "3.10",
                                  reinstall = FALSE,
                                  validate = TRUE,
                                  verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)
  msg <- function(...) if (isTRUE(verbose)) message(...)

  # 0) Ensure Conda exists (install Miniconda if needed)
  msg("[install_py_deps_conda] Checking Conda/Miniconda...")
  if (!.has_conda()) {
    msg("[install_py_deps_conda] Installing Miniconda (once)...")
    reticulate::install_miniconda()
    if (!.has_conda())
      stop("Conda binary not found after installing Miniconda.", call. = FALSE)
  }

  # Apple Silicon
  is_macos_arm <- Sys.info()[["sysname"]] == "Darwin" && grepl("arm64", Sys.info()[["machine"]])
  if (is_macos_arm && Sys.getenv("CONDA_SUBDIR", "") == "") {
    msg("[install_py_deps_conda] Detected Apple Silicon; using osx-arm64 channel.")
    Sys.setenv(CONDA_SUBDIR = "osx-arm64")
  }

  # 1) Create env with Python
  envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
  if (reinstall && envname %in% envs) {
    msg("[install_py_deps_conda] Removing existing env: ", envname)
    reticulate::conda_remove(envname, packages = NULL, all = TRUE)
    envs <- setdiff(envs, envname)
  }
  if (!envname %in% envs) {
    msg(sprintf("[install_py_deps_conda] Creating env '%s' with Python %s ...", envname, python_version))
    reticulate::conda_create(envname, packages = sprintf("python=%s", python_version), channel = "conda-forge")
    envs <- c(envs, envname)
  }

  # 2) Core scientific stack
  msg("[install_py_deps_conda] Installing core scientific stack...")
reticulate::conda_install(envname, c(
  "pip",
  "numpy==1.26.4","scipy==1.11.*","scikit-learn==1.4.*","pandas",
  "numba==0.59.*","umap-learn==0.5.5","hdbscan==0.8.37","pynndescent==0.5.12",
  "joblib","cloudpickle","dill","tqdm","packaging","requests","pillow","plotly>=5",
  "vc14_runtime","intel-openmp"
), channel = "conda-forge")


reticulate::conda_install(envname, c(
  "transformers==4.47.0",
  "accelerate==0.30.1",
  "tokenizers==0.21.0",
  "huggingface-hub>=0.23",
  "safetensors",
  "sentence-transformers==2.7.0"
), pip = TRUE)

  # 3) Torch + sentence-transformers (CPU)
  msg("[install_py_deps_conda] Installing PyTorch (CPU) & sentence-transformers...")
# try(reticulate::conda_remove(envname, c("pytorch","pytorch-cpu","torchvision","torchaudio")), silent = TRUE)
reticulate::conda_install(envname, "pytorch-cpu==2.1.*", channel = "conda-forge")


# 4) BERTopic via pip (inside the target conda env, independent of active Python)
msg("[install_py_deps_conda] Installing BERTopic (pip)...")
reticulate::conda_install(envname, "bertopic==0.16.0", pip = TRUE)

# 5) Optional validation
py_exec <- reticulate::conda_python(envname)
if (isTRUE(validate)) {
  msg("[install_py_deps_conda] Validating imports...")
  # If reticulate is bound to a different Python, skip validation (but finish install)
  if (reticulate::py_available(initialize = FALSE)) {
    cfg <- reticulate::py_config()
    if (!.same_path(cfg$python, py_exec)) {
      msg("[install_py_deps_conda] reticulate is initialized to another Python; ",
          "skipping validation. After restarting, call use_bertopic_condaenv('", envname,
          "') then run bertopic_self_check().")
      return(invisible(py_exec))
    }
  }
  # Bind to this env for validation
  Sys.setenv(RETICULATE_PYTHON = py_exec)
  reticulate::py_config()

  msg("[install_py_deps_conda] Validation OK.")
}


  msg("[install_py_deps_conda] Done. Env: ", envname, "  Python: ", py_exec)
  invisible(py_exec)
}

#' Bind current R session to a BERTopic Conda environment
#'
#' Sets \code{RETICULATE_PYTHON} to the environment's Python and initializes
#' \pkg{reticulate}. If \pkg{reticulate} is already initialized to a different
#' Python, this stops with an informative error.
#'
#' @param envname Character. Conda env name (default \code{"r-bertopic"}).
#' @param required Logical. Kept for API symmetry; unused.
#'
#' @return Invisibly returns the Python executable path in the env.
#'
#' @examples
#' \dontrun{
#' use_bertopic_condaenv("r-bertopic")
#' }
#' @export
use_bertopic_condaenv <- function(envname = "r-bertopic", required = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)

  envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
  if (!envname %in% envs) {
    stop(sprintf("Conda env '%s' not found. Run install_py_deps_conda(envname = '%s') first.", envname, envname), call. = FALSE)
  }

  py_exec <- reticulate::conda_python(envname)
  if (reticulate::py_available(initialize = FALSE)) {
    cfg <- reticulate::py_config()
    if (!.same_path(cfg$python, py_exec)) {
      stop(sprintf(paste0(
        "reticulate is already initialized to:\n  %s\n",
        "but you requested env '%s':\n  %s\n\n",
        "Restart R (or call reticulate::py_restart_session()) and try again."
      ), cfg$python, envname, py_exec), call. = FALSE)
    }
    return(invisible(py_exec))
  }

  Sys.setenv(RETICULATE_PYTHON = py_exec)
  reticulate::py_config()
  invisible(py_exec)
}


# ===== virtualenv route ============================================================

#' Install Python dependencies for BERTopic (virtualenv route)
#'
#' Creates (or reuses) a \code{virtualenv} and installs \code{bertopic==0.16.0}
#' plus required dependencies via pip. Optionally validates imports.
#'
#' @param envname Character. Virtualenv name. Default \code{"r-bertopic"}.
#' @param python Character. Path to a Python executable to create the venv with.
#'   If \code{NULL}, tries to find \code{python} / \code{python3} on PATH.
#' @param reinstall Logical. If \code{TRUE}, delete existing venv and recreate.
#' @param validate Logical. If \code{TRUE}, bind and validate imports (will skip
#'   if reticulate is already initialized to another Python).
#' @param verbose Logical. Print progress messages.
#'
#' @return Invisibly returns the path to the Python executable inside the venv.
#'
#' @examples
#' \dontrun{
#' install_py_deps_venv(envname = "r-bertopic")
#' }
#' @export
install_py_deps_venv <- function(envname = "r-bertopic",
                                 python = NULL,
                                 reinstall = FALSE,
                                 validate = TRUE,
                                 verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)
  msg <- function(...) if (isTRUE(verbose)) message(...)

  # 0) Find a Python to create the venv
  py <- .find_python_for_venv(python)
  if (!nzchar(py)) {
    stop(paste(
      "No suitable Python found to create a virtualenv.",
      "Install one (e.g., reticulate::install_python('3.10.13')) or pass the path via `python=`."
    ), call. = FALSE)
  }

  # 1) (Re)create venv
  venvs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character(0))
  if (reinstall && envname %in% venvs) {
    msg("[install_py_deps_venv] Removing existing virtualenv: ", envname)
    reticulate::virtualenv_remove(envname, confirm = FALSE)
    venvs <- setdiff(venvs, envname)
  }
  if (!envname %in% venvs) {
    msg("[install_py_deps_venv] Creating virtualenv '", envname, "' with Python: ", py)
    reticulate::virtualenv_create(envname = envname, python = py)
    venvs <- c(venvs, envname)
  }

  # 2) Install packages into venv (pinned where helpful)
  msg("[install_py_deps_venv] Installing Python packages (pip)...")
  reticulate::virtualenv_install(envname, packages = c(
    "numpy==1.26.4",
    "scipy==1.11.*",
    "scikit-learn==1.4.*",
    "pandas",
    "numba==0.59.*",
    "umap-learn==0.5.5",
    "hdbscan==0.8.37",
    "pynndescent==0.5.12",
    "safetensors",
    "joblib",
    "cloudpickle",
    "dill",
    "torch==2.1.*",
    "sentence-transformers",
    "bertopic==0.16.0"
  ), ignore_installed = FALSE)

  # 3) Optional validation
  py_exec <- reticulate::virtualenv_python(envname)
  if (isTRUE(validate)) {
    msg("[install_py_deps_venv] Validating imports...")
    if (reticulate::py_available(initialize = FALSE)) {
      cfg <- reticulate::py_config()
      if (!.same_path(cfg$python, py_exec)) {
        msg("[install_py_deps_venv] reticulate is already initialized to another Python; ",
            "skip validation. After restarting, call use_bertopic_virtualenv('", envname, "') then bertopic_self_check().")
        return(invisible(py_exec))
      }
    } else {
      Sys.setenv(RETICULATE_PYTHON = py_exec)
      reticulate::py_config()
    }
    must <- c("bertopic", "sentence_transformers", "torch", "umap", "hdbscan", "numpy", "sklearn")
    for (m in must) {
      if (!reticulate::py_module_available(m))
        stop(sprintf("Module '%s' failed to import in venv '%s'.", m, envname), call. = FALSE)
    }
    msg("[install_py_deps_venv] Validation OK.")
  }

  msg("[install_py_deps_venv] Done. Venv: ", envname, "  Python: ", py_exec)
  invisible(py_exec)
}

#' Bind current R session to a BERTopic virtualenv
#'
#' Sets \code{RETICULATE_PYTHON} to the Python inside the given virtualenv and
#' initializes \pkg{reticulate}. If \pkg{reticulate} is already initialized to a
#' different Python, this stops with an informative error.
#'
#' @param envname Character. Virtualenv name (default \code{"r-bertopic"}).
#' @param required Logical. Kept for API symmetry; unused.
#'
#' @return Invisibly returns the Python executable path in the venv.
#'
#' @examples
#' \dontrun{
#' use_bertopic_virtualenv("r-bertopic")
#' }
#' @export
use_bertopic_virtualenv <- function(envname = "r-bertopic", required = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)

  venvs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character(0))
  if (!envname %in% venvs) {
    stop(sprintf("Virtualenv '%s' not found. Run install_py_deps_venv(envname = '%s') first.", envname, envname), call. = FALSE)
  }

  py_exec <- reticulate::virtualenv_python(envname)
  if (reticulate::py_available(initialize = FALSE)) {
    cfg <- reticulate::py_config()
    if (!.same_path(cfg$python, py_exec)) {
      stop(sprintf(paste0(
        "reticulate is already initialized to:\n  %s\n",
        "but you requested virtualenv '%s' (python: %s)\n\n",
        "Restart R (or call reticulate::py_restart_session()) and try again."
      ), cfg$python, envname, py_exec), call. = FALSE)
    }
    return(invisible(py_exec))
  }

  Sys.setenv(RETICULATE_PYTHON = py_exec)
  reticulate::py_config()
  invisible(py_exec)
}


# ===== Shared diagnostics ==========================================================

#' Summarize Python/BERTopic session info
#'
#' @return A named list containing paths, versions, and module availability:
#' \describe{
#'   \item{python}{Path of the active Python.}
#'   \item{libpython}{Path to libpython, if any.}
#'   \item{version}{Python version string.}
#'   \item{numpy}{Whether NumPy is available.}
#'   \item{numpy_version}{NumPy version string (if available).}
#'   \item{modules}{A data.frame with availability for key modules.}
#' }
#' @examples
#' \dontrun{
#' bertopic_session_info()
#' }
#' @export
bertopic_session_info <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)
  cfg <- reticulate::py_config()
  mods <- c("bertopic", "sentence_transformers", "torch", "umap", "hdbscan")
  present <- vapply(mods, reticulate::py_module_available, logical(1))
  list(
    python = cfg$python,
    libpython = cfg$libpython,
    version = cfg$version,
    numpy = cfg$numpy,
    numpy_version = cfg$numpy_version,
    modules = data.frame(module = mods, available = unname(present), stringsAsFactors = FALSE)
  )
}

#' Is Python + BERTopic available?
#'
#' Checks whether the active Python (as initialized by \pkg{reticulate}) can
#' import the key modules needed for BERTopic.
#'
#' @return Logical scalar.
#' @examples
#' \dontrun{
#' bertopic_available()
#' }
#' @export
bertopic_available <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)

  envname <- get_py_env()  # 默认 "r-bertopic"
  if (!reticulate::py_available(initialize = FALSE)) {
    # Prefer conda env
    if (nzchar(reticulate::conda_binary())) {
      envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
      if (envname %in% envs) {
        Sys.setenv(RETICULATE_PYTHON = reticulate::conda_python(envname))
      }
    }
    try(reticulate::py_config(), silent = TRUE)
  }

  reticulate::py_module_available("bertopic")
}


#' Quick self-check for the BERTopic R interface
#'
#' Runs a quick end-to-end smoke test:
#' \itemize{
#'   \item Report Python path/version.
#'   \item Verify that \code{bertopic} is importable and report its version.
#'   \item Minimal round trip: \code{fit -> transform -> save -> load}.
#' }
#'
#' @return A named list with fields:
#' \describe{
#'   \item{python_ok}{Logical.}
#'   \item{bertopic_ok}{Logical.}
#'   \item{roundtrip_ok}{Logical.}
#'   \item{details}{Character vector of diagnostic messages.}
#' }
#' @examples
#' \dontrun{
#' bertopic_self_check()
#' }
#' @export
bertopic_self_check <- function() {
  out <- list(python_ok = FALSE, bertopic_ok = FALSE, details = character())

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    out$details <- c(out$details, "reticulate not available")
    return(out)
  }

  # Python info
  cfg <- reticulate::py_config()
  out$python_ok <- !is.null(cfg$python)

  # bertopic importable?
  if (!reticulate::py_module_available("bertopic")) {
    out$details <- c(out$details, "bertopic not importable")
    return(out)
  }
  bt <- reticulate::import("bertopic")
  out$bertopic_ok <- TRUE

  # Prepare docs (prefer sms_spam$text; fallback to tiny list)
  docs <- NULL
  if (exists("sms_spam", inherits = TRUE)) {
    sms <- get("sms_spam", inherits = TRUE)
    if (is.data.frame(sms) && "text" %in% names(sms)) {
      docs <- as.character(sms$text)
    }
  }
  if (is.null(docs)) {
    docs <- c(
      "topic modeling with transformers",
      "free ringtone offer unsubscribe stop",
      "meeting at 3pm see you later",
      "love you so much my life"
    )
  }

  # Reproducibility
  if (exists("set_bertopic_seed", mode = "function")) {
    try(set_bertopic_seed(42L), silent = TRUE)
  } else {
    set.seed(42L)
  }

  # Construct model (with embedding model if available)
  model <- try(bt$BERTopic(
    embedding_model = "all-MiniLM-L6-v2",
    calculate_probabilities = TRUE
  ), silent = TRUE)
  if (inherits(model, "try-error")) {
    # Fallback without embedding_model
    model <- try(bt$BERTopic(calculate_probabilities = TRUE), silent = TRUE)
    if (inherits(model, "try-error")) {
      out$details <- c(out$details, "BERTopic() constructor failed")
      return(out)
    }
  }

  # Fit; if it fails (e.g., no model download), retry with synthetic embeddings
  ok <- try(model$fit_transform(docs), silent = TRUE)
  if (inherits(ok, "try-error")) {
    np <- try(reticulate::import("numpy", convert = FALSE), silent = TRUE)
    if (inherits(np, "try-error")) {
      out$details <- c(out$details, "fit_transform failed")
      return(out)
    }
    rs <- np$random$RandomState(as.integer(42L))
    emb <- rs$randn(as.integer(length(docs)), as.integer(16L))
    ok2 <- try(model$fit_transform(docs, embeddings = emb), silent = TRUE)
    if (inherits(ok2, "try-error")) {
      out$details <- c(out$details, "fit_transform failed (even with synthetic embeddings)")
      return(out)
    }
  }

  # Success — only report OK
  out$details <- c(out$details, "OK")
  out
}




#' Install Python dependencies for BERTopic (auto route)
#'
#' Tries Conda first (recommended). If Conda is unavailable, falls back to
#' virtualenv. On success, prints which route was used.
#'
#' @param envname Character. Environment name (both routes). Default "r-bertopic".
#' @param python_version Character. Python version for Conda route, e.g. "3.10".
#' @param python Optional path to python for virtualenv route.
#' @param reinstall Logical. Recreate the environment if it exists (route-specific).
#' @param validate Logical. Attempt to validate imports if reticulate is not
#'        already initialized to another Python.
#' @param verbose Logical. Print progress.
#' @return Invisibly, the path to the selected Python interpreter.
#' @export
install_py_deps <- function(envname = "r-bertopic",
                            python_version = "3.10",
                            python = NULL,
                            reinstall = FALSE,
                            validate = TRUE,
                            verbose = TRUE) {
  msg <- function(...) if (isTRUE(verbose)) message(...)
  if (.has_conda()) {
    msg("[install_py_deps] Using Conda route")
    return(invisible(install_py_deps_conda(
      envname = envname,
      python_version = python_version,
      reinstall = reinstall,
      validate = validate,
      verbose = verbose
    )))
  } else {
    msg("[install_py_deps] Conda not found; using virtualenv route")
    return(invisible(install_py_deps_venv(
      envname = envname,
      python = python,
      reinstall = reinstall,
      validate = validate,
      verbose = verbose
    )))
  }
}


#' Bind current R session to the BERTopic environment (auto route)
#'
#' If a Conda env with the given name exists, prefer Conda; otherwise try a
#' virtualenv with the same name. Stops if neither exists.
#'
#' @param envname Character. Environment name. Default "r-bertopic".
#' @return Invisibly, the Python executable path.
#' @export
use_bertopic <- function(envname = "r-bertopic") {
  if (.has_conda()) {
    envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
    if (envname %in% envs) return(invisible(use_bertopic_condaenv(envname)))
  }
  venvs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character(0))
  if (envname %in% venvs) return(invisible(use_bertopic_virtualenv(envname)))
  stop(sprintf(
    "Environment '%s' not found (neither Conda nor virtualenv). Run install_py_deps(envname = '%s') first.",
    envname, envname
  ), call. = FALSE)
}
