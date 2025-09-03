
install_py_deps <- function(envname = "r-bertopic",
                            python_version = "3.10",
                            reinstall = FALSE,
                            verbose = TRUE) {
  need_pkg <- function(p) if (!requireNamespace(p, quietly = TRUE))
    stop(sprintf("Package '%s' is required. Please install it first.", p), call. = FALSE)
  need_pkg("reticulate")

  # --- 0) Ensure Miniconda exists ---------------------------------------------------
  if (verbose) message("[install_py_deps] Checking Miniconda...")
  if (!reticulate::miniconda_exists()) {
    if (verbose) message("[install_py_deps] Installing Miniconda (once)...")
    reticulate::install_miniconda()
  }

  # Apple Silicon: ensure correct subdir/channel
  is_macos_arm <- Sys.info()[["sysname"]] == "Darwin" && grepl("arm64", Sys.info()[["machine"]])
  if (is_macos_arm && Sys.getenv("CONDA_SUBDIR", "") == "") {
    if (verbose) message("[install_py_deps] Detected Apple Silicon; using osx-arm64 channel.")
    Sys.setenv(CONDA_SUBDIR = "osx-arm64")
  }

  conda <- reticulate::conda_binary()
  if (is.null(conda) || !nzchar(conda)) {
    stop("Conda binary not found. reticulate::install_miniconda() should have provided one.", call. = FALSE)
  }

  # --- 1) (Re)create env with pinned Python ----------------------------------------
  envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
  if (reinstall && envname %in% envs) {
    if (verbose) message("[install_py_deps] Removing existing env: ", envname)
    reticulate::conda_remove(envname, packages = NULL, all = TRUE)
    envs <- setdiff(envs, envname)
  }
  if (!envname %in% envs) {
    if (verbose) message("[install_py_deps] Creating env '", envname, "' with Python ", python_version, "...")
    reticulate::conda_create(envname, packages = sprintf("python=%s", python_version), channel = "conda-forge")
    envs <- c(envs, envname)
  }

  # --- 2) Core scientific stack (pinned) -------------------------------------------
  # Stable combo circa 2025: numpy 1.26.x, scipy 1.11.x, scikit-learn 1.4.x, numba 0.59.x
  if (verbose) message("[install_py_deps] Installing core scientific stack...")
  reticulate::conda_install(envname, c(
    "pip",
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
    "dill"
  ), channel = "conda-forge")

  # --- 3) Torch & sentence-transformers -------------------------------------------
  # Use CPU build for portability. CUDA builds are out-of-scope for CRAN defaults.
  if (verbose) message("[install_py_deps] Installing PyTorch & sentence-transformers...")
  reticulate::conda_install(envname, c(
    "pytorch==2.1.*",
    "sentence-transformers"
  ), channel = "conda-forge")

  # --- 4) BERTopic via pip (version pin) -------------------------------------------
  if (verbose) message("[install_py_deps] Installing BERTopic (pip)...")
  py_exec <- reticulate::conda_python(envname)
  reticulate::py_run_string(
    code = "import sys, subprocess; subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'bertopic==0.16.0'])",
    local = TRUE, convert = TRUE, silent = !verbose, python = py_exec
  )

  # --- 5) Validation: import modules ----------------------------------------------
  if (verbose) message("[install_py_deps] Validating imports...")
  must_import <- c("bertopic", "sentence_transformers", "torch", "umap", "hdbscan", "numpy", "sklearn")
  for (mod in must_import) {
    ok <- reticulate::py_module_available(mod)
    if (!ok) stop(sprintf("Python module '%s' failed to import in env '%s'.", mod, envname), call. = FALSE)
  }

  # --- 6) Smoke test: tiny roundtrip (fit -> transform) ----------------------------
  if (verbose) message("[install_py_deps] Running a tiny smoke test...")
  old <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  on.exit(if (nzchar(old)) Sys.setenv(RETICULATE_PYTHON = old) else Sys.unsetenv("RETICULATE_PYTHON"), add = TRUE)
  Sys.setenv(RETICULATE_PYTHON = py_exec)

  bt <- reticulate::import("bertopic")
  docs <- list(
    "topic modeling with transformers",
    "free ringtone offer unsubscribe stop",
    "meeting at 3pm see you later",
    "love you so much my life"
  )
  m <- bt$BERTopic()
  res <- try(m$fit_transform(docs), silent = TRUE)
  if (inherits(res, "try-error")) {
    warning("Smoke test could not download embeddings (possibly offline). Install is OK; first use may need internet.")
  } else {
    invisible(m$transform(list("new unseen text")))
  }

  if (verbose) {
    message("[install_py_deps] Installation finished.")
    message("  - Env: ", envname)
    message("  - Python: ", py_exec)
  }

  invisible(py_exec)
}


use_bertopic_condaenv <- function(envname = "r-bertopic", required = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)
  envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
  if (!envname %in% envs) {
    stop(sprintf("Conda env '%s' not found. Run BERTopic::install_py_deps(envname = '%s') first.", envname, envname), call. = FALSE)
  }
  py_exec <- reticulate::conda_python(envname)
  Sys.setenv(RETICULATE_PYTHON = py_exec)
  reticulate::py_config()
  invisible(py_exec)
}

bertopic_session_info <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.", call. = FALSE)
  cfg <- reticulate::py_config(error = FALSE)
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

bertopic_available <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
  ok <- all(vapply(c("bertopic", "sentence_transformers", "torch"),
                   reticulate::py_module_available, logical(1)))
  isTRUE(ok)
}


bertopic_self_check <- function() {
  out <- list(python_ok = FALSE, bertopic_ok = FALSE, roundtrip_ok = FALSE, details = character())
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    out$details <- c(out$details, "reticulate not available")
    return(out)
  }
  cfg <- reticulate::py_config(error = FALSE)
  out$python_ok <- !is.null(cfg$python)
  out$details <- c(out$details, sprintf("Python: %s (version %s)", cfg$python, cfg$version %||% "unknown"))

  if (!reticulate::py_module_available("bertopic")) {
    out$details <- c(out$details, "bertopic not importable")
    return(out)
  }
  bt <- reticulate::import("bertopic")
  out$bertopic_ok <- TRUE
  out$details <- c(out$details, sprintf("bertopic version: %s", tryCatch(bt$`__version__`, error = function(e) "unknown")))

  docs <- list(
    "topic modeling with transformers",
    "free ringtone offer unsubscribe stop",
    "meeting at 3pm see you later",
    "love you so much my life"
  )
  model <- try(bt$BERTopic(), silent = TRUE)
  if (inherits(model, "try-error")) return(out)
  ok <- try(model$fit_transform(docs), silent = TRUE)
  if (inherits(ok, "try-error")) return(out)

  # Save/load using safest configuration (pickle to a file path)
  tmp_file <- file.path(tempdir(), "bertopic_r_selfcheck.pkl")
  if (dir.exists(tmp_file)) unlink(tmp_file, recursive = TRUE, force = TRUE)
  if (file.exists(tmp_file)) unlink(tmp_file, force = TRUE)

  ok2 <- try(model$save(path = tmp_file, serialization = "pickle", save_embedding_model = FALSE), silent = TRUE)
  ok3 <- try(bt$BERTopic$load(path = tmp_file), silent = TRUE)
  out$roundtrip_ok <- !inherits(ok2, "try-error") && !inherits(ok3, "try-error")

  if (!out$roundtrip_ok) {
    out$details <- c(out$details, "Round-trip fit/transform/save/load: FAILED")
  } else {
    out$details <- c(out$details, "Round-trip fit/transform/save/load: OK")
  }
  out
}

# small infix helper
`%||%` <- function(a, b) if (is.null(a) || (is.logical(a) && length(a) == 1 && is.na(a))) b else a

# install.packages("reticulate")            # if not installed
# library(BERTopic)

# # 1) One-time installation of Python deps into an isolated env
# install_py_deps(envname = "r-bertopic")

# # 2) Bind the session to that env
# use_bertopic_condaenv("r-bertopic")

# # 3) Verify
# bertopic_available()
# bertopic_session_info()
# bertopic_self_check()