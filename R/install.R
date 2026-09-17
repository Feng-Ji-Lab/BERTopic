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


# Read the single dependency specification used by both installation routes.
# system.file() covers an installed package; the fallback supports pkgload and
# direct source-tree testing.
#' @keywords internal
.bertopic_python_requirements <- function() {
  path <- system.file("python", "requirements.txt", package = "BERTopic")
  if (!nzchar(path)) {
    source_path <- file.path("inst", "python", "requirements.txt")
    if (file.exists(source_path)) path <- source_path
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("BERTopic's Python requirements file is missing.", call. = FALSE)
  }

  requirements <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  requirements <- requirements[nzchar(requirements) & !startsWith(requirements, "#")]
  if (!length(requirements)) {
    stop("BERTopic's Python requirements file is empty.", call. = FALSE)
  }
  requirements
}

#' @keywords internal
.validate_bertopic_modules <- function(environment) {
  required <- c(
    "bertopic", "sentence_transformers", "torch", "transformers",
    "umap", "hdbscan", "numpy", "scipy", "sklearn", "pandas", "plotly"
  )
  unavailable <- required[!vapply(required, reticulate::py_module_available, logical(1))]
  if (length(unavailable)) {
    stop(sprintf(
      "Python environment '%s' cannot import required module(s): %s.",
      environment,
      paste(unavailable, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}


# ===== Conda route ================================================================

#' Install Python dependencies for BERTopic (Conda route)
#'
#' Creates (or reuses) a Conda environment with a pinned Python toolchain and
#' installs the package's exact Python dependency specification via pip. The
#' Conda and virtualenv routes consume the same specification.
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

  # 2) Install the shared, exact dependency specification via pip.
  py_exec <- reticulate::conda_python(envname)
  pip_available <- tryCatch(
    identical(system2(py_exec, c("-m", "pip", "--version"),
                      stdout = FALSE, stderr = FALSE), 0L),
    error = function(e) FALSE
  )
  if (!pip_available) {
    msg("[install_py_deps_conda] Installing pip...")
    reticulate::conda_install(envname, "pip", channel = "conda-forge")
  }
  msg("[install_py_deps_conda] Installing pinned Python packages...")
  reticulate::conda_install(
    envname,
    .bertopic_python_requirements(),
    pip = TRUE
  )

  # 3) Optional validation
  if (isTRUE(validate)) {
    msg("[install_py_deps_conda] Validating imports...")
    if (reticulate::py_available(initialize = FALSE)) {
      cfg <- reticulate::py_config()
      if (!.same_path(cfg$python, py_exec)) {
        msg("[install_py_deps_conda] reticulate is initialized to another Python; ",
            "skipping validation. After restarting, call use_bertopic_condaenv('", envname,
            "') then run bertopic_self_check().")
        return(invisible(py_exec))
      }
    }
    Sys.setenv(RETICULATE_PYTHON = py_exec)
    reticulate::py_config()
    .validate_bertopic_modules(envname)
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
#' Creates (or reuses) a \code{virtualenv} and installs the package's exact
#' Python dependency specification via pip. The Conda and virtualenv routes
#' consume the same specification.
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

  # 2) Install the same exact specification used by the Conda route.
  msg("[install_py_deps_venv] Installing pinned Python packages...")
  reticulate::virtualenv_install(
    envname,
    packages = .bertopic_python_requirements(),
    ignore_installed = FALSE
  )

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
    .validate_bertopic_modules(envname)
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
#'   \item{bertopic_version}{BERTopic version string (if available).}
#'   \item{modules}{A data.frame with availability and exact installed versions for key modules.}
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
  modules <- c(
    "bertopic", "sentence_transformers", "torch", "transformers",
    "umap", "hdbscan", "numpy", "scipy", "sklearn", "pandas", "plotly"
  )
  distributions <- c(
    bertopic = "bertopic",
    sentence_transformers = "sentence-transformers",
    torch = "torch",
    transformers = "transformers",
    umap = "umap-learn",
    hdbscan = "hdbscan",
    numpy = "numpy",
    scipy = "scipy",
    sklearn = "scikit-learn",
    pandas = "pandas",
    plotly = "plotly"
  )
  available <- vapply(modules, reticulate::py_module_available, logical(1))
  metadata <- try(reticulate::import("importlib.metadata"), silent = TRUE)
  versions <- vapply(modules, function(module) {
    if (!available[[module]] || inherits(metadata, "try-error")) return(NA_character_)
    tryCatch(
      as.character(metadata$version(unname(distributions[[module]]))),
      error = function(e) NA_character_
    )
  }, character(1))
  module_info <- data.frame(
    module = modules,
    available = unname(available),
    version = unname(versions),
    stringsAsFactors = FALSE
  )
  list(
    python = cfg$python,
    libpython = cfg$libpython,
    version = cfg$version,
    numpy = unname(available[["numpy"]]),
    numpy_version = unname(versions[["numpy"]]),
    bertopic_version = unname(versions[["bertopic"]]),
    modules = module_info
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
#' Runs a deterministic fit/transform/save/load check with synthetic embeddings
#' and compares assignments, probabilities, and topic metadata before and after
#' loading the model.
#'
#' @return A named list with fields:
#' \describe{
#'   \item{python_ok}{Logical.}
#'   \item{bertopic_ok}{Logical.}
#'   \item{roundtrip_ok}{Logical; true only when all compared outputs agree.}
#'   \item{details}{Character vector of diagnostic messages.}
#' }
#' @examples
#' \dontrun{
#' bertopic_self_check()
#' }
#' @export
bertopic_self_check <- function() {
  out <- list(
    python_ok = FALSE,
    bertopic_ok = FALSE,
    roundtrip_ok = FALSE,
    details = character()
  )
  fail <- function(message) {
    out$details <- c(out$details, message)
    out
  }
  equal_numeric <- function(a, b) {
    if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
    isTRUE(all.equal(as.matrix(a), as.matrix(b), tolerance = 1e-12, check.attributes = FALSE))
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(fail("reticulate not available"))
  }
  cfg <- try(reticulate::py_config(), silent = TRUE)
  if (inherits(cfg, "try-error") || is.null(cfg$python)) {
    return(fail("Python is not available"))
  }
  out$python_ok <- TRUE
  if (!reticulate::py_module_available("bertopic")) {
    return(fail("bertopic not importable"))
  }
  out$bertopic_ok <- TRUE

  documents <- c(
    rep("apple orange banana fruit market", 15L),
    rep("football team match score coach", 15L),
    rep("software code computer data model", 15L)
  )
  groups <- rep(seq_len(3L), each = 15L)
  set.seed(42L)
  centers <- rbind(
    c(-4, 0, 0, 0, 0),
    c(0, 4, 0, 0, 0),
    c(0, 0, 4, 0, 0)
  )
  embeddings <- centers[groups, , drop = FALSE] +
    matrix(stats::rnorm(length(documents) * 5L, sd = 0.02), ncol = 5L)

  components <- try({
    dimensionality <- reticulate::import("bertopic.dimensionality", convert = FALSE)
    hdbscan <- reticulate::import("hdbscan", convert = FALSE)
    text_features <- reticulate::import("sklearn.feature_extraction.text", convert = FALSE)
    list(
      umap = dimensionality$BaseDimensionalityReduction(),
      hdbscan = hdbscan$HDBSCAN(
        min_cluster_size = as.integer(5L),
        metric = "euclidean",
        cluster_selection_method = "eom",
        prediction_data = TRUE
      ),
      vectorizer = text_features$CountVectorizer(stop_words = "english")
    )
  }, silent = TRUE)
  if (inherits(components, "try-error")) {
    return(fail("Failed to construct deterministic self-check components"))
  }

  model <- try(bertopic_fit(
    documents,
    embeddings = embeddings,
    umap_model = components$umap,
    hdbscan_model = components$hdbscan,
    vectorizer_model = components$vectorizer,
    calculate_probabilities = TRUE
  ), silent = TRUE)
  if (inherits(model, "try-error")) return(fail("fit failed"))

  transformed_before <- try(bertopic_transform(model, documents, embeddings), silent = TRUE)
  info_before <- try(bertopic_topics(model), silent = TRUE)
  if (inherits(transformed_before, "try-error") || inherits(info_before, "try-error")) {
    return(fail("transform or topic-info extraction failed before save"))
  }

  model_path <- file.path(tempdir(), paste0("bertopic-self-check-", Sys.getpid(), ".pkl"))
  on.exit(if (file.exists(model_path) || dir.exists(model_path)) unlink(model_path, recursive = TRUE, force = TRUE), add = TRUE)
  saved <- try(bertopic_save(model, model_path, serialization = "pickle", overwrite = TRUE), silent = TRUE)
  if (inherits(saved, "try-error")) return(fail("save failed"))
  restored <- try(bertopic_load(model_path), silent = TRUE)
  if (inherits(restored, "try-error")) return(fail("load failed"))

  transformed_after <- try(bertopic_transform(restored, documents, embeddings), silent = TRUE)
  info_after <- try(bertopic_topics(restored), silent = TRUE)
  if (inherits(transformed_after, "try-error") || inherits(info_after, "try-error")) {
    return(fail("transform or topic-info extraction failed after load"))
  }

  comparisons <- c(
    cached_topics = identical(as.integer(model$topics), as.integer(restored$topics)),
    cached_probabilities = equal_numeric(model$probs, restored$probs),
    topic_metadata = isTRUE(all.equal(as.data.frame(info_before), as.data.frame(info_after), check.attributes = FALSE)),
    transformed_topics = identical(as.integer(transformed_before$topics), as.integer(transformed_after$topics)),
    transformed_probabilities = equal_numeric(transformed_before$probs, transformed_after$probs)
  )
  out$roundtrip_ok <- all(comparisons)
  if (out$roundtrip_ok) {
    out$details <- "OK: fit/transform/save/load outputs agree"
  } else {
    out$details <- paste("Mismatch after load:", paste(names(comparisons)[!comparisons], collapse = ", "))
  }
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
