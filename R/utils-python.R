#' Return the Python environment name used by BERTopic
#' @keywords internal
get_py_env <- function() {
  getOption("BERTopic.env", "r-bertopic")
}

#' Check Python availability and required modules at runtime
#' @keywords internal
.need_py <- function() {
  # Do not initialize Python implicitly; just check availability.
  if (!reticulate::py_available(initialize = FALSE)) {
    rlang::abort("Python is not available. Call install_py_deps() or configure reticulate.")
  }
  if (!reticulate::py_module_available("bertopic")) {
    rlang::abort("Python module 'bertopic' not found. Run install_py_deps().")
  }
  invisible(TRUE)
}

#' Install Python dependencies for BERTopic (user-invoked)
#'
#' This function is never called automatically during installation or load.
#' It creates (or reuses) a virtualenv and installs required Python packages.
#'
#' @param env Name of the virtualenv to create/use. Defaults to option `BERTopic.env`.
#' @param python Path to a Python executable (e.g., "python3").
#' @return Invisibly returns TRUE if installation finishes without error.
#' @export
install_py_deps <- function(env = get_py_env(), python = "python3") {
  reticulate::virtualenv_create(envname = env, python = python)
  reticulate::virtualenv_install(env, c(
    "bertopic==0.16.0",
    "umap-learn",
    "hdbscan",
    "numpy",
    "pandas",
    "scikit-learn",
    "sentence-transformers"
  ))
  # Attach the env for current session only (no global side-effects).
  reticulate::use_virtualenv(env, required = TRUE)
  invisible(TRUE)
}
