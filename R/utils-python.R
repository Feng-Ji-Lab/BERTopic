#' Return the Python environment name used by BERTopic
#' @keywords internal
get_py_env <- function() {
  # Prefer the new option; fall back to old key if it exists; default to "r-bertopic"
  opt <- getOption("BERTopic.env", NULL)
  if (is.null(opt) || !nzchar(opt)) {
    opt <- getOption("bertopicr.env", NULL)
  }
  if (is.null(opt) || !nzchar(opt)) opt <- "r-bertopic"
  opt
}

#' Check Python availability and required modules at runtime (lazy init)
#'
#' Tries to initialize reticulate to the configured env (Conda first, then virtualenv).
#' Falls back to the default Python if the named env does not exist.
#' Errors with a clear message if 'bertopic' is still not importable.
#' @keywords internal
.need_py <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    rlang::abort("Package 'reticulate' is required. Install it and try again.")
  }

  envname <- get_py_env()

  # Initialize Python if not yet initialized: prefer the named Conda/venv
  if (!reticulate::py_available(initialize = FALSE)) {
    # Try Conda env
    has_conda <- tryCatch({
      cb <- reticulate::conda_binary()
      is.character(cb) && length(cb) == 1 && nzchar(cb)
    }, error = function(e) FALSE)

    if (isTRUE(has_conda)) {
      conda_envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
      if (envname %in% conda_envs) {
        Sys.setenv(RETICULATE_PYTHON = reticulate::conda_python(envname))
        reticulate::py_config()  # initialize to this env
      }
    }

    # If still not initialized, try virtualenv
    if (!reticulate::py_available(initialize = FALSE)) {
      venvs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character(0))
      if (envname %in% venvs) {
        Sys.setenv(RETICULATE_PYTHON = reticulate::virtualenv_python(envname))
        reticulate::py_config()
      }
    }

    # Last resort: initialize default Python so we can produce a helpful error
    if (!reticulate::py_available(initialize = FALSE)) {
      reticulate::py_config()
    }
  }

  # At this point reticulate is initialized to *something* — check bertopic
  if (!reticulate::py_module_available("bertopic")) {
    cfg <- reticulate::py_config()
    rlang::abort(sprintf(
      paste0(
        "Python module 'bertopic' not found in the active Python:\n  %s\n\n",
        "Fix:\n",
        "  1) install_py_deps(envname = '%s', python_version = '3.10')\n",
        "  2) use_bertopic('%s')  # or use_bertopic_condaenv()/use_bertopic_virtualenv()\n"
      ),
      cfg$python %||% "<unknown>", envname, envname
    ))
  }

  invisible(TRUE)
}
