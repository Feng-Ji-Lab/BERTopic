#' Save a BERTopic model to disk
#' @param model A "bertopic_r" model.
#' @param path File path (directory or file depending on BERTopic version).
#' @param serialization Serialization backend, e.g. "pickle" or "safetensors" (passed to Python if supported).
#' @param save_embedding_model Whether to save embedding model (passed to Python if supported).
#' @export
bertopic_save <- function(model, path,
                          serialization = "pickle",
                          save_embedding_model = FALSE) {
  if (!inherits(model, "bertopic_r"))
    rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  ok <- TRUE
  tryCatch({
    # Prefer keyword args (v0.16+), fallback to positional for older versions
    model$.py$save(path = path,
                   serialization = serialization,
                   save_embedding_model = save_embedding_model)
  }, error = function(e) {
    ok <<- FALSE
  })
  if (!ok) {
    # fallback: positional only
    model$.py$save(path)
  }
  invisible(normalizePath(path, mustWork = FALSE))
}

#' Load a BERTopic model from disk
#' @param path Directory path passed to [bertopic_save()].
#' @return A "bertopic_r" object with loaded Python model.
#' @export
bertopic_load <- function(path) {
  .need_py()
  reticulate::use_virtualenv(get_py_env(), required = FALSE)
  bt <- reticulate::import("bertopic")
  py_model <- NULL
  ok <- TRUE
  tryCatch({
    py_model <- bt$BERTopic$load(path = path)
  }, error = function(e) {
    ok <<- FALSE
  })
  if (!ok) {
    py_model <- bt$BERTopic$load(path)
  }
  structure(list(.py = py_model, topics = NULL, probs = NULL),
            class = "bertopic_r")
}
