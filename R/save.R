#' Save a BERTopic model
#'
#' Save a fitted BERTopic model to disk. Depending on the serialization method,
#' this may produce either a single file (e.g., *.pkl / *.pt / *.safetensors)
#' or a directory bundle. The function does not pre-create the target path; it
#' only ensures the parent directory exists and lets BERTopic decide the layout.
#'
#' @param model A "bertopic_r" model.
#' @param path Destination path (file or directory, as required by BERTopic).
#' @param serialization One of "pickle", "safetensors", or "pt". Default "pickle".
#' @param save_embedding_model Logical; whether to include the embedding model. Default FALSE.
#' @param overwrite Logical; if TRUE and the target exists, it will be replaced.
#' @return Invisibly returns the normalized path.
#' @export
bertopic_save <- function(model, path,
                          serialization = c("pickle", "safetensors", "pt"),
                          save_embedding_model = FALSE,
                          overwrite = FALSE) {
  if (!inherits(model, "bertopic_r"))
    rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  serialization <- match.arg(serialization)

  # If the exact target already exists, remove it when overwrite = TRUE
  if (file.exists(path) || dir.exists(path)) {
    if (overwrite) {
      unlink(path, recursive = TRUE, force = TRUE)
    } else {
      warning(sprintf("Path already exists: %s (use overwrite = TRUE to replace).", path))
    }
  }

  # Ensure parent dir exists; do not create the target path itself
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  ok <- TRUE
  tryCatch({
    model$.py$save(
      path = path,
      serialization = serialization,
      save_embedding_model = isTRUE(save_embedding_model)
    )
  }, error = function(e) {
    ok <<- FALSE
  })

  # Fallback for older BERTopic that only accepts positional args
  if (!ok) {
    model$.py$save(path)
  }

  invisible(normalizePath(path, mustWork = FALSE))
}


#' Load a BERTopic model
#'
#' Load a BERTopic model from disk that was saved with [bertopic_save()].
#'
#' @param path Path used in [bertopic_save()] (file or directory).
#' @return A "bertopic_r" object with the loaded Python model.
#' @export
bertopic_load <- function(path) {
  .need_py()
  reticulate::use_condaenv(get_py_env(), required = FALSE)
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
