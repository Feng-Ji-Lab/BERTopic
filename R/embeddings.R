#' Replace or set the embedding model
#'
#' Set a new embedding model on a fitted BERTopic instance. This enables
#' `transform()` after loading when the embedding model was not saved.
#'
#' @param model A "bertopic_r" model.
#' @param embedding_model Either a character identifier (e.g., "all-MiniLM-L6-v2")
#'   or a Python embedding model object (e.g., a SentenceTransformer instance).
#' @return The input model (invisibly).
#' @export
bertopic_set_embedding_model <- function(model, embedding_model) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  ok <- TRUE
  res <- try(model$.py$set_embedding_model(embedding_model = embedding_model), silent = TRUE)
  if (inherits(res, "try-error")) ok <- FALSE
  if (!ok) {
    res <- try(model$.py$set_embedding_model(embedding_model), silent = TRUE)
    if (inherits(res, "try-error")) rlang::abort("Python `set_embedding_model()` failed.")
  }
  invisible(model)
}

#' Does the model have a usable embedding model?
#'
#' @param model A "bertopic_r" model.
#' @return Logical; TRUE if `embedding_model` is present and not None.
#' @export
bertopic_has_embedding_model <- function(model) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  em <- try(reticulate::py_get_attr(model$.py, "embedding_model"), silent = TRUE)
  if (inherits(em, "try-error")) return(FALSE)
  if (is.null(em)) return(FALSE)
  if (reticulate::py_is_null_xptr(em)) return(FALSE)
  # Convert to R: Python None becomes NULL if convertible
  em_r <- try(reticulate::py_to_r(em), silent = TRUE)
  if (!inherits(em_r, "try-error") && is.null(em_r)) return(FALSE)
  TRUE
}
