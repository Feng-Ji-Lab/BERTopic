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
  if (!inherits(model, "bertopic_r")) rlang::abort("model must be a bertopic_r object.")
  .need_py()

  backend_utils <- try(
    reticulate::import("bertopic.backend._utils", convert = FALSE),
    silent = TRUE
  )
  if (inherits(backend_utils, "try-error")) {
    rlang::abort(paste0(
      "Could not import Python embedding backend utilities: ",
      conditionMessage(attr(backend_utils, "condition"))
    ))
  }

  language <- try(reticulate::py_get_attr(model$.py, "language"), silent = TRUE)
  if (inherits(language, "try-error")) language <- NULL
  selected <- try(
    backend_utils$select_backend(embedding_model, language = language),
    silent = TRUE
  )
  if (inherits(selected, "try-error")) {
    rlang::abort(paste0(
      "Python embedding backend selection failed: ",
      conditionMessage(attr(selected, "condition"))
    ))
  }

  assigned <- try(
    reticulate::py_set_attr(model$.py, "embedding_model", selected),
    silent = TRUE
  )
  if (inherits(assigned, "try-error")) {
    rlang::abort(paste0(
      "Could not set the Python embedding model: ",
      conditionMessage(attr(assigned, "condition"))
    ))
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
