#' Coerce to a document-topic probability matrix
#'
#' Extract the document-topic probabilities as a matrix. If probabilities were
#' not computed during fitting, returns NULL (with a warning).
#'
#' @param model A "bertopic_r" model object.
#' @param sparse Logical; if TRUE and Matrix is available, returns a sparse matrix.
#' @param prefix Logical; if TRUE, prefix columns as topic ids.
#' @return A matrix or sparse Matrix of size n_docs x n_topics, or NULL.
#' @export
bertopic_as_document_topic_matrix <- function(model, sparse = TRUE, prefix = TRUE) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  probs <- model$probs %||% NULL
  if (is.null(probs)) {
    warning("Model does not contain probabilities; re-fit with `calculate_probabilities=TRUE`.")
    return(NULL)
  }
  M <- as.matrix(probs)
  storage.mode(M) <- "double"
  if (isTRUE(prefix)) {
    ids <- tryCatch({
      info <- reticulate::py_to_r(model$.py$get_topic_info())
      as.integer(info$Topic)
    }, error = function(e) integer())
    # BERTopic commonly omits the outlier topic (-1) from probabilities_.
    # Drop it only when doing so makes the metadata match the matrix.
    if (length(ids) != ncol(M) && any(ids == -1L)) {
      ids <- ids[ids != -1L]
    }
    if (length(ids) != ncol(M)) {
      rlang::abort("Cannot map probability columns to BERTopic topic IDs.")
    }
    colnames(M) <- paste0("topic_", ids)
  }
  if (isTRUE(sparse) && requireNamespace("Matrix", quietly = TRUE)) {
    return(Matrix::Matrix(M, sparse = TRUE))
  }
  M
}

#' Predict method for BERTopic models
#'
#' @param object A "bertopic_r" model.
#' @param newdata Character vector of new documents.
#' @param type One of "class", "prob", or "both".
#' @param embeddings Optional numeric matrix of embeddings.
#' @param ... Reserved for future arguments.
#' @return Depending on `type`, an integer vector, a matrix/data frame, or a list.
#' @export
predict.bertopic_r <- function(object, newdata, type = c("both", "class", "prob"), embeddings = NULL, ...) {
  if (!inherits(object, "bertopic_r")) rlang::abort("`object` must be a 'bertopic_r' model.")
  if (!is.character(newdata)) rlang::abort("`newdata` must be character.")
  type <- match.arg(type)
  res <- bertopic_transform(object, newdata, embeddings = embeddings)
  if (type == "class") return(res$topics)
  if (type == "prob")  return(res$probs)
  res
}
