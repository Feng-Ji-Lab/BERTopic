#' Transform new documents with a fitted BERTopic model
#'
#' @param model A "bertopic_r" model from [bertopic_fit()].
#' @param new_text Character vector of new documents.
#' @param embeddings Optional numeric matrix for new documents.
#' @return A list with `topics` and `probs` for the new documents.
#' @export
bertopic_transform <- function(model, new_text, embeddings = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (!is.character(new_text)) rlang::abort("`new_text` must be a character vector.")
  .need_py()

  py_text <- unname(as.character(new_text))
  if (is.null(embeddings)) {
    out <- model$.py$transform(py_text)
  } else {
    emb <- as.matrix(embeddings)
    if (!is.numeric(emb)) rlang::abort("`embeddings` must be a numeric matrix.")
    out <- model$.py$transform(py_text, embeddings = reticulate::r_to_py(emb))
  }

  list(
    topics = unname(reticulate::py_to_r(out[[1]])),
    probs  = tryCatch(reticulate::py_to_r(out[[2]]), error = function(e) NULL)
  )
}
