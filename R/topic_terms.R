
#' Get top terms for a topic
#' @param model A "bertopic_r" model
#' @param topic_id Integer topic id
#' @param top_n Number of top terms to return
#' @return A tibble with columns `term` and `weight`
#' @export
bertopic_topic_terms <- function(model, topic_id, top_n = 10L) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (length(topic_id) != 1L || !is.numeric(topic_id)) rlang::abort("`topic_id` must be a single integer.")
  .need_py()
  tw <- model$.py$get_topic(as.integer(topic_id))
  if (is.null(tw)) {
    return(tibble::tibble(term = character(), weight = numeric()))
  }
  mat <- reticulate::py_to_r(tw)
  # BERTopic returns list of (term, weight)
  df <- tibble::tibble(
    term   = vapply(mat, `[[`, character(1), 1L),
    weight = as.numeric(vapply(mat, `[[`, numeric(1), 2L))
  )
  if (is.finite(top_n) && top_n > 0) df <- utils::head(df, top_n)
  df
}
