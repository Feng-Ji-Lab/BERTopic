#' Compute topics over time
#'
#' Wrapper for Python `BERTopic.topics_over_time()`. Returns a tibble and
#' attaches the original Python dataframe in the `"_py"` attribute for use in
#' visualization.
#'
#' @param model A "bertopic_r" model.
#' @param docs Character vector of documents.
#' @param timestamps A vector of timestamps (Date, POSIXt, or character).
#' @param nr_bins Optional number of temporal bins.
#' @param datetime_format Optional strftime-style format if timestamps are strings.
#' @return A tibble with topics-over-time data; attribute `"_py"` stores the
#'   original Python dataframe.
#' @export
bertopic_topics_over_time <- function(model, docs, timestamps,
                                      nr_bins = NULL, datetime_format = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (!is.character(docs)) rlang::abort("`docs` must be character.")
  if (length(docs) != length(timestamps)) rlang::abort("`docs` and `timestamps` lengths must match.")
  .need_py()

  # Convert timestamps to character (ISO) unless already character
  ts_out <- if (inherits(timestamps, "POSIXt")) {
    format(timestamps, "%Y-%m-%dT%H:%M:%S")
  } else if (inherits(timestamps, "Date")) {
    format(timestamps, "%Y-%m-%d")
  } else {
    as.character(timestamps)
  }

  res <- try({
    model$.py$topics_over_time(
      docs = unname(as.character(docs)),
      timestamps = unname(ts_out),
      nr_bins = as.integer(nr_bins) %||% NULL,
      datetime_format = datetime_format
    )
  }, silent = TRUE)
  if (inherits(res, "try-error")) rlang::abort("Python `topics_over_time()` failed.")

  r_tbl <- tibble::as_tibble(reticulate::py_to_r(res))
  attr(r_tbl, "_py") <- res
  r_tbl
}
