#' Summary for BERTopic models
#'
#' @param object A "bertopic_r" model.
#' @param ... Unused.
#' @return Invisibly returns a named list of summary fields.
#' @export
summary.bertopic_r <- function(object, ...) {
  if (!inherits(object, "bertopic_r")) rlang::abort("`object` must be a 'bertopic_r' model.")
  .need_py()
  info <- try(bertopic_topics(object), silent = TRUE)
  n_topics <- if (inherits(info, "try-error") || is.null(info)) NA_integer_ else nrow(info)
  noise <- if (!inherits(info, "try-error") && !is.null(info) && nrow(info)) {
    sum(info$Topic == -1L, na.rm = TRUE)
  } else NA_integer_
  cat("<bertopic_r summary>\n")
  cat("  topics (rows in get_topic_info):", n_topics, "\n")
  cat("  noise topics (-1):", noise, "\n")
  invisible(list(n_topics = n_topics, n_noise = noise))
}

#' Coefficients (top terms) for BERTopic
#'
#' @param object A "bertopic_r" model.
#' @param top_n Number of terms per topic.
#' @param ... Unused.
#' @return A data.frame with columns topic, term, weight.
#' @export
coef.bertopic_r <- function(object, top_n = 10L, ...) {
  if (!inherits(object, "bertopic_r")) rlang::abort("`object` must be a 'bertopic_r' model.")
  info <- try(bertopic_topics(object), silent = TRUE)
  if (inherits(info, "try-error") || is.null(info) || !nrow(info)) {
    return(data.frame(topic = integer(), term = character(), weight = numeric(), stringsAsFactors = FALSE))
  }
  topics <- info$Topic
  out <- lapply(topics, function(tid) {
    tt <- try(bertopic_topic_terms(object, tid, top_n = top_n), silent = TRUE)
    if (inherits(tt, "try-error") || is.null(tt) || !nrow(tt)) {
      return(data.frame(topic = tid, term = character(), weight = numeric(), stringsAsFactors = FALSE))
    }
    cbind(topic = tid, as.data.frame(tt, stringsAsFactors = FALSE))
  })
  do.call(rbind, out)
}

#' Coerce to data.frame
#'
#' @param x A "bertopic_r" model.
#' @param ... Unused.
#' @return A data.frame equal to [bertopic_topics()].
#' @export
as.data.frame.bertopic_r <- function(x, ...) {
  tibble::as_tibble(bertopic_topics(x))
}

#' Fortify method for ggplot2
#'
#' @param model A "bertopic_r" model.
#' @param data Ignored.
#' @param ... Unused.
#' @return A data.frame of document-topic assignments.
#' @export
fortify.bertopic_r <- function(model, data, ...) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' model.")
  dt <- NULL
  if (!is.null(model$topics)) {
    dt <- tibble::tibble(doc_id = seq_along(model$topics), topic = as.integer(model$topics))
  } else {
    dt <- tibble::tibble(doc_id = integer(), topic = integer())
  }
  as.data.frame(dt, stringsAsFactors = FALSE)
}
