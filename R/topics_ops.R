#' Update topic representations
#'
#' Call Python `BERTopic.update_topics()` to recompute topic representations.
#'
#' @param model A "bertopic_r" model.
#' @param text Character vector of training documents used in `fit`.
#' @return The input model (invisibly), updated in place on the Python side.
#' @export
bertopic_update_topics <- function(model, text) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (!is.character(text)) rlang::abort("`text` must be character.")
  .need_py()
  res <- try(model$.py$update_topics(unname(as.character(text))), silent = TRUE)
  if (inherits(res, "try-error")) rlang::abort("Python `update_topics()` failed.")
  invisible(model)
}

#' Reduce/merge topics
#'
#' Wrapper over Python `reduce_topics`.
#'
#' @param model A "bertopic_r" model.
#' @param nr_topics Target number (integer) or "auto".
#' @param representation_model Optional Python representation model.
#' @param docs Character vector of training docs.
#' @return The input model (invisibly).
#' @export
bertopic_reduce_topics <- function(model,
                                   nr_topics = "auto",
                                   representation_model = NULL,
                                   docs = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (is.null(docs)) rlang::abort("`docs` must be provided for `reduce_topics()`.")
  if (!is.character(docs)) rlang::abort("`docs` must be character.")
  if (!(identical(nr_topics, "auto") ||
        (is.numeric(nr_topics) && length(nr_topics) == 1L && is.finite(nr_topics) &&
         nr_topics == as.integer(nr_topics) && nr_topics > 0))) {
    rlang::abort("`nr_topics` must be a positive integer or \"auto\".")
  }
  .need_py()

  docs <- unname(as.character(docs))
  if (is.numeric(nr_topics)) nr_topics <- as.integer(nr_topics)

  if (is.null(representation_model)) {
    res <- try(model$.py$reduce_topics(
      docs,
      nr_topics = nr_topics
    ), silent = TRUE)
  } else {
    res <- try(model$.py$reduce_topics(
      docs,
      nr_topics = nr_topics,
      representation_model = representation_model
    ), silent = TRUE)
  }
  if (inherits(res, "try-error")) rlang::abort("Python `reduce_topics()` failed.")

  # Keep the cached R fields synchronized with Python after reduction.
  model$topics <- tryCatch(
    unname(reticulate::py_to_r(model$.py$topics_)),
    error = function(e) model$topics
  )
  model$probs <- tryCatch(
    reticulate::py_to_r(model$.py$probabilities_),
    error = function(e) model$probs
  )
  invisible(model)
}

#' Relabel topics
#'
#' Set custom labels for topics. Accepts a named character vector or a
#' data.frame with columns `topic` and `label`.
#'
#' @param model A "bertopic_r" model.
#' @param labels A named character vector (names are topic ids) or a data.frame.
#' @return The input model (invisibly).
#' @export
bertopic_set_topic_labels <- function(model, labels) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  mapping <- NULL
  if (is.character(labels) && !is.null(names(labels))) {
    ids <- suppressWarnings(as.integer(names(labels)))
    if (anyNA(ids)) rlang::abort("Names of `labels` must be integer topic ids.")
    mapping <- as.list(as.character(labels))
    names(mapping) <- as.character(ids) # keep as str keys for Python dict
  } else if (is.data.frame(labels)) {
    if (!all(c("topic", "label") %in% names(labels))) {
      rlang::abort("Data frame `labels` must contain columns `topic` and `label`.")
    }
    ids <- suppressWarnings(as.integer(labels$topic))
    if (anyNA(ids)) rlang::abort("`labels$topic` must be integers.")
    mapping <- as.list(as.character(labels$label))
    names(mapping) <- as.character(ids)
  } else {
    rlang::abort("`labels` must be a named character vector or data.frame(topic,label).")
  }

  ok <- TRUE
  res <- try(model$.py$set_topic_labels(labels = mapping), silent = TRUE)
  if (inherits(res, "try-error")) ok <- FALSE
  if (!ok) {
    res <- try(model$.py$set_topic_labels(mapping), silent = TRUE)
    if (inherits(res, "try-error")) rlang::abort("Python `set_topic_labels()` failed.")
  }
  invisible(model)
}
