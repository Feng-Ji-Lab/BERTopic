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
#' Wrapper over Python `reduce_topics`, compatible with multiple signatures.
#'
#' @param model A "bertopic_r" model.
#' @param nr_topics Target number (integer) or "auto".
#' @param representation_model Optional Python representation model.
#' @param docs Optional character vector of training docs (used if required by backend).
#' @return The input model (invisibly).
#' @export
bertopic_reduce_topics <- function(model,
                                   nr_topics = "auto",
                                   representation_model = NULL,
                                   docs = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()

  # Try: with docs (named args)
  if (!is.null(docs)) {
    res <- try(model$.py$reduce_topics(
      unname(as.character(docs)),
      nr_topics = nr_topics,
      representation_model = representation_model
    ), silent = TRUE)
    if (!inherits(res, "try-error")) return(invisible(model))
  }

  # Try: named without docs
  res <- try(model$.py$reduce_topics(
    nr_topics = nr_topics,
    representation_model = representation_model
  ), silent = TRUE)
  if (!inherits(res, "try-error")) return(invisible(model))

  # Fallbacks: positional
  if (!is.null(docs)) {
    res <- try(model$.py$reduce_topics(unname(as.character(docs))), silent = TRUE)
    if (!inherits(res, "try-error")) return(invisible(model))
  }
  res <- try(model$.py$reduce_topics(nr_topics), silent = TRUE)
  if (!inherits(res, "try-error")) return(invisible(model))

  rlang::abort("Python `reduce_topics()` failed.")
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
