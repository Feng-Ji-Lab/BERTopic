#' Document-level information
#'
#' Retrieve document-level information for the provided documents.
#'
#' @param model A "bertopic_r" model.
#' @param docs Character vector of documents to query (required).
#' @return A tibble with document-level information.
#' @export
bertopic_get_document_info <- function(model, docs) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (!is.character(docs)) rlang::abort("`docs` must be character.")
  .need_py()
  res <- try(model$.py$get_document_info(unname(as.character(docs))), silent = TRUE)
  if (inherits(res, "try-error")) {
    rlang::abort("Failed to retrieve document info from Python BERTopic.")
  }
  tibble::as_tibble(reticulate::py_to_r(res))
}

#' Find nearest topics for a query string
#'
#' Use `BERTopic.find_topics()` to retrieve the closest topics for a query
#' string. Augments topic IDs/scores with topic labels when available.
#'
#' @param model A "bertopic_r" model.
#' @param query_text A length-1 character query.
#' @param top_n Number of nearest topics to return.
#' @return A tibble with columns `topic`, `score`, and `label`.
#' @export
bertopic_find_topics <- function(model, query_text, top_n = 5L) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (!is.character(query_text) || length(query_text) != 1L)
    rlang::abort("`query_text` must be a single character string.")
  .need_py()

  py_res <- try(model$.py$find_topics(as.character(query_text), as.integer(top_n)), silent = TRUE)
  if (inherits(py_res, "try-error")) {
    rlang::abort("Python `find_topics()` failed.")
  }
  r_res <- reticulate::py_to_r(py_res)
  topics <- as.integer(r_res[[1]] %||% integer())
  scores <- as.numeric(r_res[[2]] %||% numeric())
  info <- try(bertopic_topics(model), silent = TRUE)
  label <- if (!inherits(info, "try-error") && !is.null(info) && nrow(info)) {
    nm <- info$Name[match(topics, info$Topic)]
    nm <- ifelse(is.na(nm), "N/A", nm)
    nm
  } else rep_len("N/A", length(topics))
  tibble::tibble(topic = topics, score = round(scores, 6), label = label)
}

#' Representative documents for a topic
#'
#' Retrieve representative documents for a given topic using
#' `BERTopic.get_representative_docs()`. Falls back across signature variants.
#'
#' @param model A "bertopic_r" model.
#' @param topic_id Integer topic id.
#' @param top_n Number of representative documents to return.
#' @return A tibble with columns `rank` and `document`. If scores are available
#'   in the current BERTopic version, a `score` column is included.
#' @export
bertopic_get_representative_docs <- function(model, topic_id, top_n = 5L) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  if (length(topic_id) != 1L || !is.numeric(topic_id)) rlang::abort("`topic_id` must be a single integer.")
  .need_py()

  docs <- NULL
  has_scores <- FALSE

  ok <- TRUE
  out <- try({
    # Most recent signature allows `nr_docs`
    model$.py$get_representative_docs(as.integer(topic_id), nr_docs = as.integer(top_n))
  }, silent = TRUE)
  if (inherits(out, "try-error")) {
    ok <- FALSE
  }
  if (!ok) {
    out <- try(model$.py$get_representative_docs(as.integer(topic_id), as.integer(top_n)), silent = TRUE)
  }

  if (inherits(out, "try-error")) {
    rlang::abort("Python `get_representative_docs()` failed.")
  }

  r <- reticulate::py_to_r(out)
  # If returns a plain character vector
  if (is.character(r)) {
    docs <- r
  } else if (is.list(r)) {
    # recent versions may return a dict/list with docs + scores
    if (!is.null(r$Document) && !is.null(r$Score)) {
      o <- order(-as.numeric(r$Score))
      docs <- as.character(r$Document[o])
      scr  <- as.numeric(r$Score[o])
      has_scores <- TRUE
    } else if (!is.null(r$docs)) {
      docs <- as.character(r$docs)
    } else {
      docs <- as.character(unlist(r, use.names = FALSE))
    }
  } else {
    docs <- as.character(r)
  }

  df <- tibble::tibble(rank = seq_along(docs), document = docs)
  if (isTRUE(has_scores)) {
    df$score <- round(scr[seq_along(docs)], 6)
  }
  utils::head(df, top_n)
}
