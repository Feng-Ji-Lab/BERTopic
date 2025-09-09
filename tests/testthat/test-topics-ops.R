#' Relabel topics
#'
#' Set custom labels for topics. Accepts a named character vector or a
#' data.frame with columns `topic` and `label`. The function is robust to
#' backend signature differences by trying multiple call patterns.
#'
#' @param model A "bertopic_r" model.
#' @param labels A named character vector (names are topic ids) or a data.frame.
#' @return The input model (invisibly).
#' @export
bertopic_set_topic_labels <- function(model, labels) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()

  # normalize input -> integer topic ids + character labels
  if (is.character(labels) && !is.null(names(labels))) {
    ids    <- suppressWarnings(as.integer(names(labels)))
    labvec <- as.character(unname(labels))
  } else if (is.data.frame(labels)) {
    if (!all(c("topic", "label") %in% names(labels))) {
      rlang::abort("Data frame `labels` must contain columns `topic` and `label`.")
    }
    ids    <- suppressWarnings(as.integer(labels$topic))
    labvec <- as.character(labels$label)
  } else {
    rlang::abort("`labels` must be a named character vector or data.frame(topic,label).")
  }
  if (anyNA(ids)) rlang::abort("Topic ids must be integers (no NA).")
  if (length(ids) != length(labvec)) rlang::abort("Lengths of topics and labels must match.")
  if (!length(ids)) return(invisible(model))

  # helper: did labels take effect?
  .labels_applied <- function() {
    info <- try(bertopic_topics(model), silent = TRUE)
    if (inherits(info, "try-error") || is.null(info) || !"Name" %in% names(info)) return(FALSE)
    any(info$Name %in% labvec | grepl(paste0("^(", paste0(unique(labvec), collapse="|"), ")"), info$Name))
  }

  # 1) Preferred: parallel vectors (topics + labels)
  ok <- TRUE
  res <- try(model$.py$set_topic_labels(labels = as.list(labvec), topics = as.integer(ids)), silent = TRUE)
  if (inherits(res, "try-error") || !.labels_applied()) ok <- FALSE
  if (ok) return(invisible(model))

  # 2) Mapping with integer keys: build Python dict(int->str) via builtins.dict(list of tuples)
  ok <- TRUE
  builtins <- reticulate::import_builtins()
  kv_pairs <- lapply(seq_along(ids), function(i) reticulate::tuple(as.integer(ids[i]), as.character(labvec[i])))
  py_map   <- try(builtins$dict(kv_pairs), silent = TRUE)
  if (!inherits(py_map, "try-error")) {
    res <- try(model$.py$set_topic_labels(labels = py_map), silent = TRUE)
    if (inherits(res, "try-error") || !.labels_applied()) ok <- FALSE
    if (ok) return(invisible(model))
  } else {
    ok <- FALSE
  }

  # 3) Fallback: named list (string keys) – older backends sometimes accept this
  mapping <- as.list(as.character(labvec)); names(mapping) <- as.character(ids)
  res <- try(model$.py$set_topic_labels(labels = mapping), silent = TRUE)
  if (!inherits(res, "try-error") && .labels_applied()) return(invisible(model))

  # 4) Last resort: positional variants
  res <- try(model$.py$set_topic_labels(as.list(labvec), as.integer(ids)), silent = TRUE)
  if (!inherits(res, "try-error") && .labels_applied()) return(invisible(model))
  res <- try(model$.py$set_topic_labels(mapping), silent = TRUE)
  if (!inherits(res, "try-error") && .labels_applied()) return(invisible(model))

  rlang::abort("Python `set_topic_labels()` did not apply labels (backend signature mismatch).")
}
