#' Fit BERTopic from R
#'
#' A high-level wrapper around Python 'BERTopic'. Python dependencies are checked at runtime.
#'
#' @param text Character vector of documents.
#' @param embeddings Optional numeric matrix (n_docs x dim). If supplied, passed through to Python.
#' @param ... Additional arguments forwarded to `bertopic.BERTopic(...)`.
#'
#' @return An S3 object of class "bertopic_r" containing:
#' \itemize{
#'   \item \code{.py}: the underlying Python model (reticulate object)
#'   \item \code{topics}: integer vector of topic assignments
#'   \item \code{probs}: numeric matrix/data frame of topic probabilities (if available)
#' }
#' @examples
#' \dontrun{
#' if (reticulate::py_module_available("bertopic")) {
#'   m <- bertopic_fit(c("a doc", "another doc"))
#'   print(class(m))
#' }
#' }
#' @export
bertopic_fit <- function(text, embeddings = NULL, ...) {
  if (!is.character(text)) {
    rlang::abort("`text` must be a character vector.")
  }
  .need_py()

  # Try to attach the configured env; do not force failure if missing.
  reticulate::use_condaenv(get_py_env(), required = FALSE)

  bt <- reticulate::import("bertopic")
  model <- bt$BERTopic(...)

  py_text <- unname(as.character(text))

  if (is.null(embeddings)) {
    out <- model$fit_transform(py_text)
  } else {
    emb <- as.matrix(embeddings)
    if (!is.numeric(emb)) {
      rlang::abort("`embeddings` must be a numeric matrix.")
    }
    out <- model$fit_transform(py_text, embeddings = reticulate::r_to_py(emb))
  }

  structure(
    list(
      .py    = model,
      topics = unname(reticulate::py_to_r(out[[1]])),
      probs  = tryCatch(reticulate::py_to_r(out[[2]]), error = function(e) NULL)
    ),
    class = "bertopic_r"
  )
}

#' Get topic info as a tibble
#'
#' @param model A "bertopic_r" object returned by [bertopic_fit()].
#' @return A tibble with topic-level information from Python `get_topic_info()`.
#' @export
bertopic_topics <- function(model) {
  if (!inherits(model, "bertopic_r")) {
    rlang::abort("`model` must be a 'bertopic_r' object.")
  }
  info <- model$.py$get_topic_info()
  tibble::as_tibble(reticulate::py_to_r(info))
}

#' Print method for bertopic_r
#' @param x A "bertopic_r" object.
#' @param ... Unused.
#' @export
print.bertopic_r <- function(x, ...) {
  cat("<bertopic_r>\n")
  # Try to read some basic info from Python-side object
  ver <- tryCatch(reticulate::py_get_attr(x$.py, "__class__")$`__name__`, error = function(e) NA)
  cat("  python_class:", ver %||% "BERTopic", "\n", sep = " ")
  ti <- tryCatch(x$.py$get_topic_info(), error = function(e) NULL)
  if (!is.null(ti)) {
    n_topics <- tryCatch(NROW(reticulate::py_to_r(ti)), error = function(e) NA_integer_)
    cat("  topics:", n_topics, "\n")
  }
  invisible(x)
}

# `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
`%||%` <- function(a, b) if (is.null(a) || (is.logical(a) && length(a) == 1 && is.na(a))) b else a
