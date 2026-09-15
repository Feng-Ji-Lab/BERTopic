#' Save a BERTopic model
#'
#' Pickle saves a file containing the fitted components. Safetensors and
#' PyTorch save a lightweight directory without the reducer or clusterer.
#'
#' @param model A "bertopic_r" model.
#' @param path Destination file or directory.
#' @param serialization One of "pickle", "safetensors", or "pt". "pt" is
#'   forwarded as Python's "pytorch" serialization.
#' @param save_embedding_model Logical; whether to include the embedding model
#'   for pickle or its reference for lightweight formats. Default FALSE.
#'   For lightweight formats, a nonempty Hugging Face model identifier may
#'   also be supplied. FALSE leaves no embedding-model reference in the bundle.
#' @param overwrite Logical; whether an existing destination may be replaced.
#' @return Invisibly returns the normalized path. Backend errors are propagated
#'   without retrying a different serialization.
#' @export
bertopic_save <- function(model, path,
                          serialization = c("pickle", "safetensors", "pt"),
                          save_embedding_model = FALSE,
                          overwrite = FALSE) {
  if (!inherits(model, "bertopic_r"))
    rlang::abort("model must be a 'bertopic_r' object.")
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    rlang::abort("path must be one nonempty file or directory path.")
  serialization <- match.arg(serialization)
  logical_option <- is.logical(save_embedding_model) &&
    length(save_embedding_model) == 1L && !is.na(save_embedding_model)
  reference_option <- is.character(save_embedding_model) &&
    length(save_embedding_model) == 1L && !is.na(save_embedding_model) &&
    nzchar(save_embedding_model)
  if (!(logical_option || (reference_option && serialization != "pickle")))
    rlang::abort("save_embedding_model must be TRUE/FALSE, or a nonempty model identifier for lightweight formats.")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite))
    rlang::abort("overwrite must be TRUE or FALSE.")
  .need_py()
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (file.exists(path) || dir.exists(path)) {
    if (!overwrite) rlang::abort("Destination exists; use overwrite = TRUE to replace it.")
    target <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (identical(target, normalizePath(getwd(), winslash = "/")) ||
        identical(dirname(target), target))
      rlang::abort("Cannot overwrite the working directory or a filesystem root.")
    unlink(target, recursive = TRUE, force = TRUE)
    if (file.exists(target) || dir.exists(target)) rlang::abort("Could not remove the existing destination.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  python_format <- if (serialization == "pt") "pytorch" else serialization
  omit_reference <- serialization != "pickle" && identical(save_embedding_model, FALSE)
  # BERTopic 0.16.0 passes an invalid argument to its logger when this option
  # is false. Skip that warning branch, then remove any inferred reference
  # to preserve the caller's request without changing the backend.
  model$.py$save(path = path, serialization = python_format,
                save_embedding_model = if (omit_reference) TRUE else save_embedding_model)
  if (omit_reference) {
    config_path <- file.path(path, "config.json")
    if (!file.exists(config_path)) rlang::abort("Backend did not produce a lightweight config.json.")
    json <- reticulate::import("json", convert = FALSE)
    config <- json$loads(paste(readLines(config_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
    config$pop("embedding_model", NULL)
    config_text <- reticulate::py_to_r(json$dumps(config, indent = 2L))
    con <- file(config_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(config_text, con, useBytes = TRUE)
  }
  if (!file.exists(path) && !dir.exists(path)) rlang::abort("Backend did not create the requested destination.")
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Load a BERTopic model
#'
#' Load a BERTopic model from disk that was saved with [bertopic_save()].
#'
#' @param path Path used in [bertopic_save()] (file or directory).
#' @return A "bertopic_r" object with the loaded Python model.
#' @export
bertopic_load <- function(path) {
  .need_py()
  reticulate::use_condaenv(get_py_env(), required = FALSE)
  bt <- reticulate::import("bertopic")
  py_model <- NULL
  ok <- TRUE
  tryCatch({
    py_model <- bt$BERTopic$load(path = path)
  }, error = function(e) {
    ok <<- FALSE
  })
  if (!ok) {
    py_model <- bt$BERTopic$load(path)
  }
  topics <- tryCatch(unname(reticulate::py_to_r(py_model$topics_)), error = function(e) NULL)
  probs <- tryCatch(reticulate::py_to_r(py_model$probabilities_), error = function(e) NULL)
  structure(list(.py = py_model, topics = topics, probs = probs), class = "bertopic_r")
}
