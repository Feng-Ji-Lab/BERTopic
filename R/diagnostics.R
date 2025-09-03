#' Quick self-check for BERTopic R interface
#'
#' Runs a series of diagnostics:
#' - Is Python available and which executable is used?
#' - Is the 'bertopic' module importable and what's its version?
#' - Minimal round trip: fit -> topics -> transform -> save -> load
#'
#' @return A named list with fields:
#'   \itemize{
#'     \item python_ok (logical)
#'     \item python (character; path)
#'     \item py_version (character)
#'     \item bertopic_ok (logical)
#'     \item bertopic_version (character)
#'     \item roundtrip_ok (logical)
#'     \item details (character vector of messages)
#'   }
#' @export
bertopic_self_check <- function(verbose = TRUE) {
  msgs <- character()
  add <- function(x) { msgs <<- c(msgs, x); if (verbose) message(x) }
  out <- list(
    python_ok = FALSE,
    python = NA_character_,
    py_version = NA_character_,
    bertopic_ok = FALSE,
    bertopic_version = NA_character_,
    roundtrip_ok = FALSE,
    details = character()
  )

  # Attach user-chosen env if configured (no hard failure)
  try(reticulate::use_virtualenv(getOption("BERTopic.env", "r-bertopic"), required = FALSE), silent = TRUE)

  # 1) Python availability
  if (!reticulate::py_available(initialize = FALSE)) {
    add("Python not initialized. Trying to read py_config() ...")
  }
  cfg <- try(reticulate::py_config(), silent = TRUE)
  if (inherits(cfg, "try-error")) {
    add("py_config() failed. Python not available to reticulate.")
    out$details <- msgs
    return(out)
  }
  out$python_ok <- TRUE
  out$python <- cfg$python
  out$py_version <- cfg$version
  add(sprintf("Python: %s (version %s)", out$python, out$py_version))

  # 2) bertopic module
  if (!reticulate::py_module_available("bertopic")) {
    add("Python module 'bertopic' is not importable.")
    out$details <- msgs
    return(out)
  }
  bt <- reticulate::import("bertopic")
  ver <- tryCatch(as.character(bt$`__version__`), error = function(e) NA_character_)
  out$bertopic_ok <- TRUE
  out$bertopic_version <- ver %||% "unknown"
  add(sprintf("bertopic version: %s", out$bertopic_version))

  # 3) Minimal round-trip
  rr_ok <- FALSE
  try({
    # tiny docs
    docs <- c(
      "this is a tiny document about topic modeling",
      "we run a minimal BERTopic round trip from R",
      "another short document for clustering"
    )
    # fit
    model <- bt$BERTopic()
    res <- model$fit_transform(docs)
    stopifnot(length(res[[1]]) == length(docs))
    # topics info
    info <- model$get_topic_info()
    stopifnot(!is.null(info))
    # transform
    newres <- model$transform(list("new unseen text"))
    stopifnot(length(newres[[1]]) == 1)
    # save/load
    tmp <- file.path(tempdir(), "bertopic_r_selfcheck")
    model$save(path = tmp, serialization = "pickle", save_embedding_model = FALSE)
    model2 <- bt$BERTopic$load(path = tmp)
    stopifnot(!is.null(model2))
    rr_ok <- TRUE
  }, silent = TRUE)
  out$roundtrip_ok <- isTRUE(rr_ok)
  add(sprintf("Round-trip fit/transform/save/load: %s", if (out$roundtrip_ok) "OK" else "FAILED"))

  out$details <- msgs
  out
}

#' Summarize Python/BERTopic session info
#'
#' @return A data.frame with key versions and paths
#' @export
bertopic_session_info <- function() {
  py <- try(reticulate::py_config(), silent = TRUE)
  bt_ver <- NA_character_
  if (reticulate::py_module_available("bertopic")) {
    bt <- reticulate::import("bertopic")
    bt_ver <- tryCatch(as.character(bt$`__version__`), error = function(e) NA_character_)
  }
  data.frame(
    python = if (inherits(py, "try-error")) NA_character_ else py$python,
    python_version = if (inherits(py, "try-error")) NA_character_ else py$version,
    bertopic_version = bt_ver,
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a) || (is.logical(a) && length(a) == 1 && is.na(a))) b else a
