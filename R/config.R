#' Use a virtualenv for BERTopic (session-scoped)
#' @param env Virtualenv name. Defaults to option BERTopic.env
#' @export
use_bertopic_virtualenv <- function(env = getOption("BERTopic.env", "r-bertopic")) {
  reticulate::use_virtualenv(env, required = FALSE)
  invisible(env)
}

#' Use a conda env for BERTopic (session-scoped)
#' @param env Conda env name
#' @export
use_bertopic_condaenv <- function(env) {
  reticulate::use_condaenv(env, required = FALSE)
  invisible(env)
}

#' Is Python + BERTopic available?
#' @return logical
#' @export
bertopic_available <- function() {
  reticulate::py_available(initialize = FALSE) &&
    reticulate::py_module_available("bertopic")
}
