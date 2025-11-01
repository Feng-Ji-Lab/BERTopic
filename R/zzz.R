#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- list(BERTopic.env = "r-bertopic")
  toset <- toset[setdiff(names(toset), names(op))]
  if (length(toset)) options(toset)
  invisible()
}
