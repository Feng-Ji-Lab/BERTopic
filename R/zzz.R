#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # Do not network, do not fail here.
  op <- options(bertopicr.env = "r-bertopic")
  invisible(op)
}
