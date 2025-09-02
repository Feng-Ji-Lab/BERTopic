#' Set random seed for R and Python backends
#' @param seed Integer seed
#' @export
set_bertopic_seed <- function(seed) {
  stopifnot(length(seed) == 1L, is.numeric(seed), is.finite(seed))
  set.seed(as.integer(seed))
  if (reticulate::py_available(initialize = FALSE)) {
    # Initialize a minimal Python session to set seeds if possible
    reticulate::py_run_string(sprintf("
import os, random
os.environ['PYTHONHASHSEED'] = '%d'
random.seed(%d)
try:
    import numpy as np
    np.random.seed(%d)
except Exception:
    pass
", as.integer(seed), as.integer(seed), as.integer(seed)))
  }
  invisible(TRUE)
}
