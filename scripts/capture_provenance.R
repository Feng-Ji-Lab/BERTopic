# Capture reproducibility metadata for a release rerun.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else file.path("provenance", "session.txt")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
repo <- normalizePath(".", winslash = "/", mustWork = TRUE)
git <- function(...) tryCatch(system2("git", c(...), stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
lines <- c(
  paste("timestamp:", format(Sys.time(), tz = "UTC")),
  paste("git_commit:", paste(git("rev-parse", "HEAD"), collapse = " ")),
  paste("git_status:", paste(git("status", "--short"), collapse = " | ")),
  paste("os:", paste(Sys.info(), collapse = "; ")),
  paste("R:", R.version.string),
  capture.output(sessionInfo())
)
if (requireNamespace("reticulate", quietly = TRUE)) {
  lines <- c(lines, capture.output(print(tryCatch(BERTopic::bertopic_session_info(), error = function(e) conditionMessage(e)))))
}
writeLines(lines, out, useBytes = TRUE)
message("Wrote provenance: ", normalizePath(out, winslash = "/", mustWork = FALSE))
