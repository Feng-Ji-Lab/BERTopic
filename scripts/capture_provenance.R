# Capture reproducibility metadata for a release rerun.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = "") {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) return(default)
  args[[index + 1L]]
}

legacy_output <- if (length(args) && !startsWith(args[[1]], "--")) args[[1]] else ""
out <- get_arg(
  "--output",
  if (nzchar(legacy_output)) legacy_output else file.path("provenance", "session.txt")
)
python <- get_arg("--python", Sys.getenv("RETICULATE_PYTHON", unset = ""))
check_status <- get_arg("--check-status", "not recorded")
repo <- normalizePath(".", winslash = "/", mustWork = TRUE)

if (nzchar(python)) {
  python <- normalizePath(python, winslash = "/", mustWork = TRUE)
  Sys.setenv(RETICULATE_PYTHON = python)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload to capture provenance from the source tree.")
}
pkgload::load_all(repo, quiet = TRUE)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

git <- function(...) {
  result <- tryCatch(
    system2(
      "git",
      c("-c", paste0("safe.directory=", repo), "-C", repo, ...),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(result, "status", exact = TRUE)
  if (!is.null(status) && status != 0L) {
    return(paste0("ERROR(", status, "): ", paste(result, collapse = " | ")))
  }
  paste(result, collapse = " | ")
}

thread_vars <- c(
  "PYTHONHASHSEED", "OMP_NUM_THREADS", "MKL_NUM_THREADS",
  "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS"
)
thread_settings <- paste(
  paste0(thread_vars, "=", Sys.getenv(thread_vars, unset = "<unset>")),
  collapse = "; "
)
lines <- c(
  paste("timestamp_utc:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste("source_package_version:", as.character(utils::packageVersion("BERTopic"))),
  paste("git_commit:", git("rev-parse", "HEAD")),
  paste("git_status:", git("status", "--short")),
  paste("r_cmd_check:", check_status),
  paste("thread_settings:", thread_settings),
  paste("os:", paste(names(Sys.info()), Sys.info(), sep = "=", collapse = "; ")),
  paste("R:", R.version.string),
  capture.output(sessionInfo())
)

backend <- tryCatch(
  BERTopic::bertopic_session_info(),
  error = function(e) paste0("ERROR: ", conditionMessage(e))
)
lines <- c(lines, "", "backend:", capture.output(print(backend)))
writeLines(lines, out, useBytes = TRUE)
message("Wrote provenance: ", normalizePath(out, winslash = "/", mustWork = FALSE))
