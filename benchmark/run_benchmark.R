# Run paired fresh-process R-versus-Python BERTopic benchmarks on Windows.
options(stringsAsFactors = FALSE, digits = 17)
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = "") {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) return(default)
  args[[index + 1L]]
}
as_bool <- function(value) tolower(value) %in% c("1", "true", "yes")

repo <- normalizePath(".", winslash = "/", mustWork = TRUE)
python <- get_arg("--python", Sys.getenv("RETICULATE_PYTHON", unset = ""))
if (!nzchar(python)) stop("Pass --python with the exact benchmark interpreter.", call. = FALSE)
python <- normalizePath(python, winslash = "/", mustWork = TRUE)
documents <- normalizePath(get_arg("--documents", "data/sms_spam.csv"), winslash = "/", mustWork = TRUE)
embeddings <- normalizePath(get_arg("--embeddings", "benchmark/inputs/reduced_embeddings.npy"), winslash = "/", mustWork = TRUE)
output <- get_arg("--output", "benchmark/results")
repetitions <- as.integer(get_arg("--repetitions", "5"))
max_docs <- as.integer(get_arg("--max-docs", "2247"))
seed <- as.integer(get_arg("--seed", "42"))
min_cluster_size <- as.integer(get_arg("--min-cluster-size", "10"))
package_mode <- get_arg("--package-mode", "installed")
expected_package_version <- get_arg("--expected-package-version", "0.1.2")
expected_bertopic_version <- get_arg("--expected-bertopic-version", "0.16.0")
release_tag <- get_arg("--release-tag", "v0.1.2")
package_archive <- get_arg("--package-archive", if (package_mode == "installed") paste0("BERTopic_", expected_package_version, ".tar.gz") else "")
if (nzchar(package_archive)) package_archive <- normalizePath(package_archive, winslash = "/", mustWork = TRUE)
overwrite <- as_bool(get_arg("--overwrite", "false"))
if (anyNA(c(repetitions, max_docs, seed, min_cluster_size)) || repetitions < 1L || max_docs < 1L || min_cluster_size < 2L) {
  stop("Invalid numeric benchmark parameter.", call. = FALSE)
}
if (!package_mode %in% c("installed", "source")) stop("--package-mode must be installed or source.", call. = FALSE)

output <- normalizePath(output, winslash = "/", mustWork = FALSE)
if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  if (!overwrite) stop("Output directory is not empty; pass --overwrite true or choose another directory.", call. = FALSE)
  unlink(output, recursive = TRUE, force = TRUE)
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
logs <- file.path(output, "logs")
runs_root <- file.path(output, "runs")
dir.create(logs, recursive = TRUE)
dir.create(runs_root, recursive = TRUE)

rscript <- file.path(R.home("bin"), "Rscript.exe")
r_worker <- file.path(repo, "benchmark", "r_worker.R")
python_worker <- file.path(repo, "benchmark", "python_worker.py")
# On Windows, system2(env = ...) appends assignments as command arguments.
# Set them in the driver so every fresh worker inherits the same environment.
Sys.setenv(
  RETICULATE_PYTHON = python,
  PYTHONHASHSEED = as.character(seed),
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)
run_process <- function(command, arguments, log_base) {
  stdout_path <- paste0(log_base, ".stdout.log")
  stderr_path <- paste0(log_base, ".stderr.log")
  started <- proc.time()[["elapsed"]]
  status <- system2(command, arguments, stdout = stdout_path, stderr = stderr_path)
  elapsed <- unname(proc.time()[["elapsed"]] - started)
  if (!identical(status, 0L)) {
    paths <- c(stdout_path, stderr_path)
    detail <- unlist(lapply(paths[file.exists(paths)], function(path) {
      c(paste0("--- ", basename(path), " ---"), utils::tail(readLines(path, warn = FALSE), 30L))
    }))
    stop(sprintf("Worker failed (%s):\n%s", status, paste(detail, collapse = "\n")), call. = FALSE)
  }
  elapsed
}
provenance_args <- c(
  shQuote(file.path(repo, "benchmark", "capture_provenance.py")),
  "--repo", shQuote(repo), "--documents", shQuote(documents),
  "--embeddings", shQuote(embeddings),
  "--output", shQuote(file.path(output, "provenance.json")),
  "--release-tag", shQuote(release_tag),
  if (nzchar(package_archive)) c("--package-archive", shQuote(package_archive))
)
invisible(run_process(python, provenance_args, file.path(logs, "provenance")))

worker_args <- function(worker_output) c(
  "--documents", shQuote(documents),
  "--embeddings", shQuote(embeddings),
  "--output", shQuote(worker_output),
  "--max-docs", max_docs,
  "--seed", seed,
  "--min-cluster-size", min_cluster_size
)
read_metrics <- function(path) {
  value <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  stats::setNames(value$value, value$metric)
}
read_probabilities <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(NULL)
  as.matrix(utils::read.csv(path, header = FALSE, check.names = FALSE))
}
read_output <- function(path) list(
  topics = scan(file.path(path, "topics.csv"), what = integer(), quiet = TRUE),
  info = utils::read.csv(file.path(path, "topic_info.csv"), stringsAsFactors = FALSE, check.names = FALSE),
  terms = utils::read.csv(file.path(path, "topic_terms.csv"), stringsAsFactors = FALSE, check.names = FALSE),
  probabilities = read_probabilities(file.path(path, "probabilities.csv")),
  metrics = read_metrics(file.path(path, "metrics.csv"))
)
max_difference <- function(a, b) {
  if (is.null(a) || is.null(b) || !identical(dim(a), dim(b))) return(Inf)
  if (!length(a)) return(0)
  max(abs(as.numeric(a) - as.numeric(b)))
}

runs <- vector("list", repetitions * 2L)
equivalence <- vector("list", repetitions)
for (iteration in seq_len(repetitions)) {
  r_output <- file.path(runs_root, sprintf("r-%02d", iteration))
  py_output <- file.path(runs_root, sprintf("python-%02d", iteration))
  dir.create(r_output, recursive = TRUE)
  dir.create(py_output, recursive = TRUE)

  run_r <- function() run_process(
    rscript,
    c(shQuote(r_worker), "--repo", shQuote(repo), "--package-mode", package_mode, worker_args(r_output)),
    file.path(logs, sprintf("r-%02d", iteration))
  )
  run_python <- function() run_process(
    python,
    c(shQuote(python_worker), worker_args(py_output)),
    file.path(logs, sprintf("python-%02d", iteration))
  )
  if (iteration %% 2L == 1L) {
    r_cold <- run_r()
    py_cold <- run_python()
  } else {
    py_cold <- run_python()
    r_cold <- run_r()
  }
  r_value <- read_output(r_output)
  py_value <- read_output(py_output)
  if (!identical(as.character(r_value$metrics[["package_version"]]), expected_package_version)) {
    stop(sprintf("R worker used package %s; expected %s.", r_value$metrics[["package_version"]], expected_package_version), call. = FALSE)
  }
  backend_versions <- c(r_value$metrics[["bertopic_version"]], py_value$metrics[["bertopic_version"]])
  if (!all(as.character(backend_versions) == expected_bertopic_version)) {
    stop(sprintf("Workers used BERTopic %s; expected %s.", paste(backend_versions, collapse = ", "), expected_bertopic_version), call. = FALSE)
  }
  probability_difference <- max_difference(r_value$probabilities, py_value$probabilities)
  weight_difference <- max_difference(as.matrix(r_value$terms$Weight), as.matrix(py_value$terms$Weight))
  equivalence[[iteration]] <- data.frame(
    iteration = iteration,
    topics_equal = identical(r_value$topics, py_value$topics),
    metadata_equal = identical(r_value$info, py_value$info),
    term_keys_equal = identical(r_value$terms[, c("Topic", "Rank", "Term")], py_value$terms[, c("Topic", "Rank", "Term")]),
    term_weights_equal_1e12 = is.finite(weight_difference) && weight_difference <= 1e-12,
    max_term_weight_difference = weight_difference,
    probability_dimensions_equal = identical(dim(r_value$probabilities), dim(py_value$probabilities)),
    probabilities_equal_1e12 = is.finite(probability_difference) && probability_difference <= 1e-12,
    max_probability_difference = probability_difference
  )
  runs[[2L * iteration - 1L]] <- data.frame(
    iteration = iteration, path = "R", fit_seconds = as.numeric(r_value$metrics[["fit_seconds"]]),
    cold_seconds = r_cold, peak_rss_mib = as.numeric(r_value$metrics[["peak_rss_bytes"]]) / 1024^2
  )
  runs[[2L * iteration]] <- data.frame(
    iteration = iteration, path = "Python", fit_seconds = as.numeric(py_value$metrics[["fit_seconds"]]),
    cold_seconds = py_cold, peak_rss_mib = as.numeric(py_value$metrics[["peak_rss_bytes"]]) / 1024^2
  )
  message(sprintf("Completed pair %d/%d", iteration, repetitions))
}
runs <- do.call(rbind, runs)
equivalence <- do.call(rbind, equivalence)
utils::write.csv(runs, file.path(output, "runs.csv"), row.names = FALSE)
utils::write.csv(equivalence, file.path(output, "equivalence.csv"), row.names = FALSE)

summarize <- function(values) c(median = stats::median(values), q1 = unname(stats::quantile(values, 0.25)), q3 = unname(stats::quantile(values, 0.75)))
r_rows <- runs[runs$path == "R", ]
py_rows <- runs[runs$path == "Python", ]
summary <- do.call(rbind, list(
  data.frame(metric = "r_fit_seconds", t(summarize(r_rows$fit_seconds)), unit = "seconds"),
  data.frame(metric = "python_fit_seconds", t(summarize(py_rows$fit_seconds)), unit = "seconds"),
  data.frame(metric = "fit_difference", t(summarize(r_rows$fit_seconds - py_rows$fit_seconds)), unit = "seconds"),
  data.frame(metric = "r_cold_seconds", t(summarize(r_rows$cold_seconds)), unit = "seconds"),
  data.frame(metric = "python_cold_seconds", t(summarize(py_rows$cold_seconds)), unit = "seconds"),
  data.frame(metric = "cold_difference", t(summarize(r_rows$cold_seconds - py_rows$cold_seconds)), unit = "seconds"),
  data.frame(metric = "r_peak_rss", t(summarize(r_rows$peak_rss_mib)), unit = "MiB"),
  data.frame(metric = "python_peak_rss", t(summarize(py_rows$peak_rss_mib)), unit = "MiB"),
  data.frame(metric = "peak_rss_difference", t(summarize(r_rows$peak_rss_mib - py_rows$peak_rss_mib)), unit = "MiB")
))
row.names(summary) <- NULL
utils::write.csv(summary, file.path(output, "summary.csv"), row.names = FALSE)

manifest_path <- sub("\\.npy$", ".manifest.json", embeddings)
equality_columns <- grepl("equal", names(equivalence))
all_equivalence <- all(unlist(equivalence[, equality_columns, drop = FALSE]))
manifest <- c(
  paste("timestamp_utc:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste("git_commit:", system2("git", c("-c", paste0("safe.directory=", repo), "-C", repo, "rev-parse", "HEAD"), stdout = TRUE)),
  paste("package_mode:", package_mode),
  paste("python:", python),
  paste("documents:", documents),
  paste("embeddings:", embeddings),
  paste("repetitions:", repetitions),
  paste("max_docs:", max_docs),
  paste("seed:", seed),
  paste("min_cluster_size:", min_cluster_size),
  paste("expected_package_version:", expected_package_version),
  paste("expected_bertopic_version:", expected_bertopic_version),
  "process_order: alternating; R first on odd iterations, Python first on even iterations",
  paste("all_equivalence_checks_passed:", all_equivalence),
  "",
  "input_manifest:",
  if (file.exists(manifest_path)) readLines(manifest_path, warn = FALSE, encoding = "UTF-8") else "<missing>"
)
writeLines(manifest, file.path(output, "manifest.txt"), useBytes = TRUE)
print(equivalence)
print(summary)
if (!all_equivalence) stop("One or more R-versus-Python equivalence checks failed.", call. = FALSE)