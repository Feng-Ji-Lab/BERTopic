# Run one R-interface BERTopic benchmark process.
options(stringsAsFactors = FALSE, digits = 17)
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = "") {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) return(default)
  args[[index + 1L]]
}

repo <- normalizePath(get_arg("--repo", "."), winslash = "/", mustWork = TRUE)
documents_path <- normalizePath(get_arg("--documents"), winslash = "/", mustWork = TRUE)
embeddings_path <- normalizePath(get_arg("--embeddings"), winslash = "/", mustWork = TRUE)
output <- get_arg("--output")
max_docs <- as.integer(get_arg("--max-docs", "2247"))
seed <- as.integer(get_arg("--seed", "42"))
min_cluster_size <- as.integer(get_arg("--min-cluster-size", "10"))
package_mode <- get_arg("--package-mode", "installed")
if (!package_mode %in% c("installed", "source")) stop("--package-mode must be installed or source")
if (!nzchar(output) || anyNA(c(max_docs, seed, min_cluster_size))) stop("Invalid worker arguments")

if (package_mode == "source") {
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required for --package-mode source")
  pkgload::load_all(repo, quiet = TRUE)
} else {
  library(BERTopic)
}
library(reticulate)
set_bertopic_seed(seed)
set.seed(seed)

documents <- utils::read.csv(documents_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")$text
documents <- as.character(utils::head(documents, max_docs))
np <- import("numpy", convert = FALSE)
embeddings <- py_to_r(np$load(embeddings_path, allow_pickle = FALSE))
embeddings <- embeddings[seq_along(documents), , drop = FALSE]
if (nrow(embeddings) != length(documents)) stop("The frozen embedding rows do not match the selected documents")

dimensionality <- import("bertopic.dimensionality", convert = FALSE)
hdbscan <- import("hdbscan", convert = FALSE)
text_features <- import("sklearn.feature_extraction.text", convert = FALSE)
dimensionality_model <- dimensionality$BaseDimensionalityReduction()
cluster_model <- hdbscan$HDBSCAN(
  min_cluster_size = as.integer(min_cluster_size),
  metric = "euclidean",
  cluster_selection_method = "eom",
  prediction_data = TRUE
)
vectorizer_model <- text_features$CountVectorizer(stop_words = "english")

started <- proc.time()[["elapsed"]]
model <- bertopic_fit(
  documents,
  embeddings = embeddings,
  umap_model = dimensionality_model,
  hdbscan_model = cluster_model,
  vectorizer_model = vectorizer_model,
  calculate_probabilities = TRUE
)
fit_seconds <- unname(proc.time()[["elapsed"]] - started)

dir.create(output, recursive = TRUE, showWarnings = FALSE)
writeLines(as.character(as.integer(model$topics)), file.path(output, "topics.csv"), useBytes = TRUE)
if (is.null(model$probs)) {
  writeLines(character(), file.path(output, "probabilities.csv"), useBytes = TRUE)
} else {
  utils::write.table(
    as.matrix(model$probs), file.path(output, "probabilities.csv"),
    sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE
  )
}
info <- as.data.frame(bertopic_topics(model))[, c("Topic", "Count", "Name"), drop = FALSE]
utils::write.csv(info, file.path(output, "topic_info.csv"), row.names = FALSE, fileEncoding = "UTF-8")
terms <- do.call(rbind, lapply(as.integer(info$Topic), function(topic) {
  value <- as.data.frame(bertopic_topic_terms(model, topic, top_n = 10L))
  if (!nrow(value)) return(NULL)
  data.frame(Topic = topic, Rank = seq_len(nrow(value)), Term = value$term, Weight = value$weight)
}))
utils::write.csv(terms, file.path(output, "topic_terms.csv"), row.names = FALSE, fileEncoding = "UTF-8")

psutil <- import("psutil", convert = TRUE)
versions <- bertopic_session_info()
module_version <- function(name) {
  match <- versions$modules$version[versions$modules$module == name]
  if (length(match)) match[[1L]] else NA_character_
}
metrics <- data.frame(
  metric = c(
    "fit_seconds", "peak_rss_bytes", "R_version", "package_version", "python_version",
    "bertopic_version", "numpy_version", "sklearn_version", "torch_version",
    "transformers_version", "umap_version", "hdbscan_version", "package_library", "documents", "topics", "outliers"
  ),
  value = c(
    fit_seconds,
    as.numeric(psutil$Process()$memory_info()$peak_wset),
    R.version.string,
    as.character(utils::packageVersion("BERTopic")),
    as.character(versions$version),
    versions$bertopic_version,
    versions$numpy_version,
    module_version("sklearn"),
    module_version("torch"),
    module_version("transformers"),
    module_version("umap"),
    module_version("hdbscan"),
    find.package("BERTopic"),
    length(documents),
    nrow(info),
    sum(as.integer(model$topics) == -1L)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(metrics, file.path(output, "metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"), useBytes = TRUE)
