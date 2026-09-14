# Reproducible R-vs-Python BERTopic benchmark for release 0.1.1.
options(stringsAsFactors = FALSE)
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
} else {
  stop("Install pkgload to run this benchmark")
}
get_arg <- function(flag, default) {
  i <- match(flag, cli)
  if (is.na(i) || i == length(cli)) return(default)
  cli[[i + 1L]]
}
output_dir <- get_arg("--output", "benchmark/results")
seed <- as.integer(get_arg("--seed", "42"))
embedding_dim <- as.integer(get_arg("--embedding-dim", "16"))
max_docs <- as.integer(get_arg("--max-docs", "2247"))
if (is.na(seed) || is.na(embedding_dim) || is.na(max_docs) || embedding_dim < 1L || max_docs < 1L) stop("Invalid --seed, --embedding-dim, or --max-docs")
library(reticulate)
data(sms_spam, package = "BERTopic")
docs <- as.character(sms_spam$text)[seq_len(min(max_docs, nrow(sms_spam)))]

Sys.setenv(PYTHONHASHSEED = as.character(seed), OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
set_bertopic_seed(seed)
set.seed(seed)
# Fixed embeddings make the two calls directly comparable.
embeddings <- matrix(rnorm(length(docs) * embedding_dim), nrow = length(docs), ncol = embedding_dim)
py_emb <- r_to_py(embeddings)

measure <- function(expr) {
  st <- proc.time()
  value <- force(expr)
  elapsed <- unname((proc.time() - st)[["elapsed"]])
  list(value = value, elapsed_seconds = elapsed)
}

r_run <- measure(bertopic_fit(docs, embeddings = embeddings, calculate_probabilities = TRUE))
r_model <- r_run$value
.need_py()
bt <- import("bertopic")
py_run <- measure({
  model <- bt$BERTopic(calculate_probabilities = TRUE)
  result <- model$fit_transform(docs, embeddings = py_emb)
  list(model = model, result = result)
})
py_model <- py_run$value$model
py_result <- py_run$value$result

r_topics <- as.integer(r_model$topics)
py_topics <- as.integer(py_to_r(py_result[[1]]))
r_probs <- r_model$probs
py_probs <- tryCatch(py_to_r(py_result[[2]]), error = function(e) NULL)
r_info <- bertopic_topics(r_model)
py_info <- as.data.frame(py_to_r(py_model$get_topic_info()))

equal_or_na <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}
rows <- data.frame(
  metric = c("documents", "r_topics_equal_python", "topic_info_equal", "r_probability_dimensions", "python_probability_dimensions", "probabilities_equal"),
  value = c(
    length(docs),
    equal_or_na(r_topics, py_topics),
    equal_or_na(r_info$Topic, py_info$Topic),
    paste(dim(as.matrix(r_probs)), collapse = " x "),
    if (is.null(py_probs)) NA_character_ else paste(dim(as.matrix(py_probs)), collapse = " x "),
    equal_or_na(r_probs, py_probs)
  ), stringsAsFactors = FALSE
)
rows <- rbind(rows,
  data.frame(metric = c("r_fit_seconds", "python_fit_seconds"), value = c(r_run$elapsed_seconds, py_run$elapsed_seconds)))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(rows, file.path(output_dir, "summary.csv"), row.names = FALSE)
writeLines(c(
  paste("release:", as.character(utils::packageVersion("BERTopic"))),
  paste("seed:", seed),
  paste("documents:", length(docs)),
  paste("R:", R.version.string),
  capture.output(print(tryCatch(BERTopic::bertopic_session_info(), error = function(e) conditionMessage(e))))
), file.path(output_dir, "environment.txt"))
saveRDS(list(r_topics = r_topics, py_topics = py_topics, r_info = r_info, py_info = py_info, r_probs = r_probs, py_probs = py_probs), file.path(output_dir, "raw.rds"))
print(rows)
