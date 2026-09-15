# Rerun the manuscript worked example against the installed Windows release.
options(stringsAsFactors = FALSE, digits = 17)
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  get_arg <- function(flag, default = "") {
    i <- match(flag, args)
    if (is.na(i) || i == length(args)) default else args[[i + 1L]]
  }
  python <- normalizePath(get_arg("--python", Sys.getenv("RETICULATE_PYTHON")), winslash = "/", mustWork = TRUE)
  output <- get_arg("--output", "benchmark/example-results")
  seed <- as.integer(get_arg("--seed", "42"))
  max_docs <- as.integer(get_arg("--max-docs", "2247"))
  revision <- get_arg("--model-revision", "1110a243fdf4706b3f48f1d95db1a4f5529b4d41")
  archive <- normalizePath(get_arg("--package-archive", "BERTopic_0.1.2.tar.gz"), winslash = "/", mustWork = TRUE)
  if (anyNA(c(seed, max_docs)) || max_docs < 20L) stop("Invalid example parameters")
  if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) stop("Example output directory must be empty")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output, winslash = "/", mustWork = TRUE)
  sink(file.path(output, "console.log"), split = TRUE)
  on.exit(sink(), add = TRUE)
  Sys.setenv(RETICULATE_PYTHON = python, PYTHONHASHSEED = as.character(seed),
             OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
             OPENBLAS_NUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1")
  for (pkg in c("Matrix", "jsonlite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Install the R package ", pkg, " before running the example")
  source("benchmark/example_io.R", local = TRUE)
  library(BERTopic)
  library(reticulate)
  if (packageVersion("BERTopic") != "0.1.2") stop("The worked example requires installed BERTopic 0.1.2")
  backend <- bertopic_session_info()
  backend$version <- as.character(backend$version)
  if (!identical(backend$bertopic_version, "0.16.0")) stop("The worked example requires Python BERTopic 0.16.0")
  print(backend)
  data("sms_spam", package = "BERTopic")
  sms_spam <- head(sms_spam, max_docs)
  docs <- as.character(sms_spam$text)
  write_example_csv(sms_spam, file.path(output, "documents.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  cat("Documents:", length(docs), "\n")
  print(table(sms_spam$label))
  chars <- nchar(docs, type = "chars")
  words <- lengths(strsplit(trimws(docs), "\\s+"))
  descriptions <- data.frame(
    measure = c("characters", "whitespace_words"),
    median = c(median(chars), median(words)),
    q1 = c(quantile(chars, .25), quantile(words, .25)),
    q3 = c(quantile(chars, .75), quantile(words, .75)),
    minimum = c(min(chars), min(words)), maximum = c(max(chars), max(words))
  )
  write_example_csv(descriptions, file.path(output, "data_summary.csv"), row.names = FALSE)
  set_bertopic_seed(seed)
  set.seed(seed)
  st <- import("sentence_transformers", convert = FALSE)
  encoder <- st$SentenceTransformer("all-MiniLM-L6-v2", revision = revision)
  umap <- import("umap", convert = FALSE)
  reducer <- umap$UMAP(n_neighbors = 15L, n_components = 5L, min_dist = 0,
                       metric = "cosine", random_state = seed)
  cat("Fitting the full worked-example pipeline...\n")
  model <- bertopic_fit(docs, embedding_model = encoder, umap_model = reducer,
                       calculate_probabilities = TRUE)
  print(model)
  info <- bertopic_topics(model)
  print(head(info, 8L))
  write_example_csv(info, file.path(output, "topic_info.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  valid <- info$Topic[info$Topic != -1L]
  if (length(valid) < 3L) stop("Worked example requires at least three non-outlier topics for its visualizations")
  terms <- do.call(rbind, lapply(info$Topic, function(id) {
    value <- as.data.frame(bertopic_topic_terms(model, id, top_n = 10L))
    data.frame(Topic = id, Rank = seq_len(nrow(value)), value)
  }))
  write_example_csv(terms, file.path(output, "topic_terms.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  document_info <- bertopic_get_document_info(model, docs)
  write_example_csv(document_info, file.path(output, "document_info.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  query <- bertopic_find_topics(model, "free subscription message", top_n = 5L)
  print(query)
  write_example_csv(query, file.path(output, "query_topics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  representatives <- bertopic_get_representative_docs(model, valid[1L], top_n = 3L)
  print(representatives)
  write_example_csv(representatives, file.path(output, "representative_documents.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  new_docs <- c("Love you so much, see you tonight.", "Free subscription! Reply STOP to unsubscribe.")
  predictions <- predict(model, new_docs, type = "both")
  print(predictions$topics)
  write_example_csv(data.frame(text = new_docs, Topic = as.integer(predictions$topics)), file.path(output, "predictions.csv"), row.names = FALSE)
  write.table(as.matrix(predictions$probs), file.path(output, "prediction_probabilities.csv"), sep = ",",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  matrix <- bertopic_as_document_topic_matrix(model, sparse = FALSE, prefix = TRUE)
  sparse <- bertopic_as_document_topic_matrix(model, sparse = TRUE, prefix = TRUE)
  stopifnot(nrow(matrix) == length(docs), isTRUE(all.equal(matrix, as.matrix(sparse), tolerance = 1e-12)))
  write_example_csv(matrix, file.path(output, "document_topic_matrix.csv"), row.names = FALSE)
  cat("Document-topic matrix:", dim(matrix), "\n")
  bertopic_visualize_topics(model, file = file.path(output, "viz_topics_wrapper.html"))
  bertopic_visualize_heatmap(model, file = file.path(output, "viz_heatmap_wrapper.html"))
  # Retain API-produced HTML and add self-contained HTML and Plotly JSON for offline use.
  for (name in c("topics", "heatmap")) {
    figure <- py_get_attr(model$.py, paste0("visualize_", name))()
    html <- py_to_r(figure$to_html(full_html = TRUE, include_plotlyjs = TRUE))
    writeLines(html, file.path(output, paste0("viz_", name, ".html")), useBytes = TRUE)
    writeLines(py_to_r(figure$to_json()), file.path(output, paste0("viz_", name, ".json")), useBytes = TRUE)
  }
  # Static figures use this worked-example model, independently of the controlled benchmark.
  top <- head(info[info$Topic != -1L, ], 8L)
  draw_counts <- function() {
    par(mar = c(4, 11, 3, 1))
    barplot(rev(top$Count), names.arg = rev(top$Name), horiz = TRUE, las = 1,
            col = "#3178A5", border = NA, cex.names = .65,
            xlab = "Messages", main = "Largest worked-example topics")
  }
  render <- function(name, draw, width = 9, height = 5) {
    png(file.path(output, paste0(name, ".png")), width = width * 200, height = height * 200, res = 200)
    draw(); dev.off()
    pdf(file.path(output, paste0(name, ".pdf")), width = width, height = height)
    draw(); dev.off()
  }
  render("topic_counts", draw_counts)
  embeddings <- py_to_r(model$.py$topic_embeddings_)
  index <- as.integer(valid) + 1L + as.integer(any(info$Topic == -1L))
  topic_vectors <- embeddings[index, , drop = FALSE]
  norms <- sqrt(rowSums(topic_vectors^2))
  similarity <- tcrossprod(topic_vectors / pmax(norms, .Machine$double.eps))
  write_example_csv(similarity, file.path(output, "topic_similarity.csv"), row.names = FALSE)
  render("topic_similarity", function() {
    par(mar = c(4, 4, 3, 1))
    n <- nrow(similarity)
    image(seq_len(n), seq_len(n), t(similarity[n:1L, , drop = FALSE]),
          col = hcl.colors(64L, "YlOrRd", rev = TRUE), zlim = c(0, 1),
          xlab = "Topic ID", ylab = "Topic ID", axes = FALSE,
          main = "Worked-example cosine topic similarity")
    at <- unique(round(seq(1, n, length.out = min(n, 15L))))
    axis(1, at, valid[at]); axis(2, at, rev(valid)[at], las = 1); box()
  }, width = 7, height = 6)
  bertopic_update_topics(model, docs)
  updated_info <- bertopic_topics(model)
  write_example_csv(updated_info, file.path(output, "updated_topic_info.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  updated_predictions <- predict(model, new_docs, type = "both")
  # Match the manuscript's lightweight save/load metadata check.
  lightweight_path <- file.path(output, "model-safetensors")
  bertopic_save(model, lightweight_path, serialization = "safetensors", overwrite = FALSE)
  lightweight <- bertopic_load(lightweight_path)
  metadata <- function(x) as.data.frame(bertopic_topics(x))[, c("Topic", "Count", "Name")]
  lightweight_metadata <- identical(metadata(model), metadata(lightweight))
  # Pickle preserves the full clusterer and embedding backend for an exact inference round trip.
  pickle_path <- file.path(output, "model.pkl")
  bertopic_save(model, pickle_path, serialization = "pickle", save_embedding_model = TRUE, overwrite = FALSE)
  restored <- bertopic_load(pickle_path)
  restored_predictions <- predict(restored, new_docs, type = "both")
  equal_numeric <- function(a, b) !is.null(a) && !is.null(b) &&
    isTRUE(all.equal(as.matrix(a), as.matrix(b), tolerance = 1e-12, check.attributes = FALSE))
  checks <- data.frame(
    check = c("safetensors_topic_metadata", "pickle_topic_metadata", "pickle_cached_topics",
              "pickle_cached_probabilities", "pickle_transform_topics", "pickle_transform_probabilities"),
    passed = c(lightweight_metadata, identical(metadata(model), metadata(restored)),
               identical(as.integer(model$topics), as.integer(restored$topics)),
               equal_numeric(model$probs, restored$probs),
               identical(as.integer(updated_predictions$topics), as.integer(restored_predictions$topics)),
               equal_numeric(updated_predictions$probs, restored_predictions$probs))
  )
  print(checks)
  write_example_csv(checks, file.path(output, "restoration_checks.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(output, "r_session_info.txt"), useBytes = TRUE)
  hashlib <- import("hashlib", convert = FALSE)
  pathlib <- import("pathlib", convert = FALSE)
  sha <- function(path) py_to_r(hashlib$sha256(pathlib$Path(path)$read_bytes())$hexdigest())
  files <- list.files(output, recursive = TRUE, full.names = TRUE)
  files <- files[basename(files) != "console.log"]
  record <- list(
    timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = as.character(packageVersion("BERTopic")),
    package_library = find.package("BERTopic"), package_archive = archive,
    package_archive_sha256 = sha(archive),
    script_sha256 = sha(normalizePath("benchmark/run_example.R", winslash = "/")),
    helper_sha256 = sha(normalizePath("benchmark/example_io.R", winslash = "/")),
    python = python, backend = backend, seed = seed, model_revision = revision,
    documents = length(docs), umap = list(n_neighbors = 15L, n_components = 5L, min_dist = 0, metric = "cosine", random_state = seed),
    artifact_sha256 = setNames(lapply(files, sha), substring(files, nchar(output) + 2L)),
    notes = c("The lightweight safetensors check covers topic metadata; it does not assert HDBSCAN inference equality.",
              "The pickle check preserves the full backend and compares cached fields and post-load inference.",
              "Model weights and the large generated pickle are reproduced by this script rather than committed to Git.")
  )
  writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             file.path(output, "manifest.json"), useBytes = TRUE)
  if (!all(checks$passed)) stop("Worked-example restoration check failed")
  cat("Worked example completed with every restoration check passing.\n")
}
main()
