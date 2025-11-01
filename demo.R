# demo-bertopic.R
suppressPackageStartupMessages({
  library(devtools)   # for load_all() during development
})
# Load the package from the project (skip if installed from source already)
if (basename(getwd()) != "BERTopic") {
  message("NOTE: current working directory is not the package root; load_all() may fail.")
}
try(load_all(), silent = TRUE)

suppressPackageStartupMessages({
  library(BERTopic)
  library(reticulate)
  library(tibble)
})

say <- function(...) cat(sprintf(paste0(..., "\n")))

say("\n[0] Environment check")
BERTopic::bertopic_available()
# 1) Data -----------------------------------------------------------------------
say("\n[1] Load packaged dataset")
data("sms_spam", package = "BERTopic")
docs <- sms_spam$text
N <- length(docs) 
docs <- docs[seq_len(N)]
say("  - Documents:", length(docs))

# 2) Fit ------------------------------------------------------------------------
say("\n[2] Fit BERTopic model")
set_bertopic_seed(42)
m <- bertopic_fit(
  text = docs,
  calculate_probabilities = TRUE
)
print(m)

# 3) Topic overview -------------------------------------------------------------
say("\n[3] Topic overview (head)")
topics_tbl <- bertopic_topics(m)
print(utils::head(topics_tbl, 10))

# 4) Top terms for a valid topic ------------------------------------------------
say("\n[4] Top terms for a valid topic")
valid_topics <- topics_tbl$Topic[topics_tbl$Topic != -1]
if (length(valid_topics) > 0) {
  tt <- bertopic_topic_terms(m, topic_id = valid_topics[1], top_n = 10)
  print(tt)
} else {
  say("  - All topics are -1 (noise) in this subset.")
}

# 5) Find topics for a query string --------------------------------------------
say("\n[5] find_topics() results for a short query")
ft <- bertopic_find_topics(m, query_text = "free subscription message", top_n = 3)
print(ft)


# 7) Representative documents (if supported) -----------------------------------
say("\n[7] Representative documents for a valid topic (if available)")
if (length(valid_topics) > 0 && reticulate::py_has_attr(m$.py, "get_representative_docs")) {
  rd <- try(bertopic_get_representative_docs(m, topic_id = valid_topics[1], top_n = 3), silent = TRUE)
  if (!inherits(rd, "try-error")) {
    print(rd)
  } else {
    say("  - get_representative_docs() not supported in this backend signature.")
  }
} else {
  say("  - No valid topics or backend lacks get_representative_docs().")
}

# 8) Transform / Predict on new texts ------------------------------------------
say("\n[8] Transform / Predict on new texts")
new_docs <- c(
  "Love you so much, see you tonight.",
  "Free subscription! Reply STOP to unsubscribe."
)
pred_both <- predict(m, new_docs, type = "both")
say("  - Predicted topics:"); print(pred_both$topics)
if (!is.null(pred_both$probs)) {
  say("  - Probability rows x cols:", paste(dim(as.matrix(pred_both$probs)), collapse = " x "))
}

# 9) Export doc-topic matrix (if available) -------------------------------------
say("\n[9] Export document-topic probability matrix")
mat <- bertopic_as_document_topic_matrix(m, sparse = FALSE, prefix = TRUE)
if (is.null(mat)) {
  say("  - Model has no probabilities. Refit with calculate_probabilities = TRUE to enable.")
} else {
  say("  - Matrix shape:", paste(dim(mat), collapse = " x "))
  print(utils::head(mat[, seq_len(min(5, ncol(mat)))], 5))
}


# 11) Topics over time + visualization (HTML, robust)
say("\n[11] Topics over time + visualization (HTML, robust)")
viz_ok <- reticulate::py_module_available("plotly")


ti_now <- bertopic_topics(m)
n_valid <- sum(ti_now$Topic != -1, na.rm = TRUE)
say("  - Valid (non -1) topics:", n_valid)

if (!viz_ok) {
  say("  - Python 'plotly' not available; skipping HTML visuals.")
} else if (n_valid < 3) {
  say("  - Fewer than 3 valid topics; skipping visualize_topics() to avoid backend failure.")
} else {

  ts <- seq(from = Sys.Date() - length(docs), by = "1 day", length.out = length(docs))
  tot <- try(bertopic_topics_over_time(m, docs, ts, nr_bins = 6), silent = TRUE)
  if (!inherits(tot, "try-error")) {
    out_tot <- file.path(tempdir(), "viz_topics_over_time.html")
    try({
      invisible(bertopic_visualize_topics_over_time(m, tot, top_n = min(8L, max(3L, n_valid)), file = out_tot))
      say("  - Wrote:", out_tot)
    }, silent = TRUE)
  } else {
    say("  - topics_over_time failed; skipping its visualization.")
  }


  f_topics <- file.path(tempdir(), "viz_topics.html")
  vt_try <- try({

    py_fig <- m$.py$visualize_topics(top_n_topics = as.integer(min(15L, max(3L, n_valid))))

    BERTopic:::.bertopic_fig_to_html(py_fig, file = f_topics)
  }, silent = TRUE)

  if (inherits(vt_try, "try-error")) {
    say("  - visualize_topics() failed; falling back to hierarchy/barchart.")

    f_hier <- file.path(tempdir(), "viz_hierarchy.html")
    f_bar  <- file.path(tempdir(), "viz_barchart.html")
    try(invisible(bertopic_visualize_hierarchy(m, file = f_hier)), silent = TRUE)
    try({
      vt <- ti_now$Topic[ti_now$Topic != -1]
      if (length(vt) > 0) {
        invisible(bertopic_visualize_barchart(m, topic_id = vt[1], file = f_bar))
      } else {
        invisible(bertopic_visualize_barchart(m, file = f_bar))
      }
    }, silent = TRUE)
    if (file.exists(f_hier)) say("  - Wrote:", f_hier)
    if (file.exists(f_bar))  say("  - Wrote:", f_bar)
  } else {
    say("  - Wrote:", f_topics)
  }


  f_heatmap <- file.path(tempdir(), "viz_heatmap.html")
  f_trank   <- file.path(tempdir(), "viz_term_rank.html")
  f_docs    <- file.path(tempdir(), "viz_documents.html")
  try(invisible(bertopic_visualize_heatmap(m, file = f_heatmap)), silent = TRUE)
  try(invisible(bertopic_visualize_term_rank(m, file = f_trank)), silent = TRUE)
  try(invisible(bertopic_visualize_documents(m, docs = docs[1:min(40, length(docs))], file = f_docs)), silent = TRUE)
  if (file.exists(f_heatmap)) say("  - Wrote:", f_heatmap)
  if (file.exists(f_trank))   say("  - Wrote:", f_trank)
  if (file.exists(f_docs))    say("  - Wrote:", f_docs)
}



# 10) Topic operations: update, reduce, relabel ---------------------------------
say("\n[10] Topic operations: update_topics(), reduce_topics(), set_topic_labels()")
invisible(bertopic_update_topics(m, docs))
# try to reduce; backend-dependent (docs may be required). We pass docs to be safe.
invisible(bertopic_reduce_topics(m, nr_topics = "auto", docs = docs))

topics_after <- bertopic_topics(m)
say("  - Topics (rows) before vs after reduce:", nrow(topics_tbl), "->", nrow(topics_after))

valid_after <- topics_after$Topic[topics_after$Topic != -1]
if (length(valid_after) > 0) {
  take <- head(valid_after, min(3L, length(valid_after)))
  new_labels <- setNames(paste0("CustomLabel_", seq_along(take)), take)
  invisible(bertopic_set_topic_labels(m, new_labels))
  topics_labeled <- bertopic_topics(m)
  if ("Name" %in% names(topics_labeled)) {
    say("  - Any custom labels applied?:", any(grepl("^CustomLabel_", topics_labeled$Name)))
  }
}
# 12) Persistence: save/load to file and directory ------------------------------
say("\n[12] Persistence: save/load (file and directory)")
tmpdir <- tempdir()
file_path <- file.path(tmpdir, "bertopic_model.pkl")
dir_path  <- file.path(tmpdir, "bertopic_model_dir")

# single-file
invisible(bertopic_save(m, file_path, serialization = "pickle", save_embedding_model = TRUE, overwrite = TRUE))
say("  - Saved file:", file_path, "exists?", file.exists(file_path))
m_file <- bertopic_load(file_path)
say("  - Loaded from file. Can transform?")
tf1 <- try(bertopic_transform(m_file, c("new unseen text")), silent = TRUE)
if (inherits(tf1, "try-error")) {
  say("    -> transform() failed after load (backend quirk). Consider bertopic_set_embedding_model().")
} else {
  say("    -> transform() OK.")
}

# directory bundle
invisible(bertopic_save(m, dir_path, serialization = "pickle", save_embedding_model = TRUE, overwrite = TRUE))
say("  - Saved directory:", dir_path, "exists?", dir.exists(dir_path) || file.exists(dir_path))
m_dir <- bertopic_load(dir_path)
say("  - Loaded from dir. Can transform?")
tf2 <- try(bertopic_transform(m_dir, c("another text")), silent = TRUE)
if (inherits(tf2, "try-error")) {
  say("    -> transform() failed after load (backend quirk). Consider bertopic_set_embedding_model().")
} else {
  say("    -> transform() OK.")
}

# 13) Embedding model presence / reset (optional) --------------------------------
say("\n[13] Embedding model presence / reset (optional)")
has_em <- bertopic_has_embedding_model(m)
say("  - Has embedding model?:", has_em)
if (has_em) {
  em_obj <- try(reticulate::py_get_attr(m$.py, "embedding_model"), silent = TRUE)
  if (!inherits(em_obj, "try-error") && !reticulate::py_is_null_xptr(em_obj)) {
    reset_try <- try(bertopic_set_embedding_model(m, em_obj), silent = TRUE)
    if (!inherits(reset_try, "try-error")) {
      say("  - Reset embedding model on current model: OK")
    } else {
      say("  - set_embedding_model() not supported by backend/signature.")
    }
  } else {
    say("  - Embedding model object not accessible.")
  }
}

# 14) S3 helpers ----------------------------------------------------------------
say("\n[14] S3 helpers: summary / coef / as.data.frame / fortify")
s <- summary(m)
say("  - summary(): topics rows =", s$n_topics, " noise =", s$n_noise)

cf <- coef(m, top_n = 5)
say("  - coef() head:")
print(utils::head(cf, 10))

adf <- as.data.frame(m)
say("  - as.data.frame(): ", nrow(adf), " rows")
print(utils::head(adf, 6))

ff <- tryCatch({
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::fortify(m)
  } else {
    BERTopic:::fortify.bertopic_r(m)
  }
}, error = function(e) NULL)
if (!is.null(ff)) {
  say("  - fortify(): ", nrow(ff), " rows (doc_id/topic)")
  print(utils::head(ff, 6))
} else {
  say("  - fortify() unavailable.")
}

say("\n[Done] Demo completed.")
