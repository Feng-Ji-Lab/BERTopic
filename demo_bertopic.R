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
})

# --- 1) Python environment binding (CRITICAL when starting a fresh R session) --
# You can customize the env name by setting Sys.setenv(BERTopic_CONDA_ENV = "your-env")
env_name <- Sys.getenv("BERTopic_CONDA_ENV", unset = "r-bertopic")

# Ensure the env exists; if not, stop with a clear message
avail_envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
if (!env_name %in% avail_envs) {
  stop(sprintf(
    "Conda env '%s' not found.\n- Available: %s\n- Create it first or set Sys.setenv(BERTopic_CONDA_ENV='...')",
    env_name, paste(avail_envs, collapse = ", ")
  ))
}

# Bind the exact Python interpreter from that env BEFORE any Python initialization
py_exec <- reticulate::conda_python(env_name)
Sys.setenv(RETICULATE_PYTHON = py_exec)

cat("\n[Env] Binding Python to conda env:", env_name, "\n")
print(reticulate::py_config())

# Sanity check: can we import required modules?
needed <- c("bertopic", "sentence_transformers", "torch", "umap", "hdbscan")
missing <- needed[!vapply(needed, reticulate::py_module_available, logical(1))]
if (length(missing)) {
  stop(sprintf(
    "Missing Python modules in env '%s': %s\nInstall them in this env and rerun.",
    env_name, paste(missing, collapse = ", ")
  ))
}

# Package-level availability check
if (!BERTopic::bertopic_available()) {
  stop("BERTopic Python backend not available via reticulate; please verify environment binding.")
}
cat("\n[Demo] Using packaged dataset `sms_spam`...\n")
data(sms_spam)
cat("Rows:", nrow(sms_spam), "  Spam ratio:",
    round(mean(sms_spam$label == "spam"), 3), "\n")


docs <- sms_spam$text

set_bertopic_seed(42)
cat("[Fit] Training BERTopic on", length(docs), "SMS messages (subset)...\n")
m <- bertopic_fit(
  sms_spam$text,
  calculate_probabilities = TRUE,     
  embedding_model = "all-MiniLM-L6-v2"
)


cat("\n[Topics] Overview (head):\n")
print(utils::head(bertopic_topics(m), 10))


tp <- bertopic_topics(m)$Topic
valid <- tp[tp != -1]
if (length(valid) > 0) {
  cat("\n[Topic Terms] Top 5 terms for topic", valid[1], ":\n")
  print(bertopic_topic_terms(m, valid[1], top_n = 5))
} else {
  cat("\n[Topic Terms] All assigned to -1 (noise) for this tiny subset.\n")
}

cat("\n[Transform] Predicting two new messages...\n")

new_docs <- c(
  "Love you so much, you are my life, I miss you every day.",
  "Free chat service! Reply STOP to unsubscribe from daily text updates."
)

pred <- bertopic_transform(m, new_docs)
probs <- pred$probs
stopifnot(is.matrix(probs) || is.data.frame(probs))
probs <- as.matrix(probs)

topics_info <- bertopic_topics(m)

soft_predict <- function(model, texts, topics_info, top_n = 1L) {
  out <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    res  <- model$.py$find_topics(texts[[i]], as.integer(top_n))
    rres <- reticulate::py_to_r(res)          # list(topics, scores)
    t_id <- as.integer(rres[[1]])
    scr  <- as.numeric(rres[[2]])
    lab  <- topics_info$Name[match(t_id, topics_info$Topic)]
    out[[i]] <- data.frame(
      topic = t_id,
      label = ifelse(is.na(lab), "N/A", lab),
      score = round(scr, 3),
      stringsAsFactors = FALSE
    )
  }
  out
}

soft_res <- soft_predict(m, new_docs, topics_info, top_n = 3L)

cat("\n[Prediction Results]\n")
for (i in seq_along(new_docs)) {
  cat("Message:", new_docs[i], "\n")
  cat("Soft top topics:\n")
  print(soft_res[[i]])
  cat("\n")
}