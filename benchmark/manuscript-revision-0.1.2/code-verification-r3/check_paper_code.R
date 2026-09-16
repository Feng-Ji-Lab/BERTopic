.libPaths(c("C:/Users/zby15/Desktop/ss/.bertopic-release-lib-0.1.2", .libPaths()))
Sys.setenv(RETICULATE_CONDA = "C:/Users/zby15/miniconda/Scripts/conda.exe")
library(BERTopic)
library(reticulate)

stopifnot(packageVersion("BERTopic") == "0.1.2")
use_bertopic_condaenv("r-bertopic-016")
stopifnot(bertopic_available())
backend <- bertopic_session_info()

stopifnot(backend$bertopic_version == "0.16.0")
model <- bertopic_load("C:/Users/zby15/Desktop/ss/BERTopic/benchmark/example-results/model.pkl")
updated_topic_info <- bertopic_topics(model)
new_docs <- c("Love you so much, see you tonight.", "Free subscription! Reply STOP to unsubscribe.")
setwd("C:/Users/zby15/Desktop/ss/.bertopic-paper-code-r3")
# Use a new destination; overwrite is disabled by default.
bertopic_save(
  model,
  path = "sms_bertopic_model",
  serialization = "safetensors",
  overwrite = FALSE
)
restored_model <- bertopic_load("sms_bertopic_model")
restored_topic_info <- bertopic_topics(restored_model)
stopifnot(identical(
  as.data.frame(restored_topic_info)[c("Topic", "Count", "Name")],
  as.data.frame(updated_topic_info)[c("Topic", "Count", "Name")]
))

before_save <- predict(model, new_docs, type = "both")
bertopic_save(
  model,
  path = "sms_bertopic_model.pkl",
  serialization = "pickle",
  save_embedding_model = TRUE,
  overwrite = FALSE
)
full_model <- bertopic_load("sms_bertopic_model.pkl")
after_load <- predict(full_model, new_docs, type = "both")
stopifnot(identical(
  as.integer(before_save$topics),
  as.integer(after_load$topics)
))
stopifnot(isTRUE(all.equal(
  as.matrix(before_save$probs),
  as.matrix(after_load$probs),
  tolerance = 1e-12,
  check.attributes = FALSE
)))

cat("PASS: binding, lightweight metadata, full-pickle transformed topics and strengths.\n")
