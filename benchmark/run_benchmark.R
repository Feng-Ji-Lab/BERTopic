# Reproducible R-vs-Python BERTopic benchmark for release 0.1.1.
print(rows)
saveRDS(list(r_topics = r_topics, py_topics = py_topics, r_info = r_info, py_info = py_info, r_probs = r_probs, py_probs = py_probs), file.path(output_dir, "raw.rds"))
.need_py()
bt <- import("bertopic")
reset_python_seed <- function() {
  py_run_string(sprintf("import os, random; os.environ[\\"PYTHONHASHSEED\\"]=\\"%d\\"; random.seed(%d); import numpy as np; np.random.seed(%d)", seed, seed, seed))
}
reset_python_seed()
r_run <- measure(bertopic_fit(docs, embeddings = embeddings, calculate_probabilities = TRUE))
r_model <- r_run$value
reset_python_seed()
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
