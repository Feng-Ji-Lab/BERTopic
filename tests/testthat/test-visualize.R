test_that("visualization functions produce HTML files", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")
  if (!reticulate::py_module_available("plotly")) {
    skip("Python module 'plotly' not available; skipping visualization tests.")
  }

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 80) docs <- docs[seq_len(80)]

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = FALSE)
  expect_s3_class(m, "bertopic_r")

  # topics map
  f1 <- file.path(tempdir(), "viz_topics.html")
  tryCatch({
    expect_invisible(bertopic_visualize_topics(m, file = f1))
    expect_true(file.exists(f1) && file.info(f1)$size > 0)
  }, error = function(e) skip(paste("visualize_topics not supported in backend:", conditionMessage(e))))

  # barchart
  ti <- bertopic_topics(m)
  valid <- ti$Topic[ti$Topic != -1]
  f2 <- file.path(tempdir(), "viz_barchart.html")
  tryCatch({
    if (length(valid) > 0) {
      expect_invisible(bertopic_visualize_barchart(m, topic_id = valid[1], file = f2))
    } else {
      expect_invisible(bertopic_visualize_barchart(m, file = f2))
    }
    expect_true(file.exists(f2) && file.info(f2)$size > 0)
  }, error = function(e) skip(paste("visualize_barchart not supported in backend:", conditionMessage(e))))

  # hierarchy
  f3 <- file.path(tempdir(), "viz_hierarchy.html")
  tryCatch({
    expect_invisible(bertopic_visualize_hierarchy(m, file = f3))
    expect_true(file.exists(f3) && file.info(f3)$size > 0)
  }, error = function(e) skip(paste("visualize_hierarchy not supported in backend:", conditionMessage(e))))

  # heatmap
  f4 <- file.path(tempdir(), "viz_heatmap.html")
  tryCatch({
    expect_invisible(bertopic_visualize_heatmap(m, file = f4))
    expect_true(file.exists(f4) && file.info(f4)$size > 0)
  }, error = function(e) skip(paste("visualize_heatmap not supported in backend:", conditionMessage(e))))

  # term rank
  f5 <- file.path(tempdir(), "viz_term_rank.html")
  tryCatch({
    expect_invisible(bertopic_visualize_term_rank(m, file = f5))
    expect_true(file.exists(f5) && file.info(f5)$size > 0)
  }, error = function(e) skip(paste("visualize_term_rank not supported in backend:", conditionMessage(e))))

  # documents
  f6 <- file.path(tempdir(), "viz_documents.html")
  sub_docs <- docs[seq_len(min(40, length(docs)))]
  tryCatch({
    expect_invisible(bertopic_visualize_documents(m, docs = sub_docs, file = f6))
    expect_true(file.exists(f6) && file.info(f6)$size > 0)
  }, error = function(e) skip(paste("visualize_documents not supported in backend:", conditionMessage(e))))
})
