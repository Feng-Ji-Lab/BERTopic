test_that("exporters and predict work on fitted model", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 120) docs <- docs[seq_len(120)]

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = TRUE)
  expect_s3_class(m, "bertopic_r")

  mat <- bertopic_as_document_topic_matrix(m, sparse = FALSE, prefix = TRUE)
  if (is.null(mat)) {
    succeed("Model did not contain probabilities; skipping matrix assertions.")
  } else {
    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), length(docs))
    expect_true(ncol(mat) >= 1)
    expect_true(all(grepl("^topic_", colnames(mat))))
  }

  new_docs <- c(
    "Love you so much, see you tonight.",
    "Free subscription! Reply STOP to unsubscribe."
  )
  pr_both <- predict(m, new_docs, type = "both")
  expect_true(is.list(pr_both) && length(pr_both) >= 2)
  expect_length(pr_both$topics, length(new_docs))

  # class may be integer vector OR list; normalize to integer vector
  pr_cls <- predict(m, new_docs, type = "class")
  cls_vec <- as.integer(unlist(pr_cls, use.names = FALSE))
  expect_length(cls_vec, length(new_docs))

  pr_prob <- predict(m, new_docs, type = "prob")
  if (is.matrix(pr_prob) || is.data.frame(pr_prob)) {
    expect_equal(nrow(pr_prob), length(new_docs))
  } else {
    expect_length(pr_prob, length(new_docs))
  }
})
