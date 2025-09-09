test_that("S3 methods (summary, coef, as.data.frame, fortify) behave as expected", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 100) docs <- docs[seq_len(100)]

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = TRUE)
  expect_s3_class(m, "bertopic_r")

  s <- summary(m)
  expect_type(s, "list")
  expect_true(all(c("n_topics", "n_noise") %in% names(s)))

  cf <- coef(m, top_n = 5)
  expect_s3_class(cf, "data.frame")
  expect_true(all(c("topic", "term", "weight") %in% names(cf)) || nrow(cf) == 0)

  adf <- as.data.frame(m)
  expect_s3_class(adf, "tbl_df")
  expect_gte(nrow(adf), 1)

  ff <- if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::fortify(m)
  } else {
    BERTopic:::fortify.bertopic_r(m)
  }
  expect_s3_class(ff, "data.frame")
  expect_true(all(c("doc_id", "topic") %in% names(ff)))
})
