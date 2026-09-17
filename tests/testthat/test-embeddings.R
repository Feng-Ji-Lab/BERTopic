test_that("embedding model presence and re-setting works", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 80) docs <- docs[seq_len(80)]

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = FALSE)
  expect_s3_class(m, "bertopic_r")

  expect_true(bertopic_has_embedding_model(m))

  # Reset the existing backend object without downloading another model.
  em_obj <- reticulate::py_get_attr(m$.py, "embedding_model")
  expect_false(reticulate::py_is_null_xptr(em_obj))
  expect_invisible(bertopic_set_embedding_model(m, em_obj))
  expect_true(bertopic_has_embedding_model(m))
})
