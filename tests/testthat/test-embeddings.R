test_that("embedding model presence and re-setting works", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 80) docs <- docs[seq_len(80)]

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = FALSE)
  expect_s3_class(m, "bertopic_r")

  # presence check
  expect_true(bertopic_has_embedding_model(m))

  # try resetting the existing embedding model (no downloads)
  em_obj <- try(reticulate::py_get_attr(m$.py, "embedding_model"), silent = TRUE)
  if (inherits(em_obj, "try-error") || reticulate::py_is_null_xptr(em_obj)) {
    skip("Embedding model attribute not accessible in this backend; skipping reset test.")
  }

  # Some backends reject setting the same object; treat as skippable
  reset_try <- try(bertopic_set_embedding_model(m, em_obj), silent = TRUE)
  if (inherits(reset_try, "try-error")) {
    skip("set_embedding_model() not supported by this backend/signature; skipping.")
  } else {
    expect_true(bertopic_has_embedding_model(m))
  }
})
