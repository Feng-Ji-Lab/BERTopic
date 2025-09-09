# tests/testthat/test-roundtrip.R
test_that("BERTopic end-to-end round trip works (using sms_spam data)", {
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  # Use packaged dataset
  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  expect_true(is.character(docs))
  # keep runtime modest but >30 to avoid all -1 assignments
  if (length(docs) > 120) docs <- docs[seq_len(120)]

  # Fit
  set_bertopic_seed(123)
  m <- bertopic_fit(docs)
  expect_s3_class(m, "bertopic_r")

  # Topics overview
  topics_tbl <- bertopic_topics(m)
  expect_s3_class(topics_tbl, "tbl_df")
  expect_true(nrow(topics_tbl) >= 1)

  # Topic terms for a valid (non -1) topic
  valid_topics <- topics_tbl$Topic[topics_tbl$Topic != -1]
  if (length(valid_topics) > 0) {
    tt <- bertopic_topic_terms(m, valid_topics[1], top_n = 5)
    expect_s3_class(tt, "tbl_df")
    expect_true(nrow(tt) > 0)
  }

  # Transform new documents
  pred <- bertopic_transform(m, c("new unseen sentence", "embedding based topic model"))
  expect_length(pred$topics, 2)

  # Probabilities can be matrix/data.frame or NULL depending on config
  if (!is.null(pred$probs)) {
    if (is.matrix(pred$probs) || is.data.frame(pred$probs)) {
      expect_equal(nrow(pred$probs), 2)
    } else {
      expect_equal(length(pred$probs), 2)
    }
  }
})
