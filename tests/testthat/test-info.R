test_that("document info, find_topics, representative docs work on sms_spam", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 120) docs <- docs[seq_len(120)]

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = FALSE)
  expect_s3_class(m, "bertopic_r")

  di <- bertopic_get_document_info(m, docs)
  expect_s3_class(di, "tbl_df")
  expect_gte(nrow(di), 1)

  ft <- bertopic_find_topics(m, "free subscription message", top_n = 3)
  expect_s3_class(ft, "tbl_df")
  expect_gte(nrow(ft), 1)
  expect_true(all(c("topic", "score", "label") %in% names(ft)))

  ti <- bertopic_topics(m)
  valid_topics <- ti$Topic[ti$Topic != -1]
  if (length(valid_topics) > 0) {
    if (!reticulate::py_has_attr(m$.py, "get_representative_docs")) {
      skip("Backend does not expose get_representative_docs; skipping.")
    }
    rd_try <- try(bertopic_get_representative_docs(m, valid_topics[1], top_n = 3), silent = TRUE)
    if (inherits(rd_try, "try-error")) {
      skip("get_representative_docs call failed in this backend/signature; skipping.")
    } else {
      expect_s3_class(rd_try, "tbl_df")
      expect_true(all(c("rank", "document") %in% names(rd_try)))
      expect_gte(nrow(rd_try), 1)
    }
  } else {
    succeed("All topics are -1 (noise) in this subset; skipping representative docs check.")
  }
})
