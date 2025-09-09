test_that("topics over time computes and visualizes", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")
  if (!reticulate::py_module_available("plotly")) {
    skip("Python module 'plotly' not available; skipping visualization test.")
  }

  data(sms_spam, package = "BERTopic")
  docs <- sms_spam$text
  if (length(docs) > 90) docs <- docs[seq_len(90)]

  start_date <- Sys.Date() - length(docs)
  ts <- seq(from = start_date, by = "1 day", length.out = length(docs))

  set_bertopic_seed(123)
  m <- bertopic_fit(docs, calculate_probabilities = FALSE)
  expect_s3_class(m, "bertopic_r")

  tot <- bertopic_topics_over_time(m, docs, ts, nr_bins = 6)
  expect_s3_class(tot, "tbl_df")
  expect_true(nrow(tot) >= 1)
  expect_true(!is.null(attr(tot, "_py")))

  f <- file.path(tempdir(), "tot.html")
  expect_invisible(bertopic_visualize_topics_over_time(m, tot, top_n = 5, file = f))
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)
})
