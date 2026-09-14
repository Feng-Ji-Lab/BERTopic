test_that("BERTopic visualization wrappers produce HTML files", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")
  skip_if_not(
    reticulate::py_module_available("plotly"),
    "Python module plotly not available"
  )

  data("sms_spam", package = "BERTopic")
  df <- sms_spam
  if (nrow(df) > 80L) {
    df <- df[seq_len(80L), , drop = FALSE]
  }
  docs <- df$text
  classes <- df$label

  expect_true(length(docs) > 0L)

  set_bertopic_seed(123L)
  m <- bertopic_fit(text = docs, calculate_probabilities = TRUE)
  expect_s3_class(m, "bertopic_r")

  f1 <- file.path(tempdir(), "viz_topics.html")
  expect_invisible(bertopic_visualize_topics(m, file = f1))
  expect_true(file.exists(f1) && file.info(f1)$size > 0)

  ti <- bertopic_topics(m)
  valid_topics <- ti$Topic[ti$Topic != -1L]
  f2 <- file.path(tempdir(), "viz_barchart.html")
  if (length(valid_topics) > 0L) {
    expect_invisible(
      bertopic_visualize_barchart(m, topic_id = valid_topics[1L], file = f2)
    )
  } else {
    expect_invisible(bertopic_visualize_barchart(m, file = f2))
  }
  expect_true(file.exists(f2) && file.info(f2)$size > 0)

  f3 <- file.path(tempdir(), "viz_hierarchy.html")
  expect_invisible(bertopic_visualize_hierarchy(m, file = f3))
  expect_true(file.exists(f3) && file.info(f3)$size > 0)

  f4 <- file.path(tempdir(), "viz_heatmap.html")
  expect_invisible(bertopic_visualize_heatmap(m, file = f4))
  expect_true(file.exists(f4) && file.info(f4)$size > 0)

  f5 <- file.path(tempdir(), "viz_term_rank.html")
  expect_invisible(bertopic_visualize_term_rank(m, file = f5))
  expect_true(file.exists(f5) && file.info(f5)$size > 0)

  f6 <- file.path(tempdir(), "viz_documents.html")
  expect_invisible(bertopic_visualize_documents(m, docs = docs, file = f6))
  expect_true(file.exists(f6) && file.info(f6)$size > 0)

  probs <- m$probs
  expect_false(is.null(probs))
  if (is.vector(probs)) {
    prob_vec <- as.numeric(probs)
  } else {
    prob_vec <- as.numeric(probs[1L, ])
  }
  prob_vec <- prob_vec[is.finite(prob_vec)]
  expect_gt(length(prob_vec), 0L)
  f7 <- file.path(tempdir(), "viz_distribution.html")
  expect_invisible(
    bertopic_visualize_distribution(m, probs = prob_vec, file = f7)
  )
  expect_true(file.exists(f7) && file.info(f7)$size > 0)

  topics_per_class_py <- m$.py$topics_per_class(docs, classes)
  f8 <- file.path(tempdir(), "viz_topics_per_class.html")
  expect_invisible(
    bertopic_visualize_topics_per_class(
      m,
      topics_per_class = topics_per_class_py,
      top_n_topics = 5L,
      file = f8
    )
  )
  expect_true(file.exists(f8) && file.info(f8)$size > 0)

  hierarchical_topics_py <- m$.py$hierarchical_topics(docs)
  f9 <- file.path(tempdir(), "viz_hierarchical_documents.html")
  expect_invisible(
    bertopic_visualize_hierarchical_documents(
      m,
      docs = docs,
      hierarchical_topics = hierarchical_topics_py,
      file = f9
    )
  )
  expect_true(file.exists(f9) && file.info(f9)$size > 0)

  timestamps <- seq.Date(
    from = as.Date("2020-01-01"),
    by = "day",
    length.out = length(docs)
  )
  topics_over_time_tbl <- bertopic_topics_over_time(
    model = m,
    docs = docs,
    timestamps = timestamps,
    nr_bins = 6L
  )
  f10 <- file.path(tempdir(), "viz_topics_over_time.html")
  expect_invisible(
    bertopic_visualize_topics_over_time(
      model = m,
      topics_over_time = topics_over_time_tbl,
      top_n = 5L,
      file = f10
    )
  )
  expect_true(file.exists(f10) && file.info(f10)$size > 0)
})
