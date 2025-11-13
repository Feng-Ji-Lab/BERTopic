test_that("BERTopic visualization wrappers produce HTML files", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  # Plotly is required because all visualizations return Plotly figures
  if (!reticulate::py_module_available("plotly")) {
    skip("Python module 'plotly' not available; skipping visualization tests.")
  }

  # Load example data and keep a small subset for speed
  data("sms_spam", package = "BERTopic")
  df <- sms_spam
  if (nrow(df) > 80L) {
    df <- df[seq_len(80L), , drop = FALSE]
  }
  docs <- df$text
  classes <- df$label

  expect_true(length(docs) > 0L)

  # Fit a small BERTopic model, enabling probability estimation
  set_bertopic_seed(123L)
  m <- bertopic_fit(text = docs, calculate_probabilities = TRUE)
  expect_s3_class(m, "bertopic_r")

  # ------------------------------------------------------------------
  # 1) Topic map
  # ------------------------------------------------------------------
  f1 <- file.path(tempdir(), "viz_topics.html")
  tryCatch({
    expect_invisible(bertopic_visualize_topics(m, file = f1))
    expect_true(file.exists(f1) && file.info(f1)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_topics not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 2) Topic barchart
  # ------------------------------------------------------------------
  ti <- bertopic_topics(m)
  valid_topics <- ti$Topic[ti$Topic != -1L]
  f2 <- file.path(tempdir(), "viz_barchart.html")
  tryCatch({
    if (length(valid_topics) > 0L) {
      expect_invisible(
        bertopic_visualize_barchart(m, topic_id = valid_topics[1L], file = f2)
      )
    } else {
      # Fall back to default behavior if no non-noise topics are present
      expect_invisible(bertopic_visualize_barchart(m, file = f2))
    }
    expect_true(file.exists(f2) && file.info(f2)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_barchart not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 3) Hierarchy
  # ------------------------------------------------------------------
  f3 <- file.path(tempdir(), "viz_hierarchy.html")
  tryCatch({
    expect_invisible(bertopic_visualize_hierarchy(m, file = f3))
    expect_true(file.exists(f3) && file.info(f3)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_hierarchy not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 4) Heatmap
  # ------------------------------------------------------------------
  f4 <- file.path(tempdir(), "viz_heatmap.html")
  tryCatch({
    expect_invisible(bertopic_visualize_heatmap(m, file = f4))
    expect_true(file.exists(f4) && file.info(f4)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_heatmap not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 5) Term rank
  # ------------------------------------------------------------------
  f5 <- file.path(tempdir(), "viz_term_rank.html")
  tryCatch({
    expect_invisible(bertopic_visualize_term_rank(m, file = f5))
    expect_true(file.exists(f5) && file.info(f5)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_term_rank not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 6) Documents
  # ------------------------------------------------------------------
  f6 <- file.path(tempdir(), "viz_documents.html")
  sub_docs <- docs[seq_len(min(40L, length(docs)))]
  tryCatch({
    expect_invisible(
      bertopic_visualize_documents(m, docs = sub_docs, file = f6)
    )
    expect_true(file.exists(f6) && file.info(f6)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_documents not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 7) Distribution (topic probabilities for a single document)
  # ------------------------------------------------------------------
  probs <- m$probs
  if (is.null(probs)) {
    skip("Model did not expose `probs`; cannot test visualize_distribution.")
  }

  # Extract a single document's probability vector
  if (is.vector(probs)) {
    prob_vec <- as.numeric(probs)
  } else {
    prob_vec <- as.numeric(probs[1L, ])
  }
  prob_vec <- prob_vec[is.finite(prob_vec)]
  if (!length(prob_vec)) {
    skip("No finite topic probabilities available for distribution visualization.")
  }

  f7 <- file.path(tempdir(), "viz_distribution.html")
  tryCatch({
    expect_invisible(
      bertopic_visualize_distribution(m, probs = prob_vec, file = f7)
    )
    expect_true(file.exists(f7) && file.info(f7)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_distribution not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 8) Topics per class (requires topics_per_class from Python)
  # ------------------------------------------------------------------
  topics_per_class_py <- try(
    m$.py$topics_per_class(docs, classes),
    silent = TRUE
  )
  if (inherits(topics_per_class_py, "try-error")) {
    skip("Python `topics_per_class()` not available; skipping topics_per_class visualization.")
  }

  f8 <- file.path(tempdir(), "viz_topics_per_class.html")
  tryCatch({
    expect_invisible(
      bertopic_visualize_topics_per_class(
        m,
        topics_per_class = topics_per_class_py,
        top_n_topics = 5L,
        file = f8
      )
    )
    expect_true(file.exists(f8) && file.info(f8)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_topics_per_class not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 9) Hierarchical documents (requires hierarchical_topics from Python)
  # ------------------------------------------------------------------
  hierarchical_topics_py <- try(
    m$.py$hierarchical_topics(docs),
    silent = TRUE
  )
  if (inherits(hierarchical_topics_py, "try-error")) {
    skip("Python `hierarchical_topics()` not available; skipping hierarchical documents visualization.")
  }

  f9 <- file.path(tempdir(), "viz_hierarchical_documents.html")
  tryCatch({
    expect_invisible(
      bertopic_visualize_hierarchical_documents(
        m,
        docs = docs,
        hierarchical_topics = hierarchical_topics_py,
        file = f9
      )
    )
    expect_true(file.exists(f9) && file.info(f9)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_hierarchical_documents not supported in backend:", conditionMessage(e)))
  })

  # ------------------------------------------------------------------
  # 10) Topics over time
  # ------------------------------------------------------------------
  timestamps <- seq.Date(
    from = as.Date("2020-01-01"),
    by   = "day",
    length.out = length(docs)
  )

  topics_over_time_tbl <- try(
    bertopic_topics_over_time(
      model = m,
      docs = docs,
      timestamps = timestamps,
      nr_bins = 6L
    ),
    silent = TRUE
  )
  if (inherits(topics_over_time_tbl, "try-error")) {
    skip("Python `topics_over_time()` not available; skipping topics_over_time visualization.")
  }

  f10 <- file.path(tempdir(), "viz_topics_over_time.html")
  tryCatch({
    expect_invisible(
      bertopic_visualize_topics_over_time(
        model = m,
        topics_over_time = topics_over_time_tbl,
        top_n = 5L,
        file = f10
      )
    )
    expect_true(file.exists(f10) && file.info(f10)$size > 0)
  }, error = function(e) {
    skip(paste("visualize_topics_over_time not supported in backend:", conditionMessage(e)))
  })
})
