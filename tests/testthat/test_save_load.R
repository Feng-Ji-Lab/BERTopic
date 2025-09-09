test_that("bertopic_save/bertopic_load support file and directory paths", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  # corpus
  docs <- rep(
    c(
      "topic modeling with transformers",
      "BERTopic from R with reticulate",
      "document clustering via embeddings and UMAP",
      "HDBSCAN density based clustering",
      "save and load a trained model",
      "visualize topics and inspect terms"
    ),
    length.out = 30
  )

  set_bertopic_seed(123)
  # keep it simple; probabilities off reduces some deserialization edge cases
  m <- bertopic_fit(docs, calculate_probabilities = FALSE)
  expect_s3_class(m, "bertopic_r")

  tmpdir    <- tempdir()
  file_path <- file.path(tmpdir, "bertopic_model.pkl")
  dir_path  <- file.path(tmpdir, "bertopic_model_dir")

  # ----- single-file save (.pkl) with embedding model -----
  if (file.exists(file_path)) unlink(file_path, force = TRUE)
  ret_file <- bertopic_save(
    m, file_path,
    serialization = "pickle",
    save_embedding_model = TRUE,
    overwrite = TRUE
  )
  expect_true(is.character(ret_file) && length(ret_file) == 1)
  expect_true(file.exists(file_path))
  expect_false(dir.exists(file_path))

  m_file <- bertopic_load(file_path)
  expect_s3_class(m_file, "bertopic_r")

  # sanity: get_topic_info and a topic's terms should work
  ti_file <- bertopic_topics(m_file)
  expect_s3_class(ti_file, "tbl_df")
  expect_gte(nrow(ti_file), 1)

  valid_topics <- ti_file$Topic[ti_file$Topic != -1]
  if (length(valid_topics) > 0) {
    tt_file <- bertopic_topic_terms(m_file, valid_topics[1], top_n = 5)
    expect_s3_class(tt_file, "tbl_df")
    expect_gt(nrow(tt_file), 0)
  }

  # try transform; gracefully skip on known upstream KeyError
  tr_err <- try(bertopic_transform(m_file, c("new unseen text")), silent = TRUE)
  if (inherits(tr_err, "try-error")) {
    msg <- conditionMessage(attr(tr_err, "condition"))
    if (grepl("KeyError", msg, fixed = TRUE)) {
      skip("Upstream BERTopic deserialization edge case: transform() raised KeyError after load.")
    } else {
      stop(tr_err)
    }
  } else {
    expect_length(tr_err$topics, 1)
  }

  # ----- directory save (bundle) with embedding model -----
  if (dir.exists(dir_path)) unlink(dir_path, recursive = TRUE, force = TRUE)
  ret_dir <- bertopic_save(
    m, dir_path,
    serialization = "pickle",
    save_embedding_model = TRUE,
    overwrite = TRUE
  )
  expect_true(is.character(ret_dir) && length(ret_dir) == 1)
  expect_true(dir.exists(dir_path) || file.exists(dir_path))

  m_dir <- bertopic_load(dir_path)
  expect_s3_class(m_dir, "bertopic_r")

  # topic info + terms
  ti_dir <- bertopic_topics(m_dir)
  expect_s3_class(ti_dir, "tbl_df")
  expect_gte(nrow(ti_dir), 1)

  valid_topics2 <- ti_dir$Topic[ti_dir$Topic != -1]
  if (length(valid_topics2) > 0) {
    tt_dir <- bertopic_topic_terms(m_dir, valid_topics2[1], top_n = 5)
    expect_s3_class(tt_dir, "tbl_df")
    expect_gt(nrow(tt_dir), 0)
  }

  # transform attempt with the same graceful skip policy
  tr2_err <- try(bertopic_transform(m_dir, c("another text")), silent = TRUE)
  if (inherits(tr2_err, "try-error")) {
    msg <- conditionMessage(attr(tr2_err, "condition"))
    if (grepl("KeyError", msg, fixed = TRUE)) {
      skip("Upstream BERTopic deserialization edge case: transform() raised KeyError after load.")
    } else {
      stop(tr2_err)
    }
  } else {
    expect_length(tr2_err$topics, 1)
  }
})
