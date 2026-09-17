test_that("representative documents wrapper matches the Python API", {
  # BERTopic::get_representative_docs() accepts a topic id only.  Keep this
  # regression test independent of model downloads by using a minimal backend
  # double that rejects any extra arguments.
  testthat::local_mocked_bindings(
    .need_py = function() invisible(TRUE),
    .package = "BERTopic"
  )

  requested_topic <- NULL
  py_model <- new.env(parent = emptyenv())
  py_model$get_representative_docs <- function(topic) {
    requested_topic <<- topic
    c("document one", "document two", "document three")
  }

  model <- structure(
    list(.py = py_model, topics = NULL, probs = NULL),
    class = "bertopic_r"
  )

  result <- bertopic_get_representative_docs(
    model,
    topic_id = 7L,
    top_n = 2L
  )

  expect_identical(requested_topic, 7L)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("rank", "document"))
  expect_identical(result$rank, 1:2)
  expect_identical(result$document, c("document one", "document two"))
})
