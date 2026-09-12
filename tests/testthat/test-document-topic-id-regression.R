test_that("document-topic matrix preserves non-contiguous topic IDs", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  py_model <- new.env(parent = emptyenv())
  py_model$get_topic_info <- function() data.frame(Topic = c(-1L, 2L, 5L))
  model <- structure(list(.py = py_model, probs = matrix(1:4, nrow = 2)), class = "bertopic_r")
  dense <- bertopic_as_document_topic_matrix(model, sparse = FALSE, prefix = TRUE)
  expect_identical(colnames(dense), c("topic_2", "topic_5"))
})
