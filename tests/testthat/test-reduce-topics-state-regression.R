test_that("reduce_topics synchronizes cached R state", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  py_model <- new.env(parent = emptyenv())
  py_model$topics_ <- c(2L, 2L, 0L)
  py_model$probabilities_ <- matrix(c(.1, .9, .8, .2), nrow = 2)
  py_model$reduce_topics <- function(docs, nr_topics) {
    py_model$topics_ <- c(1L, 1L, 0L)
    py_model$probabilities_ <- matrix(c(.2, .8, .7, .3), nrow = 2)
    invisible(NULL)
  }
  model <- structure(list(.py = py_model, topics = c(2L, 2L, 0L), probs = NULL), class = "bertopic_r")
  updated <- bertopic_reduce_topics(model, docs = c("a", "b", "c"), nr_topics = 2L)
  expect_identical(updated$topics, c(1L, 1L, 0L))
  expect_equal(updated$probs, py_model$probabilities_)
})