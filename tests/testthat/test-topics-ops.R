test_that("topic labels use the package implementation", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  py_model <- new.env(parent = emptyenv())
  py_model$set_topic_labels <- function(labels) {
    expect_true(inherits(labels, "python.builtin.dict"))
    invisible(NULL)
  }
  model <- structure(list(.py = py_model), class = "bertopic_r")
  expect_invisible(BERTopic::bertopic_set_topic_labels(model, c("1" = "Label")))
})
