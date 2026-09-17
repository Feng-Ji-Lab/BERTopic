test_that("visualize_topics propagates backend errors", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  py_model <- new.env(parent = emptyenv())
  py_model$visualize_topics <- function() stop("backend boom")
  model <- structure(list(.py = py_model), class = "bertopic_r")
  expect_error(bertopic_visualize_topics(model, file = tempfile(fileext = ".html")), class = "rlang_error")
})
