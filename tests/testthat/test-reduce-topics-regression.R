test_that("reduce topics passes docs and nr_topics to the Python API", {
  testthat::local_mocked_bindings(
    .need_py = function() invisible(TRUE),
    .package = "BERTopic"
  )

  calls <- list()
  py_model <- new.env(parent = emptyenv())
  py_model$reduce_topics <- function(docs, nr_topics) {
    calls[[length(calls) + 1L]] <<- list(docs = docs, nr_topics = nr_topics)
    if (!identical(docs, c("alpha", "beta", "gamma"))) {
      stop("unexpected docs", call. = FALSE)
    }
    if (!identical(nr_topics, 2L)) {
      stop("unexpected nr_topics", call. = FALSE)
    }
    "ok"
  }

  model <- structure(
    list(.py = py_model, topics = NULL, probs = NULL),
    class = "bertopic_r"
  )

  expect_invisible(bertopic_reduce_topics(
    model,
    nr_topics = 2L,
    docs = c("alpha", "beta", "gamma")
  ))
  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$nr_topics, 2L)
})

test_that("reduce topics validates required arguments before Python calls", {
  testthat::local_mocked_bindings(
    .need_py = function() invisible(TRUE),
    .package = "BERTopic"
  )

  calls <- 0L
  py_model <- new.env(parent = emptyenv())
  py_model$reduce_topics <- function(...) {
    calls <<- calls + 1L
    "ok"
  }
  model <- structure(
    list(.py = py_model, topics = NULL, probs = NULL),
    class = "bertopic_r"
  )

  expect_error(bertopic_reduce_topics(model, nr_topics = 2L), "`docs` must be provided")
  expect_error(bertopic_reduce_topics(model, nr_topics = 0L, docs = "alpha"), "`nr_topics`")
  expect_error(bertopic_reduce_topics(model, nr_topics = 1.5, docs = "alpha"), "`nr_topics`")
  expect_identical(calls, 0L)
})
