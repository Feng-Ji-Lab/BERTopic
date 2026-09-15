test_that("save failures keep the original cause without falling back to pickle", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  calls <- 0L
  backend <- new.env(parent = emptyenv())
  backend$save <- function(...) {
    calls <<- calls + 1L
    stop("original safetensors failure")
  }
  model <- structure(list(.py = backend), class = "bertopic_r")
  expect_error(bertopic_save(model, tempfile(), serialization = "safetensors"), "original safetensors failure")
  expect_identical(calls, 1L)
})

test_that("pt maps to pytorch and explicit references remain strings", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  received <- NULL
  backend <- new.env(parent = emptyenv())
  backend$save <- function(path, serialization, save_embedding_model) {
    received <<- list(format = serialization, embedding = save_embedding_model)
    dir.create(path)
  }
  model <- structure(list(.py = backend), class = "bertopic_r")
  path <- file.path(withr::local_tempdir(), "model")
  bertopic_save(model, path, serialization = "pt",
                save_embedding_model = "sentence-transformers/all-MiniLM-L6-v2")
  expect_identical(received$format, "pytorch")
  expect_identical(received$embedding, "sentence-transformers/all-MiniLM-L6-v2")
})

test_that("existing destinations require explicit overwrite", {
  local_mocked_bindings(.need_py = function() invisible(TRUE), .package = "BERTopic")
  backend <- new.env(parent = emptyenv())
  backend$save <- function(...) stop("must not save")
  model <- structure(list(.py = backend), class = "bertopic_r")
  path <- file.path(withr::local_tempdir(), "existing.pkl")
  writeLines("keep original content", path)
  expect_error(bertopic_save(model, path), "overwrite = TRUE")
  expect_identical(readLines(path), "keep original content")
  expect_error(bertopic_save(model, tempfile(), save_embedding_model = NA), "save_embedding_model")
  expect_error(bertopic_save(model, tempfile(), save_embedding_model = "identifier"), "save_embedding_model")
})

test_that("default lightweight saves restore metadata without embedding references", {
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")
  set_bertopic_seed(42)
  set.seed(42)
  docs <- rep(c("love life smile friend", "claim free prize message",
                "class school exam lesson", "train travel ticket station"), each = 25L)
  embeddings <- matrix(rnorm(length(docs) * 3L, sd = .08), ncol = 3L)
  embeddings[, 1L] <- embeddings[, 1L] + rep(c(-10, -3, 3, 10), each = 25L)
  dimensionality <- reticulate::import("bertopic.dimensionality", convert = FALSE)
  hdbscan <- reticulate::import("hdbscan", convert = FALSE)
  model <- bertopic_fit(docs, embeddings = embeddings,
                       umap_model = dimensionality$BaseDimensionalityReduction(),
                       hdbscan_model = hdbscan$HDBSCAN(min_cluster_size = 5L, prediction_data = TRUE),
                       calculate_probabilities = TRUE)
  root <- withr::local_tempdir()
  json <- reticulate::import("json", convert = TRUE)
  metadata <- function(x) as.data.frame(bertopic_topics(x))[, c("Topic", "Count", "Name")]
  for (format in c("safetensors", "pt")) {
    path <- file.path(root, format)
    bertopic_save(model, path, serialization = format)
    weights <- if (format == "pt") "topic_embeddings.bin" else "topic_embeddings.safetensors"
    expect_true(file.exists(file.path(path, weights)))
    config <- json$loads(paste(readLines(file.path(path, "config.json"), warn = FALSE), collapse = "\n"))
    expect_false("embedding_model" %in% names(config))
    restored <- bertopic_load(path)
    expect_identical(metadata(restored), metadata(model))
    expect_identical(as.integer(restored$topics), as.integer(model$topics))
  }
  path <- file.path(root, "with-pointer")
  pointer <- "sentence-transformers/all-MiniLM-L6-v2"
  bertopic_save(model, path, serialization = "safetensors", save_embedding_model = pointer)
  config <- json$loads(paste(readLines(file.path(path, "config.json"), warn = FALSE), collapse = "\n"))
  expect_identical(config$embedding_model, pointer)
  # Also verify FALSE strips an upstream-inferred reference, without loading weights.
  types <- reticulate::import("types", convert = FALSE)
  backend <- types$SimpleNamespace()
  reticulate::py_set_attr(backend, "_hf_model", pointer)
  model$.py$embedding_model <- backend
  path <- file.path(root, "without-inferred-pointer")
  bertopic_save(model, path, serialization = "safetensors")
  config <- json$loads(paste(readLines(file.path(path, "config.json"), warn = FALSE), collapse = "\n"))
  expect_false("embedding_model" %in% names(config))
})
