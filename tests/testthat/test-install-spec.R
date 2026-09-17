test_that("Python installation routes share one exact dependency specification", {
  requirements <- BERTopic:::.bertopic_python_requirements()

  expect_true(length(requirements) > 0L)
  expect_true(all(grepl("^[A-Za-z0-9_.-]+==[^=<>!*~]+$", requirements)))

  packages <- sub("==.*$", "", requirements)
  expect_false(anyDuplicated(tolower(packages)) > 0L)
  expect_true(all(c(
    "bertopic==0.16.0",
    "torch==2.1.2",
    "sentence-transformers==2.7.0",
    "plotly==5.24.1"
  ) %in% requirements))

  conda_body <- paste(deparse(body(install_py_deps_conda)), collapse = "\n")
  venv_body <- paste(deparse(body(install_py_deps_venv)), collapse = "\n")
  expect_match(conda_body, ".bertopic_python_requirements", fixed = TRUE)
  expect_match(venv_body, ".bertopic_python_requirements", fixed = TRUE)
})

test_that("shared import validation covers runtime and visualization modules", {
  validator <- paste(deparse(body(BERTopic:::.validate_bertopic_modules)), collapse = "\n")
  for (module in c(
    "bertopic", "sentence_transformers", "torch", "transformers",
    "umap", "hdbscan", "numpy", "scipy", "sklearn", "pandas", "plotly"
  )) {
    expect_match(validator, paste0('"', module, '"'), fixed = TRUE)
  }
})
