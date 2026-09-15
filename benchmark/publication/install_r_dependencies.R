# Install the public R packages needed to reproduce the archived Windows analysis.
install.packages(
  c("reticulate", "rlang", "tibble", "testthat", "withr",
    "Matrix", "jsonlite", "htmltools", "ggplot2"),
  repos = "https://cloud.r-project.org"
)
