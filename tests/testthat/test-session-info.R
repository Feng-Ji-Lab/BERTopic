test_that("session info reports exact Python dependency versions", {
  skip_on_cran()
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  info <- bertopic_session_info()

  expect_type(info$numpy, "logical")
  expect_length(info$numpy, 1L)
  expect_true(info$numpy)
  expect_match(info$numpy_version, "^[0-9]+")
  expect_match(info$bertopic_version, "^[0-9]+")
  expect_s3_class(info$modules, "data.frame")
  expect_named(info$modules, c("module", "available", "version"))
  expect_true(all(c("bertopic", "numpy", "sklearn", "torch") %in% info$modules$module))
  expect_true(all(nzchar(info$modules$version[info$modules$available])))
})
