test_that("self-check compares a real save/load round trip", {
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  result <- bertopic_self_check()

  expect_true(result$python_ok)
  expect_true(result$bertopic_ok)
  expect_true(result$roundtrip_ok, info = paste(result$details, collapse = "\n"))
  expect_match(result$details, "outputs agree", fixed = TRUE)
})