# Regression for the real list-column export failure in the worked example.
source("benchmark/example_io.R")
run_case <- function() {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  frame <- data.frame(Topic = c(-1L, 0L), Count = c(3L, 2L), Name = c("noise", "topic"))
  frame$Representation <- list(c("hello", "world"), c("love", "life"))
  frame$Representative_Docs <- list(c("text, with comma", "quoted \"text\""),
                                    c("line one\nline two", "\u4e2d\u6587"))
  write_example_csv(frame, path, row.names = FALSE, fileEncoding = "UTF-8")
  restored <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  stopifnot(identical(restored$Topic, frame$Topic), identical(restored$Count, frame$Count),
            identical(restored$Name, frame$Name))
  for (name in c("Representation", "Representative_Docs")) {
    for (i in seq_len(nrow(frame))) {
      stopifnot(identical(jsonlite::fromJSON(restored[[name]][[i]]), frame[[name]][[i]]))
    }
  }
  cat("PASS: nested metadata, quotes, commas, newlines, and Unicode round trip\n")
}
run_case()
