# Preserve nested backend metadata in CSV as JSON-valued cells.
write_example_csv <- function(value, file, ...) {
  frame <- as.data.frame(value)
  for (name in names(frame)) {
    if (is.list(frame[[name]])) {
      frame[[name]] <- vapply(frame[[name]], function(cell) {
        as.character(jsonlite::toJSON(cell, auto_unbox = TRUE, null = "null"))
      }, character(1))
    }
  }
  utils::write.csv(frame, file, ...)
}
