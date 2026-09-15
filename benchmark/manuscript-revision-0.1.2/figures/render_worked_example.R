# Render a paper figure from the exact archived worked-example metadata.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag) {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) stop("Pass ", flag)
  args[[index + 1L]]
}
input <- get_arg("--input")
output <- get_arg("--output")
info <- read.csv(input, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
top <- head(info[info$Topic != -1L, ], 8L)
stopifnot(nrow(top) == 8L, all(is.finite(top$Count)), all(top$Count > 0))
draw <- function() {
  par(mar = c(3.8, 13, .8, 1), bg = "white")
  positions <- barplot(rev(top$Count), names.arg = rev(top$Name),
                       horiz = TRUE, las = 1, border = NA,
                       col = "#3178A5", cex.names = .8, axes = FALSE,
                       xlim = c(0, max(top$Count) * 1.15))
  ticks <- pretty(c(0, max(top$Count)))
  axis(1, at = ticks[ticks >= 0 & ticks <= max(top$Count) * 1.15], cex.axis = .8)
  mtext("Messages", side = 1, line = 2.4, cex = .85)
  text(top$Count[nrow(top):1L], positions, labels = rev(top$Count),
       pos = 4, cex = .8, offset = .35)
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
pdf(paste0(output, ".pdf"), width = 6.5, height = 4, pointsize = 11)
draw()
dev.off()
png(paste0(output, ".png"), width = 1950, height = 1200, res = 300, pointsize = 11)
draw()
dev.off()
cat("Rendered paper figure from ", normalizePath(input, winslash = "/"), "\n", sep = "")