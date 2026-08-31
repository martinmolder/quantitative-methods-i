# Quantitative Methods I -- chapter balance check.
#
# Run from the site directory:  Rscript tools/check_balance.R
#
# Principle 8.1: one chapter is one seminar. Principle 8.2: chapters should
# be of roughly even length, and one that is twice another should be split or
# cut. This measures each chapter and flags the outliers.
#
# It also checks that every chapter carries the structural pieces the course
# promises: at least one discussion question, and -- from session 6 onwards,
# where methods produce results to write up -- a reporting box.

chapters <- sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$"))

measure <- data.frame(
  chapter = chapters,
  words = NA_integer_,
  code = NA_integer_,
  chunks = NA_integer_,
  discussion = NA_integer_,
  reporting = NA_integer_,
  swirl = NA_integer_
)

for (k in seq_along(chapters)) {

  lines <- readLines(chapters[k], warn = FALSE)

  fence <- grepl("^```", lines)
  chunk_id <- cumsum(fence)
  in_chunk <- chunk_id %% 2 == 1
  hidden <- unique(chunk_id[grepl("include: false", lines, fixed = TRUE)])

  is_code <- in_chunk & !fence & !(chunk_id %in% hidden)
  prose <- lines[!in_chunk & !fence]

  words <- unlist(strsplit(paste(prose, collapse = " "), "\\s+"))
  words <- words[nzchar(words)]

  measure$words[k] <- length(words)
  measure$code[k] <- sum(is_code)
  measure$chunks[k] <- sum(fence) / 2
  measure$discussion[k] <- sum(grepl("{.discussion}", lines, fixed = TRUE))
  measure$reporting[k] <- sum(grepl("{.reporting}", lines, fixed = TRUE))
  measure$swirl[k] <- sum(grepl("{.swirl}", lines, fixed = TRUE))
}

print(measure, row.names = FALSE)

cat("\n")

median_words <- median(measure$words)
longest <- measure$chapter[which.max(measure$words)]
shortest <- measure$chapter[which.min(measure$words)]

cat("Median chapter:", median_words, "words\n")
cat(sprintf(
  "Longest to shortest: %s (%d) / %s (%d) = %.2f\n",
  longest, max(measure$words), shortest, min(measure$words),
  max(measure$words) / min(measure$words)
))
cat("Principle 8.2 asks for this ratio to stay under about 2.\n")

problems <- character(0)

for (k in seq_len(nrow(measure))) {

  ratio <- measure$words[k] / median_words

  if (ratio > 1.5 || ratio < 0.67) {
    problems <- c(problems, sprintf(
      "%s: %d words, %.2f times the median",
      measure$chapter[k], measure$words[k], ratio
    ))
  }

  if (measure$discussion[k] == 0) {
    problems <- c(problems, sprintf(
      "%s: no discussion question", measure$chapter[k]
    ))
  }

  if (measure$swirl[k] == 0) {
    problems <- c(problems, sprintf(
      "%s: no swirl practical", measure$chapter[k]
    ))
  }

  # Sessions 6 onwards produce results that have to be written up.
  number <- as.integer(substr(measure$chapter[k], 1, 2))
  if (number >= 6 && measure$reporting[k] == 0) {
    problems <- c(problems, sprintf(
      "%s: no reporting box", measure$chapter[k]
    ))
  }
}

cat("\n")

if (length(problems) == 0) {
  cat("Chapters are balanced and complete.\n")
} else {
  cat(length(problems), "item(s) to check:\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
}
