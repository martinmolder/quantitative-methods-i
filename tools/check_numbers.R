# Quantitative Methods I -- hand-written numbers check.
#
# Run from the site directory:  Rscript tools/check_numbers.R
#
# Principle 6.1: no number that comes out of the data should be typed into
# the text. A typed number is right when it is typed and wrong the moment the
# data or the model changes, and nothing warns you about the difference.
#
# The check looks for numbers in prose that are not produced by inline R
# code and that look like they came from a computation -- a decimal, or a
# large integer. Structural numbers (session references, scale endpoints,
# years, conventional thresholds) are filtered out.
#
# Some of what it reports will be legitimate. It is a list to read, not a
# list of faults.

chapters <- c("index.qmd", sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$")))

# Values that appear in prose for reasons other than being a result.
allowed <- c(
  "0.05", "0.5", "1.96", "0.3", "0.2", "0.4", "0.7", "0.8", "0.9", "0.95",
  "0.6", "0.1", "1.5", "2.5", "97.5", "0.25", "0.75",
  "100", "1000", "120", "110", "300", "200", "500"
)

problems <- character(0)

for (chapter in chapters) {

  lines <- readLines(chapter, warn = FALSE)

  fence <- grepl("^```", lines)
  in_chunk <- cumsum(fence) %% 2 == 1
  prose_lines <- which(!in_chunk & !fence)

  for (i in prose_lines) {

    line <- lines[i]

    # Structural lines carry no claims about the data.
    if (grepl("^\\s*$", line)) next
    if (grepl("^:", line)) next                      # table attributes
    if (grepl("^\\|", line)) next                    # table rows
    if (grepl("<img|style=|src=", line)) next        # raw HTML
    if (grepl("^#+ ", line)) next                    # headings
    if (grepl("doi:|https?://", line)) next          # citations and links
    if (grepl("^\\s*[0-9]+\\. ", line)) next         # numbered list markers

    # Inline R code is the approved way to put a computed number in prose.
    stripped <- gsub("`r [^`]*`", "", line)
    stripped <- gsub("`[^`]*`", "", stripped)
    stripped <- gsub("\\$\\$?[^$]*\\$\\$?", "", stripped)   # maths

    # A number is a run of digits, optionally with a decimal part. A full
    # stop that ends a sentence is not part of it.
    numbers <- regmatches(
      stripped,
      gregexpr("[0-9]+(?:\\.[0-9]+)?", stripped, perl = TRUE)
    )[[1]]

    # Keep only what could plausibly be a result: a decimal, or an integer
    # of 100 or more that is not a year.
    looks_computed <- function(n) {
      value <- suppressWarnings(as.numeric(n))
      if (is.na(value)) return(FALSE)
      if (n %in% allowed) return(FALSE)
      if (grepl("\\.", n)) return(TRUE)
      if (value >= 100 && !(value >= 1900 && value <= 2100)) return(TRUE)
      FALSE
    }

    numbers <- Filter(looks_computed, numbers)

    if (length(numbers) > 0) {
      problems <- c(problems, sprintf(
        "%s:%d  [%s]\n      %s",
        chapter, i, paste(unique(numbers), collapse = ", "),
        trimws(substr(line, 1, 120))
      ))
    }
  }
}

if (length(problems) == 0) {
  cat("No hand-written result numbers found in prose.\n")
} else {
  cat(length(problems), "line(s) to check:\n\n")
  cat(paste0("  ", problems, collapse = "\n\n"), "\n")
}
