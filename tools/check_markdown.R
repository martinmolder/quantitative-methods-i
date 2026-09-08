# Quantitative Methods I -- markdown that renders as something else.
#
# Run from the site directory:  Rscript tools/check_markdown.R
#
# Pandoc needs a blank line before a block. A "> " line that follows a
# paragraph directly is read as a lazy continuation of it, so the marks set
# as literal text: the reader gets "> > - The percentage of women ..." in
# the middle of a sentence. Nothing errors, and it is only visible on the
# rendered page, which is how one survived to publication.

files <- sort(c(
  list.files(pattern = "^[0-9]{2}-.*\\.qmd$"),
  list.files(pattern = "^index\\.qmd$")
))

problems <- character(0)

for (file in files) {
  lines <- readLines(file, warn = FALSE)

  # Fenced code is not prose; a > inside it is R, not a quote.
  fence <- grepl("^```", lines)
  in_code <- cumsum(fence) %% 2 == 1

  for (i in seq_along(lines)) {
    if (i == 1 || in_code[i] || !grepl("^>", lines[i])) next

    before <- lines[i - 1]
    if (nzchar(trimws(before)) && !grepl("^>", before)) {
      problems <- c(problems, sprintf(
        "%s:%d  blockquote with no blank line before it\n      %s\n      %s",
        file, i, trimws(before), trimws(lines[i])
      ))
    }
  }
}

cat(length(files), "files checked\n\n")

if (length(problems) == 0) {
  cat("No markdown that renders as something else.\n")
} else {
  cat(length(problems), "item(s) to check:\n\n")
  cat(paste0("  - ", problems, collapse = "\n\n"), "\n")
  quit(status = 1)
}
