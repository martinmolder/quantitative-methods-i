# Quantitative Methods I -- code style check.
#
# Run from the site directory:  Rscript tools/check_style.R
#
# Checks the code in the chapters against section 2 of the guidance:
#   2.1  operations are not nested inside one another
#   2.3  no alignment with runs of spaces
#   2.5  code lines stay within 80 characters
#
# It looks at displayed code only. Setup chunks are hidden from the reader,
# and inline code in prose is not read as code.

chapters <- sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$"))

problems <- character(0)

for (chapter in chapters) {

  lines <- readLines(chapter, warn = FALSE)

  fence <- grepl("^```", lines)
  chunk_id <- cumsum(fence)
  in_chunk <- chunk_id %% 2 == 1
  hidden <- unique(chunk_id[
    grepl("include: false", lines, fixed = TRUE) |
      grepl("echo: false", lines, fixed = TRUE)
  ])

  is_code <- in_chunk & !fence & !(chunk_id %in% hidden)

  for (i in which(is_code)) {

    line <- lines[i]
    if (grepl("^#\\|", line)) next        # chunk options
    if (grepl("^\\s*#", line)) next       # comments wrap like prose

    if (nchar(line) > 80) {
      problems <- c(problems, sprintf(
        "%s:%d  %d characters\n      %s", chapter, i, nchar(line), trimws(line)
      ))
    }

    # Alignment padding: two or more spaces before an equals sign.
    if (grepl("[^ ]  +=[^=]", line)) {
      problems <- c(problems, sprintf(
        "%s:%d  aligned with spaces\n      %s", chapter, i, trimws(line)
      ))
    }

    # Principle 2.1 is about analysis steps being buried inside one another,
    # as in round(cor(zap_labels(x), use = "complete.obs"), 3). It is not
    # about sum(is.na(x)), which is one idea written the natural way. So the
    # test is depth: three or more function calls nested inside each other on
    # one line is the shape that should be broken up.
    depth <- 0
    max_depth <- 0
    chars <- strsplit(line, "")[[1]]

    for (j in seq_along(chars)) {
      if (chars[j] == "(") {
        before <- substr(line, 1, j - 1)
        is_call <- grepl("[a-zA-Z0-9_.]$", before)
        if (is_call) {
          depth <- depth + 1
          max_depth <- max(max_depth, depth)
        } else {
          depth <- depth + 1
        }
      } else if (chars[j] == ")") {
        depth <- max(0, depth - 1)
      }
    }

    calls <- lengths(regmatches(
      line,
      gregexpr("[a-zA-Z_.][a-zA-Z0-9_.]*\\(", line, perl = TRUE)
    ))

    # round(coef(summary(m)), 3) is three calls deep and perfectly clear,
    # because two of them only pull a piece out of an object. Nesting is only
    # a problem when the buried calls are analysis steps in their own right.
    wrappers <- paste0(
      "round|print|summary|coef|c|as\\.[a-z.]+|unclass|head|names|colnames|",
      "rownames|nrow|ncol|length|seq_len|seq_along|is\\.na|fmt|format|",
      "formatC|unname|rev|sort|diag|everything|starts_with|desc|vars|paste|",
      "paste0|mutate|summarise|select|filter|arrange|group_by|across"
    )
    call_names <- gsub("\\($", "", regmatches(
      line,
      gregexpr("[a-zA-Z_.][a-zA-Z0-9_.]*\\(", line, perl = TRUE)
    )[[1]])
    substantive <- call_names[!grepl(paste0("^(", wrappers, ")$"), call_names)]

    if (max_depth >= 3 && length(substantive) >= 2) {
      problems <- c(problems, sprintf(
        "%s:%d  %d levels of nesting\n      %s",
        chapter, i, max_depth, trimws(line)
      ))
    }
  }
}

if (length(problems) == 0) {
  cat("No style problems found.\n")
} else {
  cat(length(problems), "item(s) to check:\n\n")
  cat(paste0("  ", problems, collapse = "\n\n"), "\n")
}
