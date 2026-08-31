# Quantitative Methods I -- lecture slide check.
#
# Run from the site directory:  Rscript tools/check_slides.R
#
# The decks live in ../slides, outside the Quarto project, so that the two
# can be worked on and published independently.
#
# The slides exist to be talked through alongside the chapters, so every code
# example on a slide should be one the reader can find in the chapter. This
# checks that, and a few structural things:
#
#   - a deck exists for every chapter, and vice versa;
#   - every deck compiled;
#   - frame counts are in the range that fits a 45-60 minute lecture;
#   - R source shown on a slide appears in the matching chapter.
#
# Console output pasted onto a slide is not checked -- it is reformatted to
# fit, so it will not match character for character.

# The decks live outside the Quarto project, in a sibling directory.
slides_dir <- "../slides"

chapters <- sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$"))
decks <- sort(list.files(slides_dir, pattern = "^[0-9]{2}-.*\\.tex$"))

problems <- character(0)

chapter_numbers <- substr(chapters, 1, 2)
deck_numbers <- substr(decks, 1, 2)

for (n in setdiff(chapter_numbers, deck_numbers)) {
  problems <- c(problems, paste("chapter", n, "has no deck"))
}
for (n in setdiff(deck_numbers, chapter_numbers)) {
  problems <- c(problems, paste("deck", n, "has no chapter"))
}

# Pull the R source out of a chapter, ignoring hidden chunks.
chapter_code <- function(path) {
  lines <- readLines(path, warn = FALSE)
  fence <- grepl("^```", lines)
  chunk_id <- cumsum(fence)
  hidden <- unique(chunk_id[grepl("include: false", lines, fixed = TRUE)])
  code <- lines[chunk_id %% 2 == 1 & !fence & !(chunk_id %in% hidden)]
  code <- c(code, unlist(regmatches(
    lines, gregexpr("`r [^`]*`", lines)
  )))
  paste(code, collapse = "\n")
}

# Pull the R source out of a deck. Only the Rstyle listings, not the output
# blocks, and not the inline \verb spans.
deck_code <- function(path) {
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^\\\\begin\\{lstlisting\\}", lines)
  ends <- grep("^\\\\end\\{lstlisting\\}", lines)
  out <- character(0)
  for (k in seq_along(starts)) {
    if (grepl("Outstyle", lines[starts[k]])) next
    if (starts[k] + 1 <= ends[k] - 1) {
      out <- c(out, lines[(starts[k] + 1):(ends[k] - 1)])
    }
  }
  out
}

# Compare on the identifiers rather than character by character, since a
# slide wraps lines to fit and a chapter does not. Comments are annotation
# for the lecture, not code, so they are stripped first.
signature <- function(x) {
  # Split first: a comment runs to the end of its own line, not to the end
  # of the whole block, and "." matches a newline here.
  parts <- unlist(strsplit(x, "\n", fixed = TRUE))
  parts <- sub("#.*$", "", parts)
  bits <- unlist(regmatches(
    parts, gregexpr("[a-zA-Z_.][a-zA-Z0-9_.]*", parts)
  ))
  bits[nchar(bits) > 2]
}

cat(sprintf("%-34s %7s %7s %s\n", "deck", "frames", "pdf", "code lines checked"))

for (deck in decks) {

  n <- substr(deck, 1, 2)
  chapter <- chapters[chapter_numbers == n]
  if (length(chapter) == 0) next

  deck_path <- file.path(slides_dir, deck)
  lines <- readLines(deck_path, warn = FALSE)
  frames <- sum(grepl("\\\\begin\\{frame\\}", lines))

  pdf_path <- file.path(slides_dir, "pdf", sub("\\.tex$", ".pdf", deck))
  built <- file.exists(pdf_path)
  if (!built) {
    problems <- c(problems, paste(deck, "has not been built"))
  }

  if (frames < 12 || frames > 28) {
    problems <- c(problems, sprintf(
      "%s has %d frames -- aim for 12 to 28 in a 45-60 minute lecture",
      deck, frames
    ))
  }

  ref <- signature(chapter_code(chapter))
  code <- deck_code(deck_path)
  code <- code[grepl("[a-zA-Z]", code)]

  unmatched <- 0
  for (line in code) {
    bits <- signature(line)
    if (length(bits) == 0) next   # a comment-only line
    if (mean(bits %in% ref) < 0.6) {
      unmatched <- unmatched + 1
      problems <- c(problems, sprintf(
        "%s: code not found in %s\n      %s", deck, chapter, trimws(line)
      ))
    }
  }

  cat(sprintf(
    "%-34s %7d %7s %d\n",
    sub("\\.tex$", "", deck), frames,
    ifelse(built, "ok", "MISSING"), length(code)
  ))
}

cat("\n")

if (length(problems) == 0) {
  cat("Decks match their chapters.\n")
} else {
  cat(length(problems), "item(s) to check:\n\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
}
