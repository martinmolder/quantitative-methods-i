# Quantitative Methods I -- encoding check.
#
# Run from the site directory:  Rscript tools/check_encoding.R
#
# Principle 6.3 and 3.5: everything is UTF-8, and the letters outside plain
# English have to survive into the rendered pages. A broken encoding shows up
# as mojibake rather than as an error, so it has to be looked for.

files <- c(
  list.files(pattern = "\\.qmd$"),
  list.files("data", pattern = "\\.csv$", full.names = TRUE),
  list.files("data-raw", pattern = "\\.(R|csv)$", full.names = TRUE),
  list.files("tools", pattern = "\\.R$", full.names = TRUE),
  list.files("swirl", pattern = "\\.(yaml|R)$", recursive = TRUE, full.names = TRUE),
  "styles.css"
)

# This script contains the mojibake patterns it searches for, so it has to
# leave itself out.
files <- setdiff(files, "tools/check_encoding.R")

problems <- character(0)

for (f in files) {
  raw <- readBin(f, "raw", file.size(f))
  text <- rawToChar(raw)
  if (!validUTF8(text)) {
    problems <- c(problems, paste(f, "is not valid UTF-8"))
  }
  # Mojibake: the bytes of a UTF-8 letter read as Latin-1 and re-encoded.
  if (grepl("Ã¤|Ã¶|Ã¥|Ã–|Ã„|Ã…|â€", text)) {
    problems <- c(problems, paste(f, "contains mojibake"))
  }
}

cat("Checked", length(files), "files for valid UTF-8.\n")

# The Swedish letters must survive into the rendered pages.
pages <- list.files("docs", pattern = "\\.html$", full.names = TRUE)
expected <- c("Götaland", "Östergötland", "Åkesson", "Lööf", "Vänsterpartiet")

found_any <- FALSE
for (page in pages) {
  text <- rawToChar(readBin(page, "raw", file.size(page)))
  if (!validUTF8(text)) {
    problems <- c(problems, paste(page, "rendered output is not valid UTF-8"))
  }
  if (grepl("Ã¤|Ã¶|Ã¥|â€", text)) {
    problems <- c(problems, paste(page, "rendered output contains mojibake"))
  }
  if (any(sapply(expected, grepl, x = text, fixed = TRUE))) found_any <- TRUE
}

cat("Checked", length(pages), "rendered pages.\n")

if (!found_any) {
  problems <- c(problems, "No Swedish diacritics found in any rendered page -- check that they survived")
}

cat("\n")

if (length(problems) == 0) {
  cat("Encoding is clean throughout.\n")
} else {
  cat(length(problems), "problem(s):\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
}
