# Quantitative Methods I -- first-use check.
#
# Run from the site directory:  Rscript tools/check_firstuse.R
#
# Principle 1.1 says that everything used for the first time must be
# explained: functions, and also their arguments, which is where the gaps
# usually are. Principle 1.4 says to check this by machine rather than from
# memory, which is what this does.
#
# For every chapter, in order, it walks the code chunks and records the first
# appearance of each function and of each named argument. It then asks
# whether that name appears anywhere in the prose of the chapter it first
# appears in. A name that never appears in prose is reported.
#
# The check is deliberately generous -- it only asks whether the name is
# mentioned, not whether the mention is any good. It finds omissions, not
# bad explanations.

chapters <- sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$"))

# Things that need no explanation: R itself, and words a reader already has.
assumed <- c(
  "c", "library", "function", "if", "for", "in", "return", "TRUE", "FALSE",
  "NA", "NULL", "list", "print", "cat", "paste", "paste0", "seq_len",
  "data.frame", "readRDS", "nrow", "ncol", "names", "head", "round", "sum",
  "mean", "length", "rep", "which", "is.na", "factor", "levels", "table",
  "min", "max", "range", "sd", "var", "median", "summary", "class", "sqrt",
  "exp", "log", "abs", "diff", "rev", "sort", "order", "unique", "as.numeric",
  "as.character", "as.data.frame", "as.matrix", "as.table", "matrix", "apply",
  "sapply", "vapply", "unname", "setdiff", "match", "ifelse", "stopifnot",
  "formatC", "format", "Sys.setlocale", "options", "set.seed", "trimws"
)

# Names of columns in the course data. When one of these appears before an
# equals sign it is a column being created or renamed, not an argument.
data_names <- unique(c(
  names(readRDS("data/swe.rds")),
  names(read.csv("data/world.csv")),
  names(read.csv("data/turnout.csv")),
  names(read.csv("data/counties.csv")),
  c("V", "MP", "S", "C", "L", "KD", "M", "SD")
))

# Names that the creation-context test above cannot see, because the call
# they sit in contains nested brackets -- for example
# c(small = nobs(model_small), big = nobs(model_big)). Keep this list short
# and add to it only after checking the case by hand.
known_names <- c("big", "small")

assumed_args <- c(
  "x", "y", "data", "n", "digits", "levels", "labels", "file", "sep",
  "header", "size", "breaks", "col", "main", "xlab", "ylab", "each",
  "collapse", "decreasing", "by", "nrow", "ncol", "byrow", "times"
)

seen_fun <- character(0)
seen_arg <- character(0)
problems <- character(0)

for (chapter in chapters) {

  lines <- readLines(chapter, warn = FALSE)

  # Split the file into code chunks and prose.
  fence <- grepl("^```", lines)
  in_chunk <- cumsum(fence) %% 2 == 1
  is_code <- in_chunk & !fence

  code <- lines[is_code]
  prose <- lines[!is_code]

  # Setup chunks are hidden from the reader, so nothing in them counts as
  # having been introduced.
  chunk_id <- cumsum(fence)
  hidden <- unique(chunk_id[grepl("include: false", lines, fixed = TRUE)])
  code <- lines[is_code & !(chunk_id %in% hidden)]

  # Chunk options are instructions to knitr, not R the reader has to learn.
  code <- code[!grepl("^#\\|", code)]

  prose_text <- paste(prose, collapse = "\n")

  # A call can be spread over several lines, so the test for "is this a name
  # being created rather than an argument" needs to see the whole call. Join
  # the code into one string with the line breaks turned into spaces.
  code_flat <- paste(code, collapse = " ")
  code_flat <- gsub("\\s+", " ", code_flat)

  # Inline R code counts as prose for this purpose -- it is displayed.
  prose_text <- paste(prose_text, paste(code[grepl("^#", code)], collapse = "\n"))

  for (line in code) {

    # Function calls: a name immediately followed by an opening bracket.
    funs <- regmatches(line, gregexpr("\\b[a-zA-Z_.][a-zA-Z0-9_.]*(?=\\()", line, perl = TRUE))[[1]]

    for (fn in funs) {
      if (fn %in% assumed || fn %in% seen_fun) next
      seen_fun <- c(seen_fun, fn)
      if (!grepl(paste0("\\b", gsub("\\.", "\\\\.", fn), "\\b"), prose_text)) {
        problems <- c(problems, sprintf(
          "%s: function `%s()` is used but never named in the prose", chapter, fn
        ))
      }
    }

    # Named arguments: a name followed by = but not ==, <- or =>.
    args <- regmatches(line, gregexpr("\\b[a-zA-Z_.][a-zA-Z0-9_.]*(?=\\s*=[^=])", line, perl = TRUE))[[1]]

    for (ag in args) {
      if (ag %in% assumed_args || ag %in% seen_arg) next
      if (ag %in% data_names || ag %in% known_names) next
      # A name given to a new column or list element, rather than an argument.
      creation <- paste0(
        "(mutate|summarise|summarize|tibble|data.frame|list|c|rename|table|",
        "aggregate|setNames)\\([^()]*\\b",
        gsub("\\.", "\\\\.", ag), "\\s*="
      )
      if (grepl(creation, code_flat)) next
      # Skip assignments to new objects: those are names, not arguments.
      if (grepl(paste0("^\\s*", gsub("\\.", "\\\\.", ag), "\\s*="), line)) next
      seen_arg <- c(seen_arg, ag)
      if (!grepl(paste0("\\b", gsub("\\.", "\\\\.", ag), "\\b"), prose_text)) {
        problems <- c(problems, sprintf(
          "%s: argument `%s =` is used but never named in the prose", chapter, ag
        ))
      }
    }
  }
}

# The register: what has been introduced, in order.
register <- c(
  "# Register of functions and arguments",
  "",
  "Generated by `tools/check_firstuse.R`. Lists everything the materials",
  "introduce, in the order it first appears.",
  "",
  "## Functions",
  "",
  paste0("`", seen_fun, "()`", collapse = ", "),
  "",
  "## Arguments",
  "",
  paste0("`", seen_arg, " =`", collapse = ", "),
  ""
)
writeLines(register, "REGISTER.md")

cat("Functions introduced:", length(seen_fun), "\n")
cat("Arguments introduced:", length(seen_arg), "\n")
cat("Register written to REGISTER.md\n\n")

if (length(problems) == 0) {
  cat("No unexplained first uses found.\n")
} else {
  cat(length(problems), "possible omission(s):\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
}
