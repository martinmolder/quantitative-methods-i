# Check that the swirl lessons only use code the chapters have shown.
#
# Run from the site directory:  Rscript tools/check_swirl_code.R
#
# A swirl lesson is practice for its chapter, not a second course. So every
# function a student types in lesson N, and every named argument, must
# already appear in the visible code of chapters 1 to N. Hidden chunks do not
# count -- the reader never sees them -- and neither does initLesson.R, which
# the student never sees either.
#
# What the student types is the CorrectAnswer of each cmd_question. Function
# names are taken from the parse tree, so a function passed as an argument
# (tapply(x, g, mean)) is caught as well as one that is called.

library(yaml)

course_dir <- "swirl/Quantitative_Methods_I"
manifest <- readLines(file.path(course_dir, "MANIFEST"))
manifest <- manifest[nzchar(manifest)]

chapters <- sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$"))

creators <- c("c", "list", "data.frame", "summarise", "mutate", "rename")

# Names in a piece of code: functions (called or passed by name) and the
# names of arguments.
code_names <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) NULL)
  if (is.null(exprs)) return(list(fun = character(0), arg = character(0)))
  pd <- getParseData(exprs)
  fun <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]
  # A bare symbol that names a function in base R, passed as a value.
  sym <- pd$text[pd$token == "SYMBOL"]
  passed <- sym[vapply(sym, function(s) {
    exists(s, envir = baseenv()) && is.function(get(s, envir = baseenv()))
  }, logical(1))]
  # A name inside c(), summarise() and the like is a new column or element
  # the student is naming, not an argument they have to have been shown.
  sub <- pd[pd$token == "SYMBOL_SUB", ]
  naming <- vapply(sub$parent, function(call_id) {
    head_id <- pd$id[pd$parent == call_id & pd$token == "expr"][1]
    called <- pd$text[pd$parent == head_id & pd$token == "SYMBOL_FUNCTION_CALL"]
    length(called) == 1 && called %in% creators
  }, logical(1))
  list(
    fun = unique(c(fun, passed)),
    arg = unique(sub$text[!naming])
  )
}

# The visible code of one chapter, chunk by chunk.
chapter_code <- function(chapter) {
  lines <- readLines(chapter, warn = FALSE)
  fence <- grepl("^```", lines)
  chunk_id <- cumsum(fence)
  is_code <- chunk_id %% 2 == 1 & !fence
  hidden <- unique(chunk_id[
    grepl("include: false", lines, fixed = TRUE) |
      grepl("echo: false", lines, fixed = TRUE)
  ])
  keep <- is_code & !(chunk_id %in% hidden) & !grepl("^#\\|", lines)
  split(lines[keep], chunk_id[keep])
}

shown_fun <- character(0)
shown_arg <- character(0)
problems <- character(0)

for (i in seq_along(manifest)) {

  lesson <- manifest[i]
  chapter <- chapters[i]

  for (chunk in chapter_code(chapter)) {
    found <- code_names(paste(chunk, collapse = "\n"))
    shown_fun <- union(shown_fun, found$fun)
    shown_arg <- union(shown_arg, found$arg)
  }

  items <- yaml.load_file(file.path(course_dir, lesson, "lesson.yaml"))

  for (item in items) {
    if (!identical(item$Class, "cmd_question")) next
    answer <- item$CorrectAnswer
    found <- code_names(answer)
    for (fn in setdiff(found$fun, shown_fun)) {
      problems <- c(problems, sprintf(
        "%s: `%s()` is not in the code of chapters 1-%d  [%s]",
        lesson, fn, i, answer
      ))
    }
    for (ag in setdiff(found$arg, shown_arg)) {
      problems <- c(problems, sprintf(
        "%s: argument `%s =` is not in the code of chapters 1-%d  [%s]",
        lesson, ag, i, answer
      ))
    }
  }
}

if (length(problems) == 0) {
  cat("Every function and argument in the lessons is in the chapters.\n")
} else {
  cat(length(problems), "swirl answer(s) use code the chapters have not shown:\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
}
