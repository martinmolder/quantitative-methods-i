# Check the swirl lessons.
#
# Run from the site directory:  Rscript tools/check_swirl.R
#
# For every lesson it checks that
#   1. the YAML parses;
#   2. every item has the fields its Class requires;
#   3. every CorrectAnswer runs without error, in order, in a clean
#      environment seeded by initLesson.R -- this catches an answer that
#      uses an object the lesson has not created yet;
#   4. every mult_question's CorrectAnswer is one of its AnswerChoices;
#   5. the lesson is roughly the right length for half an hour.

library(yaml)
library(swirl)

# Run with --show to print what each correct answer actually produces. The
# text of a swirl lesson cannot compute its numbers the way the chapters do,
# so any figure quoted in an Output line is typed by hand and has to be
# checked against this.
show_output <- "--show" %in% commandArgs(trailingOnly = TRUE)

course_dir <- "swirl/Quantitative_Methods_I"
manifest <- readLines(file.path(course_dir, "MANIFEST"))
manifest <- manifest[nzchar(manifest)]

problems <- character(0)

note <- function(lesson, msg) {
  problems <<- c(problems, paste0(lesson, ": ", msg))
}

for (lesson in manifest) {

  path <- file.path(course_dir, lesson, "lesson.yaml")

  if (!file.exists(path)) {
    note(lesson, "no lesson.yaml")
    next
  }

  # A colon followed by a space inside an unquoted YAML value makes the
  # parser read it as a nested key. The resulting error names a line number
  # but not the cause, so it is worth naming here.
  lines <- readLines(path, warn = FALSE)
  scalars <- grep("^\\s+(Output|Hint|CorrectAnswer|AnswerChoices):\\s", lines)
  for (i in scalars) {
    value <- sub("^\\s+[A-Za-z]+:\\s+", "", lines[i])
    quoted <- grepl("^['\"]", value)
    if (!quoted && grepl(": ", value, fixed = TRUE)) {
      note(lesson, paste0(
        "line ", i, ": a colon followed by a space in an unquoted value ",
        "will break the YAML parser -- use a dash, or quote the whole value"
      ))
    }
  }

  items <- tryCatch(
    yaml.load_file(path),
    error = function(e) {
      note(lesson, paste("YAML does not parse:", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(items)) next

  classes <- vapply(items, function(x) as.character(x$Class), character(1))

  if (classes[1] != "meta") {
    note(lesson, "first item is not the meta block")
  }

  # required fields
  for (i in seq_along(items)) {
    item <- items[[i]]
    needed <- switch(
      item$Class,
      meta = c("Course", "Lesson", "Author", "Type", "Version"),
      text = "Output",
      cmd_question = c("Output", "CorrectAnswer", "AnswerTests", "Hint"),
      mult_question = c("Output", "CorrectAnswer", "AnswerChoices",
                        "AnswerTests", "Hint"),
      character(0)
    )
    missing_fields <- needed[!needed %in% names(item)]
    if (length(missing_fields) > 0) {
      note(lesson, paste0(
        "item ", i, " (", item$Class, ") is missing: ",
        paste(missing_fields, collapse = ", ")
      ))
    }
  }

  # mult_question answers must be among the choices
  for (i in seq_along(items)) {
    item <- items[[i]]
    if (item$Class != "mult_question") next
    choices <- strsplit(item$AnswerChoices, ";")[[1]]
    choices <- trimws(choices)
    if (!trimws(item$CorrectAnswer) %in% choices) {
      extra <- ""
      if (grepl(";", item$CorrectAnswer, fixed = TRUE)) {
        extra <- " -- the answer contains a semicolon, which is what separates the choices"
      }
      note(lesson, paste0(
        "item ", i, ": CorrectAnswer is not one of the AnswerChoices", extra
      ))
    }
  }

  # every CorrectAnswer must run, in order
  # swirl sources initLesson.R without changing the working directory, and
  # lessons find their data files through swirl's courses directory. Pointing
  # that option at the source tree makes the check exercise the same path
  # logic that will run in the seminar.
  options(swirl_courses_dir = normalizePath("swirl"))

  # swirl loads the packages named in dependson.txt before the lesson runs,
  # so the check has to as well, or every tidyverse verb looks undefined.
  deps_file <- file.path(course_dir, lesson, "dependson.txt")
  if (file.exists(deps_file)) {
    deps <- readLines(deps_file, warn = FALSE)
    deps <- trimws(deps)
    deps <- deps[nzchar(deps)]
    for (dep in deps) {
      ok <- suppressWarnings(suppressMessages(
        require(dep, character.only = TRUE, quietly = TRUE)
      ))
      if (!ok) {
        note(lesson, paste0("dependson.txt names '", dep, "', which is not installed"))
      }
    }
  }

  env <- new.env(parent = globalenv())
  init <- file.path(course_dir, lesson, "initLesson.R")
  if (file.exists(init)) {
    init_error <- tryCatch(
      {
        sys.source(init, envir = env)
        NULL
      },
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
    if (!is.null(init_error)) {
      note(lesson, paste("initLesson.R fails --", init_error))
    }
  }

  for (i in seq_along(items)) {
    item <- items[[i]]
    if (item$Class != "cmd_question") next
    answer <- item$CorrectAnswer
    result <- tryCatch(
      {
        value <- eval(parse(text = answer), envir = env)
        if (show_output) {
          cat("\n--- item ", i, ": ", answer, "\n", sep = "")
          if (!is.null(value)) print(value)
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(result)) {
      note(lesson, paste0("item ", i, ": `", answer, "` fails -- ", result))
    }
  }

  # Each lesson ships its own copy of the data it uses. A copy that has
  # fallen behind data/ makes the lesson run on old numbers without saying
  # so. tools/build_swirl_zip.R refreshes them; this reports the drift.
  shipped <- list.files(
    file.path(course_dir, lesson),
    pattern = "\\.(rds|csv|sav)$"
  )

  for (f in shipped) {

    canonical <- file.path("data", f)
    target <- file.path(course_dir, lesson, f)

    if (!file.exists(canonical)) {
      note(lesson, paste0("ships ", f, ", which is not in data/"))
      next
    }

    stale <- file.size(canonical) != file.size(target) ||
      !identical(
        readBin(canonical, "raw", file.size(canonical)),
        readBin(target, "raw", file.size(target))
      )

    if (stale) {
      note(lesson, paste0(
        "its copy of ", f, " differs from data/", f,
        " -- run tools/build_swirl_zip.R"
      ))
    }
  }

  # length
  n_questions <- sum(classes %in% c("cmd_question", "mult_question"))
  if (n_questions < 15 || n_questions > 30) {
    note(lesson, paste0(
      n_questions, " questions -- aim for 18 to 28 for a 30 minute lesson"
    ))
  }

  cat(sprintf(
    "%-28s %2d items, %2d questions\n",
    lesson, length(items), n_questions
  ))
}

cat("\n")

if (length(problems) == 0) {
  cat("No problems found.\n")
} else {
  cat(length(problems), "problem(s):\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
  quit(status = 1)
}
