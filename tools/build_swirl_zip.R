# Package the swirl course as the .zip file that goes on Moodle, then test
# that swirl can install it -- into a temporary directory, so that this does
# not touch the swirl installation on this machine.
#
# Run from the site directory:  Rscript tools/build_swirl_zip.R

library(swirl)

course <- "Quantitative_Methods_I"
zip_path <- file.path("data", paste0(course, ".zip"))

# Remove any earlier build, including the " 2.zip" copies macOS leaves behind
# when a file it thinks is in use gets rewritten.
old_builds <- list.files(
  "data",
  pattern = "^Quantitative_Methods_I.*\\.zip$",
  full.names = TRUE
)
if (length(old_builds) > 0) {
  file.remove(old_builds)
}

# Each lesson ships its own copy of whatever data it uses, so that nothing
# needs an internet connection in the seminar. Those copies can fall behind
# the files in data/ whenever the data is rebuilt -- and a stale copy does not
# announce itself, it just makes the lesson run on old numbers.
#
# So refresh them here rather than trusting anyone to remember. Which files a
# lesson needs is worked out from what it already contains, so adding a data
# file to a lesson needs no change to this script.

manifest <- readLines(file.path("swirl", course, "MANIFEST"), warn = FALSE)
manifest <- manifest[nzchar(manifest)]

same_file <- function(a, b) {
  if (file.size(a) != file.size(b)) return(FALSE)
  identical(
    readBin(a, "raw", file.size(a)),
    readBin(b, "raw", file.size(b))
  )
}

refreshed <- 0

for (lesson in manifest) {

  lesson_dir <- file.path("swirl", course, lesson)
  shipped <- list.files(lesson_dir, pattern = "\\.(rds|csv|sav)$")

  for (f in shipped) {

    canonical <- file.path("data", f)

    if (!file.exists(canonical)) {
      stop(
        "Lesson ", lesson, " ships ", f,
        ", but there is no data/", f, " to refresh it from."
      )
    }

    target <- file.path(lesson_dir, f)

    if (!same_file(canonical, target)) {
      file.copy(canonical, target, overwrite = TRUE)
      cat("  refreshed", lesson, "/", f, "\n")
      refreshed <- refreshed + 1
    }
  }
}

if (refreshed == 0) {
  cat("Lesson data was already current.\n")
} else {
  cat("Refreshed", refreshed, "stale lesson data file(s).\n")
}

# The course directory has to sit at the top level of the zip, so the zip is
# built from inside swirl/ rather than from the project root.
old_wd <- getwd()
setwd("swirl")

# -r recurses into the directory, -q keeps it quiet, and -x excludes the
# macOS metadata files that would otherwise be included.
zip(
  zipfile = file.path("..", zip_path),
  files = course,
  flags = "-r9Xq",
  extras = "-x *.DS_Store"
)

setwd(old_wd)

cat("Built", zip_path, "-", file.size(zip_path), "bytes\n")

# test the install path a student would take
test_dir <- file.path(tempdir(), "swirl_test_courses")
dir.create(test_dir, showWarnings = FALSE)
options(swirl_courses_dir = test_dir)

install_course_zip(zip_path)

installed <- list.dirs(test_dir, recursive = FALSE, full.names = FALSE)
cat("Installed into a temporary directory:", installed, "\n")

lessons <- list.dirs(
  file.path(test_dir, course),
  recursive = FALSE,
  full.names = FALSE
)
cat("Lessons found:", paste(lessons, collapse = ", "), "\n")

manifest <- readLines(file.path("swirl", course, "MANIFEST"))
manifest <- manifest[nzchar(manifest)]

missing_lessons <- manifest[!manifest %in% lessons]
if (length(missing_lessons) > 0) {
  stop("In the MANIFEST but not installed: ", paste(missing_lessons, collapse = ", "))
}

unlink(test_dir, recursive = TRUE)

cat("The zip installs correctly.\n")
