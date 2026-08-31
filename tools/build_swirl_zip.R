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
