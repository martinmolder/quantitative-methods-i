# The survey data ships inside the lesson folder, so that the lesson works
# with no internet connection in the seminar.
#
# swirl sources this file without changing the working directory, so the path
# has to be built from swirl's own courses directory rather than assumed.

.get_course_path <- function() {
  tryCatch(
    swirl:::swirl_courses_dir(),
    error = function(c) file.path(find.package("swirl"), "Courses")
  )
}

.path2rds <- file.path(
  .get_course_path(),
  "Quantitative_Methods_I",
  "09_Correlation",
  "swe.rds"
)

swe <- readRDS(.path2rds)
