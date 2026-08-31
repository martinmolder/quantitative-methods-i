# The data files ship inside the lesson folder, so that the lesson works with
# no internet connection in the seminar.
#
# swirl sources this file without changing the working directory, so the path
# has to be built from swirl's own courses directory rather than assumed.

.get_course_path <- function() {
  tryCatch(
    swirl:::swirl_courses_dir(),
    error = function(c) file.path(find.package("swirl"), "Courses")
  )
}

.lesson_path <- file.path(
  .get_course_path(),
  "Quantitative_Methods_I",
  "03_Data_Management_II"
)

swe <- readRDS(file.path(.lesson_path, "swe.rds"))

# read_csv() rather than read.csv(): the county names contain a, a and o with
# diacritics, and read.csv() does not mark the encoding. Unmarked text can
# fail to compare equal to identical text that is marked, with no error, and
# the join in this lesson would then silently lose 1,695 of the 2,845 rows.
counties <- readr::read_csv(
  file.path(.lesson_path, "counties.csv"),
  show_col_types = FALSE
)
counties <- as.data.frame(counties)
