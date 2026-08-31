# The country data ships inside the lesson folder, so that the lesson works
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

.path2csv <- file.path(
  .get_course_path(),
  "Quantitative_Methods_I",
  "04_Logical_And_Visual",
  "world.csv"
)

# read_csv() rather than read.csv(): country names contain letters outside
# plain English, and read.csv() does not record the encoding.
world <- readr::read_csv(.path2csv, show_col_types = FALSE)
world <- as.data.frame(world)
