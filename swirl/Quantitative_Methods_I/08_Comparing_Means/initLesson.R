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
  "08_Comparing_Means",
  "swe.rds"
)

swe <- readRDS(.path2rds)

# Gender is stored as 0/1. Tables read better with words in them.
swe$sex <- factor(
  swe$female,
  levels = c(0, 1),
  labels = c("Men", "Women")
)

# The same people measured twice: hypothetical anxiety scores from Field,
# Miles and Field (2012), Discovering Statistics Using R, chapter 9.
spider <- data.frame(
  picture = c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45, 40, 50),
  real = c(40, 35, 50, 55, 65, 55, 50, 35, 30, 50, 60, 39)
)
