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
  "04_Visualisation",
  "swe.rds"
)

swe <- readRDS(.path2rds)

# This lesson is about drawing, not about missing data -- that was sessions 2
# and 3. Left whole, every plot here would carry an NA category and print
# "Removed N rows" into the console, and because swirl prints the plot from
# inside its own task callback the warning arrives as "warning messages from
# top-level task callback 'mini'", which reads like a failure. So the lesson
# works on the respondents who answered all six questions it plots. The
# shapes are the same; the count is 1,953 rather than 2,845, and the lesson
# says so.
swe <- swe[
  stats::complete.cases(swe[, c("age", "like_s", "like_m",
                                "lr_self", "vote", "bloc")]),
]
swe$vote <- droplevels(swe$vote)
swe$bloc <- droplevels(swe$bloc)
