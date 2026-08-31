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
  "Regression_Presentation",
  "swe.rds"
)

swe <- readRDS(.path2rds)

# The trust score built in session 10, so that the lesson can start from it.
.trust_items <- c(
  "tr_parl", "tr_govt", "tr_court",
  "tr_sci", "tr_party", "tr_media"
)
swe$trust <- psych::fa(swe[, .trust_items], nfactors = 1, fm = "ml")$scores[, 1]
