# Helpers for checking answers in this lesson.

# Whenever swirl is running, its callback is at the top of its call stack.
# Swirl's state, named e, is stored in the environment of the callback.
getState <- function() {
  environment(sys.function(1))$e
}

# The value the user entered, or the value their command produced.
getVal <- function() {
  getState()$val
}

# The last expression the user typed at the console.
getExpr <- function() {
  getState()$expr
}

# swirl grades a command by comparing expressions, and an expression that
# defines a function -- \(x) mean(x, na.rm = TRUE) -- defeats both of its
# matchers: the first evaluates the function's argument x, which does not
# exist outside it, and the fallback identical() fails on the srcref that
# every parsed function carries. So the across() question is graded on what
# the command produced instead: the eight party means, in a one-row table.
like_means_match <- function() {
  e <- get("e", parent.frame())
  swe <- e$snapshot$swe
  expected <- dplyr::summarise(
    swe,
    dplyr::across(dplyr::starts_with("like_"), \(x) mean(x, na.rm = TRUE))
  )
  isTRUE(all.equal(e$val, expected, check.attributes = FALSE))
}
