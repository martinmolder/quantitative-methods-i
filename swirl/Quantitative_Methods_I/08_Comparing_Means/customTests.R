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
