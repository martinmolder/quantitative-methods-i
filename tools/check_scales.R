# Quantitative Methods I -- measurement scale check.
#
# Run from the site directory:  Rscript tools/check_scales.R
#
# Principle 5.2: if the materials state a rule about measurement scales, every
# example has to follow it. The rule here is that an ordinal variable may be
# treated as continuous when it has five or more equally spaced categories.
#
# This finds every variable used where a continuous variable is expected --
# as a mean, a Pearson correlation, or an outcome or predictor in lm() -- and
# reports how many categories it actually has. Anything with fewer than five
# is either a breach of the rule or an exception that has to be argued for
# (principle 5.3), so the output has to be read against the text.

suppressMessages(library(dplyr))

swe <- readRDS("data/swe.rds")

n_categories <- sapply(swe, function(x) {
  if (is.factor(x) || is.character(x)) return(NA_integer_)
  length(unique(x[!is.na(x)]))
})

chapters <- sort(list.files(pattern = "^[0-9]{2}-.*\\.qmd$"))

continuous_use <- c("mean\\(", "sd\\(", "cor\\(", "cor\\.test\\(",
                    "lm\\(", "t\\.test\\(", "scale\\(")

found <- list()

for (chapter in chapters) {

  lines <- readLines(chapter, warn = FALSE)
  fence <- grepl("^```", lines)
  chunk_id <- cumsum(fence)
  hidden <- unique(chunk_id[grepl("include: false", lines, fixed = TRUE)])
  code <- lines[chunk_id %% 2 == 1 & !fence & !(chunk_id %in% hidden)]

  for (line in code) {
    if (!any(sapply(continuous_use, grepl, x = line))) next
    # Spearman is explicitly for ordinal data, so it is not a breach.
    if (grepl("spearman", line, fixed = TRUE)) next
    # mean(is.na(x)) is a proportion missing, not a mean of x.
    if (grepl("is\\.na\\(", line)) next

    for (v in names(n_categories)) {
      if (is.na(n_categories[[v]])) next
      if (n_categories[[v]] >= 5) next
      if (grepl(paste0("\\b", v, "\\b"), line)) {
        found[[length(found) + 1]] <- data.frame(
          chapter = chapter,
          variable = v,
          categories = n_categories[[v]],
          line = trimws(substr(line, 1, 70))
        )
      }
    }
  }
}

if (length(found) == 0) {
  cat("No variable with fewer than five categories is used as continuous.\n")
} else {
  result <- do.call(rbind, found)
  result <- unique(result)
  cat(nrow(result), "use(s) of a short scale as continuous.\n")
  cat("Each needs an exception stated in the text (principle 5.3).\n\n")
  print(result, row.names = FALSE)
}
