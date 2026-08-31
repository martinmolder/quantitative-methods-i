# Quantitative Methods I -- run every check.
#
# Run from the site directory:  Rscript tools/check_all.R
#
# This is the technical check from section 10 of the guidance, as far as it
# can be automated. What it cannot do is judge whether an explanation is any
# good, or whether a conclusion in the text matches the output it sits next
# to -- those still have to be read.

checks <- c(
  "check_firstuse.R",
  "check_numbers.R",
  "check_style.R",
  "check_scales.R",
  "check_balance.R",
  "check_encoding.R",
  "check_swirl.R",
  "check_slides.R"
)

for (check in checks) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(check, "\n")
  cat(strrep("=", 70), "\n")
  result <- system2("Rscript", file.path("tools", check), stdout = TRUE, stderr = TRUE)
  cat(paste(result, collapse = "\n"), "\n")
}

cat("\n")
cat(strrep("=", 70), "\n")
cat("Still to be done by reading:\n")
cat("  - does every conclusion in the text match the output beside it?\n")
cat("  - is every explanation actually an explanation?\n")
cat("  - render from a clean environment on another machine\n")
cat(strrep("=", 70), "\n")
