# Step 3: check that the codebook matches the data set, then publish it.
#
# The codebook is written by hand, so it can drift out of step with the
# build script. This check makes drift impossible to miss: every column of
# usa.rds must appear in the codebook, and every codebook row must
# correspond to a real column.

library(readr)

swe <- readRDS("data/swe.rds")

codebook <- read_csv(
  "data-raw/codebook.csv",
  col_types = cols(.default = col_character()),
  progress = FALSE
)

in_data_only <- setdiff(names(swe), codebook$variable)
in_book_only <- setdiff(codebook$variable, names(swe))

if (length(in_data_only) > 0) {
  stop("In the data but not in the codebook: ", paste(in_data_only, collapse = ", "))
}

if (length(in_book_only) > 0) {
  stop("In the codebook but not in the data: ", paste(in_book_only, collapse = ", "))
}

# Put the codebook in the same order as the columns of the data set.
codebook <- codebook[match(names(swe), codebook$variable), ]

write_excel_csv(codebook, "data/codebook.csv")

cat("Codebook matches the data:", nrow(codebook), "variables.\n")
