# Step 4: reported turnout against real turnout, for all 18 CSES studies.
#
# Output: site/data/turnout.csv
#
# A small data set used in session 6 to show what a *systematic* error looks
# like, as opposed to a random one. Every study in the module over-reports.
# It is also a convenient size for practising plots in session 5.

library(readr)
library(dplyr)

cses6 <- read_csv(
  "../CSES module 6/cses6.csv",
  col_types = cols_only(
    F1004 = col_character(),
    F1006_NAM = col_character(),
    F3010 = col_double()
  ),
  progress = FALSE
)

# Official turnout in each election, as a percentage of registered voters.
# Sources: national electoral commissions and IDEA.
official <- tribble(
  ~study,      ~country,           ~year, ~official, ~compulsory,
  "AUS_2022",  "Australia",         2022,      89.8,  TRUE,
  "AUT_2024",  "Austria",           2024,      77.3,  FALSE,
  "BRA_2022",  "Brazil",            2022,      79.4,  TRUE,
  "DNK_2022",  "Denmark",           2022,      84.2,  FALSE,
  "FRA_2022",  "France",            2022,      73.7,  FALSE,
  "MNE_2023",  "Montenegro",        2023,      56.4,  FALSE,
  "NZL_2023",  "New Zealand",       2023,      78.2,  FALSE,
  "MKD_2024",  "North Macedonia",   2024,      51.7,  FALSE,
  "POL_2023",  "Poland",            2023,      74.4,  FALSE,
  "PRT_2022",  "Portugal",          2022,      51.4,  FALSE,
  "PRT_2024",  "Portugal",          2024,      59.9,  FALSE,
  "SVK_2023",  "Slovakia",          2023,      68.5,  FALSE,
  "SVN_2022",  "Slovenia",          2022,      70.9,  FALSE,
  "SWE_2022",  "Sweden",            2022,      84.2,  FALSE,
  "CHE_2023",  "Switzerland",       2023,      46.7,  FALSE,
  "TWN_2024",  "Taiwan",            2024,      71.9,  FALSE,
  "TUR_2023",  "Turkiye",           2023,      88.8,  TRUE,
  "USA_2024",  "United States",     2024,      63.9,  FALSE
)

# In F3010, 1 means the respondent cast a ballot and 0 means they did not.
# Code 93 is a volunteered "I am not registered", which is counted here as
# not voting. Everything else is missing.
counted <- cses6
counted$voted <- ifelse(counted$F3010 == 1, 1, NA)
counted$voted <- ifelse(counted$F3010 %in% c(0, 93), 0, counted$voted)

reported <- counted |>
  filter(!is.na(voted)) |>
  group_by(study = F1004) |>
  summarise(
    n = n(),
    voters = sum(voted),
    reported = 100 * mean(voted)
  )

turnout <- official |>
  left_join(reported, by = "study") |>
  mutate(gap = reported - official) |>
  select(study, country, year, compulsory, n, reported, official, gap) |>
  arrange(desc(gap))

turnout$reported <- round(turnout$reported, 1)
turnout$gap <- round(turnout$gap, 1)

# Every study should over-report. If one ever does not, that is worth
# knowing about rather than passing over in silence.
if (any(turnout$gap <= 0)) {
  warning("Not every study over-reports turnout any more. Check the table.")
}

print(as.data.frame(turnout), row.names = FALSE)

write_excel_csv(turnout, "data/turnout.csv")

cat("\nWrote data/turnout.csv\n")
