# Quantitative Methods I, autumn 2026
#
# Step 2: turn the raw CSES extract into the analysis data set used from
# session 4 onwards.
#
# Input:  site/data/cses6_swe_raw.csv
# Output: site/data/swe.rds        the analysis data set
#         site/data/swe.csv        the same, as plain text
#
# Three things happen here, and they are the whole content of sessions 2
# and 3: missing-value codes become NA, variables get short readable names,
# and scales are turned around so that a larger number always means more of
# whatever the variable is named after.

library(readr)
library(dplyr)

# Swedish county names contain a, a and o with diacritics. If the encoding
# is not pinned down, R may read them correctly but still fail to *compare*
# them correctly, and a comparison that fails gives no error -- it just
# quietly produces the wrong answer. Both of these lines are needed.
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
options(encoding = "UTF-8")

raw <- read_csv(
  "data/cses6_swe_raw.csv",
  col_types = cols(),
  progress = FALSE
)

# Helper 1: set missing-value codes to NA
# CSES marks refusals, don't-knows and missing answers with high numbers
# (7, 8, 9 on a four-point scale; 97, 98, 99 on a 0-10 scale, and so on).
# If these are left in, they are treated as real answers and every mean is
# wrong. `%in%` asks, for each element on its left, whether it appears
# anywhere in the vector on its right, and gives back TRUE or FALSE.
set_na <- function(x, codes) {
  x[x %in% codes] <- NA
  x
}

# Helper 2: reverse a scale
# On many CSES scales 1 is the *most* of something (1 = trust a lot). That
# makes results awkward to read, because a positive coefficient then means
# less trust. `reverse()` flips the scale so that the largest number is the
# most. `lo` and `hi` are the smallest and largest valid values.
reverse <- function(x, lo, hi) {
  (hi + lo) - x
}

# Helper 3: close the gap in a four-point scale
# A few CSES questions are coded 1, 2, 4, 5 with no 3 at all. Left alone,
# the step from 2 to 4 counts double. This squeezes the four answers back
# onto 1, 2, 3, 4 and reverses them at the same time, so that 4 is the most
# positive answer.
close_gap <- function(x) {
  case_when(
    x == 1 ~ 4,
    x == 2 ~ 3,
    x == 4 ~ 2,
    x == 5 ~ 1
  )
}

swe <- tibble(
  id = raw$F1003_2
)

# Demographics

swe$age <- set_na(raw$F2001_A, c(9997, 9998, 9999))

# Gender: CSES codes 0 = male, 1 = female.
swe$female <- set_na(raw$F2002, c(3, 7, 8, 9))

# Education on the ISCED scale, 1 (early childhood) to 9 (doctoral).
swe$educ <- set_na(raw$F2003, c(96, 97, 98, 99))

# A three-category version, because the nine ISCED levels are too fine for
# most tables. case_when() checks conditions in order and gives the value
# after the ~ for the first one that is TRUE.
swe$educ3 <- case_when(
  swe$educ <= 6 ~ "No degree",
  swe$educ == 7 ~ "Bachelor",
  swe$educ >= 8 ~ "Postgraduate"
)
swe$educ3 <- factor(
  swe$educ3,
  levels = c("No degree", "Bachelor", "Postgraduate")
)

swe$marital <- set_na(raw$F2004, c(7, 8, 9))
swe$marital <- factor(
  swe$marital,
  levels = 1:4,
  labels = c("Married", "Widowed", "Divorced", "Single")
)

swe$union <- set_na(raw$F2005, c(7, 8, 9))

swe$ses <- set_na(raw$F2008, c(7, 8, 9))
swe$ses <- factor(
  swe$ses,
  levels = 1:4,
  labels = c("White collar", "Worker", "Farmer", "Self-employed")
)

swe$income <- set_na(raw$F2010_1, c(6, 7, 8, 9))

# Attendance at religious services, 1 = never to 6 = weekly or more.
swe$relig <- set_na(raw$F2012, c(7, 8, 9))

# Born in Sweden. CSES uses the UN country code, and 752 is Sweden.
swe$bornswe <- ifelse(raw$F2015 %in% c(996, 997, 998, 999), NA, as.numeric(raw$F2015 == 752))

swe$foreignpar <- set_na(raw$F2016, c(7, 8, 9))

# Rural or urban, 1 = rural to 4 = large town or city.
swe$urban <- set_na(raw$F2020, c(7, 8, 9))

# County
# F2018 holds a county code. The lookup table lives in its own file, so that
# the session on the tidyverse has something real to join to.
counties <- read_csv(
  "data-raw/counties.csv",
  col_types = cols(),
  progress = FALSE
)

county_match <- match(raw$F2018, counties$code)
swe$county <- counties$county[county_match]

# The three historical lands, ordered from south to north. The order comes
# from the `land_order` column of the lookup file rather than from names
# typed out here, so that the result cannot depend on how this script file
# happens to be encoded.
land_levels <- unique(counties[order(counties$land_order), c("land", "land_order")])
swe$land <- factor(
  counties$land[county_match],
  levels = land_levels$land
)

# Political attitudes
# All of the following are reversed, so that a larger number means more
# interest, more trust, more agreement, and so on.

# 1 = very interested ... 4 = not at all interested, so reversed 1-4.
swe$polint <- set_na(raw$F3001, c(7, 8, 9))
swe$polint <- reverse(swe$polint, 1, 4)

# 1 = strongly agree ... 5 = strongly disagree, reversed to 1-5.
swe$inteff <- set_na(raw$F3003, c(7, 8, 9))
swe$inteff <- reverse(swe$inteff, 1, 5)

agree_items <- c(
  dem_pref = "F3004_1",
  dem_courts = "F3004_2",
  dem_strong = "F3004_3",
  dem_women = "F3004_4",
  run_business = "F3005_1",
  run_experts = "F3005_2",
  run_referenda = "F3005_3"
)

for (new_name in names(agree_items)) {
  old_name <- agree_items[[new_name]]
  values <- set_na(raw[[old_name]], c(7, 8, 9))
  swe[[new_name]] <- reverse(values, 1, 5)
}

# How democratic is the country, already 0 = not at all to 10 = completely.
swe$demlevel <- set_na(raw$F3006, c(97, 98, 99))

# Trust: 1 = trust a lot ... 4 = do not trust at all, reversed to 1-4.
trust_items <- c(
  tr_parl = "F3007_1",
  tr_govt = "F3007_2",
  tr_court = "F3007_3",
  tr_sci = "F3007_4",
  tr_party = "F3007_5",
  tr_media = "F3007_6",
  tr_socmed = "F3007_7"
)

for (new_name in names(trust_items)) {
  old_name <- trust_items[[new_name]]
  values <- set_na(raw[[old_name]], c(7, 8, 9))
  swe[[new_name]] <- reverse(values, 1, 4)
}

# 1 = very good job ... 4 = very bad job, reversed to 1-4.
swe$govperf <- set_na(raw$F3008_1, c(6, 7, 8, 9))
swe$govperf <- reverse(swe$govperf, 1, 4)

swe$govcovid <- set_na(raw$F3008_2, c(6, 7, 8, 9))
swe$govcovid <- reverse(swe$govcovid, 1, 4)

# 1 = much better ... 5 = much worse, reversed to 1-5.
swe$econ <- set_na(raw$F3009, c(7, 8, 9))
swe$econ <- reverse(swe$econ, 1, 5)

# The three satisfaction questions are all coded 1, 2, 4, 5 with no 3.
swe$satvote <- close_gap(set_na(raw$F3012_1, c(7, 8, 9)))
swe$satchoice <- close_gap(set_na(raw$F3013, c(7, 8, 9)))
swe$satdem <- close_gap(set_na(raw$F3022, c(6, 7, 8, 9)))

# 1 = conducted fairly ... 5 = conducted unfairly, reversed to 1-5.
swe$fair <- set_na(raw$F3014, c(7, 8, 9))
swe$fair <- reverse(swe$fair, 1, 5)

# Already 1 = makes no difference to 5 = makes a big difference.
swe$exteff <- set_na(raw$F3017, c(7, 8, 9))

# 1 = very fairly ... 4 = not well at all, reversed to 1-4.
swe$fairgroups <- set_na(raw$F3026, c(7, 8, 9))
swe$fairgroups <- reverse(swe$fairgroups, 1, 4)

swe$health <- set_na(raw$F3027, c(7, 8, 9))
swe$health <- reverse(swe$health, 1, 4)

# 1 = very positively ... 5 = very negatively. Reversed, so that a larger
# number means a more positive effect.
pandemic_items <- c(
  pand_unity = "F3028_1",
  pand_dem = "F3028_2",
  pand_fin = "F3028_3"
)

for (new_name in names(pandemic_items)) {
  old_name <- pandemic_items[[new_name]]
  values <- set_na(raw[[old_name]], c(7, 8, 9))
  swe[[new_name]] <- reverse(values, 1, 5)
}

swe$covid <- set_na(raw$F3028_4, c(7, 8, 9))

# Media use, days per week
swe$news_tv <- set_na(raw$F3002_1, c(97, 98, 99))
swe$news_web <- set_na(raw$F3002_5, c(97, 98, 99))
swe$news_soc <- set_na(raw$F3002_6_1, c(97, 98, 99))

# The parties
# CSES labels the parties A to H in order of their vote share. The Swedish
# abbreviations are more useful, but note the trap: the CSES letter C is the
# Moderates, while the Swedish abbreviation C is the Centre Party.
party_letter <- c(
  S = "A",    # Socialdemokraterna, Social Democrats
  SD = "B",   # Sverigedemokraterna, Sweden Democrats
  M = "C",    # Moderaterna, Moderates
  V = "D",    # Vansterpartiet, Left Party
  C = "E",    # Centerpartiet, Centre Party
  KD = "F",   # Kristdemokraterna, Christian Democrats
  MP = "G",   # Miljopartiet, Greens
  L = "H"     # Liberalerna, Liberals
)

leader_name <- c(
  S = "andersson",
  SD = "akesson",
  M = "kristersson",
  V = "dadgostar",
  C = "loof",
  KD = "busch",
  MP = "stenevi",
  L = "pehrson"
)

# Ratings of the party, 0 = strongly dislike to 10 = strongly like.
# Code 96 means the respondent had not heard of the party.
for (party in names(party_letter)) {
  letter <- party_letter[[party]]
  values <- set_na(raw[[paste0("F3018_", letter)]], c(96, 97, 98, 99))
  swe[[paste0("like_", tolower(party))]] <- values
}

# Ratings of the party leader, on the same scale.
for (party in names(party_letter)) {
  letter <- party_letter[[party]]
  values <- set_na(raw[[paste0("F3019_", letter)]], c(96, 97, 98, 99))
  swe[[paste0("lead_", leader_name[[party]])]] <- values
}

# Where the respondent places the party on the left-right scale,
# 0 = left to 10 = right. Code 95 means they had not heard of the scale.
for (party in names(party_letter)) {
  letter <- party_letter[[party]]
  values <- set_na(raw[[paste0("F3020_", letter)]], c(95, 96, 97, 98, 99))
  swe[[paste0("lr_", tolower(party))]] <- values
}

# Where the respondent places themselves.
swe$lr_self <- set_na(raw$F3020_R, c(95, 96, 97, 98, 99))

# The election

# Turnout. Only 73 respondents say they did not vote, against 2,614 who say
# they did -- a reported turnout of 97%, where the real figure was 84%.
# That is not an error in the data: every one of the 18 CSES studies
# over-reports turnout, because people who do not vote are also less likely
# to fill in a survey about an election, and because some of those who do
# answer say they voted when they did not. Sweden is not even unusual; the
# over-report is 13 points, the same as Denmark and smaller than 12 of the
# other studies. It looks extreme here only because real Swedish turnout is
# already high, which leaves almost no non-voters to count.
#
# The variable is kept because it is worth looking at, but there is not
# enough variation in it to use as an outcome. Session 6 explains why.
swe$turnout <- set_na(raw$F3010, c(97, 98, 99))
swe$turnout[swe$turnout == 93] <- 0

# Vote choice. The CSES party codes for Sweden run 752001 to 752008.
party_code <- c(
  "752001" = "S",
  "752002" = "SD",
  "752003" = "M",
  "752004" = "V",
  "752005" = "C",
  "752006" = "KD",
  "752007" = "MP",
  "752008" = "L"
)

# The parties are put in left-to-right order, because that order means
# something and it makes every table and every plot easier to read.
party_order <- c("V", "MP", "S", "C", "L", "KD", "M", "SD")

swe$vote <- unname(party_code[as.character(raw$F3011_LH_PL)])
swe$vote <- factor(swe$vote, levels = party_order)

swe$prevvote <- unname(party_code[as.character(raw$F3016_LH_PL)])
swe$prevvote <- factor(swe$prevvote, levels = party_order)

# Whether the respondent voted for a different party than last time. Only
# defined for people who voted in both elections.
swe$switched <- ifelse(
  is.na(swe$vote) | is.na(swe$prevvote),
  NA,
  as.numeric(swe$vote != swe$prevvote)
)

# The two blocs that formed around government formation in 2022. Note that
# the Centre Party is counted on the left here: it is a centre-right party,
# but in 2022 it refused to support a government relying on the Sweden
# Democrats. This grouping is about government formation, not ideology.
swe$bloc <- case_when(
  swe$vote %in% c("V", "MP", "S", "C") ~ "Left",
  swe$vote %in% c("L", "KD", "M", "SD") ~ "Right"
)
swe$bloc <- factor(swe$bloc, levels = c("Left", "Right"))

# Binary versions for logistic regression.
swe$vote_sd <- as.numeric(swe$vote == "SD")
swe$vote_right <- as.numeric(swe$bloc == "Right")

swe$vote_inc <- set_na(raw$F3011_OUTGOV, c(999996, 999997, 999998, 999999))

swe$pid <- set_na(raw$F3023_1, c(7, 8, 9))

# 1 = very close ... 3 = not very close, reversed to 1-3. This question was
# only asked of people who said they feel close to a party, so everyone
# else is missing by design.
swe$pid_close <- set_na(raw$F3023_4, c(7, 8, 9))
swe$pid_close <- reverse(swe$pid_close, 1, 3)

# Weight
# Statistics Sweden supplies one weight, built by raking the sample to the
# known population distribution of gender, age and education. It corrects
# for who answered the survey, not for what they said: it does *not* adjust
# for the fact that people over-report having voted. Session 6 uses it.
swe$wt <- raw$F1101_2

# Checks
# Every recoded variable has a range it is not allowed to leave. Checking
# this catches the kind of mistake that otherwise passes silently.
ranges <- list(
  age = c(18, 120),
  female = c(0, 1),
  educ = c(1, 9),
  union = c(0, 1),
  income = c(1, 5),
  relig = c(1, 6),
  bornswe = c(0, 1),
  foreignpar = c(0, 1),
  urban = c(1, 4),
  polint = c(1, 4),
  inteff = c(1, 5),
  demlevel = c(0, 10),
  govperf = c(1, 4),
  govcovid = c(1, 4),
  econ = c(1, 5),
  satvote = c(1, 4),
  satchoice = c(1, 4),
  satdem = c(1, 4),
  fair = c(1, 5),
  exteff = c(1, 5),
  fairgroups = c(1, 4),
  health = c(1, 4),
  covid = c(0, 1),
  news_tv = c(0, 7),
  news_web = c(0, 7),
  news_soc = c(0, 7),
  lr_self = c(0, 10),
  turnout = c(0, 1),
  switched = c(0, 1),
  vote_sd = c(0, 1),
  vote_right = c(0, 1),
  vote_inc = c(0, 1),
  pid = c(0, 1),
  pid_close = c(1, 3),
  wt = c(0.01, 100)
)

for (party in names(party_letter)) {
  ranges[[paste0("like_", tolower(party))]] <- c(0, 10)
  ranges[[paste0("lr_", tolower(party))]] <- c(0, 10)
  ranges[[paste0("lead_", leader_name[[party]])]] <- c(0, 10)
}

for (v in names(trust_items)) ranges[[v]] <- c(1, 4)
for (v in names(agree_items)) ranges[[v]] <- c(1, 5)
for (v in names(pandemic_items)) ranges[[v]] <- c(1, 5)

for (v in names(ranges)) {
  values <- swe[[v]]
  values <- values[!is.na(values)]
  if (min(values) < ranges[[v]][1] || max(values) > ranges[[v]][2]) {
    stop(
      "Variable ", v, " is outside its allowed range: ",
      min(values), " to ", max(values)
    )
  }
}

cat("All range checks passed for", length(ranges), "variables.\n")
cat("Rows:", nrow(swe), " Columns:", ncol(swe), "\n")

saveRDS(swe, "data/swe.rds")
write_excel_csv(swe, "data/swe.csv", na = "")

cat("Wrote data/swe.rds and data/swe.csv\n")
