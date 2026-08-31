# Quantitative Methods I, autumn 2026
#
# Step 1: pull the Sweden (2022) case out of the full CSES Module 6 file and
# keep only the variables that the course uses.
#
# Input:  CSES module 6/cses6.csv   (86 MB, all 18 polities)
# Output: site/data/cses6_swe_raw.csv   original CSES codes, original names
#         site/data/cses6_swe_raw.sav   the same, with value labels
#
# The output of this step is deliberately *raw*: variable names are still
# CSES codes and the missing-value codes (7, 8, 9, 97, 99, 9999 ...) are
# still in place. That is what a real survey file looks like when you
# download it, and sessions 2 and 3 work on exactly that.

library(readr)
library(haven)

cses_file <- "../CSES module 6/cses6.csv"

vars_id <- c(
  "F1004",       # polity and year, e.g. "SWE_2022"
  "F1003_2",     # respondent id within the election study
  "F1006_NAM"    # country name
)

vars_demog <- c(
  "F2001_A",     # age in years
  "F2002",       # gender
  "F2003",       # education, ISCED
  "F2004",       # marital status
  "F2005",       # union membership
  "F2006",       # employment status
  "F2008",       # socio-economic status
  "F2009",       # public or private sector
  "F2010_1",     # household income, quintiles
  "F2012",       # attendance at religious services
  "F2015",       # country of birth
  "F2016",       # was either parent born abroad
  "F2018",       # county of residence
  "F2020"        # rural or urban residence
)

vars_attitude <- c(
  "F3001",       # political interest
  "F3002_1",     # news on public television, days per week
  "F3002_5",     # online news sites, days per week
  "F3002_6_1",   # social media, days per week
  "F3003",       # internal efficacy
  "F3004_1",     # democracy preferable
  "F3004_2",     # courts should stop the government
  "F3004_3",     # strong leader who bends the rules
  "F3004_4",     # representation of women has gone too far
  "F3005_1",     # country better run by business leaders
  "F3005_2",     # country better run by independent experts
  "F3005_3",     # country better run by citizens in referendums
  "F3006",       # how democratic is the country, 0-10
  "F3007_1",     # trust: parliament
  "F3007_2",     # trust: government
  "F3007_3",     # trust: judiciary
  "F3007_4",     # trust: scientists
  "F3007_5",     # trust: political parties
  "F3007_6",     # trust: traditional media
  "F3007_7",     # trust: social media
  "F3008_1",     # government performance in general
  "F3008_2",     # government performance during the pandemic
  "F3009",       # state of the economy
  "F3012_1",     # satisfaction with own vote
  "F3013",       # satisfaction with the variety of choice
  "F3014",       # was the election conducted fairly
  "F3017",       # external efficacy
  "F3022",       # satisfaction with democracy
  "F3026",       # are all groups treated fairly
  "F3027",       # does the system guarantee adequate healthcare
  "F3028_1",     # pandemic and how united society is
  "F3028_2",     # pandemic and the functioning of democracy
  "F3028_3",     # pandemic and personal finances
  "F3028_4"      # has anyone in the household had covid-19
)

# Sweden has eight parties in parliament, and the survey asks about all of
# them. A is the Social Democrats, B the Sweden Democrats, C the Moderates,
# D the Left Party, E the Centre Party, F the Christian Democrats, G the
# Greens and H the Liberals.
vars_parties <- c(
  paste0("F3018_", LETTERS[1:8]),   # like-dislike, party
  paste0("F3019_", LETTERS[1:8]),   # like-dislike, party leader
  paste0("F3020_", LETTERS[1:8])    # left-right placement of the party
)

vars_election <- c(
  "F3010",         # did the respondent cast a ballot
  "F3011_LH_PL",   # vote choice, party list
  "F3011_OUTGOV",  # did the respondent vote for the incumbent
  "F3015_LH",      # did the respondent vote in the previous election
  "F3016_LH_PL",   # vote choice in the previous election
  "F3020_R",       # left-right placement, self
  "F3023_1",       # close to any party
  "F3023_4"        # how close to that party
)

# The Swedish study supplies a demographic weight and nothing else. The
# CSES harmonised weights (F1105_*) and the sample and political weights
# (F1101_1, F1101_3) are all 1.0 for every Swedish respondent.
vars_weight <- c("F1101_2")

vars_keep <- c(
  vars_id,
  vars_demog,
  vars_attitude,
  vars_parties,
  vars_election,
  vars_weight
)

# Three of these columns hold text, the rest hold numbers. "c" means
# character (text) and "d" means double (a number with decimals).
vars_text <- c("F1004", "F1003_2", "F1006_NAM")

spec_keep <- ifelse(vars_keep %in% vars_text, "c", "d")
names(spec_keep) <- vars_keep
spec_keep <- do.call(cols_only, as.list(spec_keep))

cses6 <- read_csv(
  cses_file,
  col_types = spec_keep,
  progress = FALSE
)

swe_raw <- cses6[cses6$F1004 == "SWE_2022", ]

cat("Rows kept:", nrow(swe_raw), "\n")
cat("Columns kept:", ncol(swe_raw), "\n")

stopifnot(nrow(swe_raw) == 2845)
stopifnot(all(vars_keep %in% names(swe_raw)))

write_excel_csv(
  swe_raw,
  "data/cses6_swe_raw.csv",
  na = ""
)

# An SPSS copy as well, so that the course can show what a labelled survey
# file looks like when it is imported with haven.
swe_labelled <- swe_raw

parties <- c(
  A = "Social Democrats",
  B = "Sweden Democrats",
  C = "Moderates",
  D = "Left Party",
  E = "Centre Party",
  F = "Christian Democrats",
  G = "Greens",
  H = "Liberals"
)

var_labels <- c(
  F1004 = "Polity and year",
  F1003_2 = "Respondent ID",
  F1006_NAM = "Country name",
  F2001_A = "Age in years",
  F2002 = "Gender",
  F2003 = "Education (ISCED)",
  F2004 = "Marital status",
  F2005 = "Union membership",
  F2006 = "Employment status",
  F2008 = "Socio-economic status",
  F2009 = "Public or private sector",
  F2010_1 = "Household income, quintile",
  F2012 = "Religious service attendance",
  F2015 = "Country of birth",
  F2016 = "Parent born abroad",
  F2018 = "County of residence",
  F2020 = "Rural or urban residence",
  F3001 = "Political interest",
  F3002_1 = "News on public TV, days per week",
  F3002_5 = "Online news, days per week",
  F3002_6_1 = "Social media, days per week",
  F3003 = "Internal efficacy",
  F3004_1 = "Democracy is preferable",
  F3004_2 = "Courts should stop the government",
  F3004_3 = "Strong leader bends the rules",
  F3004_4 = "Representation of women gone too far",
  F3005_1 = "Better run by business leaders",
  F3005_2 = "Better run by independent experts",
  F3005_3 = "Better run by citizens in referendums",
  F3006 = "How democratic is the country (0-10)",
  F3007_1 = "Trust: parliament",
  F3007_2 = "Trust: government",
  F3007_3 = "Trust: judiciary",
  F3007_4 = "Trust: scientists",
  F3007_5 = "Trust: political parties",
  F3007_6 = "Trust: traditional media",
  F3007_7 = "Trust: social media",
  F3008_1 = "Government performance",
  F3008_2 = "Government performance, pandemic",
  F3009 = "State of the economy",
  F3012_1 = "Satisfaction with own vote",
  F3013 = "Satisfaction with variety of choice",
  F3014 = "Fairness of the election",
  F3017 = "External efficacy",
  F3022 = "Satisfaction with democracy",
  F3026 = "All groups treated fairly",
  F3027 = "System guarantees healthcare",
  F3028_1 = "Pandemic and a united society",
  F3028_2 = "Pandemic and democracy",
  F3028_3 = "Pandemic and personal finances",
  F3028_4 = "Household had covid-19",
  F3010 = "Cast a ballot",
  F3011_LH_PL = "Vote choice, party list",
  F3011_OUTGOV = "Voted for the incumbent",
  F3015_LH = "Voted in the previous election",
  F3016_LH_PL = "Previous vote choice, party list",
  F3020_R = "Left-right: self",
  F3023_1 = "Close to a party",
  F3023_4 = "How close to that party",
  F1101_2 = "Demographic weight"
)

for (letter in names(parties)) {
  party <- parties[[letter]]
  var_labels[paste0("F3018_", letter)] <- paste("Like-dislike:", party)
  var_labels[paste0("F3019_", letter)] <- paste("Like-dislike: leader of the", party)
  var_labels[paste0("F3020_", letter)] <- paste("Left-right:", party)
}

for (v in names(var_labels)) {
  attr(swe_labelled[[v]], "label") <- var_labels[[v]]
}

write_sav(
  swe_labelled,
  "data/cses6_swe_raw.sav"
)

cat("Wrote data/cses6_swe_raw.csv and data/cses6_swe_raw.sav\n")
