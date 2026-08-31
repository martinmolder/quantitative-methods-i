# Quantitative Methods I, autumn 2026
#
# Build the small country-level data set used in session 4.
#
# Source: World Bank Open Data API (no key needed).
# Output: site/data/world.csv
#
# Session 4 is about what you can say about a relationship before you look at
# the data - where the limits of the two scales are, which region is
# conceptually impossible, and what shape is therefore available. That needs
# variables with real limits, which is what these are.

library(jsonlite)
library(dplyr)
library(readr)

# The indicators, and the short names they get here.
indicators <- c(
  lit_f = "SE.ADT.LITR.FE.ZS",   # adult literacy rate, female, %
  lit_m = "SE.ADT.LITR.MA.ZS",   # adult literacy rate, male, %
  gdp = "NY.GDP.PCAP.PP.KD",     # GDP per capita, PPP, constant 2021 int$
  life = "SP.DYN.LE00.IN",       # life expectancy at birth, years
  women = "SG.GEN.PARL.ZS",      # seats held by women in parliament, %
  pop = "SP.POP.TOTL"            # total population
)

# Ask for one indicator, all countries, over a range of years, and keep the
# most recent non-missing value for each country. Literacy in particular is
# measured only every few years and not in the same year everywhere.
fetch_indicator <- function(code, first_year = 2015, last_year = 2024) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/", code,
    "?format=json&per_page=20000",
    "&date=", first_year, ":", last_year
  )

  raw <- fromJSON(url, flatten = TRUE)
  values <- raw[[2]]

  values <- values[!is.na(values$value), ]
  values$year <- as.integer(values$date)

  values |>
    select(
      iso3 = countryiso3code,
      country = country.value,
      year,
      value
    ) |>
    arrange(iso3, desc(year)) |>
    group_by(iso3) |>
    slice(1) |>
    ungroup()
}

# The API returns regional and income-group aggregates alongside real
# countries. The metadata endpoint marks an aggregate by giving its region
# the value "Aggregates", which is how they get removed.
meta <- fromJSON(
  "https://api.worldbank.org/v2/country?format=json&per_page=400",
  flatten = TRUE
)[[2]]

countries_meta <- meta |>
  filter(region.value != "Aggregates") |>
  select(
    iso3 = id,
    country = name,
    region = region.value,
    income = incomeLevel.value
  )

cat("Real countries in the metadata:", nrow(countries_meta), "\n")

countries <- countries_meta

for (short_name in names(indicators)) {
  code <- indicators[[short_name]]
  cat("Fetching", short_name, "-", code, "\n")

  one <- fetch_indicator(code)
  one <- one |>
    select(iso3, value, year)

  names(one) <- c("iso3", short_name, paste0(short_name, "_year"))

  countries <- left_join(countries, one, by = "iso3")
}

# Keep the countries that have a literacy figure, since that is the pair the
# session is built around. Everything else is kept as it comes.
countries <- countries |>
  filter(!is.na(lit_f), !is.na(lit_m))

# Round to a sensible number of digits. Percentages to one decimal place,
# GDP to whole dollars, population to whole people.
countries <- countries |>
  mutate(
    lit_f = round(lit_f, 1),
    lit_m = round(lit_m, 1),
    life = round(life, 1),
    women = round(women, 1),
    gdp = round(gdp),
    pop = round(pop)
  ) |>
  select(
    iso3, country, region, income,
    lit_f, lit_m, lit_year = lit_f_year,
    gdp, life, women, pop
  ) |>
  arrange(country)

# Checks. Every one of these is a limit of the scale, and a value outside it
# would mean the download or the join went wrong.
stopifnot(
  min(countries$lit_f) >= 0, max(countries$lit_f) <= 100,
  min(countries$lit_m) >= 0, max(countries$lit_m) <= 100,
  min(countries$life, na.rm = TRUE) >= 0,
  max(countries$life, na.rm = TRUE) <= 120,
  min(countries$women, na.rm = TRUE) >= 0,
  max(countries$women, na.rm = TRUE) <= 100,
  min(countries$gdp, na.rm = TRUE) > 0,
  !any(duplicated(countries$iso3))
)

cat("All range checks passed.\n")
cat("Countries kept:", nrow(countries), "\n")
cat("Literacy years:", min(countries$lit_year), "to", max(countries$lit_year), "\n")

write_excel_csv(countries, "data/world.csv")

cat("Wrote data/world.csv\n")
