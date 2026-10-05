# ============================================================
# Blog Post 4
# Does Economic Development Narrow the Gender Gap in
# Labor Force Participation?
#
# Data source:
# World Bank, World Development Indicators (WDI)
#
# Main indicators:
# SL.TLF.CACT.FE.ZS  - Female labor force participation rate
# SL.TLF.CACT.MA.ZS  - Male labor force participation rate
# NY.GDP.PCAP.PP.KD  - GDP per capita, PPP, constant 2021
#                     international $
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------
install.packages("WDI")

library(WDI)
library(dplyr)
library(tidyr)
library(readr)


# ------------------------------------------------------------
# 2. Set project folders
# ------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 3. Define indicators and study period
# ------------------------------------------------------------

start_year <- 1990
end_year   <- 2024

indicators <- c(
  female_lfpr = "SL.TLF.CACT.FE.ZS",
  male_lfpr   = "SL.TLF.CACT.MA.ZS",
  gdp_pc      = "NY.GDP.PCAP.PP.KD"
)


# ------------------------------------------------------------
# 4. Download data from the World Bank API
# ------------------------------------------------------------

wdi_raw <- WDI(
  country = "all",
  indicator = indicators,
  start = start_year,
  end = end_year,
  extra = TRUE
)


# ------------------------------------------------------------
# 5. Save raw data
# ------------------------------------------------------------

write_csv(
  wdi_raw,
  "data/raw/wdi_gender_labor_raw.csv"
)


# ------------------------------------------------------------
# 6. Inspect the raw data
# ------------------------------------------------------------

print(head(wdi_raw))
print(names(wdi_raw))

cat("\nNumber of rows:", nrow(wdi_raw), "\n")
cat("Number of countries:", n_distinct(wdi_raw$iso3c), "\n")
cat("Year range:", min(wdi_raw$year), "-", max(wdi_raw$year), "\n")


# ------------------------------------------------------------
# 7. Clean the data
# ------------------------------------------------------------

wdi_clean <- wdi_raw |>
  mutate(
    year = as.integer(year),
    female_lfpr = as.numeric(female_lfpr),
    male_lfpr = as.numeric(male_lfpr),
    gdp_pc = as.numeric(gdp_pc)
  ) |>
  filter(
    !is.na(iso3c),
    region != "Aggregates"
  ) |>
  select(
    iso3c,
    country,
    year,
    region,
    income,
    female_lfpr,
    male_lfpr,
    gdp_pc
  )


# ------------------------------------------------------------
# 8. Create economically meaningful variables
# ------------------------------------------------------------

wdi_clean <- wdi_clean |>
  mutate(
    
    # Gender gap in percentage points
    gender_gap = male_lfpr - female_lfpr,
    
    # Female participation as a percentage of male participation
    female_male_ratio = (female_lfpr / male_lfpr) * 100,
    
    # Log GDP per capita for cross-country comparison
    log_gdp_pc = log(gdp_pc)
  )


# ------------------------------------------------------------
# 9. Remove observations with missing values in the
#    variables needed for analysis
# ------------------------------------------------------------

analysis_data <- wdi_clean |>
  filter(
    !is.na(female_lfpr),
    !is.na(male_lfpr),
    !is.na(gdp_pc),
    is.finite(log_gdp_pc)
  )


# ------------------------------------------------------------
# 10. Check missing values
# ------------------------------------------------------------

missing_summary <- analysis_data |>
  summarise(
    missing_female_lfpr = sum(is.na(female_lfpr)),
    missing_male_lfpr = sum(is.na(male_lfpr)),
    missing_gdp_pc = sum(is.na(gdp_pc)),
    missing_gender_gap = sum(is.na(gender_gap))
  )

print(missing_summary)


# ------------------------------------------------------------
# 11. Save the main processed dataset
# ------------------------------------------------------------

write_csv(
  analysis_data,
  "data/processed/gender_labor_wdi_clean.csv"
)


# ------------------------------------------------------------
# 12. Define countries for the main time-series figures
#
# These countries provide different economic and labor-market
# contexts while keeping the visualization manageable.
# ------------------------------------------------------------

selected_countries <- c(
  "United States",
  "Germany",
  "Japan",
  "United Kingdom",
  "Korea, Rep.",
  "China"
)


# ------------------------------------------------------------
# 13. Create selected-country dataset
# ------------------------------------------------------------

selected_trends <- analysis_data |>
  filter(
    country %in% selected_countries,
    year >= start_year,
    year <= end_year
  )


# ------------------------------------------------------------
# 14. Save selected-country data
# ------------------------------------------------------------

write_csv(
  selected_trends,
  "data/processed/selected_country_trends.csv"
)


# ------------------------------------------------------------
# 15. Create the 2024 cross-sectional dataset
#
# This will be used for the scatterplot:
# log GDP per capita vs. gender labor-force participation gap
# ------------------------------------------------------------

cross_section_2024 <- analysis_data |>
  filter(year == end_year) |>
  arrange(gender_gap)


# ------------------------------------------------------------
# 16. Save 2024 cross-sectional data
# ------------------------------------------------------------

write_csv(
  cross_section_2024,
  "data/processed/cross_section_2024.csv"
)


# ------------------------------------------------------------
# 17. Calculate the change in the gender gap
#    between 1990 and 2024
# ------------------------------------------------------------

gap_change <- analysis_data |>
  filter(
    year %in% c(start_year, end_year)
  ) |>
  select(
    iso3c,
    country,
    year,
    gender_gap
  ) |>
  pivot_wider(
    names_from = year,
    values_from = gender_gap,
    names_prefix = "gap_"
  ) |>
  mutate(
    gap_change = .data[[paste0("gap_", end_year)]] -
      .data[[paste0("gap_", start_year)]]
  ) |>
  arrange(gap_change)


# ------------------------------------------------------------
# 18. Save gap-change data
# ------------------------------------------------------------

write_csv(
  gap_change,
  "data/processed/gender_gap_change_1990_2024.csv"
)


# ------------------------------------------------------------
# 19. Create a selected-country version of the gap change
# ------------------------------------------------------------

selected_gap_change <- gap_change |>
  filter(country %in% selected_countries)


# ------------------------------------------------------------
# 20. Save selected-country gap changes
# ------------------------------------------------------------

write_csv(
  selected_gap_change,
  "data/processed/selected_gap_change_1990_2024.csv"
)


# ------------------------------------------------------------
# 21. Basic quality checks
# ------------------------------------------------------------

cat("\n================ QUALITY CHECKS ================\n")

cat(
  "\nCountries in selected sample:",
  paste(selected_countries, collapse = ", "),
  "\n"
)

cat(
  "\nYears available:",
  min(analysis_data$year),
  "-",
  max(analysis_data$year),
  "\n"
)

cat(
  "\nNumber of observations:",
  nrow(analysis_data),
  "\n"
)

cat(
  "\n2024 observations:",
  nrow(cross_section_2024),
  "\n"
)

cat(
  "\nSelected-country observations:",
  nrow(selected_trends),
  "\n"
)


# Check whether the selected countries are present
country_check <- tibble(
  country = selected_countries
) |>
  mutate(
    available = country %in% unique(analysis_data$country)
  )

print(country_check)


# ------------------------------------------------------------
# 22. Display the final processed data
# ------------------------------------------------------------

print(
  analysis_data |>
    select(
      iso3c,
      country,
      year,
      female_lfpr,
      male_lfpr,
      gender_gap,
      female_male_ratio,
      gdp_pc,
      log_gdp_pc
    ) |>
    head(20)
)

cat("\nData processing completed successfully.\n")