# ==============================================================================
# CHL5233: Statistical Programming and Computation in Health Data
# Week 3 In-class Assignment
#
# Purpose:
#   1. Reshape four World Bank mortality datasets from wide to long format.
#   2. Construct annual earthquake and drought indicators.
#   3. Construct a one-year-lagged armed-conflict indicator.
#   4. Merge mortality, disaster, conflict, and covariate data.
#   5. Save the final analytical dataset in data/processed.
#
# Run this script from the root directory of the project.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Load packages
# ------------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)


# ------------------------------------------------------------------------------
# 1. Define file paths
# ------------------------------------------------------------------------------

# Folder containing the original CSV files
raw_data_dir <- here("data", "raw")

# Folder where the final analytical dataset will be saved
processed_data_dir <- here("data", "processed")

# Create the processed-data directory if it does not already exist
dir.create(
  processed_data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 2. Create a function for preparing World Bank mortality data
# ------------------------------------------------------------------------------

# All four World Bank mortality datasets have the same structure:
#   - one row per country
#   - one column per year
#   - year columns named X2000, X2001, etc.
#
# This function:
#   1. Reads a mortality CSV file.
#   2. Converts the data from wide to long format.
#   3. Removes the "X" prefix from the year.
#   4. Converts year to an integer.
#   5. Retains only the country, year, and mortality variable.
#   6. Restricts the data to 2000-2019.

prepare_mortality <- function(file_name, outcome_name) {

  mortality_long <- read_csv(
    file.path(raw_data_dir, file_name),
    show_col_types = FALSE
  ) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = outcome_name
    ) |>
    mutate(
      year = as.integer(year)
    ) |>
    filter(
      between(year, 2000L, 2019L)
    ) |>
    select(
      iso,
      year,
      all_of(outcome_name)
    )

  return(mortality_long)
}


# ------------------------------------------------------------------------------
# 3. Prepare the four World Bank mortality datasets
# ------------------------------------------------------------------------------

maternal_mortality <- prepare_mortality(
  file_name = "maternal_mortality.csv",
  outcome_name = "maternal_mortality"
)

infant_mortality <- prepare_mortality(
  file_name = "infant_mortality.csv",
  outcome_name = "infant_mortality"
)

neonatal_mortality <- prepare_mortality(
  file_name = "neonatal_mortality.csv",
  outcome_name = "neonatal_mortality"
)

under5_mortality <- prepare_mortality(
  file_name = "under5_mortality.csv",
  outcome_name = "under5_mortality"
)


# Merge the four mortality datasets by ISO country code and year
mortality_data <- list(
  maternal_mortality,
  infant_mortality,
  neonatal_mortality,
  under5_mortality
) |>
  reduce(
    full_join,
    by = c("iso", "year")
  ) |>
  arrange(
    iso,
    year
  )


# ------------------------------------------------------------------------------
# 4. Prepare the disaster data
# ------------------------------------------------------------------------------

# The disaster data are event-level data. A country may therefore have multiple
# records for the same type of disaster in the same year.
#
# The requested output contains one row per country-year and two indicators:
#   earthquake = 1 if at least one earthquake was recorded; otherwise 0
#   drought    = 1 if at least one drought was recorded; otherwise 0

disaster_data <- read_csv(
  file.path(raw_data_dir, "disaster.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  filter(
    between(year, 2000L, 2019L),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(
    year,
    iso,
    disaster_type
  ) |>
  mutate(
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) |>
  group_by(
    iso,
    year
  ) |>
  summarise(
    # max() collapses multiple records in the same country-year.
    # If at least one qualifying event occurred, the maximum equals 1.
    earthquake = max(earthquake, na.rm = TRUE),
    drought = max(drought, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    iso,
    year
  )


# ------------------------------------------------------------------------------
# 5. Prepare the conflict data
# ------------------------------------------------------------------------------

# Paper definition:
# An armed conflict is present when events cumulatively result in at least
# 25 battle-related deaths within a calendar-conflict year.
#
# Therefore, deaths are first summed separately within each:
#   conflict_id x country x calendar year
#
# The data are then collapsed to the country-year level. A country-year is coded
# as having an armed conflict if at least one distinct conflict reached the
# 25-death threshold.
#
# The conflict exposure is lagged by one year:
#   conflict in year t is aligned with the outcome in year t + 1.

conflict_data <- read_csv(
  file.path(raw_data_dir, "conflict.csv"),
  show_col_types = FALSE
) |>
  group_by(
    conflict_id,
    iso,
    year
  ) |>
  summarise(
    # Preserve missingness if every death estimate in a conflict-year is missing.
    conflict_deaths = if_else(
      all(is.na(best)),
      NA_real_,
      sum(best, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  group_by(
    iso,
    year
  ) |>
  summarise(
    had_conflict = case_when(
      # At least one conflict definitely reached the threshold
      any(conflict_deaths >= 25, na.rm = TRUE) ~ 1L,

      # Conflict status cannot be determined if all totals are missing
      all(is.na(conflict_deaths)) ~ NA_integer_,

      # Observed conflict-year totals were all below the threshold
      TRUE ~ 0L
    ),
    .groups = "drop"
  ) |>
  mutate(
    # Shift exposure year forward to implement the one-year lag
    year = year + 1L
  ) |>
  filter(
    between(year, 2000L, 2019L)
  ) |>
  arrange(
    iso,
    year
  )


# ------------------------------------------------------------------------------
# 6. Prepare the covariate data
# ------------------------------------------------------------------------------

# The covariate dataset already contains one row per country-year and includes
# the other variables needed for the analytical dataset.

covariate_data <- read_csv(
  file.path(raw_data_dir, "covariates.csv"),
  show_col_types = FALSE
) |>
  mutate(
    year = as.integer(year)
  ) |>
  filter(
    between(year, 2000L, 2019L)
  ) |>
  arrange(
    iso,
    year
  )


# ------------------------------------------------------------------------------
# 7. Merge all datasets
# ------------------------------------------------------------------------------

# Use the covariate dataset as the country-year analysis frame, then append:
#   - four mortality outcomes
#   - earthquake and drought indicators
#   - the lagged armed-conflict indicator

final_data <- covariate_data |>
  left_join(
    mortality_data,
    by = c("iso", "year")
  ) |>
  left_join(
    disaster_data,
    by = c("iso", "year")
  ) |>
  left_join(
    conflict_data,
    by = c("iso", "year")
  ) |>
  mutate(
    # After joining event-level datasets to the country-year panel, an unmatched
    # row means that no qualifying event was recorded in the supplied data.
    #
    # Thus, these zeroes should be interpreted as "no qualifying event recorded,"
    # rather than definitive proof that no event occurred.
    earthquake = replace_na(earthquake, 0L),
    drought = replace_na(drought, 0L),
    had_conflict = replace_na(had_conflict, 0L)
  ) |>
  arrange(
    iso,
    year
  )


# ------------------------------------------------------------------------------
# 8. Validate the final dataset
# ------------------------------------------------------------------------------

# Check that each country-year appears no more than once
stopifnot(
  !anyDuplicated(final_data[c("iso", "year")])
)

# Check that all observations are within the requested study period
stopifnot(
  all(final_data$year %in% 2000:2019)
)

# Check that the constructed indicators only contain zeroes and ones
stopifnot(
  all(final_data$earthquake %in% c(0L, 1L)),
  all(final_data$drought %in% c(0L, 1L)),
  all(final_data$had_conflict %in% c(0L, 1L))
)


# ------------------------------------------------------------------------------
# 9. Display a short summary
# ------------------------------------------------------------------------------

cat("Number of rows:", nrow(final_data), "\n")
cat("Number of countries:", n_distinct(final_data$iso), "\n")
cat("First year:", min(final_data$year), "\n")
cat("Last year:", max(final_data$year), "\n")

glimpse(final_data)


# ------------------------------------------------------------------------------
# 10. Write the final analytical dataset
# ------------------------------------------------------------------------------

write_csv(
  final_data,
  file.path(processed_data_dir, "final_data_copilot.csv"),
  na = "NA"
)