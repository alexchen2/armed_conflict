# ~For table:~
# - exposure variable - conflict yes vs no
# - rows: mortality rate outcomes, covariates (disasters)
# - for country-yr observations: each country -> 20 entries (for 20 years); use all years
#     - treat unit of observation as year-country, not just a year; for observing missingness in this assignment
#     - usually would want to use year unit, prevent duplicated entries
# - format as .pdf

# ~For figure:~
# - use up to 2017; 2018 and 2019 -> missingness in mortality rates in dataset

# Library imports
library(here)
library(tidyverse)
library(readr)
library(janitor)
library(table1)
library(labelled)
library(summata)
library(xtable)

# Import preprocessed data from previous assignments
armed_conflict <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Function to determine degree of skewness; if outside (-1.0, 1.0) then use median in table over mean
pearson_skew <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  n_mean <- mean(x)
  n_median <- median(x)
  n_sd <- sd(x) 
  
  return(3 * (n_mean - n_median) / n_sd)
}

# Check skewness of all continuous variables manually
# sapply(
#   armed_conflict %>% select(-c(drought, earthquake, had_conflict, oecd)), 
#   \(x) pearson_skew(x)
# ) %>% dplyr::filter()

# Convert categorical variables into factors, add labels + units
armed_conflict$had_conflict <- factor(
  armed_conflict$had_conflict, 
  levels=c(0, 1),
  labels=c("No", "Yes")
)

armed_conflict$drought <- factor(
  armed_conflict$drought, 
  levels=c(0, 1),
  labels=c("No", "Yes")
)
 
armed_conflict$earthquake <- factor(
  armed_conflict$earthquake, 
  levels=c(0, 1),
  labels=c("No", "Yes")
)

armed_conflict$oecd <- factor(
  armed_conflict$oecd, 
  levels=c(0, 1),
  labels=c("No", "Yes")
)
 
armed_conflict <- armed_conflict %>%
  set_variable_labels(
    matmor = "Maternal Mortality Ratio, per 100,000 live births",
    infmor = "Infant Mortality Ratio, per 100,000 live births",
    neomor = "Neonatal Mortality Ratio, per 100,000 live births",
    un5mor = "Under-five Mortality Ratio, per 100,000 live births",
    drought = "Drought Occurrence",
    earthquake = "Earthquake Occurrence",
    gdp_1000 = "GDP per Capita",
    oecd = "OECD Member",
    pop_dens = "Population Density",
    urban = "Urban Population",
    age_dep = "Age-Dependency Ratio",
    male_edu = "Male Education Ratio",
    temp = "Temperature",
    rainfall_1000 = "Rainfall"
  )

units(armed_conflict$age_dep)  <- "years"
units(armed_conflict$gdp_1000) <- "$1,000s"
units(armed_conflict$rainfall_1000) <- "per 1000 units"
units(armed_conflict$temp) <- "°C"
units(armed_conflict$urban) <- "%"

# caption  <- "Categorical Data formatted as n [%] unless specified otherwise."
footnote <- "Data formatted as n(%) unless specified otherwise."

render.cont <- function(x) {
  var_name <- attr(x, "name")
  stats_rounded <- stats.apply.rounding(stats.default(x), digits = 3)
  skew <- pearson_skew(x)
  
  # Specific cont variables -> median [min, max]
  # var_name %in% c("age", "wt")
  if (skew > 1 | skew < -1) {
    with(stats_rounded, c("", "Median [Min, Max]" = sprintf("%s [%s, %s]", MEDIAN, MIN, MAX)))
  } else {
    with(stats_rounded, c("", "Mean (SD)" = sprintf("%s (%s)", MEAN, SD)))
  }

  # with(stats.apply.rounding(stats.default(x), digits=2), c("",
  #     "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

render.cat <- function(x) {
  round_pct <- \(x) stats.apply.rounding(stats.default(x), digits.pct = 1)
  
  c("", sapply(
    round_pct(x), 
    \(y) with(y, sprintf("%s (%s %%)", FREQ, PCT))
  ))
}

table <- table1(
  ~ matmor + infmor + neomor + un5mor + drought + earthquake + gdp_1000 + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 | had_conflict, 
  data=armed_conflict,
  # caption=caption,
  footnote=footnote,
  render.categorical = render.cat,
  # render.cont = render.cont
)

table

