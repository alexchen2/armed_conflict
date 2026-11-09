
# Library imports
library(here)
library(tidyverse)
library(readr)

# Import preprocessed data from previous assignments
conflict <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Filter dataset for increases in matmor beyond initial value in year 2000,
# anytime within range of years 2000 to 2017
conflict_diff_matmor <- conflict %>%
  dplyr::select(iso, year, matmor) %>%
  dplyr::filter(year < 2018) %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(diff_matmor = matmor - matmor[1L])

# Fetch unique list of countries satisfying conflict_diff_matmor constraints
countries_with_matmor_incr <- conflict_diff_matmor %>%
  dplyr::filter(diff_matmor > 0 & year == 2017) %>%
  dplyr::select(iso) %>%
  unique()

# Filter conflict df by countries that fit conflict_diff_matmor constraints
conflict_matmor_incr <- conflict_diff_matmor %>%
  dplyr::filter(iso %in% countries_with_matmor_incr$iso)

# Create graph
ggplot(conflict_matmor_incr, mapping = aes(x = year, y = matmor, color = iso)) +
  geom_line() +
  labs(
    title="Maternal Mortality over Time, for Countries Increasing in Mortality from 2000 to 2017",
    color="Country, by ISO 3166"
  ) +
  xlab("Year") + 
  ylab("Maternal Mortality Ratio, per 100,000 live births") +
  scale_x_continuous(
    breaks = seq(
      min(conflict$year), 
      max(conflict$year), 
      2
    )
  ) + 
  scale_y_continuous(
    breaks = seq(
      0, 
      2000, 
      250      
    ),
  ) +
  scale_y_log10()

