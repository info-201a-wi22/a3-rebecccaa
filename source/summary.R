library(dplyr)

source("source/organized_data.R")
incarceration <- load_incarceration_data()

# creates a list, then adds each summary factor
summary <- list()
summary$latest_pop <- incarceration %>%
  filter(year == max(year)) %>%
  summarise(total = sum(total_prison_pop)) %>%
  pull(total)
summary$avg_change_women <- incarceration %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  select(year, state, county_name, location, calculated_female_total) %>%
  group_by(location) %>%
  arrange(year, .by_group = TRUE) %>%
  filter(year == max(year) | year == min(year), .preserve = TRUE) %>%
  mutate(range = calculated_female_total - lag(calculated_female_total)) %>%
  pull(range) %>%
  mean(na.rm = TRUE)
summary$avg_change_men <- incarceration %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  select(year, state, county_name, location, calculated_male_total) %>%
  group_by(location) %>%
  arrange(year, .by_group = TRUE) %>%
  filter(year == max(year) | year == min(year), .preserve = TRUE) %>%
  mutate(range = calculated_male_total - lag(calculated_male_total)) %>%
  pull(range) %>%
  mean(na.rm = TRUE)
summary$avg_percent_female <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(percent_female = calculated_female_total / calculated_total) %>%
  summarise(avg = mean(percent_female, na.rm = TRUE)) %>%
  pull(avg) * 100 %>%
    round(digits = 2)
summary$avg_percent_male <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(percent_male = calculated_male_total / calculated_total) %>%
  summarise(avg = mean(percent_male, na.rm = TRUE)) %>%
  pull(avg) * 100 %>%
    round(digits = 2)
