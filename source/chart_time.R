library(dplyr)
library(ggplot2)

source("source/organized_data.R")
incarceration <- load_incarceration_data()

# arranging data to graph, totaling female prison pop by state and averaging it
# for each year
to_graph <- incarceration %>%
  group_by(year) %>%
  group_by(state, .add = TRUE) %>%
  summarise(
    total_aapi_female = sum(aapi_female_prison_pop),
    total_black_female = sum(black_female_prison_pop),
    total_latinx_female = sum(latinx_female_prison_pop),
    total_native_female = sum(native_female_prison_pop),
    total_other_race_female = sum(other_race_female_prison_pop),
    total_white_female = sum(white_female_prison_pop),
    total_female = sum(calculated_female_total)
  ) %>%
  mutate(
    percent_aapi_female = total_aapi_female / total_female,
    percent_black_female = total_black_female / total_female,
    percent_latinx_female = total_latinx_female / total_female,
    percent_native_female = total_native_female / total_female,
    percent_other_race_female = total_other_race_female / total_female,
    percent_white_female = total_white_female / total_female
  ) %>%
  group_by(year) %>%
  summarise(
    avg_aapi_female = mean(percent_aapi_female, na.rm = TRUE),
    avg_black_female = mean(percent_black_female, na.rm = TRUE),
    avg_latinx_female = mean(percent_latinx_female, na.rm = TRUE),
    avg_native_female = mean(percent_native_female, na.rm = TRUE),
    avg_other_race_female = mean(percent_other_race_female, na.rm = TRUE),
    avg_white_female = mean(percent_white_female, na.rm = TRUE),
  )

# creates line graphs for each of the avgs of race. includes labels
avg_pop_by_yr_race <- ggplot(data = to_graph, mapping = aes(x = year, )) +
  geom_line(mapping = aes(y = avg_aapi_female, color = "Asian")) +
  geom_line(mapping = aes(y = avg_black_female, color = "Black")) +
  geom_line(mapping = aes(y = avg_latinx_female, color = "Latinx")) +
  geom_line(mapping = aes(y = avg_native_female, color = "Native")) +
  geom_line(mapping = aes(y = avg_other_race_female, color = "Other Race")) +
  geom_line(mapping = aes(y = avg_white_female, color = "White")) +
  labs(
    title = "Average Female Prison Population by Year and Race",
    subtitle = "Averages percent of female prison population of each state by race",
    x = "Year",
    y = "Percent Population (in decimal)",
    color = "Race"
  )
