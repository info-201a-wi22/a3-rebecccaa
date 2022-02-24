library(dplyr)
library(ggplot2)

source("source/organized_data.R")
incarceration <- load_incarceration_data()

# arranging data to graph, totaling female prison pop by state and averaging it
# for each year
to_graph <- incarceration %>%
  group_by(year) %>%
  group_by(state, .add = TRUE) %>%
  summarise(total_aapi_female = sum(aapi_female_prison_pop),
            total_black_female = sum(black_female_prison_pop),
            total_latinx_female = sum(latinx_female_prison_pop),
            total_native_female = sum(native_female_prison_pop),
            total_other_race_female = sum(other_race_female_prison_pop),
            total_white_female = sum(white_female_prison_pop)
            ) %>%
  group_by(year) %>%
  summarise(avg_aapi_female = mean(total_aapi_female),
            avg_black_female = mean(total_black_female),
            avg_latinx_female = mean(total_latinx_female),
            avg_native_female = mean(total_native_female),
            avg_other_race_female = mean(total_other_race_female),
            avg_white_female = mean(total_white_female),
            )

# creates line graphs for each of the avgs of race. includes labels
avg_pop_by_yr_race <- ggplot(data = to_graph, mapping = aes(x = year,)) +
  geom_line(mapping = aes(y = avg_aapi_female, color = "Asian")) +
  geom_line(mapping = aes(y = avg_black_female, color = "Black")) +
  geom_line(mapping = aes(y = avg_latinx_female, color = "Latinx")) +
  geom_line(mapping = aes(y = avg_native_female, color = "Native")) +
  geom_line(mapping = aes(y = avg_other_race_female, color = "Other Race")) +
  geom_line(mapping = aes(y = avg_white_female, color = "White")) +
  labs (
    title = "Average Female Prison Population by Year and Race",
    subtitle = "Averages total state female prison population over years and race",
    x = "Year",
    y = "Population",
    color = "Race"
  )
