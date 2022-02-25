library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

source("source/organized_data.R")
incarceration <- load_incarceration_data()

# totaling 2016 incarcerated women by race
women <- incarceration %>%
  filter(year == max(year)) %>%
  summarise(
    total_aapi_female = sum(aapi_female_prison_pop),
    total_black_female = sum(black_female_prison_pop),
    total_latinx_female = sum(latinx_female_prison_pop),
    total_native_female = sum(native_female_prison_pop),
    total_other_race_female = sum(other_race_female_prison_pop),
    total_white_female = sum(white_female_prison_pop)
  ) %>%
  gather(
    key = type,
    value = female
  ) %>%
  mutate(race = substr(type, 7, str_locate(type, "_female") - 1))
# totaling 2016 incarcerated women by race
men <- incarceration %>%
  filter(year == max(year)) %>%
  summarise(
    total_aapi_male = sum(aapi_male_prison_pop),
    total_black_male = sum(black_male_prison_pop),
    total_latinx_male = sum(latinx_male_prison_pop),
    total_native_male = sum(native_male_prison_pop),
    total_other_race_male = sum(other_race_male_prison_pop),
    total_white_male = sum(white_male_prison_pop)
  ) %>%
  gather(
    key = type,
    value = male
  ) %>%
  mutate(race = substr(type, 7, str_locate(type, "_male") - 1))
# combine and gather to prepare for graphing
to_graph <- full_join(women, men, by = "race") %>%
  select(race, female, male) %>%
  gather(
    key = gender,
    value = pop,
    -race
  )

# graphing
total_gender_race <- ggplot(data = to_graph) +
  geom_col(mapping = aes(x = race, y = pop, fill = gender)) +
  labs(
    title = "Prison Populations by Race and Gender in 2016",
    subtitle = "Totals prison populations across the US in 2016",
    x = "Race",
    y = "Population",
    fill = "Gender"
  )
