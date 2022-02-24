library(dplyr)

# this function reads the csv and selects and filters down the data to show each
# county and their prison populations by race and gender
load_incarceration_data <- function() {
  read.csv("data/incarceration_trends.csv",
           stringsAsFactors = FALSE
           ) %>%
    select(year, state, county_name, fips, total_pop, urbanicity, region,
           total_prison_pop, aapi_female_prison_pop, aapi_male_prison_pop,
           black_female_prison_pop, black_male_prison_pop,
           latinx_female_prison_pop, latinx_male_prison_pop,
           native_female_prison_pop, native_male_prison_pop,
           other_race_female_prison_pop, other_race_male_prison_pop,
           white_female_prison_pop, white_male_prison_pop
           ) %>%
    filter(!is.na(aapi_female_prison_pop),
           !is.na(aapi_male_prison_pop),
           !is.na(black_female_prison_pop),
           !is.na(black_male_prison_pop),
           !is.na(latinx_female_prison_pop),
           !is.na(latinx_male_prison_pop),
           !is.na(native_female_prison_pop),
           !is.na(native_male_prison_pop),
           !is.na(other_race_female_prison_pop),
           !is.na(other_race_male_prison_pop),
           !is.na(white_female_prison_pop),
           !is.na(white_male_prison_pop)
           ) %>%
  mutate(calculated_total = aapi_female_prison_pop + aapi_male_prison_pop +
           black_female_prison_pop + black_male_prison_pop +
           latinx_female_prison_pop + latinx_male_prison_pop +
           native_female_prison_pop + native_male_prison_pop +
           other_race_female_prison_pop + other_race_male_prison_pop +
           white_female_prison_pop + white_male_prison_pop,
         calculated_female_total = aapi_female_prison_pop +
           black_female_prison_pop + latinx_female_prison_pop + 
           native_female_prison_pop + other_race_female_prison_pop +
           white_female_prison_pop,
         calculated_male_total = aapi_male_prison_pop + black_male_prison_pop
           + latinx_male_prison_pop + native_male_prison_pop +
           other_race_male_prison_pop + white_male_prison_pop
         )
}

# # checking for inconsistencies
# inconsistencies <- incarceration %>%
#   filter(total_prison_pop != calculated_total) %>%
#   mutate(error = total_prison_pop - calculated_total) %>%
#   select(county_name, total_prison_pop, calculated_total, error)
# 
# nrow(inconsistencies) # 44 inconsistencies
# mean(inconsistencies$error) # avg error of 2.5, all have lower calc total
