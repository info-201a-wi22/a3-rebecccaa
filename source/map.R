library(dplyr)

source("source/organized_data.R")
incarceration <- load_incarceration_data()

# organizing data to be graphed
states <- read.csv("data/states.csv", stringsAsFactors = FALSE) %>%
  mutate(state = str_to_lower(State)) %>%
  select(state, Code) %>%
  rename(abbrev = Code)
to_graph <- incarceration %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarise(
    total_female = sum(calculated_female_total),
    total = sum(calculated_total)
  ) %>%
  mutate(percent_female = total_female / total) %>%
  select(state, percent_female) %>%
  rename(abbrev = state) %>%
  left_join(states, by = "abbrev") %>%
  select(state, percent_female)
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(to_graph, by = "state")

# setting up theme, from the book
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

# graphing map
percent_female_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = percent_female),
    color = "white",
    size = .1
  ) +
  blank_theme +
  coord_map() +
  scale_fill_continuous(low = "#7A6384", high = "#B85CE0") +
  labs(
    title = "Female Prison Population in each State in 2016",
    subtitle = "Percent (decimal form) female prison populations in each state of US",
    fill = "% Female Prison Population"
  )
