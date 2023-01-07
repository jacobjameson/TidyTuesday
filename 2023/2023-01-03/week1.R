# ------------------------------------------------------------------------
# WEEK 1 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Bring your own data from 2022!"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext")

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-07-05')
rent.raw <- tuesdata$rent

# wrangle data ------------------------------------------------------------

cities.2008 <- rent.raw %>%
  mutate(city = str_to_title(city)) %>%
  filter(year == 2008)

cities.total <- rent.raw %>%
  mutate(city = str_to_title(city))

cities.2008 <- unique(cities.2008$city)
total.cities <- unique(cities.total$city)

cities.drop <- !(total.cities %in% cities.2008)

total.cities[cities.drop]
  

rent <- rent.raw %>%
  filter(beds == 2) %>%
  select(year, beds, price, city) %>%
  mutate(city = str_to_title(city)) %>%
  group_by(year, city) %>%
  summarise(price = mean(price)) %>%
  ungroup()


length(unique(rent$city))


# Also define the group of cities that are going to be highlighted
highlights <- c("San Francisco", "Berkeley", "Santa Cruz", 
                "Watsonville", " Palo Alto", "Half Moon Bay", "Oakland")

n <- length(highlights)


rent <- rent %>% 
  group_by(city) %>%
  mutate(ref_year = 2008,
         price_index = price[which(year == 2008)],
         price_rel = price - price_index,
    group = if_else(city %in% highlights, city, "other"),
    group = as.factor(group)) %>% 
  mutate(
    group = fct_relevel(group, "other", after = Inf),
    name_lab = if_else(year == 2018, name, NA_character_)) %>% 
  ungroup()


rent %>% filter(city == "Ben Lomond")

# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Lato"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 17, margin = margin(r = 5)),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40)
  ),
  legend.position = "none"
)

# plot --------------------------------------------------------------------


plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  rent, 
  aes(year, median.price, group = city)
) + 
  # Geometric annotations that play the role of grid lines
  geom_vline(
    xintercept = seq(2000, 2020, by = 5),
    color = "grey91", 
    size = .6
  ) +
  geom_segment(
    data = tibble(y = seq(-4, 3, by = 1), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  geom_segment(
    data = tibble(y = 0, x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .8
  ) +
  geom_vline(
    aes(xintercept = 2008), 
    color = "grey40",
    linetype = "dotted",
    size = .8
  ) +
  ## Lines for the non-highlighted countries
  geom_line(
    data = rent,
    color = "grey75",
    size = .6,
    alpha = .5
  ) +
  ## Lines for the highlighted countries.
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = city),
    size = .9
  )
plt
