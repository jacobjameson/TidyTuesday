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

library(showtext)
font_add_google("Lato")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-07-05')
rent.raw <- tuesdata$rent

# wrangle data ------------------------------------------------------------

rent <- rent.raw %>%
  filter(beds == 2) %>%
  select(year, beds, price, city) %>%
  mutate(city = str_to_title(city)) %>%
  group_by(year, city) %>%
  summarise(price = mean(price)) %>%
  ungroup()

cities.included <- rent %>%
  group_by(city) %>%
  summarise(n = n()) %>%
  filter(n > 16)

cities.included <- unique(cities.included$city)

rent <- rent %>%
  filter(city %in% cities.included)

# Also define the group of cities that are going to be highlighted
highlights <- c("San Francisco", "Berkeley", "Menlo Park", 
                "Palo Alto", " San Jose")

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
      name_lab = if_else(year == 2018, city, NA_character_)) %>% 
  ungroup()


# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Lato"))

theme_update(
  # Remove title for both x and y axes
  axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 17, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_text(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)


# plot --------------------------------------------------------------------

rm(plt)
plt <- ggplot(rent %>% filter(group != "other"), 
              aes(year, price_rel, group = city)) + 
  geom_vline(xintercept = seq(2000, 2018, by = 3),
             color = "grey91", size = .6) +
  geom_segment( data = tibble(y = seq(-1500, 3000, by = 250), 
                              x1 = 2000, x2 = 2018),
                aes(x = x1, xend = x2, y = y, yend = y),
                inherit.aes = FALSE, color = "grey91", size = .6) +
  geom_segment(data = tibble(y = 0, x1 = 2000, x2 = 2018),
               aes(x = x1, xend = x2, y = y, yend = y),
               inherit.aes = FALSE, color = "grey60", size = .8) +
  geom_vline(aes(xintercept = 2008), color = "grey40", linetype = "dotted",
             size = .8) +
  geom_line(data = rent %>% filter(group == "other"), 
            color = "grey75", size = .6, alpha = .5) +
  geom_line(aes(color = city), size = .9)

plt


plt <- plt + 
  annotate("text", x = 2008.15, y = -1000, label = "2008",
           family = "Lato", size = 8, color = "grey40",
           hjust = 0) +
  geom_text_repel(aes(color = group, label = name_lab), fontface = "bold", 
                  size = 8, direction = "y", xlim = c(2018.8, NA),
                  hjust = 0, segment.size = .7, segment.alpha = .5,
                  segment.linetype = "dotted", box.padding = .4,
                  segment.curvature = -0.1, segment.ncp = 3,
                  segment.angle = 20) + 
  coord_cartesian(clip = "off", ylim = c(-1500, 3000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2000, 2021.5), 
                     breaks = seq(2000, 2018, by = 3)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-1500, 3000, by = 500),
                     labels = glue::glue("{format(seq(-1500, 3000, by = 500), nsmall = 2)}$")) 

plt 

plt <- plt + 
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")) +
  labs(
    title = "Compared to the financial crisis in 2008, 2 bedroom prices skyrocket in the Bay",
    subtitle = "The index chart visualizes the price changes (in USD) of a 2 bedroom housing unit in the Bay Area of California based on a 2008 as index year.",
    caption = "Visualization by Jacob Jameson (@JacobCJameson) • 2023 Week 1 of #TidyTuesday, 'Bring your own data from 2022!' (07-05-2022) • Inspired by visualization by Cédric Scherer."
  )
plt
