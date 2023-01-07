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

rent <- rent.raw %>%
  select(year, beds, price) %>%
  filter(beds <= 5) %>%
  group_by(year, beds) %>%
  summarise(median.price = median(price))

unique(rent$beds)

# Also define the group of countries that are going to be highlighted
highlights <- c("0 bds", "1 bds", "2 bds", "3 bds", " 4bds", "5 bds")
n <- length(highlights)

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

plt <- ggplot(rent) + 
  geom_vline(
    xintercept = seq(2000, 2018, by = 3),
    color = "grey91", 
    size = .6
  ) +
  geom_segment(
    data = tibble(y = seq(-4, 3, by = 1), x1 = 2000, x2 = 2018),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  geom_vline(
    aes(xintercept = year), 
    color = "grey40",
    linetype = "dotted",
    size = .8
  ) +
  geom_line(
    data = rent,
    color = "grey75",
    size = .6,
    alpha = .5
  ) +
  geom_line(
    aes(color = group),
    size = .9
  )
plt
