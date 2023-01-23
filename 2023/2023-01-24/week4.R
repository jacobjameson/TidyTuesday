# ------------------------------------------------------------------------
# WEEK 4 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Alone"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", 
          "lubridate", "ggExtra", "dvmisc", "ggparliament")

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Pragati Narrow")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-01-24')

episodes <- tuesdata$episodes
survivalists <- tuesdata$survivalists


# wrangle data ------------------------------------------------------------

episodes <- episodes %>%
  select(season, episode, imdb_rating, n_ratings)

survivalists <- survivalists %>%
  select(season, episode = result, days_lasted, name)

data <- merge(survivalists, episodes, by=c('season', 'episode'))

# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Pragati Narrow"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  legend.text = element_text(color = "black",  size=15),
  legend.title = element_blank(),
  #axis.text.x = element_blank(),
  #axis.text.y = element_blank(),
  strip.text.x = element_text(size = 30),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.1, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  legend.position = 'top',
text = element_text(color = "black", size = 15),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
legend.title.align=0.5,
  plot.title = element_text(
    color = "#E1AF00", 
    size = 48, 
    face = "bold",
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    color = "grey10", 
    size = 24,
    lineheight = 1.35,
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey20", 
    size = 15,
    lineheight = 1.2, 
    hjust = 0.5,
    margin = margin(t = 30) 
  ))



# plot --------------------------------------------------------------------


df_episodes_avg <-
  data %>% 
  arrange(season, episode) %>% 
  mutate(episode_id = row_number()) %>% 
  group_by(season) %>% 
  mutate(
    avg = mean(days_lasted),
    episode_mod = episode_id + (9 * season),
    mid = mean(episode_mod)) %>% 
  ungroup() %>% 
  mutate(season = factor(season))


df_lines <-
  df_episodes_avg %>% 
  group_by(season) %>% 
  summarize(
    start_x = min(episode_mod) - 5,
    end_x = max(episode_mod) + 5,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )

# First, horizontal lines that are used as scale reference. 
# They are added first to ensure they stay in the background.
p <- df_episodes_avg %>% 
  ggplot(aes(episode_mod, days_lasted)) +
  geom_hline(
    data = tibble(y = seq(-10, 110, by = 10)),
    aes(yintercept = y),
    color = "grey82",
    size = .5
  )
p
# Add vertical segments. 
# These represent the deviation of episode's rating from the mean rating of 
# the season they appeared.
p <- p + 
  geom_segment(
    aes(
      xend = episode_mod,
      yend = avg, 
      color = season, 
      color = after_scale(colorspace::lighten(color, .2))
    )
  )

# Add lines and dots.
# These represent the mean rating per season. 
# The dots mark each episode's rating, with its size given by the number of votes.
p <- p + 
  geom_line(
    data = df_lines,
    aes(x, y),
    color = "grey40"
  ) +
  geom_line(
    data = df_lines,
    aes(
      x_group, 
      y, 
      color = season, 
      color = after_scale(colorspace::darken(color, .2))
    ),
    size = 2.5
  ) + 
  geom_point(
    aes(color = season)
  ) 

p

p <- p + 
  geom_label(
    aes(
      mid, 
      -5.20,
      label = glue::glue(" SEASON {season} "),
      color = season, 
      color = after_scale(colorspace::darken(color, .2))
    ),
    fill = NA,
    family = "Pragati Narrow",
    fontface = "bold",
    label.padding = unit(.2, "lines"),
    label.r = unit(.25, "lines"), 
    label.size = .5
  ) 

# Scale and labels customization.
# Override default colors with a much better looking palette.
p <- p + 
  scale_y_continuous(
    expand = c(.03, .03),
    limits = c(-10, 100),
    breaks = seq(0, 100, by = 10),
    sec.axis = dup_axis(name = NULL)
  ) +
  scale_color_manual(
    values = c("#9986A5", "#E1BD6D", "#0B775E", "#35274A", "#F2300F", 
               "#C7B0C1", "#B5C9C9", "#90A8C0", "#A8A890"),
    guide = FALSE 
  ) +
  labs(caption= str_wrap("@JacobCJameson", 183),
       subtitle= "",
       title="ALONE", fill="") +
  guides(
    size = guide_bins(
      show.limits = TRUE,
      direction = "horizontal",
      title.position = "top",
      title.hjust = .5))
p



