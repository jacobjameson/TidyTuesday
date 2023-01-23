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

library(png)
library(patchwork)
library(grid)

background <- png::readPNG("alone.png")
w <- matrix(rgb(background[,,1],background[,,2],
                background[,,3], background[,,4] * 0.05),
            nrow=dim(background)[1]) 


# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Pragati Narrow"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  legend.text = element_text(color = "black",  size=15),
  legend.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(color = "black", size = 15),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.1, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  legend.position = 'top',
  text = element_text(color = "black", size = 55),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
legend.title.align=0.5,
  plot.title = element_text(
    color = "black", 
    size = 68, 
    face = "bold",
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    color = "grey10", 
    size = 44,
    lineheight = 1.35,
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey20", 
    size = 18,
    lineheight = 1.2, 
    hjust = 0.5,
    margin = margin(t = 50) 
  ))



# plot --------------------------------------------------------------------


p <- df_episodes_avg %>% 
  ggplot(aes(episode_mod, days_lasted)) +
  geom_hline(
    data = tibble(y = seq(0, 100, by = 10)),
    aes(yintercept = y),
    color = "grey82",
    size = .5
  )



p <- p + 
  geom_segment(
    aes(
      xend = episode_mod,
      yend = avg, 
      color = season, 
      color = after_scale(colorspace::lighten(color, .2))
    )
  )


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
    size = 3.5
  ) + 
  geom_point(
    aes(color = season)
  ) 



p <- p + 
  geom_label(
    aes(
      mid, 
      -10.20,
      label = glue::glue(" SEASON {season} "),
      color = season, 
    ),
    size = 7,
    fill = NA,
    family = "Pragati Narrow",
    fontface = "bold",
    label.padding = unit(.5, "lines"),
    label.r = unit(.25, "lines"), 
    label.size = NA
  ) 

p <- p + 
  scale_y_continuous(
    limits = c(-15, 110),
    breaks = seq(0, 100, by = 20),
  ) +
  scale_color_manual(
    values = c("#9986A5", "#999999", "#E69F00", "#56B4E9", "#009E73",
               "#000000", "#0072B2", "#D55E00", "#CC79A7"),
    guide = FALSE 
  ) +
  labs(caption= str_wrap("Data comes from the TV series Alone, 
                         collected and shared by Dan Oehm. As described in Oehm's blog post, 
                         in the survival TV series ‘Alone’, 10 survivalists are dropped in 
                         an extremely remote area and must fend for themselves. 
                         They aim to last 100 days in the Artic winter, living off the 
                         land through their survival skills, endurance, and mental 
                         fortitude • Visualization by @JacobCJameson", 200),
       subtitle= "A Season-by-Season Breakdown of ALONE Contestant Longevity",
       title="SURVIVING SOLITUDE",
       ylab = 'Days Survived') +
  guides(
    size = guide_bins(
      show.limits = TRUE,
      direction = "horizontal",
      title.position = "top",
      title.hjust = .5)) +
  geom_label(aes(x=129,y=100, label = "Roland Welker made it 100 days"), size=5,
             hjust = 0, vjust = "inward",
             nudge_x = 0.05, nudge_y = 2,
             label.padding = unit(0.2, "lines"),
             label.size = NA, fill='#f5f5f2', color='#0072B2') +
  geom_label(aes(x=20,y=-5, label = "Josh Chavez made it 0 days"), size=5,
             hjust = 0, vjust = "inward",
             nudge_x = 0.05, nudge_y = 2,
             label.padding = unit(0.2, "lines"), 
             label.size = NA, fill='#f5f5f2', color='#9986A5') +
  annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf, rasterGrob(w)) 

p



