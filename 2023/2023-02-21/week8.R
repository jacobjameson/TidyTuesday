# ------------------------------------------------------------------------
# WEEK 8 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Bob Ross"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "tidytuesdayR", "aRt", 'showtext',
          'ggstream')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Caveat Brush")
showtext_auto()


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-02-21')
bob_ross <- tuesdata$bob_ross

# wrangle data ------------------------------------------------------------

bob_ross$episode_num <- seq.int(nrow(bob_ross))

bob_ross_colors <- bob_ross %>% 
  select(episode_num, colors, season) %>%
  separate_rows(colors, sep = ",\\s*|\\[|\\]") %>%
  filter(colors != '')

# Remove quotation marks around the color names
bob_ross_colors$colors <- gsub("'", "", bob_ross_colors$colors)


bob_ross_hex <- bob_ross %>% 
  select(episode_num, color_hex, season) %>%
  separate_rows(color_hex, sep = ",\\s*|\\[|\\]") %>%
  filter(color_hex != '')

# Remove quotation marks around the color hex
bob_ross_hex$color_hex <- gsub("'", "", bob_ross_hex$color_hex)

# Add column
bob_ross_colors$color_hex <- bob_ross_hex$color_hex 

bob_ross_colors <- bob_ross_colors %>%
  group_by(season, color_hex) %>%
  summarize(freq = n()) %>%
  ungroup()



# plot --------------------------------------------------------------------

streams(bg_col = "black", line_col = "white", 
        fill_col = unique(bob_ross_colors$color_hex), type = "right", s = 1) +
labs(title = "The Unique Color Pallette of Bob Ross",
       subtitle = "@JacobCJameson | #TidyTuesday",
       x = "",
       y = "") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_rect(fill = "black", colour = "black"),
        strip.text = element_text(family = "Caveat Brush", colour = "white", size = 11, lineheight = 0.5),
        axis.text = element_blank(),
        plot.title = element_text(family = "Caveat Brush", colour = "white",
                                  size = 40, hjust = 0.5, face = "bold",
                                  margin = margin(t = 20, b = 20)),
        plot.subtitle = element_text(family = "Caveat Brush", colour = "white",
                                     size = 24, hjust = 0.5,
                                     margin = margin(b = 20)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))





ggplot(bob_ross_colors, aes(x = season, y = freq, 
                            fill = color_hex)) +
  geom_stream(type = 'proportional') +
  labs(title = "The Unique Color Pallette of Bob Ross",
       subtitle = "@JacobCJameson | #TidyTuesday",
       x = "Season",
       y = "") +  scale_fill_manual(values = unique(bob_ross_colors$color_hex)) +
  geom_vline(xintercept = unique(bob_ross_colors$season), color = 'white',
             linetype = 'dashed', alpha = 0.3) +
  scale_x_continuous(limits=c(1,31), expand=c(0,0), breaks = 1:31) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", color = NA), 
        panel.background = element_rect(fill = "black", color = NA), 
        legend.background = element_rect(fill = "black", color = NA), 
        panel.grid.major = element_blank(),
        rect = element_rect(fill = "black", color = "black"),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(), 
        axis.title.x = element_text(size = 10, color = "white", 
                                    face = "bold"),
        axis.text = element_text(size = 10, color = "white", 
                                 face = "bold"),
        axis.ticks.length = unit(3, "pt"),
        axis.ticks = element_line(color = "white"),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_rect(fill = "black", colour = "black"),
        strip.text = element_text(family = "Caveat Brush", colour = "white", size = 11, lineheight = 0.5),
        plot.title = element_text(family = "Caveat Brush", colour = "white",
                                  size = 40, hjust = 0.5, face = "bold",
                                  margin = margin(t = 20, b = 20)),
        plot.subtitle = element_text(family = "Caveat Brush", colour = "white",
                                     size = 24, hjust = 0.5,
                                     margin = margin(b = 20)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))













