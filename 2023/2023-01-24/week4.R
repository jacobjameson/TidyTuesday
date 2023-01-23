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

font_add_google("Playfair Display")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-01-24')

survivalists <- tuesdata$survivalists
loadouts <- tuesdata$loadouts
episodes <- tuesdata$episodes
seasons <- tuesdata$seasons


# wrangle data ------------------------------------------------------------



# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Playfair Display"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  legend.text = element_text(color = "black",  size=15),
  legend.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  strip.text.x = element_text(size = 30),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.1, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  legend.position = 'top',
text = element_text(color = "black"),
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

pal <-  c("#9986A5", "#E1BD6D", "#0B775E", "#35274A", "#F2300F")


# plot --------------------------------------------------------------------


ggplot(semicircle, aes(x = x, y = y, colour = artist_race)) +
  geom_parliament_seats() + 
  facet_wrap(~book, ncol=2, scales="free_x", strip.position="bottom")+
  labs(caption= str_wrap("This dataset contains data that was used 
                         for Holland Stam’s thesis work, titled 
                         Quantifying art historical narratives. 
                         The data was collected to assess the 
                         demographic representation of artists 
                         through editions of Janson’s History of Art and 
                         Gardner’s Art Through the Ages, two of the most 
                         popular art history textbooks used in the 
                         American education system. @JacobCJameson", 183),
       subtitle= "Racial diversity among artists featured in Janson's History of Art and Gardner's Art Through the Ages",
       title="The Art of Exclusion", fill="") +
  scale_color_manual( 
    values=pal,
    name="Race of Artists",
    guide = guide_legend(keyheight = unit(10, units = "mm"), 
                          keywidth=unit(60, units = "mm"), 
                          label.position = "bottom", 
                          title.position = 'top', nrow=1)) 

