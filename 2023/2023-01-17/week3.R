# ------------------------------------------------------------------------
# WEEK 3 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", 
          "lubridate", "ggExtra", "dvmisc", "ggparliament")

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Lato")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-01-17')
art <- tuesdata$artists

# wrangle data ------------------------------------------------------------



# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Lato"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  legend.text = element_text(color = "black",  size=9),
  legend.title=element_text(size=20, color = "#DD8D29"),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
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
  #legend.background = element_rect(fill = "#f5f5f2", color = NA),
  plot.title = element_text(
    color = "#E1AF00", 
    size = 46, 
    face = "bold",
    margin = margin(t = 15)
  ),
  plot.subtitle = element_text(
    color = "grey30", 
    size = 24,
    lineheight = 1.35,
    margin = margin(t = 15)
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey50", 
    size = 15,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 30) 
  ))


art <- art %>%
  filter(artist_race != 'N/A')

art_raceG <- art %>%
  filter(book == 'Gardner') %>%
  group_by(artist_race) %>%
  summarize(n = n())

art_raceJ <- art %>%
  filter(book == 'Janson') %>%
  group_by(artist_race) %>%
  summarize(n = n())


semicircleG <- parliament_data(election_data = art_raceG,
                                 type = "semicircle",
                                 parl_rows = 20,
                                 party_seats = art_raceG$n)

semicircleG$book <- 'Gardner'

semicircleJ <- parliament_data(election_data = art_raceJ,
                               type = "semicircle",
                               parl_rows = 15,
                               party_seats = art_raceJ$n)

semicircleJ$book <- 'Janson'


semicircle <- rbind(semicircleG, semicircleJ)

pal <-  c("#E1BD6D", "#EABE94", "#0B775E", "#35274A", "#F2300F")

ggplot(semicircle, aes(x = x, y = y, colour = artist_race)) +
  geom_parliament_seats() + 
  facet_wrap(~book, ncol=2, scales="free_x")+
  labs(caption= str_wrap("This dataset contains data that was used 
                         for Holland Stam’s thesis work, titled 
                         Quantifying art historical narratives. 
                         The data was collected to assess the 
                         demographic representation of artists 
                         through editions of Janson’s History of Art and 
                         Gardner’s Art Through the Ages, two of the most 
                         popular art history textbooks used in the 
                         American education system. @JacobCJameson", 200),
       subtitle= "An investigation of the racial diversity among artists in Janson's History of Art and Gardner's Art Through the Ages\n",
       title="Missing Voices in Art History Education", fill="") +
  scale_color_manual( 
    values=pal,
    name="Race of Artists",
    guide = guide_legend(keyheight = unit(10, units = "mm"), 
                          keywidth=unit(10, units = "mm"), 
                          label.position = "bottom", 
                          title.position = 'top', nrow=1))





