# ------------------------------------------------------------------------
# WEEK 5 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Cats"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", "packcircles", 
          "lubridate", "ggExtra", "geosphere", "data.table",
          "viridis")

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Indie Flower")
showtext_auto()


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-01-31')

cats_uk <- tuesdata$cats_uk
cats_uk_reference <- tuesdata$cats_uk_reference


# wrangle data ------------------------------------------------------------

cats <- setDT(cats_uk)

findCentroid <- function(location_long, location_lat, ...){
  centroid(cbind(location_long, location_lat), ...)
}

cats[, c("cent_long", "cent_lat") := as.list(
  findCentroid(location_long, location_lat)),
  by = tag_id]

cats$distance <- distHaversine(cats[,c('location_lat','location_long')], 
                               cats[,c('cent_lat','cent_long')])

cats <- data.frame(cats) %>%
  group_by(tag_id) %>%
  summarise(diameter = max(distance)*2,
            area = 3.1459*(diameter/2)**2)


cats <- merge(cats, select(cats_uk_reference, c('tag_id', 'animal_id')), 
              on='tag_id') %>%
  select(animal_id, diameter, area)

cats$animal_id <- ifelse(cats$animal_id == 'Gracie_2', 'Gracie', cats$animal_id)
cats$animal_id <- ifelse(cats$animal_id == 'Dexter2', 'Dexter', cats$animal_id)

cats <- cats %>%
  mutate(animal_id = ifelse(diameter < 8000, '', animal_id))

packing <- circleProgressiveLayout(cats$area, sizetype='area')
packing$radius <-packing$radius

data <- cbind(cats, packing)
dat.gg <- circleLayoutVertices(packing, npoints=100)



# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Indie Flower"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_blank(),
  legend.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  legend.position = 'top',
  text = element_text(color = "black", size = 55),
  plot.background = element_rect(fill = "#FFFFEF", color = NA), 
  panel.background = element_rect(fill = "#FFFFEF", color = NA), 
  legend.title.align=0.5,
  plot.title = element_text(
    color = "black", 
    size = 88, 
    face = "bold",
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    color = "grey10", 
    size = 32,
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



ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), 
               colour = "black", alpha = 0.6) +
  scale_size_continuous(range = c(1,4)) +
  scale_fill_distiller(palette = "Set1", direction = 1 ) +
  geom_text(data = data, aes(x, y, size=area, label = animal_id), 
            color="black", family = 'Indie Flower', size =5) +
  theme(legend.position="none") + 
  labs(caption= str_wrap("Between 2013 and 2017, Roland Kays et al. 
                         convinced hundreds of volunteers in the U.S., 
                         U.K., Australia, and New Zealand to strap GPS 
                         sensors on their pet cats. Roaming area was calculated using
                         time-stamped GPS pings to estimate how far each cat was willing to venture
                         • Visualization by @JacobCJameson", 110),
       title="Feline Footloose",
       subtitle = 'Exploring the Relative Purr-imeter of Domestic Cats in the UK') 
