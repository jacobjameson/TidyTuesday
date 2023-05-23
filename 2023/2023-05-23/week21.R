# ------------------------------------------------------------------------
# WEEK 21 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Squirrel Census"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "tidytuesdayR", 'sf',
          'osmdata', 'rjson', 'ggmap', 'showtext',
          'sjPlot', 'ggimage')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Luckiest Guy")
showtext_auto()



# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-05-23')
data <- tuesdata$squirrel_data


# wrangle data ------------------------------------------------------------

big_streets <- getbb("Manhattan NY") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Manhattan NY")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- getbb("Manhattan NY")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

water_osm <- opq("Manhattan NY") %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

railway <- getbb("Manhattan NY")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

central_park <- getbb("Central Park, New York") %>%
  opq() %>%
  add_osm_feature(key = "leisure") %>%
  osmdata_sf()

# plot --------------------------------------------------------------------

ggplot() +
  geom_sf(data = central_park$osm_polygons,
          inherit.aes = FALSE,
          fill = "green",  
          color = "green",  
          size = 0.5,
          alpha = .6) +
  geom_sf(data = water_osm$osm_lines,
          inherit.aes = FALSE,
          color = "blue",
          lwd = 0,
          alpha = .3) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash") +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#3F5151",
          size = .3) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color =  "#2f3737",
          size = .2) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#8e76a4",
          size = .5) +
  coord_sf(ylim = c(40.763331, 40.802099), 
           xlim = c(-74, -73.939647),
           expand = T)  +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#eeefc9"),
    plot.title = element_text(
      size = 60,
      family = "Luckiest Guy",
      face = "bold",
      hjust = 0,
      vjust = 1,  
      color = "#654321"
    ),
    plot.subtitle = element_text(
      family = "Luckiest Guy",
      size = 35,
      color = "#654321",
      hjust = 0,
      margin = margin(2, 0, 5, 0)
    ),
    plot.caption = element_text(
      size = 15,
      family = "Luckiest Guy",
      color = "#654321",
      hjust = 0.5,
      margin = margin(2, 0, 5, 0)
    )
  ) +
  geom_point(
    data = data,
    aes(x = X, y = Y),
    size = 0.9,
    fill = "#654321",
    color = "#654321",
    pch = 21) +
  labs(
    title = "Squirrel Census",
    subtitle = "Unraveling the Nutty Secrets of Central Park",
    caption = str_wrap(
      "Data from the 2018 Central Park Squirrel Census | Chart by @JacobCJameson\n",
      70
    )
  )

