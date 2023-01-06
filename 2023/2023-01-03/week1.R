# ------------------------------------------------------------------------
# WEEK 1 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Bring your own data from 2022!"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

libs <- c("tidyverse", "tidytuesdayR", "rgdal", "broom",
          "wesanderson")

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-09-13')
bigfoot <- tuesdata$bigfoot


# wrangle data ------------------------------------------------------------

bigfoot %>% 
  group_by(state, county) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# plot --------------------------------------------------------------------




