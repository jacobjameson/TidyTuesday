# ------------------------------------------------------------------------
# WEEK 2 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext")

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Lato")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-01-10')

feederwatch <- tuesdata$PFW_2021_public

# wrangle data ------------------------------------------------------------

feederwatch <- feederwatch %>%
  filter(subnational1_code != 'XX-', subnational1_code != 'PM-')
  
  
national.code <- list(
  'Far North Region' = c('US-AK', 'CA-YT', 'CA-NT', 'CA-NT'),
  'Northwest Region' = c('US-OR', 'US-WA', 'US-ID', 'US-MT', 'US-WY', 
                         'CA-BC', 'CA-AB'),
  'Southwest Region' = c('US-CA', 'US-NV', 'US-UT', 'US-CO', 'US-AZ', 
                         'US-NM'),
  'Central Region' = c('CA-SK', 'CA-MB', 'US-ND', 'US-SD', 'US-NB', 
                       'US-KA', 'US-MN', 'US-IO', 'US-MO'),
  'Northeast Region' = c("US-MD", "US-MI", "CA-QC", "US-PA", "US-NY", 
                         "US-CT", "CA-ON", "US-ME", "US-RI", "US-OH", 
                         "US-WI",  "US-IA", "US-MA", "US-VA", "US-IL", 
                         "CA-NS", "US-IN", "US-DE", "US-WV", "US-NJ", 
                         "US-VT", "US-NH", "US-KY", "US-KS", "CA-NB", 
                         "US-NE", "CA-PE", "CA-NL", "US-DC", "US-HI"),
  'Southeast Region' = c('US-TX', 'US-OK', 'US-MS', 'US-AR', 'US-GA',
                         'US-FL', 'US-SC', 'US-NC', 'US-TN', 'US-LA', 
                         'US-AL')
)

for (i in seq(1,length(national.code))){
  name <- names(national.code[i])
  code <- national.code[[i]]
  
  feederwatch$subnational1_code <- 
    ifelse(
      feederwatch$subnational1_code %in% code, 
      name, 
      feederwatch$subnational1_code)
}



