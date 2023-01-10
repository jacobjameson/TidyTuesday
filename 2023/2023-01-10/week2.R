# ------------------------------------------------------------------------
# WEEK 2 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", 
          "lubridate", "ggExtra")

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

feederwatch$subnational2_code <- feederwatch$subnational1_code

for (i in seq(1,length(national.code))){
  name <- names(national.code[i])
  code <- national.code[[i]]
  
  feederwatch$subnational1_code <- 
    ifelse(
      feederwatch$subnational1_code %in% code, 
      name, 
      feederwatch$subnational1_code)
}

feederwatch.clean <- feederwatch %>%
  group_by(subnational1_code, Year, Month, Day, species_code) %>%
  summarise(total = sum(how_many), subnational2_code = subnational2_code) %>%
  ungroup() %>%
  group_by(subnational1_code, Year, Month, Day) %>%
  summarise(total = total, species_code = species_code, 
            subnational2_code = subnational2_code) %>%
  arrange(desc(total)) %>% 
  filter(row_number()==1) %>%
  ungroup()

top.10 <- feederwatch.clean %>%
  group_by(species_code) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  head(10)

top.10 <- top.10$species_code

feederwatch.clean <- feederwatch.clean %>%
  mutate(species = ifelse(species_code %in% top.10, species_code, 'Other'),
         species = case_when(
           species == "houspa" ~ 'House Sparrow',
           species == "moudov" ~ "Mourning Dove",
           species == "daejun" ~ 'Dark-eyed Junco',
           species == "pinsis" ~ 'Pine Siskin',
           species == "amegfi" ~ 'American Goldfinch',
           species == "comred" ~ 'Common Redstart',
           species == "houfin" ~ 'House Finch',
           species == "bkcchi" ~ 'Black-capped Chickadee',
           species == "norcar" ~ 'Northern Cardinal',
           species == "rewbla" ~ 'Red-winged Blackbird',
           TRUE ~ 'Other'))


feederwatch.clean <- feederwatch.clean %>%
  mutate(day = ifelse(Day < 10, paste0('0', as.character(Day)), as.character(Day)),
         month = ifelse(Month < 10, paste0('0', as.character(Month)), as.character(Month)),
         year = as.character(Year),
         date = as.Date(paste(year, month, day, sep = '-')))


# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Lato"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_text(size = 10, margin = margin(t = 5)),
  axis.text.y = element_text(size = 10, margin = margin(r = 5)),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  plot.title = element_text(
    color = "grey10", 
    size = 40, 
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
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) 
  ))



ggplot(feederwatch.clean, aes(x=day,y=month,fill=species)) +
  geom_tile(colour="white", width=1, height=1)+
  labs(caption="Data from FeederWatch | Chart @JacobCJameson",
       subtitle='The bird of the day',
       title="Bird of the Day", fill="") +
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.text=element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.box.margin = margin(b=7)
  )



