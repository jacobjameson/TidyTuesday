# ------------------------------------------------------------------------
# WEEK 2 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", 
          "lubridate", "ggExtra", "dvmisc")

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
  filter(subnational2_code == 'US-CA', species_code == 'calqua') %>%
  group_by(subnational1_code, Year, Month, Day, species_code) %>%
  summarise(total = sum(how_many), subnational2_code = subnational2_code) %>%
  ungroup() %>%
  group_by(subnational1_code, Year, Month, Day) %>%
  summarise(total = total, species_code = species_code, 
            subnational2_code = subnational2_code) %>%
  arrange(desc(total)) %>% 
  ungroup() 


feederwatch.clean <- feederwatch.clean %>%
  mutate(day = ifelse(Day < 10, paste0('0', as.character(Day)), as.character(Day)),
         month = ifelse(Month < 10, paste0('0', as.character(Month)), as.character(Month)),
         year = as.character(Year),
         date = as.Date(paste(year, month, day, sep = '-')))



dates <- data.frame(date = seq(as.Date("2020-11-01"), 
                               as.Date("2021-04-30"), 
                               by = "day"))

feederwatch.clean <- merge(feederwatch.clean, 
                           dates, 
                           on='date', 
                           all.y = T)

feederwatch.clean <- feederwatch.clean %>%
  mutate(total = ifelse(is.na(total), 0, total),
         species = 'California Quail',
         day = day(date),
         month = month(date),
         year = year(date),
         month = case_when(
           month == 11 ~ 'November',
           month == 12 ~ 'December',
           month == 1 ~ 'January',
           month == 2 ~ 'February',
           month == 3 ~ 'March',
           month == 4 ~ 'April'))

months_vec <- c("April", 'March', "February", 
                "January", "December", "November")

feederwatch.clean$month <- factor(feederwatch.clean$month, 
                                  levels = months_vec)


feederwatch.clean <- feederwatch.clean %>%
  mutate(number = case_when(
    total == 0 ~ '0',
    total >= 1 & total < 5 ~ '1-5',
    total >= 5 & total < 10 ~ '5-10',
    total >= 10 & total < 15 ~ '10-15',
    total >= 15 ~ '15+'))

feederwatch.clean$number <- factor(feederwatch.clean$number, 
                                  levels = c('0', '1-5', '5-10', '10-15', '15+'))

# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Lato"))

theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 16, margin = margin(r = 0)),
  axis.ticks = element_line(color = "grey91", size = .5),
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.1, "lines"),
  panel.grid = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  legend.position = c(0.8, -0.15),
  text = element_text(color = "#22211d"),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  legend.background = element_rect(fill = "#f5f5f2", color = NA),
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
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) 
  ))


pal<-c("grey90","#78B7C5", "#3B9AB2", "#EBCC2A", "#E1AF00", "#F21A00")


ggplot(feederwatch.clean, aes(x=day,y=month,fill=number)) +
  geom_tile(colour="black", width=1, height=0.9) +
  labs(caption= str_wrap("Data comes from California sightings logged to FeederWatch. 
                          FeederWatch is a November-April survey of birds 
                          that visit backyards, nature centers, community areas, 
                          and other locales in North America. @JacobCJameson", 100),
       subtitle= "No clear pattern in sightings of The California State bird!\n",
       title="Counting California Quail", fill="") +
  scale_fill_manual( 
    values=pal, 
    name="Number of Backyard Sightings", 
    guide = guide_legend( keyheight = unit(8, units = "mm"), 
                          keywidth=unit(20, units = "mm"), 
                          label.position = "bottom", 
                          title.position = 'top', nrow=1) )


