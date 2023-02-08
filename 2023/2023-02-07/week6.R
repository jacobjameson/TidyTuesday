# ------------------------------------------------------------------------
# WEEK 6 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Big Tech Stock Prices"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", "packcircles", 
          "lubridate", "ggExtra", "geosphere", "data.table",
          "viridis", 'scales', 'ggsci')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Roboto Mono")
showtext_auto()


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-02-07')

big_tech_stock_prices <- tuesdata$big_tech_stock_prices
big_tech_companies <- tuesdata$big_tech_companies

# wrangle data ------------------------------------------------------------

big_tech_stock_prices <- merge(big_tech_stock_prices, 
                               big_tech_companies, 
                               on='stock_symbol')

big_tech_stock_prices <-  big_tech_stock_prices %>%
  filter(stock_symbol == 'AAPL' | stock_symbol == 'META' | 
        stock_symbol == 'NFLX' | stock_symbol == 'AMZN' | stock_symbol == 'GOOGL') %>%
  mutate(color = case_when(
    stock_symbol == 'AAPL' ~ 'green',
    stock_symbol == 'NFLX' ~ 'red',
    stock_symbol == 'AMZN' ~ 'blue',
    stock_symbol == 'META' ~ 'orange',
    stock_symbol == 'GOOGL' ~ 'yellow'))

big_tech_stock_prices$company = factor(big_tech_stock_prices$company,
                                      levels=c("Meta Platforms, Inc.",
                                               'Apple Inc.',
                                               "Amazon.com, Inc.",
                                               "Netflix, Inc." , 
                                               "Alphabet Inc."))

# theme --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Roboto Mono"))

theme_update(
  rect = element_rect(fill = "black", color = "black"),
  axis.title = element_blank(),
  strip.text = element_blank(),
  panel.grid.minor=element_line(colour="#009d00"),
  panel.grid.major=element_line(colour="#009d00"),
  legend.title = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  legend.position = 'top',
  text = element_text(color = "#009d00", size = 20),
  plot.background = element_rect(fill = "black", color = NA), 
  panel.background = element_rect(fill = "black", color = NA), 
  legend.background = element_rect(fill = "black", color = NA), 
  legend.title.align=0.5,
  plot.title = element_text(
    color = "white", 
    size = 33, 
    face = "bold",
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    color = "white", 
    size = 18,
    lineheight = 1.35,
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "white", 
    size = 9,
    lineheight = 1.2, 
    hjust = 1,
    margin = margin(t = 50) 
  ))


# plot --------------------------------------------------------------------

big_tech_stock_prices %>% ggplot(aes(x = date, y = close, 
                                     color = company, fill = company)) +
  facet_wrap(~company, nrow=2) +
  scale_color_tron() +
  scale_fill_tron() +
  geom_line(size = 0.8, alpha = 0.5) +
  geom_ribbon(aes(ymin = -Inf, ymax = close), alpha = 0.3) +  
  scale_size_area(label = comma)  +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels=dollar_format()) +
  theme(axis.text = element_text(size = 18, color = "#009d00", 
                                 face = "bold"),
        axis.ticks.length = unit(3, "pt"),
        axis.ticks = element_line(color = "green")) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'NFLX' & date == as.Date('2021-11-17')), 
             aes(date, close), color = "green", alpha = 0.2, size = 10) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'NFLX' & date == as.Date('2021-11-17')), 
             aes(date, close),color = "green", alpha = 0.5, size = 8) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'NFLX' & date == as.Date('2021-11-17')), 
             aes(date, close), color = "green", size = 5) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AMZN' & date == as.Date('2021-07-08')), 
             aes(date, close), color = "orange", alpha = 0.2, size = 10) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AMZN' & date == as.Date('2021-07-08')), 
             aes(date, close),color = "orange", alpha = 0.5, size = 8) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AMZN' & date == as.Date('2021-07-08')), 
             aes(date, close), color = "orange", size = 5) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'META' & date == as.Date('2021-09-07')), 
             aes(date, close), color = "red", alpha = 0.2, size = 10) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'META' & date == as.Date('2021-09-07')), 
             aes(date, close),color = "red", alpha = 0.5, size = 8) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'META' & date == as.Date('2021-09-07')), 
             aes(date, close), color = "red", size = 5) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'GOOGL' & date == as.Date('2021-11-18')), 
             aes(date, close), color = "white", alpha = 0.2, size = 10) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'GOOGL' & date == as.Date('2021-11-18')), 
             aes(date, close),color = "white", alpha = 0.5, size = 8) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'GOOGL' & date == as.Date('2021-11-18')), 
             aes(date, close), color = "white", size = 5) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AAPL' & date == as.Date('2022-01-03')), 
             aes(date, close), color = "lightblue", alpha = 0.2, size = 10) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AAPL' & date == as.Date('2022-01-03')), 
             aes(date, close),color = "lightblue", alpha = 0.5, size = 8) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AAPL' & date == as.Date('2022-01-03')), 
             aes(date, close), color = "lightblue", size = 5) +
  geom_point(data = filter(big_tech_stock_prices,
                           stock_symbol == 'AAPL' & date == as.Date('2005-01-03')), 
             aes(date, close), color = "black", size = 10) +
  labs(caption= str_wrap("Data comes from Yahoo Finance 
                         via Kaggle (by Evan Gower) • Visualization by @JacobCJameson", 160),
       subtitle= "Facebook, Apple, Amazon, Netflix, and Google's Growth during the Global Pandemic\n",
       title="Pandemic Peaks in FAANG Stocks: A Plot of Price Progression") 




