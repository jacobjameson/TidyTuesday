# ------------------------------------------------------------------------
# WEEK 7 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Hollywood Age Gaps"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "tidytuesdayR", "broom",
          "wesanderson", "ggrepel", "ggtext", "showtext", "packcircles", 
          "lubridate", "ggExtra", "magick", "data.table",
          "emojifont", 'scales', 'geomtextpath', 'glue', 'ggpubr')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

font_add_google("Pragati Narrow")
font_add_google("Passion One")
showtext_auto()


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-02-14')
age_gaps <- tuesdata$age_gaps

# wrangle data ------------------------------------------------------------

grouped.1 <- age_gaps %>%
  select(gender = character_1_gender, age = actor_1_age, 
         partner.age = actor_2_age)

grouped.2 <- age_gaps %>%
  select(gender = character_2_gender, age = actor_2_age, 
         partner.age = actor_1_age)

groups <- rbind(grouped.1, grouped.2)

groups$residual <- groups$partner.age - groups$age
groups$`Love Interest's Age` <- ifelse(groups$residual >= 0, "Older", "Younger")

groups <- groups %>%
  mutate(gender = ifelse(gender == 'man', 'Men', gender),
         gender = ifelse(gender == 'woman', 'Women', gender))

top.10 <- age_gaps %>% 
  arrange(desc(age_difference)) %>% head(10)

top.10 <- top.10 %>% 
  mutate(color = case_when(movie_name=="Harold and Maude" ~ '#20A8E4',
                           movie_name=="Venus" ~ '#EFAE08',
                           movie_name=="The Quiet American" ~ '#B77E03',
                           movie_name=="The Big Lebowski" ~ '#BCCF03',
                           movie_name=="Beginners" ~ '#794D9C',
                           movie_name=="Poison Ivy" ~ '#B77E03',
                           movie_name=="Whatever Works" ~ '#BCCF03',
                           movie_name=="Entrapment" ~ '#794D9C',
                           movie_name=="Husbands and Wives" ~ '#BCCF03',
                           movie_name=="Magnolia" ~ '#794D9C'))

col_font<-'black'
col_background<-'#f5f5f2'

label<-c("Harold and Maude feature the largest Hollywood age gap at 52 years")
x<-c(50)
y<-c(4.5)
df_notes<-data.frame(label,x,y)

# plot --------------------------------------------------------------------

theme_set(theme_minimal(base_family = "Pragati Narrow"))

p.1 <- ggplot(top.10)+
  geom_segment(aes(x=actor_1_age, xend=actor_2_age, y=reorder(movie_name,actor_2_age), 
                    yend=reorder(movie_name,actor_2_age)), size=1.5) +
  geom_textbox(data=df_notes,
               aes(x=x,y=y,label=label),size=3,
               width = unit(0.3, "npc"),fill=col_background,
               color="black", box.color=col_background) +
  geom_point(aes(y=reorder(movie_name,actor_2_age),x=actor_2_age, 
                 color=character_2_gender),size=8, shape=19, stroke=2) +
  geom_point(aes(y=reorder(movie_name,actor_2_age),x=actor_1_age, color=character_1_gender),
             fill=col_background,size=8, shape=21, stroke=2) +
  geom_text(aes(y=reorder(movie_name,actor_2_age),x=actor_1_age, label=actor_1_age, face = 'bold')) +
  geom_text(aes(y=reorder(movie_name,actor_2_age),x=actor_2_age, label=actor_2_age), color="black", face = 'bold') +
  scale_x_continuous(limits=c(10,90), expand=c(0,0)) +
  scale_color_manual(values = c("woman" = "#eed5d2", "man" = "#c0d6e4")) +
  theme_minimal() +
  theme(plot.background=element_rect(fill=col_background),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=20, color='black'), 
        axis.text.x  = element_text(size=20),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title.align=0.5,
        plot.title = element_text(
          color = "black", 
          size = 70, 
          face = "bold",
          margin = margin(0,0,30,0),
          hjust = 0,
          family = 'Passion One'
        ),
        plot.subtitle = element_text(
          color = "black", 
          size = 18,
          margin = margin(0,0,30,0),
          hjust = 0
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(
          color = "black", 
          size = 9,
          lineheight = 1.2, 
          hjust = 1,
          margin = margin(t = 30) 
        ))+
  labs(x='Actor Ages',
       title="Hollywood Age Gaps",
       fill = '',
       subtitle=str_wrap('When the two main actors of a movie are a man and a woman, 
       the actress is on average 8.4 years younger than the actor.
       Actors are younger than their co-leading actress until they are 28,
       but actresses are younger than their co-leading actor until they are 52.\n', 160))


p.2 <- ggplot(groups, aes(x = age, y = partner.age)) + 
  facet_wrap(~gender, ncol=2) +
  geom_point(aes(color = `Love Interest's Age`)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2) + 
  geom_line(aes(x = age, y = age), color = "black") +
  geom_segment(aes(x = age, y = partner.age, xend = age, yend = age), color = "gray") +
  scale_color_manual(values = c("Older" = "#d1ae3c", "Younger" = "#30ba8f")) +
  ggtitle("Relationship between Age and Real Average Age") +
  theme_minimal() +
  theme(plot.background=element_rect(fill=col_background),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=20, color='black'), 
        strip.text.x = element_text(size = 20),
        axis.text.x  = element_text(size=20),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position="left",
        legend.title.align=0.5,
        plot.title = element_text(
          color = "black", 
          size = 33, 
          face = "bold",
          margin = margin(t = 15),
          hjust = 0.5,
        ),
        plot.subtitle = element_text(
          color = "black", 
          size = 18,
          lineheight = 1.35,
          margin = margin(t = 15),
          hjust = 0.5
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(
          color = "black", 
          size = 15,
          lineheight = 1.2, 
          hjust = 0.95,
          margin = margin(t = 50) 
        )) +
  labs(x='Actor Age',
       y = "Love Interest's Age\n",
       title = '',
       color = NULL,
       caption="Data from Hollywood Age Gap via Data Is Plural | Chart by @JacobCJameson\n")


ggarrange(p.1, p.2, nrow = 2, heights = c(1.5,1.5))


