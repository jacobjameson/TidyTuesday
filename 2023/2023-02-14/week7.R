# ------------------------------------------------------------------------
# WEEK 7 #TidyTuesday
# AUTHOR: Jacob Jameson
# THEME: "Hollywood Age Gaps"
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

font_add_google("Pragati Narrow")
showtext_auto()


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-02-14')
age_gaps <- tuesdata$age_gaps

# wrangle data ------------------------------------------------------------

grouped.1 <- age_gaps %>%
  group_by(character_1_gender, actor_1_age) %>%
  summarize(avg.partner.age.1 = mean(actor_2_age), n.1 = n()) %>%
  ungroup() %>%
  rename(gender = character_1_gender, age = actor_1_age)

grouped.2 <- age_gaps %>%
  group_by(character_2_gender, actor_2_age) %>%
  summarize(avg.partner.age.2 = mean(actor_1_age), n.2 = n()) %>%
  ungroup() %>%
  rename(gender = character_2_gender, age = actor_2_age)

grouped <- merge(grouped.1, grouped.2, on=c('gender, age'))

grouped <- grouped %>%
  mutate(real.avg.age = 
           (n.1*avg.partner.age.1 + n.2*avg.partner.age.2)/(n.1 + n.2))

grouped$residual <- grouped$real.avg.age - grouped$age
grouped$residual_above_or_below <- ifelse(grouped$residual >= 0, "above", "below")





grouped.3 <- age_gaps %>%
  select(gender = character_1_gender, age = actor_1_age, 
         partner.age = actor_2_age)

grouped.4 <- age_gaps %>%
  select(gender = character_2_gender, age = actor_2_age, 
         partner.age = actor_1_age)

groups <- rbind(grouped.3, grouped.4)

groups$residual <- groups$partner.age - groups$age
groups$residual_above_or_below <- ifelse(groups$residual >= 0, "above", "below")

age_gaps %>% 
  arrange(desc(age_difference)) %>% head(10)
# theme --------------------------------------------------------------------


theme_set(theme_minimal(base_family = "Pragati Narrow"))

theme_update(
  rect = element_rect(fill = "black", color = "black"),
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
library(geomtextpath)
library(magick)
library(glue)
library(ggtext)
library(emojifont)
library(personograph)

title = glue("<span style='font-family:{f};color:white;font-size:40pt;'>Netflix Content<br><span style='color:#E50914;font-size:45pt;'>E x p a n s i o n</span></span>")
subtitle = glue("<span style='font-family:{f2};color:white;font-size:11pt;'>New Netflix originals by release year. Netflix released its first original TV series in 2012. Today, a decade later, Netflix is distributing more than 700 new original programs a year.</span>")
caption=glue("<span>Source: What's On Netflix</span><br><span style='font-family:{fb};color:#E50914;'>&#xf099;</span><span style='color:black;'>.</span><span>@tanya_shapiro</span><span style='color:black;'>...</span><span style='font-family:{fb};color:#E50914;'>&#xf09b;</span><span style='color:black;'>.</span><span>tashapiro</span>")



ggplot(groups, aes(x = age, y = partner.age, color = residual_above_or_below)) + 
  facet_wrap(~gender, ncol = 2) + 
  geom_point(shape = 16, size = 5, alpha = 0.6) + 
  geom_abline(intercept = 0, slope = 1, lty = 2, color = 'black') + 
  geom_line(aes(x = age, y = age), color = "red",  linewidth=1.5) +
  geom_segment(aes(x = age, y = partner.age, xend = age, yend = age), color="white", linetype="dotted") +
  scale_color_manual(values = c("above" = "#0091ff", "below" = "#f0650e")) +
  labs(y="",
       caption = caption)+
  scale_y_continuous(expand=c(0,0), limits=c(10,85)) +
  scale_x_continuous(limits=c(18,85), expand=c(0,0))+
  theme(
    plot.background=element_rect(fill="black", color="black"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y=element_line(color="#272727", linewidth=.15),
    text = element_text(color="white"),
    plot.caption = element_textbox_simple(family="Roboto", color="#DCDCDC", margin=margin(t=10), size=8),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    plot.margin = margin(l=5, b=15,t=10),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="white", family="Roboto", size=10)
  ) 

#make sure to download these fonts locally!
fb = '"Bebas Neue"'
font="Bebas Neue"
f = '"Bebas Neue"'
f2 = '"Roboto"'

#create plot title, subtitle, and caption to apply with ggtext::element_textbox_simple
title = glue("<span style='font-family:{f};color:white;font-size:40pt;'>Netflix Content<br><span style='color:#E50914;font-size:45pt;'>E x p a n s i o n</span></span>")
subtitle = glue("<span style='font-family:{f2};color:white;font-size:11pt;'>New Netflix originals by release year. Netflix released its first original TV series in 2012. Today, a decade later, Netflix is distributing more than 700 new original programs a year.</span>")
caption=glue("<span>Source: What's On Netflix</span><br><span style='font-family:{fb};color:#E50914;'>&#xf099;</span><span style='color:black;'>.</span><span>@tanya_shapiro</span><span style='color:black;'>...</span><span style='font-family:{fb};color:#E50914;'>&#xf09b;</span><span style='color:black;'>.</span><span>tashapiro</span>")


#PLOT
ggplot(data = df_year, 
       mapping = aes(x=year, y=total))+
  geom_area(color="#E50914", 
            fill = "#FF7B82",
            alpha=0.25,
            linewidth=1.5)+
  geom_text(mapping=aes(label=year, y=-20, x=year), size=4, color="white",
            fontface="bold", family="Roboto")+
  geom_textbox(inherit.aes=FALSE, 
               mapping=aes(label=title, x=2012, y=650),
               width=unit(3.5,"in"),
               fill="black", box.size=NA,
               halign=0,
               hjust=0)+
  geom_textbox(inherit.aes=FALSE, 
               mapping=aes(label=subtitle, x=2012, y=552),
               width=unit(3.2,"in"),
               fill="black", box.size=NA,
               halign=0,
               hjust=0)+
  geom_richtext(data=labels, 
                mapping=aes(x=x,y=y,label=label, hjust=hjust),
                color="white", 
                label.size=NA,
                size=3,
                fill=NA,
                family="Roboto")+
  geom_textsegment(inherit.aes = FALSE,
                   data = eras,
                   mapping=aes(x=start, xend=end, y=-50, yend=-50, label=era), 
                   family = "Bebas Neue", color="white", size=3.25, 
                   linecolor="#E50914")+
  geom_segment(data=segments,
               mapping=aes(x=x, xend=xend, y=y, yend=yend), color="white", linetype="dotted")+
  labs(y="",
       caption = caption)+
  theme(
    plot.background=element_rect(fill="black", color="black"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y=element_line(color="#272727", linewidth=.15),
    text = element_text(color="white"),
    plot.caption = element_textbox_simple(family="Roboto", color="#DCDCDC", margin=margin(t=10), size=8),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    plot.margin = margin(l=5, b=15,t=10),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="white", family="Roboto", size=10)
  )

#save plot




top.10$movie_name
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

label<-c("Harold and Maude feature the largest Hollywood age gap at 52 years",
         "")
x<-c(50,120)
y<-c(4.5,1.5)
df_notes<-data.frame(label,x,y)

#Icons taken from Jory Raphel's art, asked for their permission directly to use in this chart. 
#Files not included in repo to respectt copyright purposes. But one can subsitute with their own graphics!
gang<-gang%>%mutate(icon=paste('icons/',member,'.png',sep=""))
gang$label<-paste0("<img src='", gang$icon,  "' width='60' /><br>*", gang$member,"*")


#BUILDING PLOT
ggplot(data=top.10)+
  geom_segment( aes(x=actor_1_age, xend=actor_2_age, y=reorder(movie_name,actor_2_age), 
                    yend=reorder(movie_name,actor_2_age)), size=1.5) +
  geom_textbox(data=df_notes,
               aes(x=x,y=y,label=label),size=4,family='Gill Sans',
               width = unit(0.3, "npc"),fill=col_background,
               color="grey10", box.color=col_background) +
  geom_point(aes(y=reorder(movie_name,actor_2_age),x=actor_2_age, 
                 color=character_2_gender),size=10, shape=19, stroke=2) +
  geom_point(aes(y=reorder(movie_name,actor_2_age),x=actor_1_age, color=character_1_gender),
             fill=col_background,size=10, shape=21, stroke=2) +
  geom_text(aes(y=reorder(movie_name,actor_2_age),x=actor_1_age, label=actor_1_age)) +
  geom_text(aes(y=reorder(movie_name,actor_2_age),x=actor_2_age, label=actor_2_age), color="white") +
  geom_text(data=df_text,aes(y=y,x=x, label=text), color="grey20", size=3) +
  scale_x_continuous(limits=c(14,83), expand=c(0,0))+
  scale_color_manual(values = c("woman" = "pink", "man" = "blue")) +
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "grey30", curvature =0.15)+ 
  theme_minimal()+
  theme(plot.title=element_text(family="Lemon Milk Bold",size=16),
        plot.background=element_rect(fill=col_background),
        text = element_text(family="Lemon Milk Light"),
        axis.text.y  = element_markdown(color = "black", size = 8, 
                                        family="Lemon Milk Bold", halign=0.6), 
        axis.text.x  = element_text(size=12),
        axis.title.x  = element_text(family="Lemon Milk Bold"),
        plot.caption = element_text(family='Gill Sans', color="grey20", 
                                    margin=margin(10,0,0,2), hjust=0.98, size=10),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position="none",
        axis.title.y = element_blank())+
  labs(x='Actor Ages',
       title="Top 10 Largest Hollywood Age Gaps",
       subtitle="Number of episodes characters caught monsters vs. were captured by monsters",
       caption="The data this week comes from Hollywood Age Gap via Data Is Plural | Chart by @JacobCJameson")











library(tidyverse)
library(ggplot2)
library(extrafont)
library(showtext)
library(ggtext)
library(tools)
library(emojifont)
library(reshape)
library(ggthemes)
#IMPORT DATA
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
#DATA CLEANING
#select varaibles
gang<-df%>%select(captured_daphnie,captured_shaggy,captured_fred,captured_velma,captured_scooby,
                  caught_daphnie,caught_shaggy,caught_fred,caught_velma,caught_scooby,
                  unmask_daphnie,unmask_shaggy,unmask_fred,unmask_velma,unmask_scooby)
#convert characters to 0s and 1s for True and False
gang[1:15] <- lapply(gang[1:15], as.logical)
gang[1:15] <- lapply(gang[1:15], as.numeric)
#replace all NAs with 0 instead, NAs will cause errors in our sum totals
gang[is.na(gang)] <- 0
gang_sum<-colSums(gang)
gang<-as.data.frame(t(t(gang_sum)))
gang<-rownames_to_column(gang)
#split up rows into two new columns, e.g. capture_fred becomes "captured" for variable and "fred" for member
gang<-gang%>%separate(rowname, c("variable", "member"), "_")%>%
  mutate(member=toTitleCase(member))
#daphne spelled wrong 
gang[gang$member=="Daphnie","member"]="Daphne"
#reshape data using reshape cast function
gang<-cast(gang, member~variable, sum)
#include ratio
gang$ratio<-round(gang$caught/gang$captured,2)
#AESTHETICS
#color palette taken from Scooby Doo group photo
gang<-gang%>%mutate(color=case_when(member=='Fred' ~ '#20A8E4',
                                    member=='Velma' ~ '#EFAE08',
                                    member=='Scooby' ~ '#B77E03',
                                    member=='Shaggy' ~ '#BCCF03',
                                    member=='Daphne' ~ '#794D9C',
))
col_background<-'grey20'
col_font<-'white'
col_background<-'grey92'
#Icons taken from Jory Raphel's art, asked for their permission directly to use in this chart. 
#Files not included in repo to respectt copyright purposes. But one can subsitute with their own graphics!
gang<-gang%>%mutate(icon=paste('icons/',member,'.png',sep=""))
gang$label<-paste0("<img src='", gang$icon,  "' width='60' /><br>*", gang$member,"*")
#ANNOTATIONS
#header caption info in place of a legend
x<-c(83,160,121.5)
y<-c(5.29,5.29,5.29)
text<-c("CAPTURED","CAUGHT","RATE CAPTURED/CAUGHT")
df_text<-data.frame(x,y,text)
#annotations for observations
label<-c("Scooby & Fred are the heroes of the show, both have caught monsters in 100+ episodes.",
         "Meanwhile, Daphne & Velma more likely to be captured by monster than catch a monster.")
x<-c(50,120)
y<-c(4.5,1.5)
desc<-c("Female","Male")
df_notes<-data.frame(label,x,y,desc)
#Arrows for notes
arrows <- tibble(
  x1 = 86,
  x2 = 81,
  y1 = 3.32,
  y2 = 3.05
)


#BUILDING PLOT
ggplot(data=gang)+
  geom_segment( aes(color=color, x=caught, xend=captured, y=reorder(label,caught), yend=reorder(label,caught)), size=1.5) +
  geom_textbox(data=df_notes,
               aes(x=x,y=y,label=label),size=4,family='Gill Sans',
               width = unit(0.3, "npc"),fill=col_background,
               color="grey10", box.color=col_background
  )+
  geom_point(aes(y=reorder(label,caught),x=caught, color=color),size=10, shape=19, stroke=2)+
  geom_point(aes(y=reorder(label,caught),x=captured, color=color),fill=col_background,size=10, shape=21, stroke=2)+
  geom_text(aes(y=reorder(label,caught),x=captured, label=captured))+
  geom_text(aes(y=reorder(label,caught),x=caught, label=caught), color="white")+
  geom_text(data=df_text,aes(y=y,x=x, label=text), color="grey20", size=3)+
  #remove Shaggy, line too narrow to plot full text at given height
  geom_text(data= gang%>%filter(member!='Shaggy'),
            aes(y=reorder(label,caught),x=(captured+caught)/2, label=paste(ratio,"x",sep="")), color="grey20",vjust=-1)+
  #create specific height for Shaggy rate text
  geom_text(data= gang%>%filter(member=='Shaggy'),
            aes(y=reorder(label,caught),label=paste(ratio,"x",sep="")), x=90, color="grey20",vjust=-3.2)+
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "grey30", curvature =0.15)+
  scale_color_identity(guide = "legend")+
  theme_minimal()+
  theme(plot.title=element_text(family="Lemon Milk Bold",size=16),
        plot.background=element_rect(fill=col_background),
        text = element_text(family="Lemon Milk Light"),
        axis.text.y  = element_markdown(color = "black", size = 8, family="Lemon Milk Bold", halign=0.6), 
        axis.text.x  = element_text(size=12),
        axis.title.x  = element_text(family="Lemon Milk Bold"),
        plot.caption = element_text(family='Gill Sans', color="grey20", margin=margin(10,0,0,2), hjust=0.98, size=10),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position="none",
        axis.title.y = element_blank())+
  labs(x='# of Episodes',
       title="Scooby-Doo: Are Female Characters Damsels in Distress?",
       subtitle="Number of episodes characters caught monsters vs. were captured by monsters",
       caption="Data from Kaggle | Icons by Jory Raphel | Chart by @tanya_shapiro")

