
# Tidytuesday week 178 2020. 
##BROADWAY MUSICALS

library(tidyverse)
library(lubridate)
logo<-"https://translate.google.es/?hl=es&tab=iT&authuser=0"


#glitter are points stored in a folder
glit<-c("glitter/softpink.png", "glitter/purple.png", "glitter/pink.png", "glitter/blue1.png", "glitter/green.png")

options(scipen = 999) #prevents scientifical notation in plots
backcol<- "white"  #background color
colortitle<-"#34558b" # color for title
colortext<- "#1271A1"  #color for text (no title)
#palette1<-c("#EA787A", "#1C6788", "#BED441")
colorlines<-"white"

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Lato Light"  #setfont

color1 <-"white" #background color
#color2<- "#FA7268"  #color  countries
colorpoint<- "#34558b" 

lowcol<-"#96DAFA"
highcol<-"#303379"



theme_ari<- function(legpos="bottom", backcolor=backcol){  
  theme(text=element_text(family = font, color=colortext),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = backcolor),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color=colortext), 
        plot.subtitle = element_text(face="italic", size=15, hjust=.5), 
        plot.caption = element_text( face="italic", size=14, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext))
}

# Get the Data


grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
#synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
#cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
#pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')


#For ploting image (title). 
library(cowplot)
library(magick)
img <- png::readPNG("logo.png") #read image in directory (logo)


top<-ggdraw() + 
  draw_image(img) 


## heatmap
agrupOrder<-c(paste0("Dec week", 5:1),
              paste0("Nov week", 5:1),
              paste0("Oct week", 5:1),
              paste0("Sep week", 5:1),
              paste0("Aug week", 5:1),
              paste0("Jul week", 5:1),
              paste0("Jun week", 5:1),
              paste0("May week", 5:1),
              paste0("Apr week", 5:1),
              paste0("Mar week", 5:1),
              paste0("Feb week", 5:1),
              paste0("Jan week", 5:1))#order for y axis


mamma<-grosses %>%
  filter(show=="Mamma Mia!")%>%
  mutate(month_= month(week_ending))%>%
  mutate(month_lab= month(week_ending, label = TRUE))%>%
  mutate(day_= day(week_ending))%>%
  mutate(year_= year(week_ending))%>%
  group_by(year_,month_)%>%
  mutate(id = row_number())%>% #create id by month
  mutate(idlab = paste0("week",row_number()))%>% #create id by month
  mutate(agrup=  paste(month_lab, idlab))%>%
  mutate(glitter= case_when(
    id==1 ~ glit[1],
    id==2 ~ glit[2],
    id==3 ~ glit[3],
    id==4 ~ glit[4],
    id==5 ~ glit[5]))

p2<-mamma%>%
  ggplot( aes(x = factor(year_), y = agrup, fill = seats_sold)) +
  geom_tile()+theme_ari(legpos = "top")+
  scale_fill_gradient(low=lowcol, high = highcol)+
  scale_y_discrete(limits=agrupOrder)+xlab("")+ylab("")+
  labs(title = "Total seats sold per week in Broadway",fill="")

library(ggimage)
p3<-mamma%>%
  ggplot()+
  geom_image(aes(x=factor(year_), y= pct_capacity, image=glitter), 
             size=0.015, position="jitter")+ #adds image
  theme_ari(legpos = "none")+
  scale_y_continuous(labels = scales::percent)+
  labs(subtitle="Percent of theatre capacity sold",
       caption= "Done by @AnguloBrunet \n #tidytuesday")+ylab("")+xlab("")



plot_grid(top, p2,p3, nrow=3, rel_heights = c(0.2,1, 0.8))


ggsave("MammaMia.png", units="cm", height = 30, width = 21)


