
# Tidytuesday week 21 2020. 
##cocktails
#Data from 
#https://www.kaggle.com/jenlooper/mr-boston-cocktail-dataset

library(tidyverse)
library(lubridate)

options(scipen = 999) #prevents scientifical notation in plots
backcol<- "white"  #background color
colortitle<-"#34558b" # color for title
colortext<- "#ff6f61"  #color for text (no title)

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Lato Light"  #setfont

color1 <-"#ECBF7B" #background color


theme_ari<- function(legpos="none", backcolor=backcol){  
  theme(text=element_text(family = font, color=colortext),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color=colortext), 
        plot.subtitle = element_text(face="italic", size=15, hjust=.5), 
        plot.caption = element_text( face="italic", size=14, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext))
}


# Get the Data

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


rum<-c("Light rum"	,"Rum"	,"Dark rum","proof rum","Malibu rum","Spiced rum","Añejo rum"	,"White rum"	,"Coconut rum","Dark Rum"	,"Rumple Minze"	,"White Rum")

cocktails%>%
  filter(ingredient %in% rum)%>%
  select(drink, category, glass)%>%
  distinct()%>%
  group_by(category, glass)%>%
  count()%>%
  ggplot(aes(x=reorder(category,-n), y = reorder(glass, n), fill=n, label =n))+
  geom_tile()+
  geom_text()+
  theme_bw()+
  scale_fill_gradient(high="#feada6", low= "#f5efef")+theme_ari()+
  labs(x="", y="", 
       title= "Drinks containing Rum", 
       caption = "Done by @AnguloBrunet\n #tidytuesday")
    
ggsave("cocktails.png")

  
