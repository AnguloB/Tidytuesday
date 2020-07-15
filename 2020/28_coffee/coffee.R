#28	2020-07-07	Coffee Ratings	James LeDoux & Coffee Quality Database	Yorgos Askalidis - TWD


library(extrafont) # first time execute font_import()
loadfonts()
font<-"Lato Light"  #setfont

library(tidyverse)
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')


coffee_ratings%>%
  select(color, aroma, flavor, aftertaste, acidity, body, balance)->Data

Data$color<-replace_na(Data$color, "Unknown")
#Data$color

Data%>%
  reshape2::melt()%>%
  filter(value>=5)%>%
  ggplot(aes(x=variable, y=value, color=color))+
  geom_jitter(alpha=.5)+
  scale_color_manual(values = c("#1164B4", "#03A2A5", "#00AD68", "white", "grey"))+
  labs(title= "Coffee properties by bean color", 
       caption= "Done by @AnguloBrunet \n #tidytuesday", 
       y="rate", x="", color="")+
  guides(color = guide_legend(override.aes = list(size=10)))+
  #theme_void()+
  theme(text=element_text(family = font, color="white"),
    legend.position = "bottom" , 
    legend.key = element_rect(fill = NA),
    panel.background = element_rect(fill = "mistyrose4",
                                    colour = "mistyrose4",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "mistyrose4"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "mistyrose4"), 
   plot.background = element_rect(fill = "mistyrose4"), 
   legend.background = element_rect(fill="mistyrose4",
                                    size=0.5, linetype="solid", 
                                    colour ="mistyrose4"), 
   plot.title = element_text(size=20, hjust=0,face="bold", color="white"), 
   plot.subtitle = element_text(face="italic", size=15, hjust=.5), 
   plot.caption = element_text( face="italic", size=14, hjust = 1), 
   axis.text=element_text(colour="white", size=13))
ggsave("coffee.png")


