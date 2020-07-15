#29	2020-07-14	Astronaut Database	Corlett, Stavnichuk & Komarova article	Corlett, Stavnichuk & Komarova article

library(tidyverse)

library(extrafont) # first time execute font_import()
loadfonts()

font<-"Love Monster Sketched" #setfont
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts%>%
  filter(eva_hrs_mission >=0.01)%>%
  filter(sex=='female')%>%
  select(total_eva_hrs, name)%>%
  distinct()%>%
  arrange(total_eva_hrs)->Data
Data$name<- factor(Data$name, levels=Data$name)

Data%>%
  ggplot(aes( x=1, y =total_eva_hrs, fill=total_eva_hrs, color=total_eva_hrs,label=paste0(total_eva_hrs, "h")))+
  geom_col()+facet_wrap(~name, nrow = 3)+coord_polar()+geom_text(vjust=1, family = font, size=5)+
 theme_void()+ 
  scale_fill_gradient(low="#561EFE",high="#FF11C7")+
  scale_color_gradient(low="#561EFE",high="#FF11C7")+
  
  labs(title="Women in space",
       subtitle = "Total duration of all extravehicular activities in hours\n",
       caption= "\n\nDone by @AnguloBrunet \n #tidytuesday")+
  theme(text=element_text(family = font, color="white", size=20),
    legend.position = "none",
        panel.background = element_rect(fill = "#12063C",
                                        colour = "#12063C",
                                        size = 0.5, linetype = "solid"),
        plot.background = element_rect(fill = "#12063C"), 
        legend.background = element_rect(fill="#12063C",
                                         size=0.5, linetype="solid", 
                                         colour ="#12063C"), 
        plot.title = element_text(size=50, hjust=.5,face="bold", color="white", family="MajorSketchy"), 
        plot.subtitle = element_text(face="italic", size=20, hjust=.5), 
        plot.caption = element_text( face="italic", size=15, hjust = 1))

ggsave("astronaut.png")

