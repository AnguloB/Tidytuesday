# 23	2020-06-02	Marble Races	Jelle's Marble Runs	Randy Olson


library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Courier New" #Fuente que voy a utlizar

# Get the Data

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)

library(tidyverse)


pole<-marbles%>%
filter(race %in%paste0("S1Q", 1:8))



points<-marbles<-marbles%>%
  filter(race %in%paste0("S1R", 1:8))

points%>%
  filter(points >= 5)%>%
  group_by(team_name)%>%
  count()

paleta<-c("#F19754","#4380B9","#237B68","#4c6085","#39a0ed","#36f1cd","#13c4a3","#32322c",
"#e9d758","#ff8552","#efbdeb","#b33f62","#f9564f","#8E8542","#96443E","#9E6B84")

points%>%
  filter(points >= 5)%>%
  mutate(metersecond=time_s/(number_laps))%>%
  ggplot(aes(x=metersecond, y=points,  color=team_name, label=team_name))+
  geom_text(size=3, vjust="inward",hjust="inward", family=font, fontface= 2)+
  facet_wrap(~race, nrow=2, scales = "free")+theme_bw()+
  scale_color_manual(values=paleta)+
  theme_minimal()+
  theme(legend.position = "none", 
        strip.background = element_rect(fill="#EF7A85"), 
        text=element_text(family = font), 
        plot.title = element_text(size=20, hjust=0,face="bold", color="#EF7A85"), 
        plot.subtitle = element_text(face="italic", size=15, hjust=0), 
        plot.caption = element_text( face="italic", size=12, hjust = 1))+
  labs(x="average time per lap", 
       caption="Done by @AnguloBrunet \n #tidytuesday", 
       title= "Marble Racing", 
       subtitle= "Points and speed by race")
ggsave("marbles.png")




