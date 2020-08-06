#30	2020-07-21	Australian Animal Outcomes	RSPCA	RSPCA Report
library(tidyverse)
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
#animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
#brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')
backcolor<-"#333533"
colortext<- "white"
colorsfill<-c("#d3fcd5","#98a6d4","#e55381",
              "#ad6a6c","#c2f970","#ff858d","#f46036")
url<-"https://image.flaticon.com/icons/svg/886/886696.png"

Data<-animal_outcomes%>%
  filter(animal_type %in% c("Cats", "Dogs"))%>%
  select(year, outcome,animal_type,Total)%>%
  group_by(year, animal_type)%>%
  mutate(rel.freq = round(100 * Total/sum(Total), 0))

library(ggimage)   


ggplot() +
  geom_area(data=Data, aes(x = as.character(year),
                 y = rel.freq,
                 group =outcome,
                 fill = outcome),color="black")+
  facet_grid(rows = vars(animal_type), scales="free_y")+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=17, hjust=0,face="bold", color="white"), 
        plot.subtitle = element_text(face="italic", size=13, hjust=0), 
        plot.caption = element_text( face="italic", size=12, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        legend.position = "bottom", 
        panel.background = element_rect(fill = backcolor))+ 
  labs(title= "Outcomes of Cats and Dogs in Australia", 
       subtitle = "", x="",y="",caption= "Done by @AnguloBrunet")+
  scale_fill_manual(values=colorsfill)

ggsave("animals.png")

