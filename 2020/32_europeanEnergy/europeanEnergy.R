#32	2020-08-04	European Energy	Eurostat Energy	Washington Post Energy
library(tidyverse)
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

colortext<-"black"
backcolor<-"#E1E4DB"

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')
names(energy_types)[7]<- "y2018"

orden<-c( "Other","Nuclear","Conventional thermal", "Pumped hydro power", 
"Hydro","Geothermal", "Solar","Wind")

ord<-c("Wind", "Solar","Geothermal","Hydro", "Pumped hydro power","Conventional thermal","Nuclear","Other")
energy_types%>%
  mutate(type= factor(type, levels=orden))%>%
  group_by(country_name)%>%
  mutate(percent = y2018 / sum(y2018) * 100)%>%
  na.omit()%>%
  ggplot(aes(x=reorder(country_name, desc(country_name)), y = percent, fill=type))+ 
  geom_col(color="black")+
  scale_fill_manual(breaks=ord,
                    values=c("#006d2c","#92D050","#00D039","#2ca25f",
                             "#99d8c9","#ffeda0","#fd8d3c","grey"))+
  coord_flip()+
  theme_bw()+
  theme(text=element_text(family = font, size=12, color=colortext),
        plot.background = element_rect(fill = backcolor),
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="black"), 
        plot.subtitle = element_text(face="italic", size=18, hjust=0, color="#F7D6E0"), 
        plot.caption = element_text( face="italic", size=10, hjust = 1), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color=colortext), 
        legend.position = "bottom")+
  labs(title= "Energy type by country", 
       #subtitle = "bill depth and length", 
       y = "", 
       x = "", 
       fill="",
       caption = "Done by @AnguloBrunet \n #tidyTuesday", colour="")
  
  
ggsave("europeanEnergy.png")
