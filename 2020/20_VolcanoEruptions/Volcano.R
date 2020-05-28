
# Tidytuesday week 20 2020. 


library(tidyverse)
library(lubridate)

options(scipen = 999) #prevents scientifical notation in plots
colortitle<-"#34558b" # color for title
colortext<- "#34558b"  #color for text (no title)
#palette1<-c("#EA787A", "#1C6788", "#BED441")
colorlines<-"white"

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Helvetica" #setfont

color1 <-"#C0E8F2" #background color
color2<- "#FA7268"  #color  countries
colorpoint<- "#34558b" 

theme_ari<- function(legpos="bottom", backcolor=backcol){  
  theme(plot.background = element_rect(fill = color1, color=NA),
        panel.background = element_rect(fill = color1), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.background = element_rect(fill=color1,
                                         size=0.5, linetype="solid"), 
        legend.text = element_text( size=10, angle =45, hjust = 1),
        legend.title = element_text(colour="white", size=10, 
                                    face="bold"), 
        axis.line=element_blank())
  
}


#Get data
# Get the Data

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
#eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
#events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
#tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
#sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


#https://github.com/aaumaitre/maps_Spain
library(rgdal) 
library(broom)
sf_regional <- readOGR("maps/Comunidades_Autonomas_ETRS89_30N.shp")
sf_regional<- spTransform(sf_regional , CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
regional_df <- tidy(sf_regional)

temp_df <- data.frame(sf_regional$Texto)
temp_df$id <- as.character(seq(0,nrow(temp_df)-1))

#Joining
regional_df2 <- left_join(regional_df, temp_df, by="id")


data<-volcano%>%
  filter(country=="Spain")

data$last_eruption_year<- factor(data$last_eruption_year, c("Unknown" ,"-3600","40", "1824", "1909", "1971", "2012" ))


canarias<-regional_df2%>%
  filter(long<=-13)%>%
  filter(lat<=30)
canarias_g<-data%>%
  filter(subregion=="Canary Islands")


spain<-regional_df2%>%
  filter(long>=-10)%>%
  filter(lat>=35)
spain_g<-data%>%
  filter(!subregion=="Canary Islands")

p_canarias<-
  ggplot() +
  geom_polygon(data=canarias,aes(x = long, y = lat, group=group),
               fill='white', color="black")+
  geom_label(data=canarias_g, aes(x=longitude, y =latitude, label=last_eruption_year), size=3)+
theme_classic()+theme_ari()+labs(title = "Canary Islands")


p_spain<-
  ggplot() +
  geom_polygon(data=spain,aes(x = long, y = lat, group=group),
               fill='white', color="black")+
  geom_label(data=spain_g, aes(x=longitude, y =latitude, label=last_eruption_year),  size=3)+
  theme_classic()+theme_ari()

library(cowplot)
p1<-plot_grid(NULL,p_spain,  nrow=1, rel_widths = c(0.3,0.7))

p2<-plot_grid(p_canarias,NULL,  nrow=1, rel_widths = c(1, 0.6))

plot1<-plot_grid(p1, p2, nrow=2, rel_heights = c(1,0.5))

data$volcano_name<- factor(data$volcano_name,
                           c("Olot Volcanic Field", "Fuerteventura", "Calatrava Volcanic Field",
                             "Gran Canaria", "Lanzarote","Tenerife", "La Palma",  "Hierro"))

plot2<-data%>%
  ggplot()+
  geom_area(aes(x=volcano_name, y=elevation,group=1),fill="white")+
  geom_label(aes(x=volcano_name, y=0, label=last_eruption_year),  size=3)+
  geom_text(aes(x=volcano_name, y=elevation,group=1,
                label = paste(evidence_category, " \n", major_rock_1)),
            fill="white", size=3)+
  scale_x_discrete(guide = guide_axis(n.dodge=3), expand = expand_scale(add = 1.5))+
  theme(plot.background = element_rect(fill = color1, color=NA),
        panel.background = element_rect(fill = color1), 
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill=color1,
                                         size=0.5, linetype="solid"), 
        legend.text = element_text( size=10, angle =45, hjust = 1),
        legend.title = element_text(colour="white", size=10, 
                                    face="bold"), 
        axis.line=element_blank(), 
        plot.margin = margin(0,0,0,0, "cm"))+labs(y="elevation", x="", caption = "Done by @AnguloBrunet \n #tidytuesday")

title <- ggdraw() + 
  draw_label(
    "    Volcanoes in Spain: last eruption year and characteristics",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) 

plot_grid(title, plot1, plot2, nrow=3, rel_heights =  c(0.1, .45, 0.45) )+
  theme_ari()
ggsave("Volcano.png", units="cm", height = 30*0.75, width = 21*0.90)

  