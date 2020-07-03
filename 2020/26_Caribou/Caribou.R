#26	2020-06-23	Caribou Locations	Movebank	B.C. Ministry of Environment

color1 <-"bisque3" #background color
color2<- "black"  #color  countries

library(tidyverse)
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Helvetica" #Fuente que voy a utlizar

theme_ari<- function(){
  theme_bw()+
    theme(
      text=element_text( color="#FA7268", family=font),
      
      plot.background = element_rect(fill = color1, color=NA),
      panel.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      plot.title = element_text(size=20, hjust=0,face="bold", color="black"), 
      plot.subtitle = element_text(face="italic", size=15, hjust=0, color="black"), 
      plot.caption = element_text( face="italic", size=12, hjust = 1,color= "black"), 
      legend.background = element_rect(fill=color1,
                                       size=0.5, linetype="solid"), 
      legend.title = element_text(colour="white", size=10, 
                                  face="bold"), 
      axis.line=element_blank(), 
      legend.position = "bottom")
  
}

# Get the Data


paleta<- c("#cda2ab","#fadd9e","#6b9ac4","#97d8c4","#f4b942","#f45b69","#dc6acf","#efc3e6")


# Get the Data
# Get the Data

individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

#id prenant caribous
preg<-filter(individuals, pregnant==TRUE)$animal_id

pregnant_individuals<-individuals%>%
  filter(animal_id %in% preg)

pregnant_locations<- locations%>%
  filter(animal_id %in% preg)%>%
  filter(study_site=="Kennedy")%>%
  filter(animal_id== "KE_car024")



library(maps)
mapp<-map_data("world")%>%
  filter(region=="Canada")
#filter(long<=-110)%>%
#filter(lat<=56)%>%
#filter(lat>=47.5)

library(gganimate)


library(rgdal) #to import shapefiles
library(broom) #to convert shapefiles into the data frame structure we need
shp_can <- readOGR("vec/waterbody_2.shp") #Provincia
map_can <- spTransform(shp_can , CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))



#Converting it to df:
can_df <- tidy(map_can)


plot<-ggplot()+
  geom_polygon(data=can_df,aes(x = long, y = lat, group = group))+
  geom_point(data=pregnant_locations,aes(x=longitude,y =latitude, color= "#FA7268"))+
  theme_ari()+
  labs(title="Movements of KE_car024", caption= "Done by @AnguloBrunet", 
       x="", y="")+
  theme(legend.position = "none")+
  transition_time(timestamp) +
  ease_aes('linear')+
  labs(title = 'Day: {frame_time}', x = '', y = '') 

p1<-animate(plot, height = 565.44, width=823.68, duration=100) 


anim_save("caribou.gif", p1)

