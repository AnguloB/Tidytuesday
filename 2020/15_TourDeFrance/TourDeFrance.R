
# Tidytuesday week 15 2020. #Tour de france
#Data from https://github.com/alastairrushworth/tdf

library(tidyverse)

# Get the Data

options(scipen = 999) #prevents scientifical notation in plots
backcol<- "black"

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Trebuchet MS" #setfont

color1 <-"black" #background color
color2<- "black"  #color  countries
colorpoint<- "#EEDE03" 

theme_ari<- function(legpos="bottom", backcolor=backcol){  
  theme(text=element_text(family = font, color="ivory1"),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = backcolor),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(color= colorpoint, size=20, hjust=.5,face="bold"), 
        plot.subtitle = element_text(face="italic", size=10), 
        plot.caption = element_text( face="italic"), 
        axis.text.x = element_text(angle = 90, hjust = 1, color="ivory1"),
        axis.text.y = element_text(color="ivory2"))
}



tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

#substract first character of nationality (there is an empty space)
tdf_winners$nationality<-substring(tdf_winners$nationality, 2) 



tdf_winners<-tdf_winners %>%
  mutate(year= lubridate::year(start_date))%>% #create variable year from date
  filter(year >=1969)    #filter laste 50 years
 

  p1<-ggplot(data=tdf_winners,aes(x= factor(year), y=winner_name, fill=age))+
    geom_tile()+scale_fill_gradient(low="white", high = colorpoint)+ # create heatmap
  theme_ari()+ xlab("")+ylab("")+
    labs(title = "Tour the France: winners of the last 50 years")
  

  
  centres<-mapp %>%  #look for the center of each 
    group_by(region)%>%
    summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))
  
    
  results_m1<- tdf_winners%>%
    mutate(region=case_when(
              nationality=="Great Britain"~ "UK",
              nationality=="United States"~ "USA", 
              TRUE ~ .$nationality)) %>%#keep values from nationality
    group_by(region)%>%
    count()%>%
    left_join(centres) # join centres of region 
  
  #there is a mistake in the centers of USA. Set values by hand
  results_m1$long1[results_m1$region=="USA"]<- -96.4247
  results_m1$lat1[results_m1$region=="USA"]<-  31.51073
    
  
library(maps)
  mapp<-map_data("world")%>%
    filter(lat>=-56)  # delate antartica from map 
  
  # table(tdf_winners$nationality) 
  #Find country names that are not in mapps -> Great Britain, United States
  #names(table(tdf_winners$nationality))%in% mapp$region
  
  m1<-ggplot()+
    geom_polygon(data=mapp,aes(x = long, y = lat, group = group),
                 fill=color2, color="ivory1")+
    geom_point(data=results_m1, aes(x=long1, y=lat1, label=n, size=n), color=colorpoint)+
    geom_text(data=results_m1, aes(x=long1, y=lat1, label=n))+
    theme_ari()+
    theme(plot.background = element_rect(fill = color1, color=NA),
          panel.background = element_rect(fill = color1), 
          axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position="none", 
          legend.background = element_rect(fill=color1,
                                           size=0.5, linetype="solid"), 
          legend.text = element_text( size=10, angle =45, hjust = 1),
          legend.title = element_text(colour="white", size=10, 
                                      face="bold"), 
          axis.line=element_blank())+
             scale_size_continuous(range=c(2,5))+
    labs(subtitle = "Nationality of winners", 
         caption="Done by @AnguloBrunet \n #tidytuesday")
  
library(cowplot)
  
plot_grid(p1, m1, nrow=2)  #save
ggsave("TourdeFrance.png")


  
  
  
