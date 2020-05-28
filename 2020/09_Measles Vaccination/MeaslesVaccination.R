# Tidytuesday week 9 2020. ##Measles

#Data by The Wallstreet Journal 
#Data from  https://github.com/WSJ/measles-data

library(tidyverse)
library(maps)
#Define colors
color1<- "#0f4c81" #panteone color 2020
color2<-"lightskyblue"
color3<-"#FA7268"

# Get the Data

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
measles$overall[measles$overall==-1]<-NA  #I think missing values sometimes are a -1

measles$mmr[measles$mmr==-1]<-NA


names(measles)[7]<-"subregion"
measles$subregion<-str_to_lower(measles$subregion, locale = "en")

county <- map_data("county")
#from maps

meas1<-measles%>% 
  group_by(subregion)%>%
  select(subregion, overall)%>%
  summarise(overall=mean(overall, na.rm=TRUE))
  distinct()
  

joined<-county%>%
  full_join(meas1, "subregion")

joined%>%
ggplot() + 
  geom_polygon( aes(x = long, y = lat,  group = group, fill=overall), color = color2, size=.1) + 
  scale_fill_gradient(low = color3, high = color1,
                      name = "School's overall \n vaccination rate", na.value = "grey70")+
  theme(plot.background = element_rect(fill = color2, color=NA), 
        panel.background = element_rect(fill = color2), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position="right", 
        legend.background = element_rect(fill=color2,
                                         size=0.5, linetype="solid"), 
        legend.text = element_text(colour=color1, size=10, angle =45, hjust = 1),
        legend.title = element_text(colour=color1, size=10, 
                                    face="bold"), 
        axis.line=element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
labs(title = "School's overall vaccination rate by subregions",
     caption= "Done by  @AnguloBrunet 
      github.com/AnguloB/Tidytuesday
      Data from https://github.com/WSJ/measles-data
     #tidytuesday")




ggsave("MeaslesVaccination.png")



