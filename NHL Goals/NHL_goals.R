# Tidytuesday week 10 2020. ##Hockey goals

#Data by  and from HockeyReference.com

library(tidyverse)
library(ggimage)
library(gganimate)

color1<- "#000000"
color2<- "#E4E5E6"
color3<- "#0098D5"

# Get the Data

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')


#In order to select the photo only the first time (season), I create headsh  
  headsh<-  season_goals%>%
    filter(rank <=50)%>% #choose the players in rank 50
    select(player,season,headshot)%>%
    group_by(player)%>%
    filter(season ==min(season))%>% #choose only first season
    distinct() #delate duplicates (created by different teams same season)
  
  
  
  dat<-season_goals%>%
    filter(rank <=50)%>%
    select(rank,player,season,goals,status)%>%
    group_by(rank,player,season, status)%>%
    summarise(total=sum(goals)) #necessary because some players are in more than 1 team per season
  #  left_join(headsh)  #join the photo only in first occurrence

    


  
plot1<-  ggplot()+
    geom_point(data=dat,aes(y=reorder(player, desc(rank)), x=season, size=total, colour=status),alpha=.5)+
    geom_image(data=headsh,aes( y=player, x =season,image=headshot), size=0.05)+ #Añado imagen
      labs(title = " Top 50 Hockey Goals scorers",
         caption= "Done by  @AnguloBrunet \n #tidytuesday \n Data from  HockeyReference.com")+
    xlab("")+ylab("")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, colour=color2),
                  axis.text.y = element_text(colour = color2),
                  plot.title = element_text(face = "bold", 
                                            colour = color2, size = 22, hjust = 0.5), 
                  plot.caption = element_text(face = "bold",
                                            colour = color2, size = 15),
                  plot.background = element_rect(fill = color1, color=NA),
                  panel.background = element_rect(fill = color1),
                  strip.background =element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(), 
                  legend.position="bottom", 
                  legend.background = element_rect(fill=color1,
                                                   size=0.5, linetype="solid"), 
                  legend.text = element_text(face = "bold", 
                                              colour = color2, size = 12, hjust = 0.5),
                  legend.key = element_rect(fill=color1, colour=color1))+
          guides(size = FALSE,colour = guide_legend(override.aes = list(size=10)))+
                scale_color_manual(values = c( color3, "white"))+
    transition_states(season, 
                    transition_length = 1,
                    state_length = 2) +
    labs(subtitle = "Season: {closest_state}" )+
    shadow_mark()
  animate(plot1, nframes = 2 * length(unique(dat$season)),  height = 1748/2, width =2480/2)
  
 
anim_save("HockeyNHL.gif")
 
  
  
``
