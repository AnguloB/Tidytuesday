#25	2020-06-16	African-American History	Black Past & Census & Slave Voyages	The Guardian
color1 <-"#474350" #background color
color2<- "#FA7268"  #color  countries


library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "DRAGONS AND CHICKENS" #Fuente que voy a utlizar

theme_ari<- function(){
  theme_bw()+
    theme(
      text=element_text( color="white", family=font),
      
      plot.background = element_rect(fill = color1, color=NA),
          panel.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
      axis.text.x = element_text(color="white"),
      axis.text.y = element_blank(), 
      plot.title = element_text(size=20, hjust=0,face="bold", color="#fadd9e"), 
      plot.subtitle = element_text(face="italic", size=15, hjust=0, color="#f45b69"), 
      plot.caption = element_text( face="italic", size=12, hjust = 1,color= "#f45b69"), 
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

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
#census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
#slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
#african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')



v1<-str_detect(blackpast$events, c("Spain", 'Spanish'))
v2<-str_detect(blackpast$subject, c("Spain", 'Spanish'))
v3<-str_detect(blackpast$country, c("Spain", 'Spanish'))
decision<-v1+v2+v3

library(stringi)
blackpast%>%
  filter(decision>=1)%>%
  mutate(labels= gsub('(.{1,10})(\\events|$)', '\\1\n', events))%>%
  mutate(labelss= str_wrap(events, width = 70))%>%
  mutate(id= c(4:1, -1:-4))%>%
  ggplot(aes(x=as.numeric(year), y =id, label=labelss, color=events))+
  geom_text(size=3.5, vjust="inward",hjust="inward", family=font )+
  scale_color_manual(values=paleta)+
  theme_ari()+
    theme(legend.position = "none")+
  labs(x="year", y = "" ,
       caption="Done by @AnguloBrunet \n #tidytuesday", 
       title= "African-American History", 
       subtitle= "Spain and the history of African America and of people of African ancestry")
  

ggsave("africanHistory.png")


