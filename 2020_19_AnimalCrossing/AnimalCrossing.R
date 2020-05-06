
# Tidytuesday week 19 2020. 
##Animal Crossing
#Data from 
# https://www.metacritic.com/game/switch/animal-crossing-new-horizons/critic-reviews
# https://github.com/jefflomacy/villagerdb

library(tidyverse)
library(lubridate)
logo<-"https://translate.google.es/?hl=es&tab=iT&authuser=0"


options(scipen = 999) #prevents scientifical notation in plots
backcol<- "#F5F6D6"  #background color
colortitle<-"#34558b" # color for title
colortext<- "#1271A1"  #color for text (no title)

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Lato Light"  #setfont

color1 <-"#ECBF7B" #background color




theme_ari<- function(legpos="bottom", backcolor=backcol){  
  theme(text=element_text(family = font, color=colortext),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color=colortext), 
        plot.subtitle = element_text(face="italic", size=15, hjust=.5), 
        plot.caption = element_text( face="italic", size=14, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext))
}


#Get data
# Get the Data

#critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
#user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
#items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')


villagers<-villagers%>%
  mutate(speciesRE=  #recode species into generic
           case_when(
             species ==   "alligator"~ "reptile",  
             species ==   "anteater"~"mammal", 
             species ==   "bear" ~"mammal",
             species ==   "bird" ~"bird", 
             species ==   "bull" ~ "mammal",
             species ==   "cat" ~ "mammal", 
             species ==   "cow"~ "mammal",   
             species ==   "cub" ~ "mammal", 
             species ==   "deer"~ "mammal",
             species ==   "dog" ~ "mammal",
             species ==   "duck" ~"bird", 
             species ==   "eagle"  ~"bird",  
             species ==   "frog" ~"amphibian", 
             species ==   "goat"  ~ "mammal",   
             species ==   "gorilla" ~ "mammal",
             species ==   "hamster"~"mammal",  
             species ==   "hippo"~"mammal",  
             species ==   "horse"  ~"mammal",  
             species ==  "koala"  ~"mammal", 
             species ==  "lion"  ~"mammal",   
             species ==  "monkey"   ~"mammal",
             species ==  "mouse"   ~"mammal",  
             species ==  "octopus" ~"invertebrate", 
             species ==  "ostrich" ~"bird",  
             species ==  "pig"  ~ "mammal",  
             species ==  "rabbit" ~"mammal",  
             species ==  "rhino"  ~"mammal",  
             species ==  "sheep"  ~ "mammal",  
             species ==  "squirrel" ~"mammal", 
             species ==  "tiger" ~"mammal",    
             species ==  "chicken" ~"bird", 
             species ==  "elephant" ~ "mammal", 
             species ==  "kangaroo"~"mammal", 
             species ==  "penguin" ~"bird",
             species ==  "wolf"    ~ "mammal"))%>%
  mutate(speciesRE2=case_when(  #recode to reagroup species
             speciesRE == "mammal" ~ "mammal",
             speciesRE == "bird" ~ "bird",
             speciesRE == "invertebrate"~ "other",
             speciesRE == "amphibian" ~"other", 
             speciesRE == "reptile" ~"other"))

# remotes::install_github("corybrunson/ggalluvial")
library(ggalluvial)
p1<-villagers%>%
  group_by( speciesRE2,gender, personality)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
ggplot(aes(axis1 = reorder(speciesRE2,n), axis2 = reorder(gender, -n), axis3 = reorder(personality,n),
           y = n)) +
  scale_x_discrete(limits = c("", "", ""), expand = c(.1, .05)) + #remove s acis
  geom_stratum(aes(fill = speciesRE2), color="#B3B39C") + #change background of squares
  geom_alluvium(aes(fill = speciesRE2)) +geom_text(stat = "stratum", infer.label = TRUE) +
  theme_minimal() +theme_ari(legpos = "none")+ #remove leegend
  scale_fill_manual(values= c( "#FF8395", "#79C5D7", "#6B246C" ), na.value="#B3B39C")+ #set manual colors
  labs(x="" ,caption= "Done by @AnguloBrunet \n #tidytuesday")


## heading
library(cowplot)
library(magick)
img <- png::readPNG("logo.png") #read image in directory (logo)

top_1<-ggdraw() +  #create plot with logo
  draw_image(img)+theme_ari() 

library(ggimage)
#create image with random villagers
set.seed(2019)
top_2<-data.frame(x= sample(1:18, 16, replace = TRUE),  #create random numbers for x axis
           y=sample(8, 16, replace=TRUE), #create randomn numbers for y axis
           url=sample(villagers$url, 16))%>% #choose 16 random urls from villagers
  ggplot(aes(x=x, y=y, image=url))+ #create plot with images
  geom_image(size=.10)+theme_minimal()+theme_ari()+
  labs(x="", y="")+
  scale_y_continuous(breaks=NULL, limits = c(0,10))+
  scale_x_continuous(breaks=NULL)

 
top<-plot_grid(top_1, top_2, nrow = 1, rel_widths = c(0.4, 0.5))

plot_grid(top, p1 ,nrow=2, rel_heights = c(0.3,0.7))


ggsave("AnimalCrossing.png", units="cm", height = 20, width = 21)

  
  
  