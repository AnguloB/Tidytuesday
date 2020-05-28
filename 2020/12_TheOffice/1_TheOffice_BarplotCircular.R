
# Tidytuesday week 12 2020. ##The office

#Data from 
#https://bradlindblad.github.io/schrute/index.html
#https://data.world/anujjain7/the-office-imdb-ratings-dataset
library(tidyverse)

# Get the Data

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-03-17')
tuesdata <- tidytuesdayR::tt_load(2020, week = 12)

# https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html
office_ratings <- tuesdata$office_ratings


##BarPlot

office_ratings<-data.frame(1:nrow(office_ratings), office_ratings, office_ratings$imdb_rating >=9)
names(office_ratings)[1]<- "id" #rename var id
names(office_ratings)[8]<- "condition" # rename var condition
office_ratings<-office_ratings%>%  #use condition to create new var for text
  mutate(titleRE = case_when(condition==TRUE ~ title, 
                             condition ==FALSE ~ ""))


#Syntax from https://www.r-graph-gallery.com/circular-barplot.html
label_data <- office_ratings
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar   
label_data$hjust<-ifelse( angle < -90, 1, 0)

label_data$angle<-ifelse(angle < -90, angle+180, angle)


colortitols<- "#414141" #titlecolor

library(extrafont) # first time run font_import()
loadfonts()

font<-"Courier New" #choose default font 


p<-ggplot(office_ratings, aes(x=as.factor(id), y=imdb_rating, fill=as.factor(season))) + 
  geom_bar(stat="identity") +
  coord_polar(start = 0) +
  ylim(-10,15) +
  scale_fill_brewer(palette="Set3")+
  labs(title = "The Office IMDb",
       subtitle= "Chapters with score equal or greater than 9",
       caption= "Done by  @AnguloBrunet \n #tidytuesday \n Data from https://data.world/anujjain7/the-office-imdb-ratings-dataset")+
  theme_minimal() + 
  theme(text = element_text(family=font),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0,-5,0,-5), "cm"),
    legend.position = "none", 
    plot.title = element_text(face = "bold",
                                 colour = colortitols, size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic",
               colour = colortitols, size = 11, hjust = 0.5))


  p+geom_text(data=label_data, aes(x=id, y=imdb_rating, label=titleRE, hjust=hjust), 
            color="black", family=font,fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, 
            inherit.aes = FALSE ) 
  
  ggsave("1_TheOffice_BarplotCircular.png",width=15, height = 15, units='cm')
  
  