

# Tidytuesday week 8 2020. 

#Data by Kasia Kulma from  https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018
#Data from https://r-tastic.co.uk/post/from-messy-to-tidy/


library(tidyverse)

#https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-18/readme.md
# Get the Data
food_consumption   <-   readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

#Define filters and recodes
#Recode food category into animal or noanimal cateogry

food_consumption$food_group   <-   dplyr::recode(food_consumption$food_category, 
                                             `Pork` = "Animal", 
                                             `Poultry` = "Animal", 
                                             `Beef` = "Animal", 
                                             `Lamb & Goat` = "Animal", 
                                             `Fish` = "Animal", 
                                             `Eggs` = "Animal", 
                                             `Milk - inc. cheese` = "Animal", 
                                             `Wheat and Wheat Products` = "Non-animal",
                                             `Rice` = "Non-animal",
                                             `Soybeans` = "Non-animal",
                                             `Nuts inc. Peanut Butter` = "Non-animal")

#Europe and UK filter
europe <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
            "Czech Republic","Denmark","Estonia","Finland",
            "France","Germany","Greece","Hungary","Ireland","Italy",
            "Latvia","Lithuania","Luxembourg","Malta",
            "Netherlands", "Poland", "Portugal","Romania",
            "Slovakia","Slovenia","Spain","Sweden",
            "United Kingdom")
#keep only european countries defined before in food_consumption data
europe_consumption  <-  food_consumption %>%
  filter(country %in% europe)

#keep only european countries defined before in world_map
mapp  <-  map_data("world")
mapp$region  <-  recode(mapp$region, UK = "United Kingdom")

europe_map  <-  mapp %>% 
  filter(region %in% europe)


# Assessing consumption
dataConsumption  <-  europe_consumption %>%
  group_by(country, food_group) %>% #group by country and food_group
  summarise(sum = sum(consumption)) %>% #summ all animal and nonanimal food
  pivot_wider(names_from = food_group, values_from = sum) %>% #reshape data into wider 
  mutate(total = Animal + `Non-animal`) %>% #compute total of kgs consumption per person
  mutate(percnonAnimal = (`Non-animal`/total)*100) %>% #compute % of nonanimal consumption
  mutate(percAnimal = (Animal/total)*100) #compute % of animal consumption

names(dataConsumption)[1] <- "region" # recode var country intro "region" 
#(needed to left_join with) europe_map

europe_consumption_map <- europe_map %>%
  left_join(dataConsumption)


plot1 <- europe_consumption_map %>%
  ggplot(aes(x = long, y = lat, group = group, fill = percnonAnimal)) +
  geom_polygon(colour = "#414141")+
  theme_classic()+
  scale_fill_gradient(low = "white", high = "#63C91A",
                      name = "% of Non-animal consumption  \n (kg/person/year)") +
  theme(plot.background = element_rect(fill = "#414141", color = NA),
        panel.background = element_rect(fill = "#414141"), 
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.background = element_rect(fill = "#414141",
                                         size = 0.5, linetype = "solid"), 
        legend.text = element_text(colour = "white", size = 10, angle  = 45, hjust = 1),
        legend.title = element_text(colour = "white", size = 10, 
                                    face = "bold"), 
        axis.line = element_blank())


# Assessing CO2

dataConsumption <- europe_consumption %>%
  group_by(country, food_group) %>% #group by country and food_group
  summarise(sum = sum(consumption)) %>% #summ all animal and nonanimal food
  pivot_wider(names_from = food_group, values_from = sum) %>% #reshape data into wider 
  mutate(total = Animal + `Non-animal`) %>% #compute total of kgs consumption per person
  mutate(percnonAnimal = (`Non-animal`/total)*100) %>% #compute % of nonanimal consumption
  mutate(percAnimal = (Animal/total)*100) #compute % of animal consumption

dataCO2 <- europe_consumption %>%
  group_by(country, food_group) %>% #group by country and food_group
  summarise(sum = sum(co2_emmission)) %>% #summ all animal and nonanimal food
  pivot_wider(names_from = food_group, values_from = sum) %>% #reshape data into wider 
  mutate(total = Animal + `Non-animal`) %>% #compute total of kgs consumption per person
  mutate(percnonAnimal = (`Non-animal`/total)*100) %>% #compute % of nonanimal consumption
  mutate(percAnimal = (Animal/total)*100) #compute % of animal consumption

names(dataCO2)[1] <- "region"

europe_CO2_map <- europe_map %>%
  left_join(dataCO2)

plot2 <- europe_CO2_map %>%
  ggplot(aes(x = long, y = lat, group = group, fill = total)) +
  geom_polygon(colour = "#414141") +
  theme_classic()+
  scale_fill_gradient(low = "white", high = "#FA7268",
                      name = "Total CO2 emission \n (kg CO2/person/year)") +
  theme(plot.background = element_rect(fill = "#414141", color = NA), 
        panel.background = element_rect(fill = "#414141"), 
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "bottom", 
        legend.background = element_rect(fill = "#414141",
                                         size = 0.5, linetype = "solid"), 
        legend.text = element_text(colour = "white", size = 10, angle  = 45, hjust = 1),
        legend.title = element_text(colour = "white", size = 10, 
                                    face = "bold"), 
        axis.line = element_blank())

library(cowplot)

#grid two plots
plot_row <- plot_grid(plot1, plot2, ncol = 2, nrow = 1)

#In order to add titles I used
#https://wilkelab.org/cowplot/articles/plot_grid.html
##Title
title  <-  ggdraw() + 
  draw_label(
    "Europe food consumption & CO2 emissions",
    fontface = 'bold', x = 0,hjust = 0,color = "grey80" ) +
  theme(plot.margin = margin(0, 0, 0, 0), 
        panel.background = element_rect(fill = "#414141", color = "#414141"))
##Footnote
footnote <-  ggdraw() + 
  draw_label(
    "*Only European Union countries and UK were included.\n 
    **Data from https://r-tastic.co.uk/post/from-messy-to-tidy/ \n by @AnguloBrunet",
    fontface = 'italic', x = 0,hjust = 0,color = "grey73" , size = 10) +
  theme(plot.margin = margin(0, 0, 0, 0), 
        panel.background = element_rect(fill = "#414141", color = "#414141"))

plot_grid(
  title, plot_row, footnote,ncol = 1,  rel_heights = c(0.1, 1, 0.1))
ggsave("EuropeConsumption.png")


