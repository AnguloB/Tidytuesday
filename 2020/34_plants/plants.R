#34	2020-08-18	Extinct Plants	IUCN Red List	Florent Lavergne infographic\
# Or read in the data manually
library(tidyverse)
library(ggimage)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
#actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
#threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

#function to get images direct from google images from a text

library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
fuente <- "Trampoline demo"

getImage <- function(y){
    require(rvest)
    require(dplyr)
    findImage <- function(x){
        
        #text<-x
        text <- stringr::str_replace(x, " ", "+")
        
        url <- paste0("https://www.google.com/search?tbm=isch&as_q=",text, "&tbs=isz:lt")
        webpage <- html_session(url)
        link.titles <- webpage %>% html_nodes("img")
        img.url <- link.titles[5] %>% html_attr("src")
        res <- c(text, img.url)
    }
    
    results <- data.frame(do.call(rbind,lapply(y,findImage)))
    names(results) <- c("what", "imagelink")
    results$what <- stringr::str_replace(results$what, '\\+', " ")
    
    return(results)
}


eur <- plants %>%
    filter(continent == "Europe")

resultats <- getImage(eur$binomial_name)
names(resultats)[1] <- "binomial_name"



names(eur)[2] <- "region"
names(eur)[4] <- "planttype"
#### europe map 
#keep only european countries defined before in world_map
mapp <- map_data("world")
mapp$region <- recode(mapp$region, UK = "United Kingdom")

europe_map <- mapp %>%
    filter(region %in% c(unique(eur$region)))


eur <- eur %>%
    left_join(resultats)

europe_map <- europe_map %>%
    left_join(eur)

centros_g <- europe_map %>%  #look for the center of each 
    group_by(region) %>%
    summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))

eur <- eur %>%
    left_join(centros_g)
#portugal -7.846118	39.57141
eur[eur$binomial_name == "Euphrasia mendoncae",26] <- -11
eur[eur$binomial_name == "Euphrasia mendoncae",27] <- 42


eur[eur$binomial_name == "Fissidens microstictus",26] <- -8
#eur[eur$binomial_name=="Fissidens microstictus",26]<- -12

eur[eur$binomial_name == "Nobregaea latinervis",26] <- -11
eur[eur$binomial_name == "Nobregaea latinervis",27] <- 38


#spain 
#Astragalus nitidiflorus
#Kunkeliella psilotoclada
#Lysimachia minoricensis	
#-2.456788	39.89524
eur[eur$binomial_name == "Astragalus nitidiflorus",27] <- 42
eur[eur$binomial_name == "Astragalus nitidiflorus",26] <- -4

eur[eur$binomial_name == "Kunkeliella psilotoclada",26] <- 1
eur[eur$binomial_name == "Kunkeliella psilotoclada",27] <- 39.5
eur[eur$binomial_name == "Lysimachia minoricensis",27] <- 37
paleta <- c("#eaf8f3","#efe1fc","#f1cae6","#f0eedd","#d4e9fc",
            "#eaa9a7","#b2a1ff","#b9f3cd","#c2c2c2")


ggplot() +
    geom_polygon(data = europe_map, aes(x = long, y = lat,
                                        group = group, fill = region, colour = region))+
    theme_bw()+
    geom_image(data = eur,
               aes( x = long1, y = lat1-1.5,image = imagelink),
               size = 0.05)+ #Añado imagen
    geom_text(data = eur,
              aes( x = long1, y = lat1,label = str_replace_all(binomial_name, " " , "\n")),
              family = fuente) + #Añado imagen+
    scale_fill_manual(values = paleta) +
    scale_colour_manual(values = paleta) +
    labs(title = "Extinct plants originary from Europe", 
         subtitle = "Bromus bromoideus, Bromus interruptus and Lysimachia minoricensis are 'only' extinct in wild", 
         caption = "Done by @AnguloBrunet \n #tidyTuesday") +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#fef9ef", color = NA),
          panel.background = element_rect(fill = "#fef9ef"), 
          panel.grid = element_blank(),
          axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
          plot.title = element_text(size = 25, hjust = .5,face = "bold",
                                    color = "black", family = fuente), 
          plot.subtitle = element_text(face = "italic", size = 18, 
                                       hjust=0.5, color = "grey", family = fuente), 
          plot.caption = element_text(face = "italic", size=10,
                                      hjust = 1, family =fuente))
ggsave ("plants.png")               
    