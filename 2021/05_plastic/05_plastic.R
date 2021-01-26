library(tidyverse)
# Get the Data

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

library(extrafont) # first time run font_import()
loadfonts()
font <- "Cosmos Logic Demo Version"

plastics %>%
    mutate(parent_company= recode(parent_company, "P&G" = "PROCTER & GAMBLE")) %>%
    mutate(parent_company = toupper(parent_company)) %>%
    
    filter(!parent_company %in% c( "GRAND TOTAL", "NULL", "UNBRANDED", "ASSORTED", "LA DOO")) %>%
    group_by(year, parent_company) %>%
    summarise(total = sum(grand_total, na.rm = TRUE)) %>%
    top_n(10) %>%
    group_by(year) %>%
    arrange(desc(total), .by_group = TRUE) %>%
    ungroup() %>%
    mutate(ordre= c(1:10, 1:10))-> dat1

plastics %>%
    mutate(parent_company= recode(parent_company, "P&G" = "PROCTER & GAMBLE")) %>%
    mutate(parent_company = toupper(parent_company)) %>%
    
    filter(!parent_company %in% c( "GRAND TOTAL", "NULL", "UNBRANDED", "ASSORTED", "LA DOO")) %>%
    filter(!parent_company %in% dat1$parent_company)%>%
    group_by(year, parent_company) %>%
    summarise(total = sum(grand_total, na.rm = TRUE))%>%
    mutate(ordre = sample(seq(from=0, to=10, by=.01), n(), replace = TRUE))%>%
    mutate(coolor = sample(1:10, n(), replace = TRUE))-> dat2


    ggplot() +
    geom_point(data=dat1, 
               aes(x=ordre, y = total),color="blue" )+
    geom_text(data=dat1, 
              aes(x=ordre, y = total, label= parent_company), 
              angle=90, size=6, vjust =.5, hjust=0, color ="#D91C70", family= font) +
        geom_jitter(data=dat2, 
                   aes(x=ordre, y = total, color=coolor))+
        scale_color_gradient2(low= "yellow", high = "blue", mid = 'red')+
    ylim(0,18000)+
    facet_wrap(~year)+ 
        labs(title= "Grand total count (all types of plastic) by source of plastic" ,
             subtitle = "Top 10 parent company by year",
             caption = "DONE BY @ANGULOBRUNET \n #tidytuesday", 
             x="", y = "Grand Total")+
        theme(text=element_text(family = font, size = 30, color = "white"),
              legend.position = "none",
              plot.background =element_rect(fill = "#2C363F"),
              panel.background = element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(), 
              axis.ticks.x = element_blank(), 
              axis.text.y = element_text(colour = "white"),
              plot.title = element_text(size=30, hjust=.5,face="bold", color="white", family = font), 
              plot.subtitle = element_text(face="italic", size=20, hjust=.5, color = "white"), 
              plot.caption = element_text( face="italic", size=20, hjust = 1, color = "white"), 
              strip.background =element_blank(),
              strip.text = element_text(colour = 'white', size = 40))
ggsave("05_plastic.png")
