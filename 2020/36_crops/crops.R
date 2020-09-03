# 36	2020-09-01	Global Crop Yields	Our World in Data	Our World in Data
library(tidyverse)
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
#fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
#tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
#land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
#arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')

library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font <- "Sketchica"

library(ggTimeSeries)
backcolor <- "grey95"
colortext <- "black"
paleta <- c("#ead94c","#0a122a","#7c0b2b","#698f3f",
          "#92d5e6","#dd7373","#7371fc","#edf67d","#745c97")

key_crop_yields %>%
    filter(Entity == "Spain") %>%
    select(-Entity, -Code) %>%
    pivot_longer(cols = -Year) %>%
    mutate(name = str_replace(str_remove(name, "(tonnes per hectare)"),"\\(\\)","")) %>%
    na.omit() %>%
    ggplot(aes(x = Year,
               y = value,
               group = name, 
               fill = name)) + 
    stat_steamgraph() +
    scale_fill_manual(values = paleta) +
    labs(title = "Product yield in Spain", 
         subtitle = "Tonnes per hectare", 
         caption = "done by @angulobrunet \n #tidytuesday", 
         y="", fill = "") +
    theme(text=element_text(family = font, size = 12, color = colortext),
          plot.background = element_rect(fill = backcolor),
          panel.background = element_rect(fill = backcolor),
          strip.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.background = element_rect(fill= backcolor,
                                           size = 0.5, linetype = "solid"),
          plot.title = element_text(size = 20, hjust = 0.5,face = "bold", color = "black"), 
          plot.subtitle = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"), 
          plot.caption = element_text( face = "italic", size = 10, hjust = 1), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(color = colortext), 
          legend.position = "bottom")
ggsave("crops.png")
