#35	2020-08-25	Chopped	Kaggle & IMDB	Vice
library(tidyverse)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
fuente <- "Roadway"

dat <- chopped %>%
    select(episode_rating, judge1, judge2, judge3) %>%
    pivot_longer(cols = judge1:judge3)%>%
    filter(episode_rating>=8)

d1 <- dat%>%
    group_by(value)%>%
    count()

dat %>%
    left_join(d1) %>%
    ggplot(aes(x = episode_rating, y = reorder(value,n), shape = name)) +
    geom_jitter() +
    labs(title = "CHOPPED", 
         subtitle = "episode ratings greater than eight and judges", 
         caption = "done by @angulobrunet \n #tidytuesday") +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#FFB033", color = NA),
          panel.background = element_rect(fill = "#FFB033"), 
          panel.grid = element_blank(),
          axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title.y = element_blank(),
          axis.text = element_text(color="#CA4F1D", family=fuente), axis.ticks.y = element_blank(), 
          plot.title = element_text(size = 25, hjust = .5,face = "bold",
                                    color = "#2A0F02", family = fuente), 
          plot.subtitle = element_text(face = "italic", size = 18, 
                                       hjust=0.5, color = "#701210", family = fuente), 
          plot.caption = element_text(face = "italic", size=10,
                                      hjust = 1, family =fuente))
ggsave("chopped.png", width = 8.06 ,height =10 )
