#27	2020-06-30	Claremont Run of X-Men	Claremont Run	Wikipedia - Uncanny X-Men

library(tidyverse)
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')
backcolor<-"#eee5e5"
theme_ari<- function(){ #defino plot para los dos graficos
  theme(plot.background = element_rect(fill = backcolor, color=backcolor), 
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#ff6f59"), 
        plot.subtitle = element_text(face="italic", size=12), 
        legend.position = "bottom")}

options( scipen=999)
df<-characters%>%
  filter(number_of_kills_humans>=1)%>%
  group_by(character)%>%
  summarise(Freq=sum(number_of_kills_humans))%>%
  mutate(prop=round(Freq/sum(Freq)*100,0))


ndeep <- 10


tb4waffles <- expand.grid(y = 1:ndeep,
                          x = seq_len(ceiling(sum(df$prop) / ndeep)))

regionvec <- rep(df$character, df$prop)

tb4waffles$region <- c(regionvec, rep(NA, nrow(tb4waffles) - length(regionvec)))


palette30<-c("#37392e","#19647e","#28afb0","#f6e27f","#e2c391","#ff6f59",
  "#f62dae","#b30089","#470063","#af3800","#F5A6E6")

filter(tb4waffles, !is.na(region)) %>% 
  

ggplot( aes(x = x, y = y, fill = region)) + 
  geom_tile(color = "white") +theme_ari()+
  scale_fill_manual(values=palette30)+
  labs(title= "X-men: Who killed the humans?", 
       subtitle = "Total of 27 humans killed", 
       x="",
       y="",
       fill="",
       caption = "Done by @AnguloBrunet \n #TidyTuesday" )
ggsave("xmen.png")


