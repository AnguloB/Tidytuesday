#31	2020-07-28	Palmer Penguins	Gorman, Williams and Fraser, 2014	Palmer Penguins
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Love Monster Sketched" #Fuente que voy a utlizar
backcolor<-"#2D3142"
colortext<-"white"
palette30<-c("#FF5A5F","#F7B801",  "#0E7C7B")



library(tidyverse)
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

penguins_raw<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')

penguins%>%
ggplot(aes(x= bill_length_mm, y = bill_depth_mm))+ 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=.5)+
  geom_point(aes(x= bill_length_mm, y = bill_depth_mm, color=species))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=FALSE)+
  scale_fill_gradient(low= "white", high="#57B8FF")+
  theme_bw()+
  theme(text=element_text(family = font, size=12, color=colortext),
        plot.background = element_rect(fill = backcolor),
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=25, hjust=.5,face="bold", color="#F7D6E0"), 
        plot.subtitle = element_text(face="italic", size=18, hjust=0, color="#F7D6E0"), 
        plot.caption = element_text( face="italic", size=10, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext), 
        legend.position = "bottom")+
  scale_color_manual(name="",values = palette30) +
  labs(title= "Penguins", 
       #subtitle = "bill depth and length", 
       y = "Bill depth (mm)", 
       x = "Bill length (mm)", 
       caption = "Done by @AnguloBrunet \n #tidyTuesday", colour="")
ggsave("penguins.png")
