
library(tidyverse)
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
#scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

backcol<-"grey22"
library(extrafont)
loadfonts()
font<- "Love Monster Sketched" #Fuente que voy a utlizar

theme_ari<-function(){
  theme(text=element_text(family = font, size=12),
    plot.background = element_rect(fill = backcol, colour=backcol), 
        panel.background = element_rect(fill = backcol, colour=backcol), 
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 
  }

library(tidytext)

textt<-avatar%>%
  select(book, full_text)%>%
  unnest_tokens(word, full_text)%>%
  group_by(book)%>%
  anti_join(stop_words, by = c("word" = "word"))%>%
  count(word, sort=TRUE)%>%
  mutate(angle = 90 * sample(c(0, 1), n(), 
                             replace = TRUE, prob = c(60, 40)))

library(ggwordcloud) # for doing wordclouds

wat<-textt%>%
  filter(book=="Water")%>%
  filter(n>=100)%>%
  ggplot(aes(label = word, size=n, angle=angle, color=n))+
  geom_text_wordcloud(shape="square", family=font) +
  # scale_size_area(max_size = 10)+
  scale_radius(range=c(1,15), limits=c(0,NA))+
  scale_color_gradient(low= "#A8BCFF", high="#4141F0")+
 theme_ari()+labs(title="Water")+
  theme(plot.title = element_text(size=30, hjust=0.5,face="bold", color="#4141F0"))


fir<-textt%>%
  filter(book=="Fire")%>%
  filter(n>=100)%>%
  ggplot(aes(label = word, size=n, angle=angle, color=n))+
  geom_text_wordcloud(shape="square", family=font) +
  # scale_size_area(max_size = 10)+
  scale_radius(range=c(1,15), limits=c(0,NA))+
  scale_color_gradient(low= "#E8B7AE", high="#E86231")+
  theme_ari()+labs(title="Fire")+
  theme(plot.title = element_text(size=30, hjust=0.5,face="bold", color="#E86231"))



ear<-textt%>%
  filter(book=="Earth")%>%
  filter(n>=100)%>%
  ggplot(aes(label = word, size=n, angle=angle, color=n))+
  geom_text_wordcloud(shape="square", family=font) +
  # scale_size_area(max_size = 10)+
  scale_radius(range=c(1,15), limits=c(0,NA))+
  scale_color_gradient(low= "#9BFAAB", high="#41B011")+
  theme_ari()+labs(title="Earth", caption="Done by @AnguloBrunet")+
  theme(plot.title = element_text(size=30, hjust=0.5,face="bold", color="#41B011"), 
        plot.caption = element_text(size=20, hjust=0.5,face="bold", color="white"))



library(cowplot)

row2<-plot_grid(wat, fir, ear, nrow = 1)


all<-textt%>%
  group_by(word)%>%
  summarise(n=sum(n))%>%
  mutate(angle = 90 * sample(c(0, 1), n(), 
                             replace = TRUE, prob = c(60, 40)))%>%
  filter(n>=25)%>%
  ggplot(aes(label = word, size=n, angle=angle, color=n))+
  geom_text_wordcloud(shape="square", family=font) +
  # scale_size_area(max_size = 10)+
  scale_radius(range=c(1,15), limits=c(0,NA))+
  scale_color_gradient(low= "#EAD8A1", high="#CB8531")+
  theme_ari()+labs(title="Avatar: The Last Airbender")+
  theme(plot.title = element_text(size=40, hjust=0.5,face="bold", color="#EFEFEF"))



plot<- plot_grid(all, row2, nrow=2, rel_heights = c(.7,.3)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"), 
        plot.background = element_rect(
          colour = "#EFEFEF",
          size = .2))
  

ggsave("avatar.png", width=23, height=23.5, units="cm")



