
# Tidytuesday week 12 2020. ##The office

#Data from schrute package

library(schrute)
library(tibble)
library(tidyverse)
library(ggwordcloud)

library(extrafont) # first time run font_import()
loadfonts()

font<-"Courier New" #choose default font 



#code mostly from
#vignette("theoffice")
mydata <- schrute::theoffice

stop_words <- tidytext::stop_words
mydata$season<- paste("Season", mydata$season)
text<-mydata %>%
  group_by(season)%>%
  tidytext::unnest_tokens(word, text)%>%
  dplyr::anti_join(stop_words, by = "word")%>%
  dplyr::count(word, sort = TRUE) %>%
 top_n(10)

colortitols<- "#414141" #titlecolor

  

set.seed(2020)
text%>%
  ggplot( aes(label = word, size=n,     
              color = factor(sample.int(10, nrow(text), replace = TRUE)))) +
  geom_text_wordcloud(shape="square", family=font) +
  scale_color_brewer(palette="Paired")+
  facet_wrap(~season)+theme_minimal()+
  labs(title = "The Office",
       subtitle= "Top 10 words by season",
       caption= "Done by  @AnguloBrunet \n #tidytuesday \n Data from package schrute")+
  theme(text = element_text(family=font, face="bold"),
        plot.title = element_text(face = "bold",
                          colour = colortitols, size = 16, hjust = 0.5),
        plot.subtitle = element_text(face = "italic",
                             colour = colortitols, size = 11, hjust = 0.5), 
        plot.caption = element_text(face = "plain",
                             colour = colortitols, size = 11))



ggsave("2_TheOffice_Text.png",width=15, height = 15, units='cm')


