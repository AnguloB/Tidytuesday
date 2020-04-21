
# Tidytuesday week 16 2020. #Best rap artists


library(tidyverse)

# Get the Data

options(scipen = 999) #prevents scientifical notation in plots
backcol<- "#E1EEE4"  #background color
colortitle<-"grey19" # color for title
colortext<- "black"  #color for text (no title)
palette1<-c("#EA787A", "#1C6788", "#BED441")

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Trebuchet MS" #setfont

#color1 <-"black" #background color
#color2<- "black"  #color  countries
#colorpoint<- "#EEDE03" 

theme_ari<- function(legpos="bottom", backcolor=backcol){  
  theme(text=element_text(family = font, color=colortext),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = backcolor),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(color= colortitle, size=20, hjust=.5,face="bold"), 
        plot.subtitle = element_text(face="italic", size=15, hjust=.5, color="grey50"), 
        plot.caption = element_text( face="italic"), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext))
}

# Get the Data

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')


### Frequency by year
p1<-rankings%>%
  group_by(year,gender)%>%
  summarise(tot = n()) %>% #create n 
  mutate(perc= round((tot/sum(tot)*100),1))%>% #create %
  ggplot(aes(x=factor(year), y =tot, fill=gender))+geom_col()+
  xlab("")+ylab("")+ #delate x and y titles
  theme_ari()+scale_fill_manual(values=palette1)+labs(caption="Done by @AnguloBrunet \n #tidytuesday")



library(tidytext)
data(stop_words)


text<-rankings %>%
  select(ID, title)%>%
  mutate(title1=
         str_replace_all(string=title, 
            pattern="[:digit:]", replacement=""))%>% #delate numbers
      unnest_tokens(word, title1)%>%
      anti_join(stop_words)%>% # remove stop word
      count(word, sort=TRUE) #count words and order




text1<-rankings %>%
  select(ID, title, gender)%>%
  mutate(title1=str_replace_all(string=title, pattern="[:digit:]", replacement=""))%>%
  unnest_tokens(word, title1)%>%
  anti_join(stop_words)%>%
  group_by(word, gender)%>%
  count(word, sort=TRUE)

df<-data.frame(text1, duplicated(text1["word"])) # create column with dup
names(df)[4]<- "isduplicated" #change name

# create data where there is no "duplicated (for genders)
text2<-text1%>%
  filter(!word%in% df$word[df$isduplicated=="TRUE"])

#Filter  words that are duplicated and use the general count
text3<-text%>%
  filter(word%in% df$word[df$isduplicated=="TRUE"])%>%
  mutate(gender= "mixed")%>%  #then asisgn mixed gender
  select(word, gender, n) # and choose same columns that text2

#join text2 and text3 and create new variable with angle for words
text_df<-bind_rows(text2, text3)%>%
 filter(n>=2)%>%
  mutate(angle = 90 * sample(c(0, 1), n(), 
            replace = TRUE, prob = c(60, 40)))


library(ggwordcloud) # for doing wordclouds

p2<-text_df%>%
  ggplot( aes(label = word, size=n,     
              color = gender, angle=angle))+
  geom_text_wordcloud(shape="cardioid", family=font,) +
  # scale_size_area(max_size = 10)+
  scale_radius(range=c(1,15), limits=c(0,NA))+
  scale_color_manual(values=palette1)+theme_ari()+ 
  theme(plot.background = element_rect(colour = backcol)) +#remove margin
  labs(title= "Best rap songs",subtitle= "BBC music’s greatest hip-hop songs",  caption = "Common words in titles (equal or greater than 2)")


library(cowplot)
plot_grid(p2, p1, nrow=2)
ggsave("BestRapArtists.png")

