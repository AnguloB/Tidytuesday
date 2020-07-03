#24	2020-06-09	African-American Achievements	Wikipedia & Wikipedia	David Blackwell & Petition for David 


color1 <-"#474350" #background color
color2<- "#FA7268"  #color  countries

theme_ari<- function(){
  theme_bw()+

    theme(
      text=element_text( color="white"),
      plot.background = element_rect(fill = color1, color=NA),
          panel.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
      axis.text.x = element_text(color="white"),
      axis.text.y = element_text(color="white"),
      plot.title = element_text(size=20, hjust=0,face="bold", color="#fadd9e"), 
      plot.subtitle = element_text(face="italic", size=15, hjust=0, color="#f45b69"), 
      plot.caption = element_text( face="italic", size=12, hjust = 1,color= "#f45b69"), 
          legend.background = element_rect(fill=color1,
                                           size=0.5, linetype="solid"), 
          legend.title = element_text(colour="white", size=10, 
                                      face="bold"), 
          axis.line=element_blank(), 
          legend.position = "bottom")
    
  }

# Get the Data

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')


firsts<-firsts%>%
  filter(gender =="Female African American Firsts")

paleta<- c("#cda2ab","#fadd9e","#6b9ac4","#97d8c4","#f4b942","#f45b69","#dc6acf","#efc3e6")

firsts%>%
  group_by(year, category)%>%
  count()%>%
  ggplot(aes(x=year, y=n,   fill=category))+
  geom_col(position="stack", stat="identity")+theme_ari()+
  scale_fill_manual(values=paleta)+
  scale_y_continuous(breaks= c(1, 2, 3, 4, 5, 6))+
  labs(fill="", 
       subtitle = "Female achievements by category", 
       title= "African-american achievements", 
       caption= "Done by @AnguloBrunet \n #tidytuesday")
  
ggsave("africanamerican.png")

