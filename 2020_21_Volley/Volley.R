#week 21 voley

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

vb_matches$country[vb_matches$country=="United States"]<-"USA"
vb_matches$country[vb_matches$country=="Macao"]<- "China"
vb_matches$country[vb_matches$country=="Korea"]<-"South Korea"
vb_matches$country[vb_matches$country=="Hong Kong"]<- "China"
vb_matches$country[vb_matches$country=="England"]<- "UK"
vb_matches$country[vb_matches$country=="Principality of Monaco"]<- "Monaco"

#do counts
Data<-vb_matches%>%
  filter(gender=="W")%>%
  group_by(country)%>%
 count()%>%
  arrange(desc(n))

names(Data)[1]<-"region"


#change country name to region in order to join with world map




world_map <- map_data("world")


#Data$region[!Data$region %in%unique(world_map$region)]

db<-world_map%>%
  left_join(Data)

p1<-ggplot(db, aes(x = long, y = lat, group = group, fill=n)) +
  geom_polygon(colour = "white")+theme_bw()+
  scale_fill_gradient(low="#FECD10", high="#950B8C", na.value="white")+
  labs(title="Countries where women Beach Volley matches were played (2000-2019)", 
       x="", y="", caption= "Done by @AnguloBrunet")+
  theme(plot.title = element_text(colour="#950B8C", size=15,  face="bold"),
        plot.caption = element_text(colour="#950B8C", size=15,  face="bold", hjust=1), 
        legend.text = element_text(colour="white", size = 16, face = "bold"))

require(ggimage) 
img = "image.png"
ggbackground(p1, img)
ggsave("Volley.png")


