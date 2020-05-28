
# Tidytuesday week 11 2020. ##College Tuition, Diversity, and Pay

#Data by  and from Tuitiontracker.org 

library(tidyverse)
library(extrafont) # first time run font_import()
#Historical averages from the NCES - cover 1985-2016.

#Tuition and fees by college/university for 2018-2019, along with school type, 
#degree length, state, in-state vs out-of-state from the Chronicle of Higher Education.

#Diversity by college/university for 2014, along with school type, 
#degree length, state, in-state vs out-of-state from the Chronicle of Higher Education.

# Get the Data

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


#Define some commoment elements between the three plots 
colorfondo <-"white" #background
color2<-"gray51" 
colortitols<- "#414141" #titlecolor
legendposition<-"bottom"
loadfonts()

font<-"Trebuchet MS"

theme_ari<- function(...) {
  theme( 
    plot.background = element_rect(fill = colorfondo, color=NA),
    panel.background = element_rect(fill = colorfondo), 
    axis.title.x=element_blank(), axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), axis.title.y=element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    legend.position=legendposition, 
    legend.background = element_rect(fill=colorfondo,
                                     size=0.5, linetype="solid"), 
    legend.text = element_text(colour="black", size=10, angle =45, hjust = 1),
    legend.title = element_text(colour="black", size=10, 
                                face="bold"), 
    axis.line=element_blank(),  
    plot.subtitle = element_text(family=font,face = "italic",
                                 colour = colortitols, size = 11, hjust = 0.5))
}


data<-diversity_school%>%
  mutate(dif = total_enrollment-enrollment)%>%  #diference between total enrollment and each cat
  group_by(state,category)%>%
  summarise(enroll= sum(enrollment), 
            dif= sum(dif))%>%
  mutate(percentage = round(enroll/(enroll+dif)*100,2))%>%
  mutate(invers= 100-percentage)%>%
  filter(category %in% c("White", "Women"))


#I used this lines in order to examine percentages carefully, 
#but finally is not necessary for the plots
#data<-data %>%
#  mutate(recoded = case_when(
#    between(percentage,  1, 10.999) ~ "1-10 %", 
#    between(percentage, 11, 20.999) ~ "11-20%",
#    between(percentage, 21, 30.999) ~ "21-30%",
#    between(percentage, 31, 40.999) ~ "31-40%",
#    between(percentage, 41, 50.999) ~ "41-50%",
#    between(percentage, 51, 60.999) ~ "51-60%",
#    between(percentage, 61, 70.999) ~ "61-70%",
#    between(percentage, 71, 80.999) ~ "71-80%",
#    between(percentage, 81, 90.999) ~ "81-90%",
#    between(percentage, 91, 100) ~ "82-100%"))
    

data$state<-str_to_lower(data$state, locale = "en") #turn into lowercase in order to have
#same format than maps

states<- map_data("state")
names(states)[5]<-"state" #change name in order to have same colname than data
dataPlot<-states%>%inner_join(data) #join data with maps information

loadfonts()

#Plot for women
p1<-dataPlot%>%
  filter(category=="Women")%>% 
  ggplot() + 
  geom_polygon( aes(x=long, y=lat, group=group, fill=percentage),color="black" )+
    scale_fill_gradient2(name= "%",low="white",mid="#A770CD", high="#410066", midpoint = 55)+
  labs(title= "Women in the US colleges", 
       subtitle="At least there is 50% of women \n except in North Dakota (49.2%)")+
  theme_ari()+theme(plot.title = element_text(family=font,face = "bold",
                                                colour = "#410066", size = 18,  hjust = 0.5))

#Plot for white
#important to use inverse in order to have "non-white" information
p2<-dataPlot%>%
  filter(category=="White")%>%
  ggplot() + 
  geom_polygon( aes(x=long, y=lat, group=group, fill=invers),color="black" )+
  scale_fill_gradient2(name="%", low="white",mid="chartreuse3", high="antiquewhite4", midpoint = 45)+
  labs(title= "Non-white people in the US colleges", 
       subtitle="Southern states have more \n Non-white people than Northern")+theme_ari()+
  theme(plot.title = element_text(family=font,face = "bold",
                                   colour = "chartreuse3", size = 18,  hjust = 0.5))




#Create data for boxplot, some manipulation is needed: want to use direct percentage for women
#invers for white (non-white)
data2_1<-diversity_school%>%
  mutate(dif = total_enrollment-enrollment)%>% 
  group_by(name,state,category)%>%
  summarise(enroll= sum(enrollment), 
            dif= sum(dif))%>%
  mutate(percentage = round(enroll/(enroll+dif)*100,2))%>%
  mutate(invers= 100-percentage)%>%
  filter(category %in% c("Women"))%>%
  select(name, state, category, percentage)


data2_2<-diversity_school%>%
  mutate(dif = total_enrollment-enrollment)%>% 
  group_by(name,state,category)%>%
  summarise(enroll= sum(enrollment), 
            dif= sum(dif))%>%
  mutate(percentage = round(enroll/(enroll+dif)*100,2))%>%
  mutate(invers= 100-percentage)%>%
  filter(category %in% c("White"))%>%
  select(name, state, category, invers) #this is different 

names(data2_2)[4]<- "percentage" #recode invers into percentage so i cant rbind
data2_2$category<-recode(data2_2$category, White = 'Non white') #Change label so no manipulation is needed in ggplot
data2<-na.omit(rbind(data2_1,data2_2)) #row bind

#Boxplot and jitter
p3<-data2%>%
  ggplot(aes(x=category, y=percentage, color=category))+
  geom_jitter(alpha=.5)+
  geom_boxplot(color="black", alpha=.3)+
  scale_color_manual(values=c( "chartreuse3", "#410066"))+theme_minimal()+coord_flip()+
  facet_wrap(~state, ncol = 5)+ylab("")+xlab("")+ labs(caption= "    by @AnguloBrunet \n    Data from Tuitiontracker.org \n    #tidytuesday")+
  theme(legend.position="none", strip.background = element_rect(color=colorfondo,fill=colorfondo), text=element_text(family=font))


library(cowplot)
p12<-plot_grid(p1,p2, nrow=2)  #Woman and Nonwhite maps in a same plot (two rows)
p123<-plot_grid(p12,p3, rel_widths = c(1, 1.3)) # add third plot in two cols. Modifies rel weights to best fit

ggsave("College_womenNonwhite.png")



