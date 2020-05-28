
# Tidytuesday week 13 2020. ## TBI 
##ChangeYourMind, #braininjuryawarenessmonth
#Data from CDC and Veterans Brain Injury Center. Additional stats can be found at CDC.gov.

library(tidyverse)
library(stringr)

##Set some defaults 
library(extrafont) # first time execute font_import()
loadfonts()
paletPie<-c("#FFC3C2", "indianred1", "indianred4") #palette

options(scipen = 999) #prevents scientifical notation in plots


backcol<-"ivory1" #set default background color

theme_ari<- function(legpos="bottom", backcolor=backcol){  
  theme(text=element_text(family = font),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = backcolor),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"))
}


font<-"Trebuchet MS"


# Get the Data

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')


#Unifying categories in injury_mechanism
tbi_year$injury_mechanism<-str_to_title(tbi_year$injury_mechanism)
tbi_age$injury_mechanism<-str_to_title(tbi_age$injury_mechanism)

p1<-tbi_year%>%
  group_by(year,type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  ggplot( aes(x=factor(year), y= Frequency, color=type, group=type))+
  geom_point(fill=backcol, size=4)+
    geom_line()+
  scale_color_manual(values=ariDivPalette)+theme_ari()+xlab("")+theme(legend.title = element_blank())+
labs(title = "Traumatic Brain Injury (TBI) by type of data", subtitle ="CDC data",
     caption= "Done by @AnguloBrunet \n #tidytuesday \n #ChangeYourMind, #braininjuryawarenessmonth")
ggsave("EvolutionTypeYear.png")

p1<-tbi_year%>%
  group_by(year)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  ggplot( aes(x=factor(year), y= Frequency, group=1))+
  geom_point(fill=backcol, size=4, color="ivory3")+
  geom_line(color="ivory3")+
  scale_color_manual(values=ariDivPalette)+
  ylim(0,3000000)+ ylab("")+
  theme_ari()+xlab("")+theme(legend.title = element_blank())+
  labs(title = "Traumatic Brain Injury (TBI)", subtitle ="CDC data",
       caption= "Done by @AnguloBrunet \n #tidytuesday \n #ChangeYourMind, #braininjuryawarenessmonth")
ggsave("Evolution.png")


tbi_age$age_group<-factor(tbi_age$age_group, levels=c("0-17","0-4", "5-14", "15-24", "25-34",
                                                      "35-44", "45-54",  "55-64","65-74","75+","Total" ))
p2<-tbi_age%>%
  filter(!age_group %in%c("0-17", "Total"))%>% #exclude redundant categories
  ggplot(aes(x=age_group, y =number_est, fill=type))+
  geom_col()+ ylab("")+
  facet_wrap(vars(injury_mechanism), scales="free")+theme_bw()+theme_ari()+
  scale_fill_manual(values=paletPie)+theme_ari()+xlab("")+
  theme(legend.title = element_blank())+
  labs(title = "Traumatic Brain Injury (TBI) in 2014", subtitle ="CDC data",
       caption= "Done by @AnguloBrunet \n #tidytuesday \n #ChangeYourMind, #braininjuryawarenessmonth")
      ggsave("ByAge.png")


tbi_age$type <- factor(tbi_age$type, levels = c("Emergency Department Visit", "Hospitalizations", "Deaths"))

#Create a plot for each injury mechanism ans save it as png 

tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Assault")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
  plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("assault.png")


tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Intentional self-harm")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("Intentional.png")


tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Motor Vehicle Crashes")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("Motor.png")



tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Other or no mechanism specified")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("Other.png")


Unintentional<-tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Other unintentional injury, mechanism unspecified")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("OtherUnintentional.png")


Unintentional<-tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Unintentional Falls")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("Unintentional.png")



tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Unintentionally struck by or against an object")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  coord_polar("y", start=0)+ xlab("")+ylab("")+
  theme_ari(legpos = "none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+scale_fill_manual(values=paletPie)
ggsave("object.png")

leg<-tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism, type)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  filter(injury_mechanism=="Unintentionally struck by or against an object")%>%
  ggplot(aes(x=injury_mechanism,y=freq, fill=type))+geom_col()+
  theme_ari(legpos = "bottom")+
  scale_fill_manual("Type of measure",values=paletPie)



#Create a list with the names of the created images
images<- c("Unintentional.png", "object.png", "Motor.png", "Assault.png", "Other.png", "OtherUnintentional.png", "Intentional.png")

dat1<-tbi_age%>%
  filter(age_group %in%c( "Total"))%>% 
  group_by(injury_mechanism)%>%
  summarise(Frequency = sum(number_est, na.rm = TRUE))%>%
  mutate(freq = (Frequency / sum(Frequency))*100)%>%
  arrange(desc(freq))  #arrange in the same way image list is created

dat2<-data.frame(dat1, images, dat1$freq+25) #create data frame with data, images and image postition
names(dat2)[5]<-"posimage" #change name for image position

library(ggimage) 
 
df <- data.frame(x1 = 6, x2 = 7, y1 = 70, y2 = 35)

plot<-ggplot(dat1, aes(x=reorder(injury_mechanism, -freq), y= freq, label = paste0(round(freq,1), "%\n n = ", Frequency )))+
    geom_col(fill="ivory3")+ ylim(0,100)+
    ylab("")+
    geom_text(vjust=-.5, family=font, size=3)+
    geom_image(data=dat2, aes(x=injury_mechanism, y=posimage, image=images), size=.15)+
    theme_ari()+xlab("")+theme(legend.title = element_blank())+
    scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(caption= "Done by @AnguloBrunet \n #tidytuesday \n #ChangeYourMind, #braininjuryawarenessmonth")+
  annotate("text",
         x = c(6),
         y = c(75),
         label = c("Intentional self-harm is the only \n injury mechanism with more Deaths \n than Emergency Department visits "),
         family = font, fontface = 3, size=3, color ="indianred4")+
geom_curve( data = df, aes(x = x1, y = y1, xend = x2, yend = y2),
            inherit.aes = FALSE,arrow = arrow(length = unit(0.03, "npc")), linetype = 2, curvature = -0.2) 
  

    library(cowplot)
  my_legend <- get_legend(leg)
  library(ggpubr)
  ggg<- as_ggplot(my_legend)
  
  
  title <- ggdraw() + 
    draw_label(
      "Traumatic Brain Injury (TBI) in 2014",
      fontface = 'bold', x = 0,hjust = 0, fontfamily = font ) 
   
  
plot_grid(title, ggg, plot, nrow=3, rel_heights = c(.1,.1,1.5))+ 
  theme(plot.margin = margin(0, 0, 0, 0),panel.background = element_rect(fill = "ivory1", color="ivory"))

ggsave("TBIbyMeasure.png")
