
# Tidytuesday week 17 2020. #GDPR

library(tidyverse)

# Get the Data

options(scipen = 999) #prevents scientifical notation in plots
backcol<- "#FA7268"  #background color
colortitle<-"#34558b" # color for title
colortext<- "#34558b"  #color for text (no title)
#palette1<-c("#EA787A", "#1C6788", "#BED441")
colorlines<-"white"

library(extrafont) # first time execute font_import()
loadfonts()
font<-"Helvetica" #setfont

color1 <-"#FA7268" #background color
color2<- "#FA7268"  #color  countries
colorpoint<- "#34558b" 

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
        plot.title = element_text(size=20, hjust=.5,face="bold", color=colortext), 
        plot.subtitle = element_text(face="italic", size=15, hjust=.5), 
        plot.caption = element_text( face="italic", hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext))
}

# Get the Data
# Get the Data

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')


### map 
dat_map_1<-gdpr_violations%>% 
  group_by(name)%>%
  summarise(total= n())%>%
  mutate(perc= paste0(round((total/sum(total))*100,1), "%"))%>%
  arrange(desc(total))%>%
  mutate(region=name)

#compute total money and milion
datMoney<-gdpr_violations%>%
  group_by(name)%>%
  summarise(money= sum(price))%>%
  mutate(milion= money/1e6)%>%
  mutate(region=name)

#some comprovations to ensure all countries are in map and viceversa
#europe[!europe%in% dat_map1$name]
#dat_map1$name[!dat_map1$name%in% europe]

#Europe and UK filter
europe <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
            "Czech Republic","Denmark","Estonia","Finland",
            "France","Germany","Greece","Hungary","Ireland","Italy",
            "Latvia","Lithuania","Luxembourg","Malta",
            "Netherlands", "Poland", "Portugal","Romania",
            "Slovakia","Slovenia","Spain","Sweden",
            "United Kingdom", "Iceland", "Norway") # countries i want to plot



#keep only european countries defined before in world_map
mapp<-map_data("world")%>%
filter(lat<=72)  #exclude a region of norwey
mapp$region<-recode(mapp$region, UK = "United Kingdom") #recode region in order to find it 

europe_map <- mapp%>% #filter only the europe countries i defined
  filter(region%in%europe)

centres<-mapp %>%  #look for the center of each country to plot text
  group_by(region)%>%
  summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))


dat_map1<- dat_map_1%>%
  left_join(centres) # join centres of region and  total


m1<-ggplot()+
  geom_polygon(data=europe_map,aes(x = long, y = lat, group = group), # fill=milion),
                color=colorlines, fill=backcol)+
  geom_point(data=dat_map1, aes(x=long1, y=lat1, label=perc, size=total), color=colorpoint, alpha=.5)+
  geom_text(data=dat_map1, aes(x=long1, y=lat1, label=perc))+
  theme_ari(legpos = "none")+
  theme(plot.background = element_rect(fill = color1, color=NA),
        panel.background = element_rect(fill = color1), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.background = element_rect(fill=color1,
                                         size=0.5, linetype="solid"), 
        legend.text = element_text( size=10, angle =45, hjust = 1),
        legend.title = element_text(colour="white", size=10, 
                                    face="bold"), 
        axis.line=element_blank())+
  scale_size_continuous(range=c(1,15))+
  scale_fill_gradient2(low="white", mid="orange",high="red", midpoint = 15)
  
####graph1
#Filter only spain 
spain<-gdpr_violations%>%
  filter(name=="Spain")

#do some recoding for Vodafone names and la lifa
spain<-spain%>%
  mutate(controllerRe = case_when(
    controller == "Vodafone Espana"	 ~ "Vodafone",
    controller == "Vodafone España"	 ~ "Vodafone",
    controller == "Vodafone España, S.A.U."	 ~ "Vodafone",
    controller == "Vodafone ONO"	 ~ "Vodafone",
    controller == "Professional Football League (LaLiga)" ~ "LaLiga",
    TRUE ~ .$controller ))


#compute totals by million and arrange for plot
dat_gr1<-spain%>%
  group_by(controllerRe)%>%
  summarise(total_1=sum(price))%>%
  mutate(total=total_1/1e6)%>%
  arrange(desc(total))%>%
  mutate(id=1:length(total)) #necessary for labels in circulat plot

  
#Syntax from https://www.r-graph-gallery.com/circular-barplot.html
label_data <- dat_gr1
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar   
label_data$hjust<-ifelse( angle < -90, 1, 0)

label_data$angle<-ifelse(angle < -90, angle+180, angle)


colortitols<- "#414141" #titlecolor

g1<-ggplot(dat_gr1, aes(x=as.factor(id), y=total)) + 
  geom_bar(stat="identity", color=colorlines, fill=colorlines) +
  geom_text(data=label_data, aes(x=id, y=total, label=controllerRe, hjust=hjust), 
            color="black", family=font,fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, 
            inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-0.5,2) +
  scale_fill_manual(values=palette)+
  theme_minimal() + 
  theme(text = element_text(family="Courier"),
        plot.background = element_rect(fill = backcol, colour=backcol),
        panel.background = element_rect(fill = color1, colour = backcol), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "none")


filtre<-filter(dat_gr1, total>=0.25)[[1]] #filter for >.25 milion
                  
#count for vodafone and la liga (second graph)
dat_3<-spain%>%
  filter(controllerRe %in%filtre)%>%
  group_by(controllerRe, type)%>%count()

#useful for doing annotations
#dat_3_0<-spain%>%
#  filter(controllerRe %in%filtre)%>%
#  select(controllerRe,type, summary)

annotation1<- "-Failed to inform implications of app: \n remotely acces microphones  \n to check pubs screening football matches"
 
annotation2<- "Non-compliance with \n lawful basis for data processing"	
annotation7<-"Failure to implement sufficient\n measures to ensure information security"	

annotation3<-  "-The company did not provide \n information to the AEPD in  \n relation to an investigation."
annotation4<- "-The data subject received  \n more than 200 SMS in 2018 \n despite his data \n was delated in 2015 "
annotation5<- "-Vodafone mistakenly charged \n  a customer whose information \n it disclosed to BADEXCUG"
annotation6<-"-Company sent a contract with \n name, address and contact \n  details of a client  to a  \n third party by accident."

annotation10<-"(1) Technical error allowed customers \n to view the personal data of others.\n
(2) Data of a customer was \n disclosed to another via SMS. \n
(3) Invoicing details of a customer \n were sent to a third party \n                                                                                                               
(4) Two clients had received \n the same security access  \n             
(5) An individual had access to  \n third parties data  in their profile. \n                                                                                               
(6) A client data was accessed \n without authorization"

annotation11<-"(1-3) Sent messages  without prior \n written consent from the customer. \n
(4)Vodafone Espana called \n the complainant after refusal. \n
(5)Company has processed personal data \n of the claimant years \n after the contractual relationsid had ended. \n
(6) Company sent a  number of emails \n without using the BCC feature. \n  
(7) Charged for a Netflix subscription  \n that they has not been requested. \n 
(8)Company sent invoicing \n details to a third party. \n
(9) Transfer of a phone subscription \n with a third party person. \n 
(10) Sent invoices of a  \n client to their neighbor.\n   
(11) Continue sending invoices \n notices even contractual obligation ended.\n
(12)Company sent  SMS to a \n person who was not client. \n"


annot<- "Spain is the country with more GDPR fines (27.6%), \n but not the one with the highest amount (France, 51.1M) \n Two companies (Vodafone[0.98M] and LaLiga [0.25M]) payed more than 0.25 milion €"

g2<-dat_3%>%
ggplot (aes(x=controllerRe, y=n,  group=type))+
                geom_col(position = position_dodge2(width = 0.9, preserve = "single"), fill="white")+
  coord_flip()+theme_ari(legpos = "none")+ ylim(0,20)+
  xlab("")+ ylab("")+
  scale_x_discrete(labels=c("Vodafone" = "Vodafone", "Professional Football League (LaLiga)" = "LaLiga"))+
annotate("text", x = 1, y = 5, label = annotation1, size=2.2, family="Courier")+
annotate("text", x = 2.1, y = 15, label = annotation2, size=2.2, family="Courier",fontface=2)+
annotate("text", x = 2.4, y = 4, label = annotation3, size=2.2, family="Courier")+
annotate("text", x = 2.25, y = 4, label = annotation4, size=2.2, family="Courier")+
annotate("text", x = 1.6, y = 4, label = annotation5, size=2.2, family="Courier")+
annotate("text", x = 1.75, y = 4, label = annotation6, size=2.2, family="Courier")+
annotate("text", x = 1.9, y = 10, label = annotation7, size=2.2, family="Courier", fontface=2)+
annotate("text", x = 1.5, y = 11, label = annotation10, size=2, family="Courier", color ="white")+
annotate("text", x = 1.4, y = 18, label = annotation11, size=2, family="Courier", color="white")+
  annotate("text", x=0.7, y=10, label= annot, size=3.8, family = font, color=colortext, fontface=2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




library(cowplot)

pl1<-plot_grid( m1, g1, ncol=1)

plot_grid(pl1, g2, nrow = 1, rel_widths = c(1.3, 2))+theme_ari()+
  theme( plot.background = element_rect(fill = backcol, colour=backcol),
         panel.background = element_rect(fill = color1, colour = backcol))+
  labs(title = "GDPR in Spain", 
       caption="Done by @AnguloBrunet \n #tidytuesday")

ggsave("GDPR.png", height=20, width=30, units="cm")

