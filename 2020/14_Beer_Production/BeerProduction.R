
# Tidytuesday week 14 2020. #Beer production
# Data from https://www.ttb.gov/beer/statistics

library(tidyverse)
# Get the Data
options(scipen = 999) #prevents scientifical notation in plots

#https://colorpalette.org/drink-beer-sunlight-color-palette/
#c("#e78c11","#9a5607","#f9efc1","#6f3c05","#f5c270","#fbdc81","#f2a743",
#  "#ce6505","#0d0404","#532b04","#fbbf51","#331a04")

backcol<- "#f9efc1"

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
#beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
#brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
#beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

options(scipen = 999) #prevents scientifical notation in plots


library(extrafont) # first time execute font_import()
loadfonts()
theme_ari<- function(legpos="top", backcolor=backcol){  
  theme(text=element_text(family = font),
        legend.position = legpos, 
        plot.background = element_rect(fill = backcolor, color=NA), 
        panel.background = element_rect(fill = backcolor),
        strip.background =element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = backcolor),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20,hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5, face="italic", size=10), 
        plot.caption = element_text( face="italic") )
}

font<-"Trebuchet MS"

p1<-brewing_materials%>%
  filter(type%in% c("Total Non-Grain products","Total Grain products"))%>%
  group_by( year, type)%>%
  summarise(tot=sum(month_current))%>%
  mutate(perc=round((tot/sum(tot)*100),1))%>%
  mutate(galeons= tot*31)%>% #in galeons
  mutate(liters= galeons*3.78541)%>% # in liters
  mutate(text1= case_when(
    type=='Total Grain products' ~ perc,  
    type== 'Total Non-Grain products' ~ NA_real_))%>%
  ggplot(aes(x=factor(year),y=perc, fill=type, label=text1))+
  geom_col()+
  scale_fill_manual("by",values=c("#e78c11", "#532b04"))+xlab("")+ylab("%")+
  labs(title="Beer brewing materials", 
       subtitle = "barrels/year (1 barrel = 31 gallons = 117 liters)")+
  geom_text( position = "dodge")+theme_ari()+ 
    theme(axis.text.y = element_blank(),
      axis.ticks = element_blank(), 
      plot.margin=unit(c(1,0, 1,0), "cm"))

p2<-brewing_materials%>%
  filter(type%in% c("Total Used"))%>%
  group_by( year, type)%>%
  summarise(tot=sum(month_current))%>%
  mutate(totNeg= tot*(-1))%>%
  mutate(totMil=paste0(format(round(tot / 1e6, 1), trim = TRUE), "M"))%>%
ggplot(aes(x=factor(year),y=totNeg,  label=totMil))+
  geom_col(fill= "black")+
xlab("")+labs(caption="Done by @AnguloBrunet \n Data from https://www.ttb.gov/beer/statistics \n #tidytuesday")+
geom_text( vjust=1.5)+theme_ari()+xlab("")+ylab("")+ylim(c(-8043719601,0))+
  theme(axis.text.x = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks = element_blank(), 
        plot.margin=unit(c(-1,0, 1,0), "cm"))+ylab("Million barrels")

library(cowplot)

plot_grid(p1, p2, nrow = 2)
ggsave("BeerProduction.png")
                    