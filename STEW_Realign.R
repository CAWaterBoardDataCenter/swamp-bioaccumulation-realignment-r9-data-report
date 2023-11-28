library(vegan)
library(ggplot2)
library(vegan3d)
library(gridExtra)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(forcats)
library(nlme)
library(multcomp)
library(lme4)
library(car)
library(stats)
library(ggpubr)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(patchwork)
library("ggthemes")


#####HG DATA######

HG=read.csv("Hg4thresholds.csv")



####Hg_By_Waterbody####
ChollasHg=subset(HG,Waterbody=="Chollas Reservoir")
CuyamacaHg=subset(HG,Waterbody=="Cuyamaca Reservoir")
DanaHg=subset(HG,Waterbody=="Dana Point Harbor")
IBHg=subset(HG,Waterbody=="Imperial Beach Pier")
SweetwaterHg=subset(HG,Waterbody=="Lower Sweetwater River")
MBHg=subset(HG,Waterbody=="Mission Bay")
OHHg=subset(HG,Waterbody=="Oceanside Harbor")
OPHg=subset(HG,Waterbody=="Oceanside Pier")
SDRHg=subset(HG,Waterbody=="San Diego River")
SDBHg=subset(HG,Waterbody=="San Diego Bay")



#########General_Hg####################
#################################


Means=HG%>%group_by(Waterbody)%>%summarise(avg=mean(Result_ww_ppb))
SD=HG%>%group_by(Waterbody)%>%summarise(sd=sd(Result_ww_ppb))
Min=HG%>%group_by(Waterbody)%>%summarise(min=min(Result_ww_ppb))
Max=HG%>%group_by(Waterbody)%>%summarise(max=max(Result_ww_ppb))

Means=HG%>%group_by(Station_species)%>%summarise(avg=mean(Result_ww_ppb))
SD=HG%>%group_by(Station_species)%>%summarise(sd=sd(Result_ww_ppb))
Min=HG%>%group_by(Station_species)%>%summarise(min=min(Result_ww_ppb))
Max=HG%>%group_by(Station_species)%>%summarise(max=max(Result_ww_ppb))


write.csv(Means,"HGmeans.csv")
write.csv(SD,"HGsd.csv")
write.csv(Min,"HGmin.csv")
write.csv(Max,"HGmax.csv")


##Hg##

ggplot(ChollasHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=3.5, y=30, label= "3+ meals")+
  annotate("text", x=3.5, y=110, label= "2 meals")+
  annotate("text", x=3.5, y=300, label= "1 meal")+
  annotate("text", x=3.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Chollas Lake")+
  theme(plot.title = element_text(vjust = 4))

  
ggsave("Chollas_Lake_Mercury.png",dpi=600)

ggplot(CuyamacaHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=6.5, y=30, label= "3+ meals")+
  annotate("text", x=6.5, y=110, label= "2 meals")+
  annotate("text", x=6.5, y=300, label= "1 meal")+
  annotate("text", x=6.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Lake Cuyamaca")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Cuyamaca_Lake_Mercury.png",dpi=600)

ggplot(DanaHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=6.5, y=30, label= "3+ meals")+
  annotate("text", x=6.5, y=110, label= "2 meals")+
  annotate("text", x=6.5, y=300, label= "1 meal")+
  annotate("text", x=6.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Dana Point Harbor")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Dana_Point_Harbor_Mercury.png",dpi=600)

ggplot(IBHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=9.5, y=30, label= "3+ meals")+
  annotate("text", x=9.5, y=110, label= "2 meals")+
  annotate("text", x=9.5, y=300, label= "1 meal")+
  annotate("text", x=9.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Imperial Beach Pier")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Imperial_Beach_Pier_Mercury.png",dpi=600)

ggplot(SweetwaterHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=2.5, y=30, label= "3+ meals")+
  annotate("text", x=2.5, y=110, label= "2 meals")+
  annotate("text", x=2.5, y=300, label= "1 meal")+
  annotate("text", x=2.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Lower Sweetwater River")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Lower_Sweetwater_River_Mercury.png",dpi=600)

ggplot(MBHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=6.5, y=30, label= "3+ meals")+
  annotate("text", x=6.5, y=110, label= "2 meals")+
  annotate("text", x=6.5, y=300, label= "1 meal")+
  annotate("text", x=6.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Mission Bay")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Mission_Bay_Mercury.png",dpi=600)

ggplot(OHHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=7.5, y=30, label= "3+ meals")+
  annotate("text", x=7.5, y=110, label= "2 meals")+
  annotate("text", x=7.5, y=300, label= "1 meal")+
  annotate("text", x=7.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Oceanside Harbor")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Oceanside_Harbor_Mercury.png",dpi=600)

ggplot(OPHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=8.5, y=30, label= "3+ meals")+
  annotate("text", x=8.5, y=110, label= "2 meals")+
  annotate("text", x=8.5, y=300, label= "1 meal")+
  annotate("text", x=8.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("Oceanside Pier")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Oceanside_Pier_Mercury.png",dpi=600)

ggplot(SDRHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=5.5, y=30, label= "3+ meals")+
  annotate("text", x=5.5, y=110, label= "2 meals")+
  annotate("text", x=5.5, y=300, label= "1 meal")+
  annotate("text", x=5.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("San Diego River")+
  theme(plot.title = element_text(vjust = 4))


ggsave("San_Diego_River_Mercury.png",dpi=600)

ggplot(SDBHg,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Mercury (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=7.5, y=30, label= "3+ meals")+
  annotate("text", x=7.5, y=110, label= "2 meals")+
  annotate("text", x=7.5, y=300, label= "1 meal")+
  annotate("text", x=7.5, y=490, label= "0 meals")+
  ylim(c(0,500))+
  ggtitle("San Diego Bay")+
  theme(plot.title = element_text(vjust = 4))


ggsave("San_Diego_Bay_Mercury.png",dpi=600)
  
  
###Stats###  
t.test(Hg_total$Result,mu=70, alternative = "less")
wilcox.test(Hg_total$Result,mu=70, alternative = "less")

t.test(Hg_total$Result,mu=150, alternative = "less")
wilcox.test(Hg_total$Result,mu=150, alternative = "less")

t.test(Hg_total$Result,mu=220, alternative = "less")
wilcox.test(Hg_total$Result,mu=220, alternative = "less")

##########################################
#########################################
######################################
##############################
####SE Data##################
##############################
#####################################
########################################
##############################################

SE=read.csv("Se4thresholds.csv")


##############Se_By_Waterbody##################
################################################
ChollasSe=subset(SE,Waterbody=="Chollas Reservoir")
CuyamacaSe=subset(SE,Waterbody=="Cuyamaca Reservoir")
DanaSe=subset(SE,Waterbody=="Dana Point Harbor")
IBSe=subset(SE,Waterbody=="Imperial Beach Pier")
SweetwaterSe=subset(SE,Waterbody=="Lower Sweetwater River")
MBSe=subset(SE,Waterbody=="Mission Bay")
OHSe=subset(SE,Waterbody=="Oceanside Harbor")
OPSe=subset(SE,Waterbody=="Oceanside Pier")
SDRSe=subset(SE,Waterbody=="San Diego River")
SDBSe=subset(SE,Waterbody=="San Diego Bay")



#########General_Se####################
#################################


Means=SE%>%group_by(Waterbody)%>%summarise(avg=mean(Result_ww_ppb))
SD=SE%>%group_by(Waterbody)%>%summarise(sd=sd(Result_ww_ppb))
Min=SE%>%group_by(Waterbody)%>%summarise(min=min(Result_ww_ppb))
Max=SE%>%group_by(Waterbody)%>%summarise(max=max(Result_ww_ppb))

Means=SE%>%group_by(Station_species)%>%summarise(avg=mean(Result_ww_ppb))
SD=SE%>%group_by(Station_species)%>%summarise(sd=sd(Result_ww_ppb))
Min=SE%>%group_by(Station_species)%>%summarise(min=min(Result_ww_ppb))
Max=SE%>%group_by(Station_species)%>%summarise(max=max(Result_ww_ppb))


write.csv(Means,"SEmeans.csv")
write.csv(SD,"SEsd.csv")
write.csv(Min,"SEmin.csv")
write.csv(Max,"SEmax.csv")


##Se##

ggplot(ChollasSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=3.5, y=1000, label= "3+ meals")+
  annotate("text", x=3.5, y=3700, label= "2 meals")+
  annotate("text", x=3.5, y=10000, label= "1 meal")+
  annotate("text", x=3.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Chollas Lake")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Chollas_Lake_Selenium.png",dpi=600)

ggplot(CuyamacaSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=6.5, y=1000, label= "3+ meals")+
  annotate("text", x=6.5, y=3700, label= "2 meals")+
  annotate("text", x=6.5, y=10000, label= "1 meal")+
  annotate("text", x=6.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Lake Cuyamaca")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Cuyamaca_Lake_Selenium.png",dpi=600)

ggplot(DanaSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=6.5, y=1000, label= "3+ meals")+
  annotate("text", x=6.5, y=3700, label= "2 meals")+
  annotate("text", x=6.5, y=10000, label= "1 meal")+
  annotate("text", x=6.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Dana Point Harbor")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Dana_Point_Harbor_Selenium.png",dpi=600)

ggplot(IBSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=9.5, y=1000, label= "3+ meals")+
  annotate("text", x=9.5, y=3700, label= "2 meals")+
  annotate("text", x=9.5, y=10000, label= "1 meal")+
  annotate("text", x=9.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Imperial Beach Pier")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Imperial_Beach_Pier_Selenium.png",dpi=600)

ggplot(SweetwaterSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=2.5, y=1000, label= "3+ meals")+
  annotate("text", x=2.5, y=3700, label= "2 meals")+
  annotate("text", x=2.5, y=10000, label= "1 meal")+
  annotate("text", x=2.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Lower Sweetwater River")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Lower_Sweetwater_River_Selenium.png",dpi=600)

ggplot(MBSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=5.5, y=1000, label= "3+ meals")+
  annotate("text", x=5.5, y=3700, label= "2 meals")+
  annotate("text", x=5.5, y=10000, label= "1 meal")+
  annotate("text", x=5.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Mission Bay")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Mission_Bay_Selenium.png",dpi=600)

ggplot(OHSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=6.5, y=1000, label= "3+ meals")+
  annotate("text", x=6.5, y=3700, label= "2 meals")+
  annotate("text", x=6.5, y=10000, label= "1 meal")+
  annotate("text", x=6.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Oceanside Harbor")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Oceanside_Harbor_Selenium.png",dpi=600)

ggplot(OPSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=8.5, y=1000, label= "3+ meals")+
  annotate("text", x=8.5, y=3700, label= "2 meals")+
  annotate("text", x=8.5, y=10000, label= "1 meal")+
  annotate("text", x=8.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("Oceanside Pier")+
  theme(plot.title = element_text(vjust = 4))


ggsave("Oceanside_Pier_Selenium.png",dpi=600)

ggplot(SDRSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=5.5, y=1000, label= "3+ meals")+
  annotate("text", x=5.5, y=3700, label= "2 meals")+
  annotate("text", x=5.5, y=10000, label= "1 meal")+
  annotate("text", x=5.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("San Diego River (Lower)")+
  theme(plot.title = element_text(vjust = 4))


ggsave("San_Diego_River_Selenium.png",dpi=600)

ggplot(SDBSe,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=3, color="blue") +
  coord_flip() +
  theme_economist_white() +
  xlab("") +
  ylab("Selenium (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept =4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=4.5, y=1000, label= "3+ meals")+
  annotate("text", x=4.5, y=3700, label= "2 meals")+
  annotate("text", x=4.5, y=10000, label= "1 meal")+
  annotate("text", x=4.5, y=17000, label= "0 meals")+
  ylim(c(0,18000))+
  ggtitle("San Diego Bay")+
  theme(plot.title = element_text(vjust = 4))

ggsave("San_Diego_Bay_Selenium.png",dpi=600)
