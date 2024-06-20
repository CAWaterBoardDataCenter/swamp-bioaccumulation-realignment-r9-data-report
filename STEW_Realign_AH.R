# Load Libraries ----------------------------------------------------------
library(here)
library(readxl)
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
library(kableExtra)
library(patchwork)
library(ggthemes)

# Read & Format Data -----------------------------------------------------

## read in tabs from xlsx ####
raw_HG = read_excel(here("data","R9_Realignment_Main_Data_20230907.xlsx"),"Mercury")
raw_HG = subset(raw_HG, select = -62) #rm col with calc "Result ww ppb")" for now - will recalc later
raw_HG = rename(raw_HG, "Result" = "Result ww") #rename "Results ww" to match SE & AS

raw_SE = read_excel(here("data","R9_Realignment_Main_Data_20230907.xlsx"),"Selenium")
raw_SE = subset(raw_SE, select = -62) #rm col with calc "Result ppb ww" for now - will recalc later

raw_AS = read_excel(here("data","R9_Realignment_Main_Data_20230907.xlsx"),"Arsenic")

## combine all metals data into one dataframe ####
metals = rbind(raw_HG, raw_SE, raw_AS)

## add col that calculates "Result_ww_ppb" ####
metals$Result_ww_ppb <- metals$Result * 1000

# LEFT OFF HERE - remove header after fixed-----------------------------------------------------
## convert records with dw in units to ww -update value in Result_ww_ppb - LEFT OFF HERE - need to find % moisture value
#metals$Result_ww_ppb[metals$UnitName == "ug/g dw"] <-(metals$Result_ww_ppb)*[1-("percent moisture"/100)]

## shorten station names ####
metals$stationname <- gsub("Imperial Beach Pier and Surrounding Ocean Waters",
                               "Imperial Beach Pier", 
                               metals$stationname)

metals$stationname <- gsub("Lower Sweetwater River at Morrison Pond",
                               "Lower Sweetwater River", 
                               metals$stationname)

metals$stationname <- gsub("Mission Bay Channel Mouth and Jetties", "Mission Bay",
                          gsub("Mission Bay at South Shores Boat Launch", "Mission Bay", 
                               metals$stationname))

metals$stationname <- gsub("Oceanside Pier and Surrounding Ocean Waters",
                               "Oceanside Pier", 
                               metals$stationname)

metals$stationname <- gsub("San Diego River Ponds P2BA Above Cabrillo Hwy",
                               "San Diego River", 
                               metals$stationname)

metals$stationname <- gsub("San Diego Bay, Baywide", "San Diego Bay",
                          gsub("SD North Bay", "San Diego Bay",
                          gsub("San Diego Bay at G Street Chula Vista", "San Diego Bay",
                                    metals$stationname)))

## add waterbody_type column ####
metals <- within(metals,  
                 waterbody_type <- ifelse(stationname == "Chollas Reservoir" | 
                                          stationname == "Cuyamaca Reservoir"|
                                          stationname == "Lower Sweetwater River" |
                                          stationname == "San Diego River",
                                          "Reservoir-River", "Coastal"))

## add Station_OrgGrp_Analyte column  ####
#metals <- within(metals,  
#                 Station_OrgGrp_Analyte <- paste(stationname, 
#                                              organismgroup, 
#                                              AnalyteName, 
#                                              sep="_"))


## add Station_species_Analyte column  ####
#metals <- within(metals,  
#                 Station_Spp_Analyte <- paste(stationname, 
#                                              commonname, 
#                                              AnalyteName, 
#                                              sep="_"))

## subset by waterbody ####
metals_Chollas    =subset(metals,stationname=="Chollas Reservoir")
metals_Cuyamaca   =subset(metals,stationname=="Cuyamaca Reservoir")
metals_Dana       =subset(metals,stationname=="Dana Point Harbor")
metals_IB         =subset(metals,stationname=="Imperial Beach Pier")
metals_Sweetwater =subset(metals,stationname=="Lower Sweetwater River")
metals_MB         =subset(metals,stationname=="Mission Bay")
metals_OH         =subset(metals,stationname=="Oceanside Harbor")
metals_OP         =subset(metals,stationname=="Oceanside Pier")
metals_SDR        =subset(metals,stationname=="San Diego River")
metals_SDB        =subset(metals,stationname=="San Diego Bay")

# Summary Stats -----------------------------------------------------------

## by Station_OrgGrp_Analyte  ####

Station_OrgGrp_Analyte_Stats = metals %>% 
  group_by(stationname,organismgroup,AnalyteName) %>% 
  summarise(avg=mean(Result_ww_ppb),
            med=median(Result_ww_ppb),
            sd=sd(Result_ww_ppb),
            min=min(Result_ww_ppb),
            max=max(Result_ww_ppb),
            n=n())

write.csv(Station_OrgGrp_Analyte_Stats, here("data_output","Stats_Station_OrgGrp_Analyte.csv"))


## by Station_Spp_Analyte ####

Station_Spp_Analyte_Stats = metals %>% 
  group_by(stationname,commonname,AnalyteName) %>% 
  summarise(avg=mean(Result_ww_ppb),
            med=median(Result_ww_ppb),
            sd=sd(Result_ww_ppb),
            min=min(Result_ww_ppb),
            max=max(Result_ww_ppb),
            n=n())

write.csv(Station_Spp_Analyte_Stats, here("data_output","Stat_Station_Spp_Analytes.csv"))


# Plots by analyte & waterbody ------------------------------------------------------

## create list of metals datasets ####
#metals_list <- c("metals_Chollas", "metals_Cuyamaca", "metals_Dana",
#                 "metals_IB", "metals_Sweetwater", "metals_MB",
#                 "metals_OH", "metals_OP", "metals_SDR", "metals_SDB")

## create list of waterbody names ####
#waterbodies <- c("Chollas Reservoir", "Cuyamaca Reservoir","Dana Point Harbor",
#                 "Imperial Beach Pier","Lower Sweetwater River","Mission Bay",
#                 "Oceanside Harbor","Oceanside Pier","San Diego River","San Diego Bay")

# Single waterbody, multiple analytes in one plot
ggplot(metals_Chollas,aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=3.75, y=15, label= "")+
  annotate("text", x=3.5, y=30, label= as.character(paste("\U2190","3+")), color = "grey30")+
  annotate("text", x=3.5, y=110, label= "2", color = "grey30")+
  annotate("text", x=3.5, y=300, label= as.character(paste("\U2190","1 meals/wk","\U2192")), color = "grey30")+
  annotate("text", x=3.5, y=490, label= as.character(paste("0","\U2192")), color = "grey30")+
  ylim(c(0,500))+
  ggtitle("Chollas Lake")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1),
        axis.text.y = element_text(hjust=0.95)) +
  facet_grid(~ AnalyteName)

ggsave(filename = here("figures","Chollas_Lake_Metals.png"), width = 9, height = 3.6, dpi = 300)


# Plots multiple waterbodies, single analyte - Hg ------------------------------------------------------
# Reservoirs and Rivers
ggplot(subset(metals, AnalyteName=="Mercury" & waterbody_type=="Reservoir-River"),
       aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=8.75, y=15, label= "")+
  annotate("text", x=8.5, y=15, label= as.character(paste("\U2190","3+")), color = "grey30")+
  annotate("text", x=8.5, y=110, label= "2", color = "grey30")+
  annotate("text", x=8.5, y=300, label= as.character(paste("\U2190","1 meals/wk","\U2192")), color = "grey30")+
  annotate("text", x=8.5, y=490, label= as.character(paste("0","\U2192")), color = "grey30")+
  ylim(c(0,500))+
  ggtitle("Mercury")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1)) +
  facet_wrap(~ stationname, nrow = 2)

ggsave(filename = here("figures","Analyte_Hg_ResRiv.png"), width = 8, height = 7.7, dpi = 300)

# Coast
ggplot(subset(metals, AnalyteName=="Mercury" & waterbody_type=="Coastal"),
       aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  geom_hline(yintercept = 70,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 150,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 440,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=19.5, y=15, label= "")+
  annotate("text", x=19, y=15, label= as.character(paste("\U2190","3+")), color = "grey30")+
  annotate("text", x=19, y=110, label= "2", color = "grey30")+
  annotate("text", x=19, y=300, label= as.character(paste("\U2190","1 meals/wk","\U2192")), color = "grey30")+
  annotate("text", x=19, y=490, label= as.character(paste("0","\U2192")), color = "grey30")+
  ylim(c(0,500))+
  ggtitle("Mercury")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1.25,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1)) +
  facet_wrap(~ stationname, nrow = 2)

ggsave(filename = here("figures","Analyte_Hg_Coast.png"), width = 9.8, height = 7.7, dpi = 300)

# Plots multiple waterbodies, single analyte - Se ------------------------------------------------------

# Reservoirs and Rivers
ggplot(subset(metals, AnalyteName=="Selenium" & waterbody_type=="Reservoir-River"),
       aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=7.75, y=900, label= "")+
  annotate("text", x=7.5, y=1000, label= as.character(paste("\U2190","3+")), color = "grey30")+
  annotate("text", x=7.5, y=3700, label= "2", color = "grey30")+
  annotate("text", x=7.5, y=10000, label= as.character(paste("\U2190","1 meals/wk","\U2192")), color = "grey30")+
  annotate("text", x=7.5, y=17000, label= as.character(paste("0","\U2192")), color = "grey30")+
  ylim(c(0,18000))+
  ggtitle("Selenium")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1)) +
  facet_wrap(~ stationname, nrow = 2)

ggsave(filename = here("figures","Analyte_Se_ResRiv.png"), width = 8, height = 7.7, dpi = 300)

# Coast
ggplot(subset(metals, AnalyteName=="Selenium" & waterbody_type=="Coastal"),
       aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  geom_hline(yintercept = 2500,linetype="dotted",color="yellow",lwd=1.5)+
  geom_hline(yintercept = 4900,linetype="dashed",color="orange",lwd=1.5)+
  geom_hline(yintercept = 15000,linetype="solid",color="red",lwd=1.5)+
  annotate("text", x=17.5, y=900, label= "")+
  annotate("text", x=17, y=1000, label= as.character(paste("\U2190","3+")), color = "grey30")+
  annotate("text", x=17, y=3700, label= "2", color = "grey30")+
  annotate("text", x=17, y=10000, label= as.character(paste("\U2190","1 meals/wk","\U2192")), color = "grey30")+
  annotate("text", x=17, y=17000, label= as.character(paste("0","\U2192")), color = "grey30")+
  ylim(c(0,18000))+
  ggtitle("Selenium")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1.25,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1)) +
  facet_wrap(~ stationname, nrow = 2)

ggsave(filename = here("figures","Analyte_Se_Coast.png"), width = 9.8, height = 7.7, dpi = 300)

# Plots multiple waterbodies, single analyte - As ------------------------------------------------------
# No vertical lines since our results are in total form and the guidance is inorganic only
# Ask Wes if there's a work around...

# Reservoirs and Rivers
ggplot(subset(metals, AnalyteName=="Arsenic" & waterbody_type=="Reservoir-River"),
       aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  ylim(c(0,1300))+
  scale_y_continuous(breaks=seq(0,1200,200))+
  ggtitle("Arsenic")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1)) +
  facet_wrap(~ stationname, nrow = 2)

ggsave(filename = here("figures","Analyte_As_ResRiv.png"), width = 8, height = 7.7, dpi = 300)

# Coast
ggplot(subset(metals, AnalyteName=="Arsenic" & waterbody_type=="Coastal"),
       aes(x=commonname, y=Result_ww_ppb)) +
  geom_segment(aes(x=commonname ,xend=commonname, y=0, yend=Result_ww_ppb), color="blue") +
  geom_point(size=2, color="blue") +
  coord_flip() +
  xlab("") +
  ylab("Concentration (ww pbb)")+
  ylim(c(0,12500))+
  ggtitle("Arsenic")+
  theme(plot.title = element_text(vjust = 2, hjust = 0.5),
        aspect.ratio = 1.25,
        plot.margin = margin(10,0,5,0),
        axis.ticks.length = unit(0, "pt"),
        axis.text.x = element_text(vjust=1)) +
  facet_wrap(~ stationname, nrow = 2)

ggsave(filename = here("figures","Analyte_As_Coast.png"), width = 9.8, height = 7.7, dpi = 300)
