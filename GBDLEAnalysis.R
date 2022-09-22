rm(list=ls())

#Mortality smooth is currently off CRAN, so take mirrored version from Tim Riffe's GitHub
#remotes::install_github("timriffe/MortalitySmooth")

library(tidyverse)
library(MortalitySmooth)
library(paletteer)
library(ragg)
library(extrafont)
library(gt)
library(geofacet)
library(sf)
library(rnaturalearth)

#remotes::install_github("timriffe/DemoTools")
library(DemoTools)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}


options(scipen=10000)

#Bring in raw data, all downloaded from https://vizhub.healthdata.org/gbd-results/
#Downloaded in a load of different files because of restrictions on fields per file
rawdata1 <- read.csv("Data/GBDLEProject/GBDData1.csv")
rawdata2 <- read.csv("Data/GBDLEProject/GBDData2.csv")
rawdata3 <- read.csv("Data/GBDLEProject/GBDData3.csv")
rawdata4 <- read.csv("Data/GBDLEProject/GBDData4.csv")
rawdata5 <- read.csv("Data/GBDLEProject/GBDData5.csv")
rawdata6 <- read.csv("Data/GBDLEProject/GBDData6.csv")
rawdata7 <- read.csv("Data/GBDLEProject/GBDData7.csv")
rawdata8 <- read.csv("Data/GBDLEProject/GBDData8.csv")

data <- bind_rows(rawdata1, rawdata2, rawdata7, rawdata8, rawdata5, rawdata6) %>% 
  select(-c(upper, lower)) %>% 
  #Back out population estimates
  spread(metric, val) %>% 
  mutate(popest=Number*100000/Rate) %>% 
  group_by(location, sex, age) %>% 
  mutate(popest=mean(popest, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #Fill in blanks
  select(-Rate) %>% 
  spread(rei, Number) %>% 
  mutate(across(c(`Alcohol use`, `Drug use`, Tobacco), ~replace_na(.x, 0))) %>% 
  #Bring in all cause mortality
  merge(bind_rows(rawdata3, rawdata4) %>%
          filter(metric=="Number") %>% 
          select(location, sex, age, val), all.x=T) %>% 
  rename("AllCause"="val") %>% 
  mutate(agestart=case_when(
    age=="<1 year" ~ 0, age=="1-4 years" ~ 1, age=="5-9 years" ~ 5, 
    TRUE ~ as.numeric(substr(age, 1, 2)))) %>% 
  #Remove countries so small that the numbers make the model fall over
  filter(!location %in% c("Tokelau"))

LEoutcomes <- data.frame(location=character(), Cause=character(), sex=character(), e0=character())

#Overall LE trend
for(i in c(unique(data$location))){
  for(k in c("Male", "Female")){
      workingdata <- data %>% 
        filter(location==i & sex==k) %>% 
        select(agestart, AllCause, popest) %>% 
        arrange(agestart)
      
      lifetable <- lt_abridged(Deaths=workingdata$AllCause, Exposures=workingdata$popest,
                               Age=workingdata$agestart, Sex=if_else(k=="Male", "m", "f"))
      
      LEoutcomes <- bind_rows(LEoutcomes,
                              c(location=i, Cause="AllCause", sex=k, e0=lifetable$ex[1]))
    
  }
}

#Cause-deleted LE trends for deaths of despair
for(i in c(unique(data$location))){
  for(j in c("Alcohol use", "Drug use", "Tobacco")){
    for(k in c("Male", "Female")){
        workingdata <- data %>% 
          filter(location==i & sex==k) %>% 
          mutate(cause_dx=get(j),
                 cause_dx=if_else(cause_dx<=0, 0.1, cause_dx)) %>% 
          select(agestart, AllCause, cause_dx, popest) %>% 
          arrange(agestart)%>% 
          mutate(rx=if_else(cause_dx==0, 0, cause_dx/AllCause), mx=AllCause/popest, 
                 qx=case_when(
                   agestart==0 ~ 1-exp(-1*mx),
                   agestart==1 ~ 1-exp(-4*mx),
                   TRUE ~ 1-exp(-5*mx)), 
                 qx_dag=1-((1-qx) ^ (1-rx)),
                 qx_dag=if_else(qx_dag<0, 0, qx_dag))
        
        lifetable <- lt_abridged(nqx=workingdata$qx_dag, Age=workingdata$agestart, 
                                 Sex=if_else(k==1, "m", "f"))    
        
        LEoutcomes <- bind_rows(LEoutcomes,
                                c(location=i, Cause=j, sex=k, e0=lifetable$ex[1]))
      
    }
  }
}

Outputs <- LEoutcomes %>% 
  #Sort out formatting
  mutate(sex=factor(sex, levels=c("Male", "Female")),
         e0=as.double(e0)) %>% 
  spread(Cause, e0) %>% 
  set_names("Country", "Sex", "Alcohol_e0", "AllCause_e0", "Drug_e0", "Tobacco_e0") %>%
  mutate(Alcohol_loss=AllCause_e0-Alcohol_e0,
         Drug_loss=AllCause_e0-Drug_e0,
         Tobacco_loss=AllCause_e0-Tobacco_e0) %>% 
  pivot_longer(cols=c(3,5,6,7:9), names_to=c("Cause",".value"), names_sep="_") %>% 
  mutate(rel_loss=loss/AllCause_e0)

write.csv(Outputs, file="Data/GBDLEProject/Outputs.csv")
