rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(readxl)
library(ragg)
library(cowplot)
library(extrafont)
library(ggtext)
library(ggrepel)
library(scales)
library(readODS)
library(geomtextpath)

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

#############################################################################################
#Drug-related deaths in Scotland
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/21/drug-related-deaths-21-tabs-figs.xlsx"
rawdata <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in DRDs by age
DRD.s <- read_excel(rawdata, sheet="4 - sex and age", range="A6:G27", col_names=FALSE) %>% 
  mutate(cause="DRD") %>% 
  select(-2) %>% 
  gather(age, deaths, c(2:6)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "under 25",
    age=="...4" ~ "25-34",
    age=="...5" ~ "35-44",
    age=="...6" ~ "45-54",
    age=="...7" ~ "55+"),
    age=factor(age, levels=c("under 25", "25-34", "35-44",
                             "45-54", "55+"))) %>% 
    rename("year"="...1")

#Alcohol-specific deaths by age for Scotland (only up to 2019 ATM)
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2020/alcohol-specific-deaths-20-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.s <- read_excel(temp, sheet="Table 4", range="A7:U48", col_names=FALSE) %>% 
  mutate(cause="ASD") %>% 
  select(-2) %>% 
  gather(age, deaths, c(2:20)) %>% 
  mutate(age=case_when(
    age %in% c("...3", "...4", "...5", "...6", "...7") ~ "under 25",
    age %in% c("...8", "...9") ~ "25-34",
    age %in% c("...10", "...11") ~ "35-44",
    age %in% c("...12", "...13") ~ "45-54",
    age %in% c("...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21") ~ "55+"),
    age=factor(age, levels=c("under 25", "25-34", "35-44",
                             "45-54", "55+"))) %>% 
  rename("year"="...1") %>% 
  group_by(year, age, cause) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

#Read in population data for 2001-2020
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2001tomid2020detailedtimeseries/ukdetailedtimeseries2001to2020.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

rawpop <- read.csv(file.path(temp2, "MYEB1_detailed_population_estimates_series_UK_(2020_geog20).csv")) %>% 
  mutate(age=case_when(
    age<25 ~ "under 25",
    age<35 ~ "25-34",
    age<45 ~ "35-44",
    age<55 ~ "45-54",
    TRUE ~ "55+")) %>% 
  gather(year, pop, c(6:25)) %>% 
  group_by(ladcode20, laname20, country, age, year) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(year=as.numeric(substr(year, 12,16)))

#Population data for 2021 for Scotland
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-21/mid-year-pop-est-21-data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop21.s <- read_excel(temp, sheet="Table 1", range="F4:CR5") %>% 
  gather(age, pop, c(1:ncol(.))) %>% 
  mutate(age=as.numeric(gsub("\\+", "", age)),
         age=case_when(
           age<25 ~ "under 25",
           age<35 ~ "25-34",
           age<45 ~ "35-44",
           age<55 ~ "45-54",
           TRUE ~ "55+")) %>% 
  group_by(age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(year=2021, country="S")

natpop <- rawpop %>% 
  group_by(country, age, year) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  bind_rows(pop21.s)

data.s <- bind_rows(DRD.s, ASD.s) %>% 
  mutate(country="S") %>% 
  merge(natpop) %>% 
  mutate(mortrate=deaths*100000/pop) %>% 
  arrange(cause, age, year) %>% 
  mutate(index=c(1:100, 1:105),
         index2=c(1:20, 22:41, 43:62, 64:83, 85:104, 1:21, 23:43, 45:65, 67:87, 89:109))

data_drd <- data.s %>% filter(cause=="DRD")

#grouped path of DRD
x1 <- c(0, data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2001],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2002],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2003],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2004],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2005],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2006],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2007],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2008],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2009],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2010],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2011],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2012],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2013],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2014],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2015],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2016],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2017],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2018],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2019],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2020],
        data_drd$mortrate[data_drd$age=="under 25" & data_drd$year==2021],
        0)

x2 <- c(0, data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2001],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2002],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2003],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2004],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2005],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2006],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2007],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2008],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2009],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2010],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2011],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2012],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2013],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2014],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2015],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2016],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2017],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2018],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2019],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2020],
        data_drd$mortrate[data_drd$age=="25-34" & data_drd$year==2021],
        0)
x3 <- c(0, data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2001],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2002],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2003],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2004],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2005],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2006],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2007],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2008],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2009],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2010],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2011],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2012],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2013],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2014],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2015],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2016],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2017],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2018],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2019],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2020],
        data_drd$mortrate[data_drd$age=="35-44" & data_drd$year==2021],
        0)

x4 <- c(0, data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2001],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2002],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2003],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2004],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2005],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2006],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2007],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2008],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2009],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2010],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2011],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2012],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2013],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2014],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2015],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2016],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2017],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2018],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2019],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2020],
        data_drd$mortrate[data_drd$age=="45-54" & data_drd$year==2021],
        0)

x5 <- c(0, data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2001],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2002],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2003],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2004],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2005],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2006],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2007],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2008],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2009],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2010],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2011],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2012],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2013],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2014],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2015],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2016],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2017],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2018],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2019],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2020],
        data_drd$mortrate[data_drd$age=="55+" & data_drd$year==2021],
        0)

DRDplot <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), y=x1), fill="Tomato")+
  geom_polygon(aes(x=c(23, 23:43, 43), y=x2), fill="Tomato")+
  geom_polygon(aes(x=c(45, 45:65, 65), y=x3), fill="Tomato")+
  geom_polygon(aes(x=c(67, 67:87, 87), y=x4), fill="Tomato")+
  geom_polygon(aes(x=c(89, 89:109, 109), y=x5), fill="Tomato")+
  geom_path(data=data_drd,aes(x=index2, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_custom()+
  scale_x_continuous(breaks=c(10,33,55,77,99), 
                     labels=c("under 25", "25-34", "35-44", "45-54", "55+"),name="Age")+
  scale_y_continuous(name="Annual drug-related deaths per 100,000")+
  labs(title="Scotland's drug death epidemic has hit 35-54 year olds hardest",
       subtitle="Annual rates of drug-related deaths by age group in Scotland 2001-2021",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

DRDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,6,4,3,9,10,12,13,10,16,15,17,11,14,18,16,20,18,21,17,15,22,0)), 
               fill="Tomato")+
  geom_line(aes(x=c(1:21), 
                y=c(6,4,3,9,10,12,13,10,16,15,17,11,14,18,16,20,18,21,17,15,22)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

DRDfull <- ggdraw()+
  draw_plot(DRDplot)+
  draw_plot(DRDinset, x=0.82, y=0.75, width=0.13, height=0.2)+
  draw_label("2001", x=0.84, y=0.76, size=10)+
  draw_label("2021", x=0.94, y=0.76, size=10)+
  draw_label("Key", x=0.88, y=0.95, size=10, fontface="bold")

tiff("Outputs/DRDScot2022.tiff", units="in", width=9, height=6.6, res=500)
ggdraw(DRDfull)
dev.off()

###################################
#TODO - update this ASD plot - new ASD data for Scotland out on 4th August

data_asd <- data.s %>% filter(cause=="ASD")

#grouped path of ASD
y1 <- c(0, data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="under 15" & data_asd$year==2019],0)

y2 <- c(0, data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="15-24" & data_asd$year==2019],0)

y3 <- c(0, data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2019],0)

y4 <- c(0, data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2019],0)

y5 <- c(0, data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2019],0)

y6 <- c(0, data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="55-64" & data_asd$year==2019],0)

y7 <- c(0, data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="65+" & data_asd$year==2019],0)

ASDplot <- ggplot()+
  geom_polygon(aes(x=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,16), y=y1), fill="SkyBlue")+
  geom_polygon(aes(x=c(17,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,32), y=y2), fill="SkyBlue")+
  geom_polygon(aes(x=c(33,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,48), y=y3), fill="SkyBlue")+
  geom_polygon(aes(x=c(49,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,64), y=y4), fill="SkyBlue")+
  geom_polygon(aes(x=c(65,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,80), y=y5), fill="SkyBlue")+
  geom_polygon(aes(x=c(81,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,96), y=y6), fill="SkyBlue")+
  geom_polygon(aes(x=c(97,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,112), y=y7), fill="SkyBlue")+
  geom_path(data=data_asd,aes(x=index, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_x_continuous(breaks=c(8,25,41,57,73,90,106), 
                     labels=c("under 15", "15-24", "25-34", "35-44", "45-54",
                              "55-64", "65+"),name="Age")+
  scale_y_continuous(name="Annual alcohol-specific deaths per 100,000")+
  labs(title="Scotland has made significant progress in reducing alcohol-specific deaths in middle ages",
       subtitle="Annual rates of alcohol-specific deaths by age group in Scotland 2004-2019",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

ASDinset <- ggplot()+
  geom_polygon(aes(x=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,16), 
                   y=c(0,15,18,16,20,16,13,11,10,15,12,9,8,10,7,9,6,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                y=c(15,18,16,20,16,13,11,10,15,12,9,8,10,7,9,6)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDfull <- ggdraw()+
  draw_plot(ASDplot)+
  draw_plot(ASDinset, x=0.15, y=0.65, width=0.1, height=0.2)+
  draw_label("2004", x=0.17, y=0.66, size=10)+
  draw_label("2019", x=0.24, y=0.66, size=10)+
  draw_label("Key", x=0.18, y=0.85, size=10, fontface="bold")

tiff("Outputs/ASDScot2020.tiff", units="in", width=9, height=6.6, res=500)
ggdraw(ASDfull)
dev.off()

#Combined plot
combplot <- ggplot()+
  geom_polygon(aes(x=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,17), y=x1), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(18,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,34), y=x2), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(35,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,51), y=x3), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(52,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,68), y=x4), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(69,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,85), y=x5), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(86,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,102), y=x6), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(103,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,119), y=x7), fill="Tomato", alpha=0.8)+
  geom_path(data=data_drd,aes(x=index, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  geom_polygon(aes(x=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,16), y=y1), fill="SkyBlue", alpha=0.5)+
  geom_polygon(aes(x=c(18,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,33), y=y2), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(35,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,50), y=y3), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(52,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,67), y=y4), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(69,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,84), y=y5), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(86,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,101), y=y6), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(103,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,118), y=y7), fill="SkyBlue", alpha=0.6)+
  geom_path(data=data_asd,aes(x=index2, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  scale_x_continuous(breaks=c(9,26,43,60,77,94,111), 
                     labels=c("under 15", "15-24", "25-34", "35-44", "45-54",
                              "55-64", "65+"),name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  labs(title="The opposing trajectories of drug and alcohol deaths in Scotland",
       subtitle="Annual rates of <span style='color:tomato3;'>drug-related</span> and <span style='color:skyblue3;'>alcohol-specific</span> deaths between 2004 and 2020<br>(Alcohol-specific deaths data for 2020 will be published in mid-August)",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

combfull <- ggdraw()+
  draw_plot(combplot)+
  draw_plot(ASDinset, x=0.1, y=0.58, width=0.13, height=0.15)+
  draw_plot(DRDinset, x=0.1, y=0.72, width=0.13, height=0.15)+
  draw_label("2004", x=0.12, y=0.58, size=10)+
  draw_label("2020", x=0.21, y=0.58, size=10)+
  draw_label("Key", x=0.15, y=0.88, size=10, fontface="bold")+
  draw_label("Alcohol", x=0.17, y=0.625, size=10)+
  draw_label("Drugs", x=0.17, y=0.77, size=10)

tiff("Outputs/ASDDRDScot2021.tiff", units="in", width=9, height=6.6, res=500)
ggdraw(combfull)
dev.off()

#############################################################################
#DRDs by sex
DRD.sex.m <- read_excel(rawdata, sheet="4 - sex and age", range="A30:B51", col_names=FALSE) %>% 
  set_names("year", "deaths") %>% 
  mutate(sex="Male")

DRD.sex.f <- read_excel(rawdata, sheet="4 - sex and age", range="A54:B75", col_names=FALSE) %>% 
  set_names("year", "deaths") %>% 
  mutate(sex="Female")

DRD.sex <- bind_rows(DRD.sex.m, DRD.sex.f)

lab.f <- DRD.sex$deaths[DRD.sex$sex=="Female" & DRD.sex$year==2021]/DRD.sex$deaths[DRD.sex$sex=="Female" & DRD.sex$year==2000]
lab.m <- DRD.sex$deaths[DRD.sex$sex=="Male" & DRD.sex$year==2021]/DRD.sex$deaths[DRD.sex$sex=="Male" & DRD.sex$year==2000]

lab.f <- paste0("+",round(lab.f*100,0),"%")
lab.m <- paste0("+",round(lab.m*100,0),"%")

tiff("Outputs/DRDScotxSex.tiff", units="in", width=9, height=6.6, res=500)
ggplot(DRD.sex)+
  geom_textline(aes(x=year, y=deaths, colour=sex, label=sex), show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual drug-related deaths", limits=c(0,NA))+
  scale_colour_manual(name="", values=c("#00cc99", "#6600cc"))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Women have seen a bigger relative increase in drug-related deaths in Scotland",
       subtitle="Since 2000, drug-related deaths <span style='color:#00cc99;'>in women</span> have increased more than fivefold, while they have trebled <span style='color:#6600cc;'>in men</span>",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")+
  annotate("text", x=2021, y=900, label=lab.m)+
  annotate("text", x=2021, y=430, label=lab.f)
dev.off()

###############################################################################
#DRDs by drug type
DRD.drg <- read_excel(rawdata, sheet="3 - drugs reported", range="A22:R35", col_names=FALSE) %>% 
  gather(drug, deaths, c(3:18)) %>% 
  rename(year=`...1`, total=`...2`) %>% 
  mutate(drug=case_when(
    drug=="...4" ~ "Heroin/morphine",
    drug=="...5" ~ "Methadone",
    drug=="...6" ~ "Bupenorphine",
    drug %in% c("...7", "...8") ~ "Codeine/Dihydrocodeine",
    drug=="...10" ~ "'Prescribable' benzodiazepine",
    drug=="...12" ~ "'Street' benzodiazepine",
    drug=="...14" ~ "Gabapentin/Pregabalin",
    drug=="...15" ~ "Cocaine")) %>% 
  group_by(year, drug, total) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  filter(!is.na(drug)) %>% 
  mutate(deathprop=deaths/total,
         drug=factor(drug, levels=c("Heroin/morphine", "Methadone", "'Prescribable' benzodiazepine",
                                    "'Street' benzodiazepine", "Codeine/Dihydrocodeine",
                                    "Gabapentin/Pregabalin", "Cocaine", "Bupenorphine")))

#Plot of totals
tiff("Outputs/DRDScotxDrugAbs.tiff", units="in", width=9, height=6, res=500)
ggplot()+
  geom_line(data=DRD.drg, aes(x=year, y=deaths, colour=drug), show.legend=FALSE)+
  geom_text_repel(data=DRD.drg %>% filter(year==2021),aes(color=drug, label=drug, x=year, y=deaths),
                  show.legend=FALSE, family="Lato", xlim=(c(2021.1, NA)), segment.color = NA)+
  scale_x_continuous(name="", limits=c(2008, 2021))+
  scale_y_continuous(name="Deaths reported as involving...")+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  coord_cartesian(clip = 'off') +
  theme_custom()+
  theme(plot.margin = unit(c(1,10,1,1), "lines"),
        plot.subtitle=element_markdown())+
  labs(title="Drug deaths in 2021 fell for the drugs involved in most deaths",
       subtitle="Deaths involving <span style='color:#C70E7B;'>opiates</span>/<span style='color:#FC6882;'>opiods</span>, <span style='color:#54BCD1;'>'street' benzodiazepine</span>, <span style='color:#F4B95A;'>Gapanentin/Pregablin</span> and <span style='color:#009F3F;'>cocaine</span> all fell",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Plot of proportions
tiff("Outputs/DRDScotxDrugProp.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_line(data=DRD.drg, aes(x=year, y=deathprop, colour=drug), show.legend=FALSE)+
  geom_text_repel(data=DRD.drg %>% filter(year==2021),aes(color=drug, label=drug, x=year, y=deathprop),
                  show.legend=FALSE, family="Lato", xlim=(c(2021.1, NA)), segment.color = NA)+
  scale_x_continuous(name="", breaks=c(2008:2021))+
  scale_y_continuous(name="Proportion of all drug-related deaths which involve...",
                     labels = scales::percent_format(accuracy = 2))+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  coord_cartesian(clip = 'off') +
  theme_custom()+
  theme(plot.margin = unit(c(1,10,1,1), "lines"),
        plot.title=element_markdown(),
        plot.subtitle=element_markdown())+
  labs(title="Deaths involving <span style='color:#54BCD1;'>'street' benzodiazepine </span> have exploded since 2015",
       subtitle="While the proportion involving <span style='color:#F4B95A;'>Gabapentin/Pregablin</span> or <span style='color:#009F3F;'>cocaine</span> has also risen, but more slowly",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#And by age
DRD.drg.age <- read_excel(rawdata, sheet="6 - sex, age and drugs", range="A16:R20", col_names=FALSE) %>% 
  gather(drug, deaths, c(3:18)) %>% 
  rename(age=`...1`, total=`...2`) %>% 
  mutate(drug=case_when(
    drug=="...4" ~ "Heroin/morphine",
    drug=="...5" ~ "Methadone",
    drug=="...6" ~ "Bupenorphine",
    drug %in% c("...7", "...8") ~ "Codeine/Dihydrocodeine",
    drug=="...10" ~ "'Prescribable' benzodiazepine",
    drug=="...12" ~ "'Street' benzodiazepine",
    drug=="...14" ~ "Gabapentin/Pregabalin",
    drug=="...15" ~ "Cocaine")) %>% 
  group_by(age, drug, total) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  filter(!is.na(drug)) %>% 
  mutate(deathprop=deaths/total,
         drug=factor(drug, levels=c("Heroin/morphine", "Methadone", "'Prescribable' benzodiazepine",
                                    "'Street' benzodiazepine", "Codeine/Dihydrocodeine",
                                    "Gabapentin/Pregabalin", "Cocaine", "Bupenorphine")),
         age=factor(age, levels=c("Under 25", "25-34", "35-44", "45-54", "55 and over"))) %>% 
  group_by(drug) %>% 
  mutate(drugtot=sum(deaths)) %>% 
  ungroup() %>% 
  mutate(deathprop2=deaths/drugtot)

tiff("Outputs/DRDScotxAgexDrugProp.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_textline(data=DRD.drg.age, aes(x=age, y=deathprop, group=drug, colour=drug, label=drug), show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Proportion of all drug-related deaths which involve...",
                     labels = scales::percent_format(accuracy = 2))+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  coord_cartesian(clip = 'off') +
  theme_custom()+
  theme(plot.margin = unit(c(1,10,1,1), "lines"),
        plot.title=element_markdown())+
  labs(title="<span style='color:#009F3F;'>Cocaine </span> is implicated in a greater proportion of deaths in younger age groups",
       subtitle="Other drugs are more likely to be involved in deaths at older ages",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

ggplot(DRD.drg.age, aes(x=deathprop2, y=drug, fill=age))+
  geom_col()+
  theme_custom()+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="Age")+
  scale_x_continuous(name="Proportion of all drug-related deaths involving...",label=label_percent(accuracy=1))

tiff("Outputs/DRDScotxAgexDrugProp2.tiff", units="in", width=9, height=6.6, res=500)
ggplot(DRD.drg.age, aes(x=deathprop, y=fct_rev(drug), fill=drug))+
  geom_col(position="dodge", show.legend=FALSE)+
  theme_custom()+
  scale_fill_paletteer_d("LaCroixColoR::paired")+
  scale_x_continuous(name="Proportion of all drug-related deaths involving...",
                     label=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  facet_wrap(~age)+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="In all age groups in Scotland,  <span style='color:#54BCD1;'>'street' benzos</span> are involved in the most deaths",
       subtitle="But in younger age groups, <span style='color:#009F3F;'>Cocaine</span> is also involved in a substantial proportion of drug-related deaths.<br>Deaths can (and often do) involve multiple drugs, so proportions within each age group sum to more than one.<br>")

dev.off()

####################################################################
#DRDs in Scotland by ICD-10 codes
DRD.cause <- read_excel(rawdata, sheet="2 - causes", range="C22:E32", col_names=FALSE) %>% 
  mutate(year=c(2011:2021)) %>% 
  gather(cause, deaths, c(1:3)) %>% 
  mutate(cause=case_when(
    cause=="...1" ~ "Drug abuse",
    cause=="...2" ~ "Accidental poisoning",
    TRUE ~ "Intentional self-poisoning"))

tiff("Outputs/DRDScotxCause.tiff", units="in", width=9, height=6.6, res=500)
ggplot(DRD.cause)+
  geom_line(aes(x=year, y=deaths, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(2011:2021))+
  scale_y_continuous(name="Annual drug-related deaths")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The rise in drug-related deaths is entirely driven by accidental overdoses",
       subtitle="Drug-related deaths in Scotland from <span style='color:#E69F00;'>accidental poisoning</span>, <span style='color:#56B4E9;'>drug abuse</span> and <span style='color:#009E73;'>intentional self-poisoning",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#################################################################################
#DRDs in Scotland by deprivation
DRD.SIMD <- read_excel(rawdata, sheet="12 - SIMD Deciles", range="F9:AS29", col_names=FALSE) %>% 
  select(`...1`, `...5`, `...9`, `...13`, `...17`, `...21`, `...25`, `...29`, `...33`, `...37`) %>% 
  set_names(c("SIMD10 (most deprived)", "SIMD9", "SIMD8", "SIMD7", "SIMD6", "SIMD5", "SIMD4", "SIMD3",
              "SIMD2", "SIMD1 (least deprived)")) %>% 
  mutate(year=2001:2021,
         SIMD2=as.numeric(SIMD2), `SIMD1 (least deprived)`=as.numeric(`SIMD1 (least deprived)`)) %>% 
  gather(SIMD, DRD.rate, c(1:10)) %>% 
  mutate(SIMD=factor(SIMD, levels=c("SIMD10 (most deprived)", "SIMD9", "SIMD8", "SIMD7", "SIMD6", "SIMD5", "SIMD4", "SIMD3",
                                    "SIMD2", "SIMD1 (least deprived)")),
         label=case_when(
           SIMD=="SIMD10 (most deprived)" ~ "Most deprived decile",
           SIMD=="SIMD1 (least deprived)" ~ "Least deprived decile",
           TRUE ~ ""))

tiff("Outputs/DRDScotxSIMD.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_line(data=DRD.SIMD, aes(x=year, y=DRD.rate, group=SIMD, colour=SIMD), show.legend=FALSE)+
  geom_text_repel(data=DRD.SIMD %>% filter(year==2021), aes(x=year, y=DRD.rate, label=label, colour=SIMD),
                  family="Lato", xlim=(c(2021.1, NA)), segment.color = NA, show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised rate of drug-related deaths per 100,000")+
  scale_colour_manual(values=c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb",
                               "#fee0b6", "#fdb863", "#e08214", "#b35806", "#7f3b08"))+
  theme_custom()+
  coord_cartesian(clip = 'off')+
  theme(plot.margin = unit(c(1,10,1,1), "lines"))+
  labs(title="Drug-related deaths in Scotland are *incredibly* unequal",
       subtitle="Age-standardised rates of drug-related deaths by decile of the Scottish Index of Multiple Deprivation.\nValues based on fewer than 10 deaths are censored.",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()  

#################################################################################
#DRDs in Scotland by HB
DRD.HB <- read_excel(rawdata, sheet="HB1 - summary", range="A5:M19", col_names=FALSE) %>% 
  gather(year, deaths, c(2:13)) %>% 
  rename(HB="...1") %>% 
  mutate(year=as.numeric(substr(year, 4,5))+2008,
         HB=str_replace(HB, "&", "and"),
         HB=str_replace(HB, " 3", ""))

#Bring in populations
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-21/mid-year-pop-est-21-time-series-data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

HBpop <- read_excel(temp, sheet="Table_2", range="B7:E1851", col_names=FALSE) %>% 
  set_names("HB", "sex", "year", "pop") %>% 
  filter(sex=="Persons" & year>=2010)

DRD.HB <- merge(DRD.HB, HBpop, all.x=TRUE) %>% 
  mutate(mortrate=deaths*100000/pop)

tiff("Outputs/DRDScotxHB.tiff", units="in", width=9, height=6.6, res=500)
ggplot(DRD.HB %>% filter(HB!="Scotland"))+
  geom_line(aes(x=year, y=mortrate, colour=HB), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(2010:2021))+
  scale_y_continuous(name="Drug-related deaths per 100,000")+
  scale_colour_manual(values=c(rep("Grey70", 6), "#c51b8a", rep("Grey70", 7)))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Scotland's drug death epidemic is centred on Glasgow",
       subtitle="Drug-related death rates in <span style='color:#c51b8a;'>Greater Glasgow & Clyde</span> compared to <span style='color:Grey70;'>other Health Board areas",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#DRDs in Scotland by HB and drug
DRD.HB.drg <- read_excel(rawdata, sheet="HB3 - drugs implicated", range="A16:R29", col_names=FALSE) %>% 
  gather(drug, deaths, c(3:18)) %>% 
  rename(HB="...1", total="...2") %>% 
  filter(total>=20) %>% 
  mutate(drug=case_when(
    drug=="...4" ~ "Heroin/morphine",
    drug=="...5" ~ "Methadone",
    drug=="...6" ~ "Bupenorphine",
    drug %in% c("...7", "...8") ~ "Codeine/Dihydrocodeine",
    drug=="...10" ~ "'Prescribable' benzodiazepine",
    drug=="...12" ~ "'Street' benzodiazepine",
    drug=="...14" ~ "Gabapentin/Pregabalin",
    drug=="...15" ~ "Cocaine")) %>% 
  group_by(HB, drug, total) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  filter(!is.na(drug)) %>% 
  mutate(deathprop=deaths/total,
         drug=factor(drug, levels=c("Heroin/morphine", "Methadone", "'Prescribable' benzodiazepine",
                                    "'Street' benzodiazepine", "Codeine/Dihydrocodeine",
                                    "Gabapentin/Pregabalin", "Cocaine", "Bupenorphine")))

tiff("Outputs/DRDScotxHBxdrug.tiff", units="in", width=10, height=8, res=500)
ggplot(DRD.HB.drg)+
  geom_col(aes(x=deathprop, y=fct_rev(HB), fill=HB), show.legend=FALSE)+
  scale_x_continuous(name="Proportion of drug-related deaths involving...", 
                     labels=scales::percent_format(accuracy=2))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c(rep("Grey70", 4), "#c51b8a", rep("Grey70", 5)))+
  facet_wrap(~drug)+
  theme_custom()+
  labs(title="There's something different about Grampian",
       subtitle="A larger proportion of deaths there are linked to cocaine or 'prescribable' benzodiazepine and a much smaller proportion to 'street' diazepine",
       caption="\n\nHealth boards with fewer than 20 deaths are excluded\nData from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

#####################################################################################
#Analysis of latest E&W DRD data




























####################################################################################################
#Bivariate map
#Read in Scottish DRD data at Council Level 2018-2020
DRD.s <- read_excel(rawdata, sheet="C1 - summary", range="A10:L41", col_names=FALSE) %>% 
  select(`...1`, `...10`, `...11`, `...12`) %>% 
  rename(LA=`...1`) %>% 
  mutate(DRD=(`...10`+`...11`+`...12`)/3) %>% 
  select(LA, DRD)

#Read in Scottish ASD data at Council Level 2017-2019
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2019/alcohol-specific-deaths-19-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.s <- as.data.frame(t(read_excel(temp, sheet="5 - Local Authority", range=c("C44:AH46"), col_names=FALSE))) %>% 
  mutate(ASD=(V1+V2+V3)/3) %>% 
  select(ASD)

#The columes in the ASD data match the DRD data, so don't bother faffing about with names
DRDASD.s <- cbind(DRD.s, ASD.s)

#Read in English & Welsh data at LTLA level
#DRDs 2018-20
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdrugmisusedeathsbylocalauthority%2fcurrent/2020localauthorities.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD.ew <- read_excel(temp, sheet="Table 6", range="A7:E438", col_names=FALSE) %>% 
  mutate(LA=coalesce(`...3`, `...4`)) %>% 
  select(`...1`, `...5`, LA) %>% 
  rename(code=`...1`, DRD=`...5`) %>%
  #adjust for the fact that the number of deaths is ths cumulative 3 year total
  mutate(DRD=DRD/3) %>% 
  #fix names that don't align with ASD data
  mutate(LA=case_when(
    LA=="Kingston upon Hull, City of" ~ "Kingston upon Hull",
    LA=="Herefordshire, County of" ~ "Herefordshire",
    LA=="Bristol, City of" ~ "Bristol",
    TRUE ~ as.character(LA)),
    code=if_else(LA=="Buckinghamshire", "E10000002", as.character(code)),
    #Tidy up Welsh LA names
    LA=if_else(substr(code, 1, 1)=="W", substr(LA, 1, regexpr("/", LA)-2), as.character(LA)))

#ASDs for England 2017-19
temp <- tempfile()
source <- "https://fingertipsws.phe.org.uk/api/all_data/csv/by_profile_id?parent_area_code=E92000001&parent_area_type_id=6&child_area_type_id=102&profile_id=87&category_area_code="
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.e <- read.csv(temp) %>% 
  filter(Indicator.ID=="91380" & Sex=="Persons" & Area.Type=="County & UA (pre 4/19)") %>% 
  select(Area.Code, Area.Name, Value, Time.period) %>% 
  rename(code=Area.Code, LA=Area.Name, rate=Value)

#Bring in population (based on 2017 data to cope with Dorset)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2017/ukmidyearestimates2017finalversion.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

LApop <- read_excel(temp, sheet="MYE2 - All", range="A6:D445", col_names=FALSE) %>% 
  select(-`...3`) %>% 
  rename(code=`...1`, LA=`...2`, pop=`...4`)

ASD.e <- merge(ASD.e, LApop, by="code", all.x=TRUE) %>% 
  select(-LA.y) %>% 
  rename(LA=LA.x) %>% 
  mutate(ASD=rate*pop/100000)

#Faff about with Dorset & Bournemouth, which are missing from the latest data
temp <- subset(ASD.e, code %in% c("E06000028", "E06000029", "E10000009") & 
                 Time.period=="2015 - 17") %>% 
  mutate(code=case_when(
    code %in% c("E06000028", "E06000029") ~ "E06000058",
    TRUE ~ "E06000059"), 
    LA=case_when(
      code=="E06000058" ~ "Bournemouth, Christchurch and Poole",
      TRUE ~ "Dorset"))

ASD.e <- ASD.e %>% 
  filter(Time.period=="2017 - 19") %>% 
  bind_rows(temp) %>% 
  select(-Time.period) %>% 
  group_by(code, LA) %>% 
  summarise(ASD=sum(ASD)) %>% 
  ungroup()

#ASDs for Wales 2015-17
temp <- tempfile()
source <- "https://www.healthmapswales.wales.nhs.uk/IAS/data/csv?viewId=155&geoId=108&subsetId=&viewer=CSV"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.w <- read.csv(temp)[c(1:22),c(2,161)] %>% 
  rename(LA=Name, ASD=Numerator.27) %>% 
  mutate(LA=if_else(LA=="The Vale of Glamorgan", "Vale of Glamorgan", as.character(LA)))

DRDASD.ew <- merge(bind_rows(ASD.e, ASD.w), DRD.ew, by="LA", all.x=TRUE) %>% 
  mutate(code=coalesce(code.x, code.y)) %>% 
  select(-code.x, -code.y)

#Read in NI DRD by LA 2017-19
temp <- tempfile()
source <- "https://www.ninis2.nisra.gov.uk/Download/Population/Drug%20Related%20Deaths%20and%20Deaths%20due%20to%20Drug%20Misuse%20(administrative%20geographies).ods"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD.ni <- read_ods(temp, sheet="LGD2014", range="A5:H15", col_names=FALSE) %>% 
  select(-C, -E, -G) %>% 
  mutate(DRD=(D+`F`+H)/3) %>% 
  rename(LA=A, code=B) %>% 
  select(LA, code, DRD)

#Read in NI ASD by LA 2017-19
temp <- tempfile()
source <- "https://www.ninis2.nisra.gov.uk/Download/Population/Alcohol%20Specific%20Deaths%20(administrative%20geographies).ods"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.ni <- read_ods(temp, sheet="LGD2014", range="A5:E15", col_names=FALSE) %>% 
  mutate(ASD=(C+D+E)/3) %>% 
  rename(LA=A, code=B) %>% 
  select(LA, code, ASD)

DRDASD.ni <- merge(DRD.ni, ASD.ni)

#Bring in populations (2019)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

LApop2 <- read_excel(temp, sheet="MYE2 - Persons", range="A6:D431", col_names=FALSE) %>% 
  select(-`...3`) %>% 
  rename(code=`...1`, LA=`...2`, pop=`...4`) %>% 
  mutate(code=if_else(code=="E06000060", "E10000002", as.character(code)))

#Scotland
DRDASD.s <- DRDASD.s %>% 
  mutate(LA=str_replace(LA, "&", "and")) %>% 
  merge(LApop2, by="LA", all.x=TRUE)

#NI
DRDASD.ni <- merge(DRDASD.ni, LApop2, all.x=TRUE)

#England
DRDASD.ew <- merge(DRDASD.ew, LApop2, all.x=TRUE, by="code") %>% 
  select(-LA.y) %>% 
  rename(LA=LA.x)

#Merge
DRDASD <- bind_rows(DRDASD.s, DRDASD.ew, DRDASD.ni) %>% 
  gather(cause, deaths, c("DRD", "ASD")) %>% 
  mutate(mortrate=deaths*100000/pop) %>% 
  mutate(country=case_when(
    substr(code, 1, 1)=="E" ~ "England",
    substr(code, 1, 1)=="W" ~ "Wales",
    substr(code, 1, 1)=="S" ~ "Scotland",
    substr(code, 1, 1)=="N" ~ "Northern Ireland"))

#Save
write.csv(DRDASD, "Data/DRDASD.csv")

#Download shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/43b324dc1da74f418261378a9a73227f_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))
names(shapefile)[names(shapefile) == "ctyua19cd"] <- "code"

map.data <- full_join(shapefile, DRDASD, by="code")

#Download Carl Baker's lovely cartogram
utla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-uppertier.gpkg")
utla <- curl_download(url=source, destfile=utla, quiet=FALSE, mode="wb")

Background <- st_read(utla, layer="7 Background")

utlacases <- st_read(utla, layer="4 UTLA-2019") %>% 
  mutate(cua.code=case_when(
    cua.code=="S12000015" ~ "S12000047",
    cua.code=="S12000046" ~ "S12000049",
    cua.code=="S12000044" ~ "S12000050",
    cua.code=="S12000024" ~ "S12000048",
    TRUE ~ cua.code)) %>% 
  left_join(DRDASD, by=c("cua.code"="code"))

Groups <- st_read(utla, layer="2 Group")

Group_labels <- st_read(utla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

#ASD map only
ASDUK <- ggplot()+
  geom_sf(data=subset(map.data, cause=="ASD"), aes(geometry=geometry, fill=mortrate), 
          colour=NA)+
  scale_fill_paletteer_c("pals::ocean.ice", direction=-1, name="Deaths\nper 100,000",
                         na.value="White")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), text=element_text(family="Roboto"))

tiff("Outputs/ASD2021UK.tiff", units="in", width=8.5, height=14, res=500)
ASDUK+labs(title="In spite of recent progress, alcohol-specific deaths remain highest in Scotland",
           subtitle="Rates of mortality from alcohol-specific causes in UK Local Authorities",
           caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        plot.subtitle=element_text(size=rel(1.2)))

dev.off()

plot1 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=utlacases %>% filter(cause=="ASD"), 
          aes(geometry=geom, fill=mortrate), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe, hjust=just), 
               size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.ice", direction=-1, name="Deaths\nper 100,000",
                         na.value="White")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))

agg_tiff("Outputs/ASD2021UKCartogram.tiff", units="in", width=9, height=10, res=800)
plot1+
  labs(title="In spite of recent progress, alcohol-specific deaths remain highest in Scotland",
       subtitle="Rates of mortality from alcohol-specific causes in UK Local Authorities",
       caption="Data from ONS, NRS, NISRA & PHE | Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")
dev.off()

#DRD map only
DRDUK <- ggplot()+
  geom_sf(data=subset(map.data, cause=="DRD"), aes(geometry=geometry, fill=mortrate), 
          colour=NA)+
  scale_fill_paletteer_c("pals::ocean.amp", name="Deaths\nper 100,000",
                         na.value="White")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), text=element_text(family="Lato"))


tiff("Outputs/DRD2021UK.tiff", units="in", width=8.5, height=14, res=500)
DRDUK+labs(title="There is huge variation in drug deaths across the country",
           subtitle="Rates of deaths from drug misuse in UK Local Authorities",
           caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        plot.subtitle=element_text(size=rel(1.2)))

dev.off()

plot2 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=utlacases %>% filter(cause=="DRD"), 
          aes(geometry=geom, fill=mortrate), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe, hjust=just), 
               size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.amp", name="Deaths\nper 100,000",
                         na.value="White")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))

agg_tiff("Outputs/DRDD2021UKCartogram.tiff", units="in", width=9, height=10, res=800)
plot2+
  labs(title="There is huge variation in drug deaths across the country",
       subtitle="Rates of deaths from drug misuse in UK Local Authorities",
       caption="Data from ONS, NRS, NISRA & PHE | Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")

dev.off()

#Both on the same plot
tiff("Outputs/ASDDRD2020UK.tiff", units="in", width=10, height=8, res=500)
plot_grid(ASDUK+labs(title="Patterns in alcohol and drug deaths across the UK",
                     subtitle="Mortality rates from <span style='color:skyblue4;'>alcohol-specific causes</span> and <span style='color:tomato4;'>drug misuse")+
            theme(plot.title=element_text(face="bold", size=rel(1.2)),
                  plot.subtitle=element_markdown()), 
          DRDUK+labs(caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction"),
          align="h")
dev.off()

tiff("Outputs/ASDDRDCartogram2020UK.tiff", units="in", width=14, height=8, res=500)
plot_grid(plot1+labs(title="Patterns in alcohol and drug deaths across the UK",
                     subtitle="Mortality rates from <span style='color:skyblue4;'>alcohol-specific causes</span> and <span style='color:tomato4;'>drug misuse")+
            theme(plot.title=element_text(face="bold", size=rel(1.2)),
                  plot.subtitle=element_markdown()), 
          plot2+labs(caption="Data from ONS, NRS, NISRA & PHE | Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction"),
          align="h")
dev.off()

#BIVARIATE MAP
bidata <- DRDASD %>% 
  select(LA, code, cause, mortrate, country) %>% 
  spread(cause, mortrate) %>% 
  #generate tertiles
  mutate(alctert=quantcut(ASD, q=3, labels=FALSE),
         drgtert=quantcut(DRD, q=3, labels=FALSE),
         #generate key for colours
         key=case_when(
           alctert==1 & drgtert==1 ~ 1,
           alctert==1 & drgtert==2 ~ 2,
           alctert==1 & drgtert==3 ~ 3,
           alctert==2 & drgtert==1 ~ 4,
           alctert==2 & drgtert==2 ~ 5,
           alctert==2 & drgtert==3 ~ 6,
           alctert==3 & drgtert==1 ~ 7,
           alctert==3 & drgtert==2 ~ 8,
           alctert==3 & drgtert==3 ~ 9),
         #assign colours
         colour=case_when(
           key==1 ~ "#CABED0",
           key==2 ~ "#BC7C5F",
           key==3 ~ "#AE3A4E",
           key==4 ~ "#89A1C8",
           key==5 ~ "#806A8A",
           key==6 ~ "#77324C",
           key==7 ~ "#4885C1",
           key==8 ~ "#435786",
           key==9 ~ "#3f2949"))

#save cutoffs
alccut1 <- quantile(bidata$ASD, probs=1/3, na.rm=TRUE)
alccut2 <- quantile(bidata$ASD, probs=2/3, na.rm=TRUE)
drgcut1 <- quantile(bidata$DRD, probs=1/3, na.rm=TRUE)
drgcut2 <- quantile(bidata$DRD, probs=2/3, na.rm=TRUE)

#generate dataframe for key
keydata <- bidata %>%
  filter(!is.na(colour)) %>%
  group_by(alctert, drgtert) %>%
  summarise(RGB=unique(colour))

bimap <- full_join(shapefile, bidata, by="code")

BIVAR <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+
  scale_fill_identity()+ labs(title="Regional patterns in deaths from alcohol and drugs across the UK",
                              subtitle="Comparative rates of alcohol-specific deaths and deaths from drug misuse by Local Authority",
                              caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")+
  #Highland
  #annotate("text", x=500000, y=970000, label="Purple areas mean\nhigh rates of alcohol and \nhigh rates of drug deaths", size=3)+
  annotate("text", x=500000, y=970000, label="Purple areas mean\nhigh rates of alcohol and \nhigh rates of drug deaths", size=3)+
  #York
  annotate("text", x=150000, y=290000, label="Blue areas mean\nhigh rates of alcohol and \nlow rates of drug deaths", size=3)+
  #Dumfires & galloway
  annotate("text", x=230000, y=470000, label="Red areas mean\nlow rates of alcohol and \nhigh rates of drug deaths", size=3)+
  #Dorset
  annotate("text", x=440000, y=27000, label="Grey areas mean\nlow rates of alcohol and \nlow rates of drug deaths", size=3)+
  geom_curve(aes(x=434000, y=955000, xend=220000, yend=850000), curvature=0.15)+
  geom_curve(aes(x=220000, y=280000, xend=315000, yend=200000), curvature=-0.15)+
  geom_curve(aes(x=300000, y=475000, xend=463000, yend=452000), curvature=-0.2)+
  geom_curve(aes(x=420000, y=57000, xend=370000, yend=100000), curvature=0.1)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.2)))

key <- ggplot(keydata)+
  geom_tile(aes(x=alctert, y=drgtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More alcohol-specific deaths" %->%  ""),
       y = expression("More drug poisoning deaths" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff("Outputs/ASDDRD2020BivariateUK.tiff", units="in", width=8.5, height=14, res=500)
ggdraw()+
  draw_plot(BIVAR, 0,0,1,1)+
  draw_plot(key, 0.03,0.48,0.29,0.74)
dev.off()

#Add zoomed in areas
#London
London <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white")+  
  xlim(500000,560000)+
  ylim(156000,200000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="Greater London")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))

#North-West England
NWEng <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white")+  
  xlim(310000,440000)+
  ylim(370000,430000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="NW England")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))

#Tyne/Tees  
NEEng <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white")+  
  xlim(405000,490000)+
  ylim(505000,580000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="NE England")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))

#Central Belt
CScot <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white")+  
  xlim(220000,341000)+
  ylim(620000,710000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="Central Scotland")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))

tiff("Outputs/ASDDRD2020BivariateUKZoomed.tiff", units="in", width=8.5, height=13, res=500)
ggdraw()+
  draw_plot(BIVAR, 0,0,0.75,1)+
  draw_plot(key, 0.03,0.7,0.24,0.24)+
  draw_plot(London, 0.72,0.12,0.27,0.18)+
  draw_plot(NWEng, 0.62,0.34, 0.35, 0.18)+
  draw_plot(NEEng, 0.72, 0.48, 0.2, 0.2)+
  draw_plot(CScot, 0.67, 0.71, 0.3, 0.2)
dev.off()

#Further explorations of the data
#scatter coloured by country
tiff("Outputs/EngScotLAALcDrg.tiff", units="in", width=7, height=6, res=500)
ggplot(bidata, aes(x=ASD, y=DRD, colour=country))+
  geom_point()+
  geom_segment(x=-10, xend=45, y=-10, yend=45, colour="Black")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_x_continuous(name="Alcohol-specific deaths per 100,000 population", limits=c(0,42))+
  scale_y_continuous(name="Drug misuse deaths per 100,000 population", limits=c(0,42))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  annotate("text", x=30, y=6, label="More deaths from alcohol", colour="DarkGrey")+
  annotate("text", x=10, y=32, label="More deaths from drugs", colour="DarkGrey")+
  labs(title="Deaths from alcohol and drugs by Local Authority", 
       subtitle="Alcohol-specific and drug misuse deaths in 2016-18",
       caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")

dev.off()

#repeat with bivariate key overlaid
tiff("Outputs/EngScotLAALcDrgTert.tiff", units="in", width=7, height=6, res=500)
ggplot(bidata, aes(x=ASD, y=DRD, colour=country))+
  geom_rect(aes(xmin=0,xmax=alccut1, ymin=0, ymax=drgcut1), fill="#CABED0", colour=NA)+
  geom_rect(aes(xmin=0,xmax=alccut1, ymin=drgcut1, ymax=drgcut2), fill="#BC7C5F", colour=NA)+
  geom_rect(aes(xmin=0,xmax=alccut1, ymin=drgcut2, ymax=42), fill="#AE3A4E", colour=NA)+
  geom_rect(aes(xmin=alccut1,xmax=alccut2, ymin=0, ymax=drgcut1), fill="#89A1C8", colour=NA)+
  geom_rect(aes(xmin=alccut1,xmax=alccut2, ymin=drgcut1, ymax=drgcut2), fill="#806A8A", colour=NA)+
  geom_rect(aes(xmin=alccut1,xmax=alccut2, ymin=drgcut2, ymax=42), fill="#77324C", colour=NA)+
  geom_rect(aes(xmin=alccut2,xmax=42, ymin=0, ymax=drgcut1), fill="#4885C1", colour=NA)+
  geom_rect(aes(xmin=alccut2,xmax=42, ymin=drgcut1, ymax=drgcut2), fill="#435786", colour=NA)+
  geom_rect(aes(xmin=alccut2,xmax=42, ymin=drgcut2, ymax=42), fill="#3f2949", colour=NA)+
  geom_point(size=1.5)+
  geom_point(shape=21, colour="White", size=1.5)+
  #geom_segment(x=-10, xend=45, y=-10, yend=45, colour="Black")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_x_continuous(name="Alcohol-specific deaths per 100,000 population", limits=c(0,42))+
  scale_y_continuous(name="Drug misuse deaths per 100,000 population", limits=c(0,42))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  labs(title="Deaths from alcohol and drugs by Local Authority", 
       subtitle="Alcohol-specific and drug misuse deaths in 2016-18 coloured by tertile",
       caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")

dev.off()

#ordered point charts
tiff("Outputs/EngScotLAALcCIs.tiff", units="in", width=8, height=6, res=500)
ggplot(bidata, aes(x=fct_reorder(as.factor(LA), ASD), y=ASD, colour=country))+
  geom_point()+
  theme_classic()+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_x_discrete(name="Local Authority")+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000")+
  labs(title="One of these countries is not like the others", 
       subtitle="Annual deaths from alcohol-specific causes per 100,000 population across UK Local Authorities",
       caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title=element_text(face="bold", size=rel(1.2)))
dev.off()

tiff("Outputs/EngScotLADrgCIs.tiff", units="in", width=8, height=6, res=500)
ggplot(bidata, aes(x=fct_reorder(as.factor(LA), DRD), y=DRD, colour=country))+
  geom_point()+
  theme_classic()+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_x_discrete(name="Local Authority")+
  scale_y_continuous(name="Drug misuse deaths per 100,000")+
  labs(title="One of these countries is not like the others", 
       subtitle="Annual deaths from drug misuse per 100,000 population across UK Local Authorities",
       caption="Data from ONS, NRS, NISRA & PHE | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title=element_text(face="bold", size=rel(1.2)))
dev.off()
