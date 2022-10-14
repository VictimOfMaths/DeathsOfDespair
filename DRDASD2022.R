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
#remotes::install_github("rOpenSci/fingertipsR", build_vignettes = TRUE, dependencies = "suggests")
library(fingertipsR)
library(jsonlite)
library(paletteer)
library(gtools)

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

#Alcohol-specific deaths by age for Scotland 
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2021/alcohol-specific-deaths-21-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.s <- read_excel(temp, sheet="Table_2A", range="A6:W48", col_names=FALSE) %>% 
  mutate(cause="ASD") %>% 
  select(-c(2,3)) %>% 
  gather(age, deaths, c(2:21)) %>% 
  mutate(age=as.numeric(gsub("...", "", age)),
    age=case_when(
      age <= 9 ~ "under 25",
      age <= 11 ~ "25-34",
      age <= 13 ~ "35-44",
      age <= 15 ~ "45-54",
      TRUE ~ "55+"),
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
  mutate(index=c(1:105, 1:105),
         index2=c(1:21, 23:43, 45:65, 67:87, 89:109, 1:21, 23:43, 45:65, 67:87, 89:109))

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
#grouped path plot of ASD

data_asd <- data.s %>% filter(cause=="ASD")

y1 <- c(0, data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2001],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2002],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2003],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2019],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2020],
        data_asd$mortrate[data_asd$age=="under 25" & data_asd$year==2021],
        0)

y2 <- c(0, data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2001],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2002],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2003],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2004],
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
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2019],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2020],
        data_asd$mortrate[data_asd$age=="25-34" & data_asd$year==2021],
        0)
y3 <- c(0, data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2001],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2002],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2003],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2004],
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
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2019],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2020],
        data_asd$mortrate[data_asd$age=="35-44" & data_asd$year==2021],
        0)

y4 <- c(0, data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2001],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2002],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2003],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2004],
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
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2019],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2020],
        data_asd$mortrate[data_asd$age=="45-54" & data_asd$year==2021],
        0)

y5 <- c(0, data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2001],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2002],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2003],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2004],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2005],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2006],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2007],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2008],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2009],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2010],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2011],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2012],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2013],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2014],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2015],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2016],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2017],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2018],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2019],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2020],
        data_asd$mortrate[data_asd$age=="55+" & data_asd$year==2021],
        0)

ASDplot <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), y=y1), fill="SkyBlue")+
  geom_polygon(aes(x=c(23, 23:43, 43), y=y2), fill="SkyBlue")+
  geom_polygon(aes(x=c(45, 45:65, 65), y=y3), fill="SkyBlue")+
  geom_polygon(aes(x=c(67, 67:87, 87), y=y4), fill="SkyBlue")+
  geom_polygon(aes(x=c(89, 89:109, 109), y=y5), fill="SkyBlue")+
  geom_path(data=data_asd,aes(x=index2, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_custom()+
  scale_x_continuous(breaks=c(10,33,55,77,99), 
                     labels=c("under 25", "25-34", "35-44", "45-54", "55+"),name="Age")+
  scale_y_continuous(name="Annual alcohol-specific deaths per 100,000")+
  labs(title="The pandemic has started to reverse falls in alcohol-specific deaths in older age groups",
       subtitle="Annual rates of alcohol-specific deaths by age group in Scotland 2001-2021",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

ASDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1:21), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDfull <- ggdraw()+
  draw_plot(ASDplot)+
  draw_plot(ASDinset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10)+
  draw_label("2021", x=0.27, y=0.66, size=10)+
  draw_label("Key", x=0.18, y=0.85, size=10, fontface="bold")

tiff("Outputs/ASDScot2022.tiff", units="in", width=9, height=6.6, res=500)
ggdraw(ASDfull)
dev.off()

#Combined plot
combplot <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), y=x1), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(23, 23:43, 43), y=x2), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(45, 45:65, 65), y=x3), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(67, 67:87, 87), y=x4), fill="Tomato", alpha=0.8)+
  geom_polygon(aes(x=c(89, 89:109, 109), y=x5), fill="Tomato", alpha=0.8)+
  geom_path(data=data_drd,aes(x=index2, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  geom_polygon(aes(x=c(1, 1:21, 21), y=y1), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(23, 23:43, 43), y=y2), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(45, 45:65, 65), y=y3), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(67, 67:87, 87), y=y4), fill="SkyBlue", alpha=0.6)+
  geom_polygon(aes(x=c(89, 89:109, 109), y=y5), fill="SkyBlue", alpha=0.6)+
  geom_path(data=data_asd,aes(x=index2, y=mortrate, group=age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  scale_x_continuous(breaks=c(10,33,55,77,99), 
                     labels=c("under 25", "25-34", "35-44", "45-54", "55+"),name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  labs(title="The contrasting trajectories of drug and alcohol deaths in Scotland",
       subtitle="Annual rates of <span style='color:tomato3;'>drug-related</span> and <span style='color:skyblue3;'>alcohol-specific</span> deaths between 2001 and 2021<br>",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

combfull <- ggdraw()+
  draw_plot(combplot)+
  draw_plot(ASDinset, x=0.1, y=0.58, width=0.13, height=0.15)+
  draw_plot(DRDinset, x=0.1, y=0.72, width=0.13, height=0.15)+
  draw_label("2001", x=0.12, y=0.58, size=10)+
  draw_label("2021", x=0.21, y=0.58, size=10)+
  draw_label("Key", x=0.15, y=0.88, size=10, fontface="bold")+
  draw_label("Alcohol", x=0.16, y=0.625, size=10)+
  draw_label("Drugs", x=0.17, y=0.77, size=10)

tiff("Outputs/ASDDRDScot2022.tiff", units="in", width=9, height=6.6, res=500)
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
#Read in Scottish DRD data at Council Level 2017-2021
DRD_LA.s <- read_excel(rawdata, sheet="C4 - age-stand death rates", range="A8:S39", col_names=FALSE) %>% 
  rename(LA=`...1`) %>% 
  gather(year, DRD, c(2:ncol(.))) %>% 
  mutate(DRD=as.numeric(DRD),
         year=as.numeric(gsub("...", "", year))+1998,
         year=paste0(year, "-", year+4),
         LA=gsub("&", "and", LA))

#Read in Scottish ASD data at Council Level 2017-2021
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2021/alcohol-specific-deaths-21-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD_LA.s <- read_excel(temp, sheet="Table_4B", range="A6:C599", col_names=FALSE) %>% 
  set_names("year", "LA", "ASD") %>% 
  mutate(year=gsub("to", "-", year)) %>% 
  filter(LA!="Scotland")

#Bring Scottish data together
DRDASD_LA.s <- merge(DRD_LA.s, ASD_LA.s, all=TRUE)

#Latest years only
DRDASD_LA.s2 <- DRDASD_LA.s %>% 
  filter(year=="2017-2021")

#Read in English & Welsh data at LTLA level
#DRDs 2019-21
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority/current/2021localauthorities.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD_LA.ew <-  read_excel(temp, sheet="Table 6", range="A9:DM431", col_names=FALSE) %>% 
  mutate(`...2`=coalesce(`...2`, `...3`, `...4`)) %>% 
  select(`...1`, `...2`, `...6`, `...12`, `...18`, `...24`, `...30`, `...36`, `...42`, `...48`, `...54`,
         `...60`, `...66`, `...72`, `...78`, `...84`, `...90`, `...96`, `...102`, `...108`, `...114`) %>% 
  set_names("Area Codes", "Area Names", "2019-21", "2018-20", "2017-19", "2016-18", "2015-17", "2014-16",
            "2013-15", "2012-14", "2011-13", "2010-12", "2009-11", "2008-10", "2007-09", "2006-08", 
            "2005-07", "2004-06", "2003-05", "2002-04", "2001-03") %>% 
  filter(!is.na(`Area Codes`)) %>% 
  mutate(across(starts_with("20"), ~as.numeric(gsub(":", "", .x)))) %>% 
  gather(year, DRD, c(3:21)) %>% 
  mutate(`Area Names`=gsub(", City of", "", `Area Names`),
         `Area Names`=gsub(", County of", "", `Area Names`),
         `Area Names`=gsub(" UA", "", `Area Names`),
         `Area Names`=gsub("&", "and", `Area Names`),
         `Area Names`=gsub("King's", "King’s", `Area Names`),
         `Area Names`=gsub(" /.*", "", `Area Names`))
  
#Download Alcohol-specific deaths by LA for England, which is bizarrely only available from OHID, not ONS
#data up to 2017-19
ASD_LA.e <- fingertips_data(IndicatorID=91380, AreaTypeID=401) %>% 
  filter(Sex=="Persons") %>% 
  select(AreaCode, AreaName, Value, Timeperiod) %>% 
  set_names("Area Codes", "Area Names", "ASD", "year") %>% 
  mutate(year=gsub(" ", "", year),
         `Area Names`=gsub(" UA", "", `Area Names`))

#Download ASD by LA for Wales, available from DHCWales
#Massive thanks to @michaelgoodier for digging out the API query, because the website is *unhelpful*
url <- "https://t.co/xgR0XhyQPC"
ASD_LA.w <- fromJSON(url)[["features"]] %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  set_names("Area Names", "2009-11", "2010-12", "2011-13", "2012-14", "2013-15", "2014-16", "2015-17",
            "2016-18", "2017-19", "2018-20", "ID") %>% 
  select(-ID) %>% 
  gather(year, ASD2, c("2009-11":"2018-20")) %>% 
  mutate(ASD2=as.numeric(ASD2))

#Combine E&W data
DRDASD_LA.ew <- merge(DRD_LA.ew, ASD_LA.e, all=T) %>% 
  merge(ASD_LA.w, all=T) %>% 
  mutate(ASD=coalesce(ASD, ASD2)) %>% 
  select(-ASD2)

#Filter latest years only
DRDASD_LA.ew2 <- DRDASD_LA.ew %>% 
  group_by(`Area Names`, `Area Codes`) %>% 
  summarise(ASD=ASD[year=="2017-19"],
            DRD=DRD[year=="2019-21"])

#Download NI data
#DRD
temp <- tempfile()
source <- "https://www.nisra.gov.uk/system/files/statistics/Drug-related%20deaths%20in%20NI%2C%202010-2020%20final%20_revised%20June%202022.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD_LA.ni <- read_excel(temp, sheet="Table 9", range="A17:F28") %>% 
  gather(year, DRD, c(2:ncol(.))) %>% 
  set_names("LA", "year", "DRD") %>% 
  #scale 'drug-related' deaths down to estimate 'drug misuse' deaths for comparability to the GB data
  mutate(DRD=DRD*0.835,
         LA=gsub("&", "and", LA))

#ASD
temp <- tempfile()
source <- "https://www.nisra.gov.uk/system/files/statistics/Alcohol_Tables_2021%20Final.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD_LA.ni <- read_excel(temp, sheet="Table 5", range="A18:L29") %>% 
  gather(year, ASD, c(2:ncol(.))) %>% 
  set_names("LA", "year", "ASD") %>% 
  mutate(LA=gsub("Armagh", "Armagh City", LA))

DRDASD_LA.ni <- merge(DRD_LA.ni, ASD_LA.ni, all=T)

DRDASD_LA.ni2 <- DRDASD_LA.ni %>% 
  gather(Metric, Value, c("ASD", "DRD")) %>% 
  na.omit() %>% 
  group_by(LA, Metric) %>% 
  filter(year==max(year)) %>% 
  select(-year) %>% 
  spread(Metric, Value)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltlarates <- st_read(ltla, layer="6 LTLA-2021") %>% 
  left_join(DRDASD_LA.ew2, by=c("Lacode"="Area Codes")) %>% 
  left_join(DRDASD_LA.s2 %>% set_names("LA", "year", "DRD2", "ASD2"), by=c("Laname"="LA")) %>% 
  mutate(ASD=coalesce(ASD, ASD2), DRD=coalesce(DRD, DRD2)) %>% 
  select(-c(ASD2, DRD2)) %>% 
  left_join(DRDASD_LA.ni2 %>% set_names("LA", "DRD2", "ASD2"), by=c("Laname"="LA")) %>% 
  mutate(ASD=coalesce(ASD, ASD2), DRD=coalesce(DRD, DRD2)) 

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

agg_tiff("Outputs/ASDCartogramUK.tiff", units="in", width=7, height=9, res=500)
ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=ltlarates, aes(geometry=geom, fill=ASD), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe, hjust=just), size=rel(2.4),
               colour="Black")+
  scale_fill_paletteer_c("pals::ocean.ice", direction=-1, limits=c(0,max(ltlarates$ASD)),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Alcohol-specific deaths in the UK",
       subtitle="Age-standardised mortality rates for causes that are 100% attributable to alcohol.\nGrey areas have too few deaths to robustly calculate these rates.\nData reflects latest available figures for each country.\n",
       caption="Data from ONS, OHID, DHC Wales, NRS & NISRA, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/DRDCartogramUK.tiff", units="in", width=7, height=9, res=500)
ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=ltlarates, aes(geometry=geom, fill=DRD), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe, hjust=just), size=rel(2.4),
               colour="Black")+
  scale_fill_paletteer_c("pals::ocean.amp", limits=c(0,max(ltlarates$DRD)),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Drug-related deaths in the UK",
       subtitle="Age-standardised mortality rates for drug misuse deaths.\nGrey areas have too few deaths to robustly calculate these rates.\nData reflects latest available figures for each country\n",
       caption="Data from ONS, NRS & NISRA, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

#Extract data
DRDASD <- ltlarates %>% 
  select(Lacode, Laname, ASD, DRD) %>% 
  st_drop_geometry() %>% 
  mutate(Country=case_when(
    substr(Lacode, 1, 1)=="E" ~ "England",
    substr(Lacode, 1, 1)=="W" ~ "Wales",
    substr(Lacode, 1, 1)=="S" ~ "Scotland",
    TRUE ~ "Northern Ireland"))

#Download shapefile for GB
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/912de82a62a048918e59d400f3a15e3a_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))
names(shapefile)[names(shapefile) == "LAD21CD"] <- "Lacode"

map.data <- full_join(shapefile, DRDASD, by="Lacode")

#Download shapefile for NI
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/70a46bf8c2834073b33c9e4cb8a0b6ba_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile.ni <- st_read(file.path(temp2, name))
names(shapefile.ni)[names(shapefile.ni) == "CTYUA21CD"] <- "Lacode"

map.data.ni <- full_join(shapefile.ni %>% filter(substr(Lacode, 1, 1)=="N"), DRDASD, by="Lacode")

map.data <- bind_rows(map.data, map.data.ni)

#Actual maps
agg_tiff("Outputs/ASDmapUK.tiff", units="in", width=7, height=9, res=500)
ggplot(map.data, aes(geometry=geometry, fill=ASD))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.ice", direction=-1, limits=c(0,max(map.data$ASD)),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Alcohol-specific deaths in the UK",
       subtitle="Age-standardised mortality rates for causes that are 100% attributable to alcohol.\nGrey areas have too few deaths to robustly calculate these rates.\nData reflects latest available figures for each country\n",
       caption="Data from ONS, OHID, DHC Wales, NRS & NISRA\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/DRDmapUK.tiff", units="in", width=7, height=9, res=500)
ggplot(map.data, aes(geometry=geometry, fill=DRD))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.amp", limits=c(0,max(map.data$ASD)),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Drug-related deaths in the UK",
       subtitle="Age-standardised mortality rates for drug misuse deaths.\nGrey areas have too few deaths to robustly calculate these rates.\nData reflects latest available figures for each country\n",
       caption="Data from ONS, NRS & NISRA\nPlot by @VictimOfMaths")

dev.off()

#BIVARIATE MAP
bidata <- DRDASD %>% 
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

bimap <- full_join(bind_rows(shapefile, shapefile.ni), bidata, by="Lacode")

BIVAR <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+
  scale_fill_identity()+ labs(title="Regional patterns in deaths from alcohol and drugs",
                              subtitle="Comparative rates of age-standardised alcohol-specific deaths and deaths from drug misuse\nby Local Authority. White areas have too few deaths to robustly calculate these rates.",
                              caption="Data from ONS, OHID, DHC Wales, NRS & NISRA | Plot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")+
  #Highland
  annotate("text", x=500000, y=970000, family="Lato",
           label="Purple areas mean\nhigh rates of alcohol and \nhigh rates of drug deaths", size=3)+
  #York
  annotate("text", x=100000, y=380000, family="Lato",
           label="Blue areas mean\nhigh rates of alcohol and \nlow rates of drug deaths", size=3)+
  #Dumfires & galloway
  annotate("text", x=550000, y=590000, family="Lato",
           label="Red areas mean\nlow rates of alcohol and \nhigh rates of drug deaths", size=3)+
  #Dorset
  annotate("text", x=440000, y=27000, family="Lato",
           label="Grey areas mean\nlow rates of alcohol and \nlow rates of drug deaths", size=3)+
  #Purple
  geom_curve(aes(x=410000, y=955000, xend=220000, yend=850000), curvature=0.15)+
  #Blue
  geom_curve(aes(x=70000, y=420000, xend=40000, yend=500000), curvature=-0.15)+
  #Red
  geom_curve(aes(x=520000, y=550000, xend=463000, yend=452000), curvature=-0.2)+
  #Grey
  geom_curve(aes(x=420000, y=57000, xend=430000, yend=105000), curvature=0.1)+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

key <- ggplot(keydata)+
  geom_tile(aes(x=alctert, y=drgtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More alcohol-specific deaths" %->%  ""),
       y = expression("More drug poisoning deaths" %->%  "")) +
  theme_custom() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff("Outputs/ASDDRD2022BivariateUK.tiff", units="in", width=7, height=11, res=500)
ggdraw()+
  draw_plot(BIVAR, 0,0,1,1)+
  draw_plot(key, 0.03,0.46,0.29,0.74)
dev.off()

png("Outputs/ASDDRD2022BivariateUK.png", units="in", width=7, height=11, res=500)
ggdraw()+
  draw_plot(BIVAR, 0,0,1,1)+
  draw_plot(key, 0.03,0.46,0.29,0.74)
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
tiff("Outputs/ASDDRDUKLAScatter.tiff", units="in", width=7, height=6, res=500)
ggplot(DRDASD, aes(x=ASD, y=DRD, colour=Country))+
  geom_point()+
  geom_segment(x=-10, xend=45, y=-10, yend=45, colour="Black")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_x_continuous(name="Alcohol-specific deaths per 100,000 population\n(age-standardised)", limits=c(0,46))+
  scale_y_continuous(name="Drug misuse deaths per 100,000 population\n(age-standardised)", limits=c(0,46))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  annotate("text", x=32, y=6, label="More alcohol-specific deaths", colour="DarkGrey")+
  annotate("text", x=10, y=32, label="More drug misuse deaths", colour="DarkGrey")+
  labs(title="Deaths from alcohol and drugs by Local Authority", 
       subtitle="Age-standardised rates of alcohol-specific and drug misuse deaths",
       caption="Data from ONS, OHID, DHC Wales, NRS & NISRA | Plot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")

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
tiff("Outputs/ASDOrderedPoints.tiff", units="in", width=8, height=6, res=500)
ggplot(DRDASD %>% filter(!is.na(ASD)), aes(x=fct_reorder(as.factor(Laname), ASD), y=ASD, colour=Country))+
  geom_point()+
  theme_classic()+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_x_discrete(name="Local Authority")+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000\n(age-standardised)")+
  labs(title="One of these countries is not like the others", 
       subtitle="Age-standardised  deaths from alcohol-specific causes per 100,000 population across UK Local Authorities",
       caption="Data from ONS, OHID, DHC Wales, NRS & NISRA | Plot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()

tiff("Outputs/DRDOrderedPoints.tiff", units="in", width=8, height=6, res=500)
ggplot(DRDASD %>% filter(!is.na(DRD)), aes(x=fct_reorder(as.factor(Laname), DRD), y=DRD, colour=Country))+
  geom_point()+
  theme_classic()+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_x_discrete(name="Local Authority")+
  scale_y_continuous(name="Drug misuse deaths per 100,000\n(age-standardised)")+
  labs(title="One of these countries is not like the others", 
       subtitle="Age-standardised deaths from drug misuse per 100,000 population across UK Local Authorities",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths\nData reflects the most recently-available figures for each jurisdiction")+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()
