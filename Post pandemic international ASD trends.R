#Take OECD and EU nations with data from 2021 or later
#Use abbreviated definition of alcohol-specific deaths
#For each country search for more recent data from their own national statistics provider
#Check alignment of older data with WHO figures as validation

#Analysis by sex, but what about age? 
#Could include drug deaths? and suicide??

rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(patchwork)
library(lubridate)
library(ggrepel)

options(scipen=999999999)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y=element_line(colour="grey95"))
}

#Define European Standard Population 2013
ESP <- data.frame(Age=c("Under 1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                        "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                  ESP=c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 
                        4000 ,2500, 1500, 1000))

#Read in WHO raw mortality data
#https://www.who.int/data/data-collection-tools/who-mortality-database
#Read in Raw WHO data for pre2002 
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part1.zip?sfvrsn=e2a4f93a_17&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData1 <- data.table::fread(file.path(temp2, "Morticd10_part1"))

#2003-2007
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part2.zip?sfvrsn=6e55000b_5&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData2 <- data.table::fread(file.path(temp2, "Morticd10_part2"))

#2008-2012
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part3.zip?sfvrsn=9f1111a2_13&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData3 <- data.table::fread(file.path(temp2, "Morticd10_part3"))

#2013-2016
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part4.zip?sfvrsn=259c5c23_24&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData4 <- data.table::fread(file.path(temp2, "Morticd10_part4"))

#2017-2020
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part5.zip?sfvrsn=ad970d0b_33&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData5 <- data.table::fread(file.path(temp2, "Morticd10_part5"))

#2021-
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part6.zip?sfvrsn=ec801a61_3"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData6 <- data.table::fread(file.path(temp2, "Morticd10_part6"))

#Read in country codes
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/mort_country_codes.zip?sfvrsn=800faac2_5&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

CountryCodes <- data.table::fread(file.path(temp2, "country_codes"))

#Read in population estimates
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/mort_pop.zip?sfvrsn=937039fc_25&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop <- data.table::fread(file.path(temp2, "pop")) %>% 
  merge(CountryCodes, by.x="Country", by.y="country", all.x=TRUE)



RawData <- bind_rows(RawData1 %>% mutate(List=as.numeric(List)), 
                     RawData2 %>% mutate(List=as.numeric(List)), 
                     RawData3, RawData4, RawData5, RawData6) %>% 
  merge(CountryCodes, by.x="Country", by.y="country", all.x=TRUE)

#Define ASD
ASD <- RawData %>% 
  select(Year, Country, Cause, name, Sex, Frmat, Deaths1:Deaths26) %>% 
  mutate(ICD10Upper=substr(Cause, 1, 3),
         CoD=case_when(
           Cause %in% c("E244", "G312", "G621", "G721", "I426", "K292", "K852", "K860", "Q860", "R780") |
             ICD10Upper %in% c("F10", "K70", "X45", "X65", "Y15") ~ "Alcohol-specific",
           TRUE ~ "Other")) %>% 
  group_by(Year, Country, name, Frmat, CoD, Sex) %>% 
  summarise(across(c(Deaths1:Deaths26), ~sum(.x))) %>% 
  ungroup()

#Whole population dataset
ASD_all <- ASD %>% 
  filter(Sex!=9) %>% 
  select(Year, name, Country, CoD, Sex, Deaths1) %>% 
  merge(Pop %>% select(Year, Sex, Country, Pop1), by=c("Country", "Sex", "Year"), all.x=TRUE) %>% 
  group_by(name, Year, CoD) %>% 
  summarise(Deaths=sum(Deaths1), Pop=sum(Pop1), .groups="drop") %>% 
  mutate(ASDrate=Deaths*100000/Pop) 

list <- ASD_all %>% group_by(name) %>% 
  filter(Year==2019) %>% 
  filter(!is.na(ASDrate)) %>% 
  slice(1) %>% 
  select(name)

ASD_all %>% merge(list) %>% 
  filter(CoD=="Alcohol-specific" & !is.na(ASDrate) & name %in% c("Australia", "Austria", "Belgium", "Brazil", "Bulgaria", "Czech Republic", "Denmark",
                                                                 "Finland", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan", "Latvia", 
                                                                 "Lithuania", "Netherlands", "Poland", "South Africa", "Sweden", "United Kingdom, Scotland",
                                                                 "United Kingdom, England and Wales", "United States of America")) %>% 
  group_by(name) %>% 
  mutate(RelChange=ASDrate/ASDrate[Year==2019]) %>% 
  ungroup() %>% 
  filter(Year>=2015) %>% 
  ggplot(aes(x=Year, y=RelChange, colour=name))+
  geom_line()+
  scale_y_continuous(trans="log")

ASD %>% filter(CoD=="Alcohol-specific" & name %in% c("Australia", "Austria", "Belgium", "Brazil", "Bulgaria", "Czech Republic", "Denmark",
                                                     "Finland", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan", "Latvia", 
                                                     "Lithuania", "Netherlands", "Poland", "South Africa", "Sweden", "United Kingdom, Scotland",
                                                     "United Kingdom, England and Wales", "United States of America")) %>% 
  group_by(name, Year) %>% 
  summarise(Deaths=sum(Deaths1)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(RelChange=Deaths/Deaths[Year==2019]) %>% 
  filter(Year>=2015) %>% 
  ggplot(aes(x=Year, y=RelChange, colour=name))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=. %>% filter(Year==max(Year)),
                  aes(label = name), size=3,
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.2, hjust=0,
                  xlim = c(2024, NA_integer_), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(2015, 2035))+
  scale_y_continuous(trans="log")+
  theme_custom()
