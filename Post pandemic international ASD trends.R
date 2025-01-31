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

#2003-2016
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part4.zip?sfvrsn=259c5c23_24&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData4 <- data.table::fread(file.path(temp2, "Morticd10_part4"))

#20017-
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part5.zip?sfvrsn=ad970d0b_32&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData5 <- data.table::fread(file.path(temp2, "Morticd10_part5_rev"))

#Read in country codes
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/mort_country_codes.zip?sfvrsn=800faac2_5&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

CountryCodes <- data.table::fread(file.path(temp2, "country_codes"))

RawData <- bind_rows(RawData1 %>% mutate(List=as.numeric(List)), 
                     RawData2 %>% mutate(List=as.numeric(List)), 
                     RawData3, RawData4, RawData5) %>% 
  merge(CountryCodes, by.x="Country", by.y="country", all.x=TRUE)

