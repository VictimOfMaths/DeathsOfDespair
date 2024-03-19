rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(lubridate)
library(mgcv)

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

#Read in data from ONS website
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2022/alcohol-specific-deaths-22-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(temp, sheet="Table_5", range="A5:H335") %>% 
  mutate(SIMD=paste(6-`SIMD quintile`, `Quintile description`),
         SIMD=gsub(" NA", "", SIMD))

data <- rawdata %>% 
  group_by(Year, Sex) %>% 
  mutate(Total=sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Prop=Deaths/Total)

ggplot(data %>% filter(Sex=="Persons"), aes(x=Year, y=Prop, colour=SIMD))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of alcohol-specific deaths",
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#ffc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))+
  theme_custom()

ggplot(data %>% filter(Sex=="Persons"), aes(x=Year, y=Deaths, fill=SIMD))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_area(position="stack")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Alcohol-specific deaths")+
  scale_fill_manual(values=c("#ffc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))+
  theme_custom()

ggplot(data %>% filter(Sex=="Persons"), aes(x=Year, y=Deaths, colour=SIMD))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Alcohol-specific deaths")+
  scale_colour_manual(values=c("#ffc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))+
  theme_custom()

