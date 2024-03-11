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
#temp <- tempfile()
#source <- ""
#temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

rawpersons <- read_excel("C:/Users/cm1cra/Desktop/Deaths by IMD 2001-2022 FINAL.xlsx",
                         sheet="1", range=cell_limits(c(6,1), c(567283, 25))) %>% 
  mutate(Sex="Total")

rawmale <- read_excel("C:/Users/cm1cra/Desktop/Deaths by IMD 2001-2022 FINAL.xlsx",
                      sheet="2", range=cell_limits(c(6,1), c(415178, 25))) %>% 
  mutate(Sex="Male")

rawfemale <- read_excel("C:/Users/cm1cra/Desktop/Deaths by IMD 2001-2022 FINAL.xlsx",
                      sheet="3", range=cell_limits(c(6,1), c(379521, 25))) %>% 
  mutate(Sex="Female")

rawdata <- bind_rows(rawpersons, rawmale, rawfemale) %>% 
  gather(Year, Deaths, c(4:25)) %>% 
  mutate(Year=as.numeric(Year),
         IMD=11-`IMD decile`,
         IMD=case_when(
           IMD=="1" ~ "1 (least deprived)",
           IMD=="10" ~ "10 (most deprived)",
           TRUE ~ as.character(IMD)),
         IMD=factor(IMD, levels=c("1 (least deprived)", "2", "3", "4", "5", "6",
                                         "7", "8", "9", "10 (most deprived)")))

ASdata <- rawdata %>% 
  mutate(Cause=if_else(`ICD-10 code` %in% c("E244", "G312", "G621", "G721", "I426",
                                            "K292", "K852", "K860", "Q860", "R780") |
                         substr(`ICD-10 code`,1,3) %in% c("F10", "K70", "X45", "X65", "Y15"),
                       "Alcohol", "Other")) %>% 
  group_by(Age, Sex, IMD, Cause, Year) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  spread(Age, Deaths) %>% 
  mutate(across(.cols=c(`0`:`118`), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(Age, Deaths, c(`0`:`118`)) %>% 
  mutate(Age=as.numeric(Age))

ggplot(ASdata %>% filter(Sex=="Total" & Cause=="Alcohol"), aes(x=Year, y=Age, fill=Deaths))+
  geom_tile()+
  facet_grid(~IMD)+
  scale_fill_paletteer_c("viridis::turbo")+
  coord_equal()+
  theme_custom()

ASdata %>% group_by(Age, Sex, IMD, Year) %>% 
  mutate(ASProp=Deaths[Cause=="Alcohol"]/sum(Deaths)) %>% 
  ungroup() %>% 
  filter(ASProp<0.3) %>% 
  ggplot(aes(x=Year, y=Age, fill=ASProp))+
  geom_tile()+
  facet_grid(~IMD)+
  scale_fill_paletteer_c("viridis::turbo")+
  coord_equal()+
  theme_custom()

shortdata <- ASdata %>% 
  group_by(Year, Sex, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")

#Bring in ONS population estimates

#Age-standardise