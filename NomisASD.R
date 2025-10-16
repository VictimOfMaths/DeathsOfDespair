rm(list=ls())

library(tidyverse)
library(extrafont)
library(ragg)
library(paletteer)
library(readxl)

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
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y=element_line(colour="grey95"))
}


#Read in data stratified by sex downladed from NOMIS
path <- "Data/NomisDeaths_Sex.xlsx"

colnames <- c("Year", "E24.4", "F10", "G31.2", "G62.1", "G72.1", "I42.6", "K29.2",
              "K70", "K85.2", "K86.0", "Q86.0", "R78.0", "X45", "X65", "Y15")

labels <- data.frame(Country=rep(c("England", "Wales"), each=84),
                     Sex=rep(c("Male", "Female"), each=42, times=2),
                     Age=rep(c("All ages", "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                               "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), each=2, times=4),
                     Metric=rep(c("Deaths", "ASMR"), times=84))

rawdata <- data.frame(Year=integer(), `E24.4`=integer(), `F10`=integer(), `G31.2`=integer(),
                      `G62.1`=integer(), `G72.1`=integer(), `I42.6`=integer(), `K29.2`=integer(),
                      `K70`=integer(), `K85.2`=integer(), `K86.0`=integer(), `Q86.0`=integer(),
                      `R78.0`=integer(), `X45`=integer(), `X65`=integer(), `Y15`=integer(),
                      Country=character(), Sex=character(), Age=character(), Metric=character())

for(i in 1:168){
  rawdata <- bind_rows(rawdata, read_excel(path, sheet=i, range="A11:P22",
                                           col_names=FALSE) %>% 
                         set_names(colnames) %>% 
                         mutate(across(`E24.4`:`Y15`, ~as.numeric(.x))) %>% 
                         mutate(Country=labels$Country[i],
                                Sex=labels$Sex[i],
                                Age=labels$Age[i],
                                Metric=labels$Metric[i]))
}

#Repeat with aggregated sex data
path2 <- "Data/NomisDeaths.xlsx"

labels2 <- data.frame(Country=rep(c("England", "Wales"), each=42),
                     Sex=rep("Overall", times=42),
                     Age=rep(c("All ages", "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                               "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), each=2, times=2),
                     Metric=rep(c("Deaths", "ASMR"), times=42))

rawdata2 <- data.frame(Year=integer(), `E24.4`=integer(), `F10`=integer(), `G31.2`=integer(),
                      `G62.1`=integer(), `G72.1`=integer(), `I42.6`=integer(), `K29.2`=integer(),
                      `K70`=integer(), `K85.2`=integer(), `K86.0`=integer(), `Q86.0`=integer(),
                      `R78.0`=integer(), `X45`=integer(), `X65`=integer(), `Y15`=integer(),
                      Country=character(), Sex=character(), Age=character(), Metric=character())

for(i in 1:82){
  rawdata2 <- bind_rows(rawdata2, read_excel(path2, sheet=i, range="A11:P22",
                                           col_names=FALSE) %>% 
                         set_names(colnames) %>% 
                         mutate(across(`E24.4`:`Y15`, ~as.numeric(.x))) %>% 
                         mutate(Country=labels2$Country[i],
                                Sex=labels2$Sex[i],
                                Age=labels2$Age[i],
                                Metric=labels2$Metric[i]))
}

#Repeat with combined E&W data
path3 <- "Data/NomisDeaths_EW.xlsx"

labels3 <- data.frame(Country=rep("England & Wales", each=126),
                     Sex=rep(c("Overall", "Male", "Female"), each=42, times=3),
                     Age=rep(c("All ages", "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                               "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), each=2, times=3),
                     Metric=rep(c("Deaths", "ASMR"), times=63))

rawdata3 <- data.frame(Year=integer(), `E24.4`=integer(), `F10`=integer(), `G31.2`=integer(),
                      `G62.1`=integer(), `G72.1`=integer(), `I42.6`=integer(), `K29.2`=integer(),
                      `K70`=integer(), `K85.2`=integer(), `K86.0`=integer(), `Q86.0`=integer(),
                      `R78.0`=integer(), `X45`=integer(), `X65`=integer(), `Y15`=integer(),
                      Country=character(), Sex=character(), Age=character(), Metric=character())

for(i in 1:126){
  rawdata3 <- bind_rows(rawdata3, read_excel(path3, sheet=i, range="A11:P22",
                                           col_names=FALSE) %>% 
                         set_names(colnames) %>% 
                         mutate(across(`E24.4`:`Y15`, ~as.numeric(.x))) %>% 
                         mutate(Country=labels3$Country[i],
                                Sex=labels3$Sex[i],
                                Age=labels3$Age[i],
                                Metric=labels3$Metric[i]))
}

data <- rawdata %>% 
  bind_rows(rawdata2, rawdata3) %>% 
  gather(Cause, Value, c(`E24.4`:`Y15`)) %>% 
  group_by(Year, Country, Sex, Age, Metric) %>% 
  summarise(Value=sum(Value, na.rm=TRUE), .groups="drop") %>% 
  spread(Metric, Value) %>% 
  mutate(Age=factor(Age, levels=c("All ages", "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))

ggplot(data %>% filter(Age=="All ages" & Sex=="Overall"), 
       aes(x=Year, y=ASMR, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  theme_custom()

ggplot(data %>% filter(Age=="All ages" & Sex!="Overall"), 
       aes(x=Year, y=ASMR, colour=Sex))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  facet_wrap(~Country)+
  theme_custom()

ggplot(data %>% filter(Age!="All ages" & Sex=="Overall"), 
       aes(x=Year, y=ASMR, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  facet_wrap(~Age)+
  theme_custom()
