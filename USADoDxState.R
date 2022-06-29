rm(list=ls())

library(curl)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(sf)


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

Alc <- read.csv("Data/CDC Data/CDCWonderAlc1920xState.txt", sep="\t") %>% 
  mutate(Cause="Alcohol")

Drg <- read.csv("Data/CDC Data/CDCWonderDrg1920xState.txt", sep="\t")%>% 
  mutate(Cause="Drugs", Crude.Rate=as.numeric(Crude.Rate), Age.Adjusted.Rate=as.numeric(Age.Adjusted.Rate))

Scd <- read.csv("Data/CDC Data/CDCWonderScd1920xState.txt", sep="\t")%>% 
  mutate(Cause="Suicide", Crude.Rate=as.numeric(Crude.Rate), Age.Adjusted.Rate=as.numeric(Age.Adjusted.Rate))

data <- bind_rows(Alc, Drg, Scd) %>% 
  filter(Notes=="") 

#Download shapefile
stateshapeurl <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_5m.zip"
tempshape3 <- tempfile()
tempshape4 <- tempfile()
tempshape3 <- curl_download(url=stateshapeurl, destfile=tempshape3, quiet=FALSE, mode="wb")
unzip(zipfile=tempshape3, exdir=tempshape4)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name2 <- list.files(tempshape4, pattern=".shp")[1]
stateshapefile <- st_read(file.path(tempshape4, name2))

mapdata <- left_join(stateshapefile, data, by=c("NAME"="State"))

ggplot(mapdata %>% filter(Year>=2019), aes(geometry=geometry, fill=Age.Adjusted.Rate))+
  geom_sf()+
  scale_fill_paletteer_c("viridis::inferno", direction=-1, limits=c(0,NA))+
  facet_wrap(Year~Cause)+
  theme_void()
