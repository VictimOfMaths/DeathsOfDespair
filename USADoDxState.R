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

Alc <- read.csv("Data/CDC Data/CDCWonderAlc1920xCounty.txt", sep="\t") %>% 
  mutate(Cause="Alcohol")

Drg <- read.csv("Data/CDC Data/CDCWonderDrg1920xCounty.txt", sep="\t")%>% 
  mutate(Cause="Drugs")

Scd <- read.csv("Data/CDC Data/CDCWonderScd1920xCounty.txt", sep="\t")%>% 
  mutate(Cause="Suicide")

data <- bind_rows(Alc, Drg, Scd) %>% 
  filter(Notes=="") %>% 
  mutate(Deaths=if_else(Deaths %in% c("Suppressed", "Missing"), NA_real_, as.numeric(Deaths)),
         Population=if_else(Population %in% c("Suppressed", "Missing"), NA_real_, as.numeric(Population)),
         Crude.Rate2=Deaths*100000/Population)

#Download shapefile
#shapefile <- counties(resolution="500k")
shapeurl <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_5m.zip"
tempshape <- tempfile()
tempshape2 <- tempfile()
tempshape <- curl_download(url=shapeurl, destfile=tempshape, quiet=FALSE, mode="wb")
unzip(zipfile=tempshape, exdir=tempshape2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(tempshape2, pattern=".shp")[1]
shapefile <- st_read(file.path(tempshape2, name)) %>% 
  mutate(County.Code=as.numeric(GEOID))

mapdata <- left_join(shapefile, data)

ggplot(mapdata %>% filter(!is.na(Cause)), aes(geometry=geometry, fill=Crude.Rate2))+
  geom_sf()+
  scale_fill_paletteer_c("viridis::inferno", direction=-1, limits=c(0,NA))+
  facet_wrap(Year~Cause)+
  theme_void()