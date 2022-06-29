rm(list=ls())

library(curl)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(sf)
library(forcats)
library(scales)

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
  filter(Notes=="") %>% 
  group_by(State, Cause) %>% 
  mutate(abschange=Age.Adjusted.Rate-lag(Age.Adjusted.Rate, 1),
         relchange=abschange/Age.Adjust.Rate)

agg_tiff("Outputs/USDoDxStateTiles.tiff", units="in", width=12, height=9, res=500)
ggplot(data, aes(x=Year, y=fct_rev(State), fill=Age.Adjusted.Rate))+
  geom_tile()+
  scale_x_continuous(breaks=c(2010, 2015, 2020), name="")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c(name="Age-adjusted\ndeaths per 100,000", "viridis::rocket", direction=-1, limits=c(0,NA))+
  facet_wrap(~Cause)+
  theme_custom()+
  labs(title="2020 was a bad year for drug and alcohol deaths in the US",
       subtitle="Age-adjusted mortality rates for deaths of despair by US state",
       caption="Data from CDC WONDER | Plot by @VictimOfMaths")
dev.off()

ggplot(data %>% filter(Year>2010), aes(x=Year, y=fct_rev(State), fill=relchange))+
  geom_tile()+
  scale_x_continuous(breaks=c(2010, 2015, 2020))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c(name="Age-adjusted\ndeaths per 100,000", "pals::kovesi.diverging_gwr_55_95_c38", 
                         limit=c(-1,1)*max(abs(data$relchange)))+
  facet_wrap(~Cause)+
  theme_custom()

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

agg_tiff("Outputs/USDoDxStateChange1920.tiff", units="in", width=8, height=9, res=500)
ggplot(mapdata %>% filter(Year==2020 & !NAME %in% c("Alaska", "Hawaii")), 
       aes(geometry=geometry, fill=relchange))+
  geom_sf(size=0.1, colour="Grey40")+
  scale_fill_paletteer_c("pals::ocean.curl", 
                         limit=c(-1,1)*max(abs(data$relchange), na.rm=TRUE), 
                         labels=label_percent(accuracy=1), name="Change in\n2020 vs. 2019")+
  facet_wrap(~Cause, strip.position="top", ncol=1)+
  theme_custom()+
  theme(axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank())+
  labs(title="2020 has seen a big rise in drug and alcohol deaths in the US",
       subtitle="Relative change in deaths attributable to alcohol, drugs or suicide in US states between 2019 and 2020",
       caption="Data from CDC WONDER | Plot by @VictiOfMaths")

dev.off()

agg_tiff("Outputs/USDoDxState2020.tiff", units="in", width=8, height=9, res=500)
ggplot(mapdata %>% filter(Year==2020 & !NAME %in% c("Alaska", "Hawaii")), 
       aes(geometry=geometry, fill=Age.Adjusted.Rate))+
  geom_sf(size=0.1, colour="Grey40")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1,
                         limit=c(0,NA), name="Age-adjusted deaths\nper 100,000")+
  facet_wrap(~Cause, strip.position="top", ncol=1)+
  theme_custom()+
  theme(axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank())+
  labs(title="The geography of Deaths of Despair varies by cause",
       subtitle="Age-adjusted rates of death attributable to alcohol, drugs or suicide in US states in 2020",
       caption="Data from CDC WONDER | Plot by @VictiOfMaths")

dev.off()
