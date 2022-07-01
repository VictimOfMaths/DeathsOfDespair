rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(sf)
library(forcats)
library(scales)
#remotes::install_github("rOpenSci/fingertipsR", build_vignettes = TRUE, dependencies = "suggests")
library(fingertipsR)
library(jsonlite)

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

#Read in suicide data by LA for England & Wales
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/suicidesbylocalauthority/current/2020latables1.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Crude numbers by single year
Scd_crude <- read_excel(temp, sheet="Table 1", range="A6:X436") %>% 
  mutate(`Area Names`=coalesce(`Area Names`, `...3`, `...4`)) %>% 
  select(-c(`...3`, `...4`)) %>% 
  filter(!is.na(`2020`)) %>% 
  gather(Year, Deaths, c(`2020`:`2001`)) %>% 
  mutate(Deaths=as.numeric(Deaths), Cause="Suicide")

#Age-standardised rates in 3-year bands
Scd_AS <- read_excel(temp, sheet="Table 2", range="A7:DG434", col_names=FALSE) %>% 
  mutate(`...2`=coalesce(`...2`, `...3`, `...4`)) %>% 
  select(-c(`...3`, `...4`, `...7`, `...10`, `...13`, `...16`, `...19`, `...22`, `...25`, `...28`, `...31`,
            `...34`, `...37`, `...40`, `...43`, `...46`, `...49`, `...52`, `...55`, `...58`, `...61`,
            `...64`, `...67`, `...70`, `...73`, `...76`, `...79`, `...82`, `...85`, `...88`, `...91`,
            `...94`, `...97`, `...100`, `...103`, `...106`, `...109`)) %>% 
  set_names("Area Codes", "Area Names", paste(rep(c("2018-20", "2017-19", "2016-18", "2015-17", "2014-16",
                                                    "2013-15", "2012-14", "2011-13", "2010-12", "2009-11",
                                                    "2008-10", "2007-09", "2006-08", "2005-07", "2004-06",
                                                    "2003-05", "2002-04", "2001-03"), each=4),
                                              rep(c("Deaths", "ASRate", "Lower", "Upper"), times=18), 
                                              sep="_")) %>% 
  filter(!is.na(`Area Codes`)) %>% 
  mutate(across(starts_with("20"), ~as.numeric(gsub(" u", "", .x)))) %>% 
  pivot_longer(cols=c(3:74), names_to=c("Year", "Metric"), names_sep="_", values_to="Value") %>% 
  mutate(Cause="Suicide")

#Download Alcohol-specific deaths by LA for England, which is bizarrely only available from OHID, not ONS
Alc_AS.E <- fingertips_data(IndicatorID=91380, AreaTypeID=401) %>% 
  filter(Sex=="Persons") %>% 
  select(AreaCode, AreaName, Value, Timeperiod, LowerCI95.0limit, UpperCI95.0limit, Count) %>% 
  set_names("Area Codes", "Area Names", "ASRate", "Year", "Lower", "Upper", "Deaths") %>% 
  gather(Metric, Value, c(ASRate, Lower, Upper, Deaths)) %>% 
  mutate(Cause="Alcohol", Year=gsub(" ", "", Year))
  
#Download alcohol-specific deaths for Wales by LA, available from DHCWales
#Massive thanks to @michaelgoodier for digging out the API query, because the website is *unhelpful*
url <- "https://t.co/xgR0XhyQPC"
Alc_AS.W <- fromJSON(url)[["features"]] %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  set_names("Area Names", "2009-11", "2010-12", "2011-13", "2012-14", "2013-15", "2014-16", "2015-17",
            "2016-18", "2017-19", "2018-20", "ID") %>% 
  select(-ID) %>% 
  gather(Year, Value, c("2009-11":"2018-20")) %>% 
  #Add in area codes
  merge(Scd_AS %>% filter(substr(`Area Codes`,1,1)=="W") %>% 
          select(`Area Codes`, `Area Names`) %>% 
          rename("Area Names Long"="Area Names") %>% 
          unique() %>% 
          mutate(`Area Names`=gsub(" /.*", "", `Area Names Long`),
                 Cause="Alcohol", Metric="ASRate")) %>% 
  select(-`Area Names`) %>% 
  rename("Area Names"="Area Names Long") %>% 
  mutate(Value=as.numeric(Value))


#Download drug misuse deaths for E&W from ONS
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority/current/2020localauthorities.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Drg_crude <- read_excel(temp, sheet="Table 2", range="A4:AF432") %>% 
  mutate(`Area Names`=coalesce(`Area Names`, `...3`, `...4`)) %>% 
  select(-c(`...3`, `...4`)) %>% 
  filter(!is.na(`2020`)) %>% 
  gather(Year, Deaths, c(`2020`:`1993`)) %>% 
  mutate(Deaths=as.numeric(Deaths), Cause="Drugs")

#Age-standardised rates in 3-year bands
Drg_AS <- read_excel(temp, sheet="Table 6", range="A9:DG431", col_names=FALSE) %>% 
  mutate(`...2`=coalesce(`...2`, `...3`, `...4`)) %>% 
  select(-c(`...3`, `...4`, `...7`, `...10`, `...13`, `...16`, `...19`, `...22`, `...25`, `...28`, `...31`,
            `...34`, `...37`, `...40`, `...43`, `...46`, `...49`, `...52`, `...55`, `...58`, `...61`,
            `...64`, `...67`, `...70`, `...73`, `...76`, `...79`, `...82`, `...85`, `...88`, `...91`,
            `...94`, `...97`, `...100`, `...103`, `...106`, `...109`)) %>% 
  set_names("Area Codes", "Area Names", paste(rep(c("2018-20", "2017-19", "2016-18", "2015-17", "2014-16",
                                                    "2013-15", "2012-14", "2011-13", "2010-12", "2009-11",
                                                    "2008-10", "2007-09", "2006-08", "2005-07", "2004-06",
                                                    "2003-05", "2002-04", "2001-03"), each=4),
                                              rep(c("Deaths", "ASRate", "Lower", "Upper"), times=18), 
                                              sep="_")) %>% 
  filter(!is.na(`Area Codes`)) %>% 
  mutate(across(starts_with("20"), ~as.numeric(gsub(":", "", .x)))) %>% 
  pivot_longer(cols=c(3:74), names_to=c("Year", "Metric"), names_sep="_", values_to="Value") %>% 
  mutate(Cause="Drugs")

#######
#Bring Age-standardised data together
ASdata <- bind_rows(Scd_AS, Drg_AS, Alc_AS.E, Alc_AS.W) %>% 
  filter(Metric=="ASRate") %>% 
  mutate(`Area Names`=gsub(", City of", "", `Area Names`),
         `Area Names`=gsub(", County of", "", `Area Names`),
         `Area Names`=gsub(" UA", "", `Area Names`),
         `Area Names`=gsub("&", "and", `Area Names`),
         `Area Names`=gsub("King's", "King’s", `Area Names`)) %>% 
  #Bring in LA populations
  spread(Cause, Value)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltlarates <- st_read(ltla, layer="6 LTLA-2021") %>% 
  left_join(ASdata, by=c("Lacode"="Area Codes"))

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

Alcmap <- ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), 
          aes(geometry=geom), fill="White")+
  geom_sf(data=ltlarates %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland") & Year=="2017-19"), 
          aes(geometry=geom, fill=Alcohol), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1, limits=c(0,30),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Alcohol-specific deaths in England & Wales in 2017-19",
       subtitle="Age-standardised mortality rates for causes that are 100% attributable to alcohol.\nGrey areas have too few deaths to robustly calculate these rates.\n",
       caption="Data from ONS, OHID & DHC Wales, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/DODxLTLAAlcohol.tiff", units="in", width=8, height=9, res=500)
Alcmap
dev.off()

Drgmap <- ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), 
          aes(geometry=geom), fill="White")+
  geom_sf(data=ltlarates %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland") & Year=="2018-20"), 
          aes(geometry=geom, fill=Drugs), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1, limits=c(0,30),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Drug misuse deaths in England & Wales in 2018-20",
       subtitle="Age-standardised rates deaths from drug misuse. Grey areas have too few deaths to robustly calculate\nthese rates.\n",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/DODxLTLADrugs.tiff", units="in", width=8, height=9, res=500)
Drgmap
dev.off()

Scdmap <- ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), 
          aes(geometry=geom), fill="White")+
  geom_sf(data=ltlarates %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland") & Year=="2018-20"), 
          aes(geometry=geom, fill=Suicide), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNation %in% c("Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1, limits=c(0,30),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Deaths from suicide in England & Wales in 2018-20",
       subtitle="Age-standardised rates for suicide deaths. Grey areas have too few deaths to robustly calculate these rates.\n",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/DODxLTLASuicide.tiff", units="in", width=8, height=9, res=500)
Scdmap
dev.off()

agg_tiff("Outputs/DODxLTLATiles.tiff", units="in", width=9, height=20, res=500)
ltlarates %>% 
  gather(Cause, Rate, c(Alcohol, Drugs, Suicide)) %>% 
  mutate(Year=as.numeric(substr(Year, 1, 4))+1) %>% 
ggplot(aes(x=Year, y=fct_rev(`Area Names`), fill=Rate))+
  geom_tile()+
  scale_x_continuous(name="", limits=c(2007,2019))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1, name="Age-standardised\ndeathsznper 100,000")+
  facet_wrap(~Cause)+
  theme_custom()+
  labs(title="")
dev.off()

