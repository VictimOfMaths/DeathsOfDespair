rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(extrafont)
library(geomtextpath)

options(scipen=99999999)

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
          panel.grid.major.y = element_line(colour="grey95"))
}

#Download ONS drug-related deaths data
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable/current/2023registrations.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Overall
Overall <- read_excel(temp, sheet="Table 4", range="A6:DR102") %>% 
  fill(Sex, .direction="down") %>% 
  filter(!is.na(`Year of death registration`)) %>% 
  select(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,
         40,41,43,44,46,47,49,50,52,53,55,56,58,59,61,62,64,65,67,68,70,71,73,74,
         76,77,79,80,82,83,85,86,88,89,91,92,94,95,97,98,100,101,103,104,106,107,
         109,110,112,113,115,116,118,119,121,122) %>% 
  set_names("Sex", "Year", paste(rep(c("Deaths", "ASMR", "LowerCI", "UpperCI"), times=20),
                                 rep(c("Drug poisoning", "Drug misuse", "All opiates", "Heroin/morphine",
                                       "Methadone", "Tramadol", "Codeine", "Dihydrocodeine",
                                       "Fentanyl", "Other opiates", "Cocaine", "Amphetamines",
                                       "Ecstasy", "Cannabis", "NPS", "Benzodiazepenes",
                                       "Zopiclone/Zolpidem", "Antidepressants", "Antipsychotics",
                                       "Paracetamol"), each=4), sep="_")) %>% 
  mutate(across(c(`Deaths_Drug poisoning`:`UpperCI_Paracetamol`), ~as.numeric(.x))) %>% 
  pivot_longer(cols=c(`Deaths_Drug poisoning`:`UpperCI_Paracetamol`), 
               names_to=c("Measure", "Grouping"), names_sep="_", values_to="Value")

agg_png("Outputs/ONSDRDOverall.png", units="in", width=8, height=5, res=600)
Overall %>% 
  filter(Measure=="ASMR" & Grouping=="Drug poisoning" & Sex=="Persons") %>% 
  ggplot(aes(x=Year, y=Value/10))+
  geom_hline(yintercept=0)+
  geom_line(colour="tomato")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  theme_custom()+
  labs(title="Drug-related deaths continued to rise in 2023",
       subtitle="Age-standardised rate of deaths due to drug poisoning in England & Wales 1993-2023\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By sex
agg_png("Outputs/ONSDRDxSex.png", units="in", width=8, height=5, res=600)
Overall %>% 
  filter(Measure=="ASMR" & Grouping=="Drug poisoning" & Sex!="Persons") %>% 
  ggplot(aes(x=Year, y=Value/10, colour=Sex))+
  geom_hline(yintercept=0)+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  scale_colour_manual(values=c("#6600cc", "#00cc99"), name="")+
  theme_custom()+
  guides(colour = guide_legend(reverse=T))+
  labs(title="Drug poisoning deaths are twice as high in men",
       subtitle="Age-standardised rate of deaths due to drug poisoning in England & Wales 1993-2023 by sex\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By age
AgeData <- read_excel(temp, sheet="Table 5", range="A6:AL102") %>% 
  fill(Sex, .direction="down") %>% 
  filter(!is.na(`Year of death registration`)) %>% 
  select(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38) %>% 
  set_names("Sex", "Year", paste(rep(c("Deaths", "ASMR", "LowerCI", "UpperCI"), times=6),
                                 rep(c("Under 20", "20-29", "30-39", "40-49", "50-69", "70+"), each=4),
                                 sep="_")) %>% 
  mutate(across(c(`Deaths_Under 20`:`UpperCI_70+`), ~as.numeric(.x))) %>% 
  pivot_longer(cols=c(`Deaths_Under 20`:`UpperCI_70+`), 
               names_to=c("Measure", "Age"), names_sep="_", values_to="Value") %>% 
  mutate(Age=factor(Age, levels=c("Under 20", "20-29", "30-39", "40-49", "50-69", "70+")))

agg_png("Outputs/ONSDRDxAge.png", units="in", width=8, height=5, res=600)
AgeData %>% 
  filter(Measure=="ASMR" & Sex=="Persons") %>% 
  ggplot(aes(x=Year, y=Value/10))+
  geom_area(fill="tomato")+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  facet_wrap(~Age, nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Rising drug deaths are driven by 40-69 year-olds",
       subtitle="Rates of drug poisoning death in England and Wales by age 1993-2023\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By substance
agg_png("Outputs/ONSDRDxSubstance.png", units="in", width=8, height=5, res=600)
Overall %>% 
  filter(Measure=="ASMR" &
           Grouping %in% c("Heroin/morphine", "Methadone", 
                           "Cocaine", "Amphetamines", 
                           "Benzodiazepenes") & Sex=="Persons") %>% 
  ggplot(aes(x=Year, y=Value/10, colour=Grouping))+
  geom_hline(yintercept=0)+
  geom_line()+
  geom_text_repel(data=. %>% filter(Year==max(Year)),
                  aes(label = Grouping),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(2023.3, NA), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(1993,2030))+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  scale_colour_manual(values=c("#EF7C12", "#54BCD1", "#009F3F", "#C70E7B", "#FC6882"), name="")+
  theme_custom()+
  theme(legend.position="none")+
  labs(title="Deaths involving cocaine have risen tenfold since 2011",
       subtitle="Age-standardised rate of deaths due to drug poisoning in England & Wales 1993-2023 by selected substances\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By IMD
IMDData <- read_excel(temp, sheet="Table 8", range="A6:AF35") %>% 
  fill(Sex, .direction="down") %>% 
  filter(!is.na(`Year of registration`)) %>% 
  select(c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32)) %>%
  set_names("Sex", "Year", paste(rep(c("Deaths", "ASMR", "LowerCI", "UpperCI"), times=5),
                                 rep(c("Q5 (most deprived)", "Q4", "Q3", "Q2", "Q1 (least deprived)"), each=4),
                                 sep="_")) %>% 
  mutate(across(c(`Deaths_Q5 (most deprived)`:`UpperCI_Q1 (least deprived)`), ~as.numeric(.x))) %>% 
  pivot_longer(cols=c(`Deaths_Q5 (most deprived)`:`UpperCI_Q1 (least deprived)`), 
               names_to=c("Measure", "IMDq"), names_sep="_", values_to="Value")
           
agg_png("Outputs/ONSDRDxIMD.png", units="in", width=8, height=5, res=600)
IMDData %>% 
  filter(Measure=="ASMR") %>% 
  ggplot(aes(x=Year, y=Value/10, colour=IMDq))+
  geom_hline(yintercept=0)+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="")+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="Drug poisoning deaths are massively unequal",
       subtitle="Age-standardised rate of deaths due to drug poisoning in England & Wales 1993-2023\nby quintiles of the Index of Multiple Deprivation\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By geography

#Comparisons with rest of UK
EngData <- read_excel(temp, sheet="Table 7", range="A7:F379")%>% 
  fill(`Year of death registration`, .direction="down") %>% 
  filter(!is.na(`Area code`)) %>% 
  mutate(Area=if_else(is.na(`Area name`), `...4`, `Area name`)) %>% 
  select(1, 5:7) %>% 
  set_names("Year", "Deaths", "ASMR", "Area") %>% 
  mutate(ASMR=ASMR/10)

#Download Scottish data from NRS
url2 <- "https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/23/drug-related-deaths-23-data.xlsx"
temp2 <- tempfile()
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

ScotData <- read_excel(temp2, sheet="Table_11", range="A7:K52") %>% 
  select(1,3,10) %>% 
  set_names("Year", "Deaths", "ASMR") %>% 
  mutate(ASMR=as.numeric(ASMR), Area="Scotland") 

#Download NI data from NISRA
url3 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Drug-related%20deaths%20in%20NI%2C%202012-2022_0.xlsx"
temp3 <- tempfile()
temp3 <- curl_download(url=url3, destfile=temp2, quiet=FALSE, mode="wb")

NIData <- read_excel(temp3, sheet="Table_1", range="A3:L6") %>% 
  gather(Year, Value, c(2:12)) %>% 
  spread(`All Persons`, Value) %>% 
  select(1,2,4) %>% 
  set_names("Year", "ASMR", "Deaths") %>% 
  mutate(Area="Northern Ireland", Year=as.numeric(Year))

UKData <- bind_rows(EngData, ScotData, NIData)

agg_png("Outputs/ONSDRDxRegion.png", units="in", width=9, height=6, res=600)
UKData %>% 
  filter(!is.na(ASMR) & !Area %in% c("England", "Scotland", "Northern Ireland")) %>% 
  ggplot(aes(x=Year, y=ASMR, colour=Area))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  geom_text_repel(data=. %>% group_by(Area) %>% filter(Year==max(Year)),
                  aes(label = Area), size=rel(3),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(2023.3, NA), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(1990, 2033))+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  theme_custom()+
  theme(legend.position="none")

dev.off()

agg_png("Outputs/ONSDRDxCountry.png", units="in", width=9, height=6, res=600)
UKData %>% 
  filter(!is.na(ASMR) & Area %in% c("England", "Scotland", "Northern Ireland", "Wales", "North East")) %>% 
  mutate(Area=if_else(Area=="North East", "North East England", Area)) %>% 
  ggplot(aes(x=Year, y=ASMR, colour=Area))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  geom_text_repel(data=. %>% group_by(Area) %>% filter(Year==max(Year)),
                  aes(label = Area), 
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(2023.3, NA), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(1990, 2033))+
  scale_y_continuous(name="Deaths per 100,000\n(age-standardised)")+
  scale_colour_manual(values=c("tomato", "#54BCD1", "darkred", "navy", "#8FDA04"))+
  theme_custom()+
  theme(legend.position="none")

dev.off()


#Get data by LA across the UK, taking the most recently-available time period
#England & Wales 2021-23
url4 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority/current/2023localauthorities.xlsx"
temp4 <- tempfile()
temp4 <- curl_download(url=url4, destfile=temp4, quiet=FALSE, mode="wb")

EWLA <- read_excel(temp4, sheet="Table 6", range="A6:F363", col_names = FALSE) %>% 
  mutate(LAname=paste0(if_else(is.na(`...2`), "", `...2`), 
                       if_else(is.na(`...3`), "", `...3`), 
                       if_else(is.na(`...4`), "", `...4`))) %>% 
  select(c(1,7,5,6)) %>% 
  set_names("LAcode", "LAname", "Deaths", "ASMR") %>% 
  mutate(Country=if_else(substr(LAcode,1,1)=="E", "England", "Wales"),
         Deaths=Deaths/3,
         ASMR=as.numeric(ASMR))

#Scotland 2019-23
ScotLA <- read_excel(temp2, sheet="Table_C4", range="A5:F665") %>% 
  filter(`Five-year period`=="2019 -2023") %>% 
  select(c(1,3,6)) %>% 
  set_names("LAname", "ASMR", "Deaths") %>% 
  mutate(Deaths=Deaths/5,
         ASMR=as.numeric(ASMR),
         Country="Scotland")

#Northern Ireland 2022
NILA <- read_excel(temp3, sheet="Table_9b", range="A3:F28") %>% 
  filter(!is.na(`2018`)) %>% 
  mutate(Metric=rep(c("Deaths", "ASMR"), each=12)) %>% 
  filter(!`Number of deaths by LGD` %in% c("Total", "ASMR per 100,000 population by LGD")) %>% 
  select(1,6,7) %>% 
  spread(Metric, `2022`) %>% 
  set_names("LAname", "ASMR", "Deaths") %>% 
  mutate(ASMR=as.numeric(ASMR),
         Deaths=as.numeric(Deaths),
         Country="Northern Ireland",
         LAname=gsub("&", "and", LAname))

#Merge together into full list of all LAs
#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-1559-23740/Local_Authority_Districts_May_2024_Boundaries_UK_BFC_7298485007179693897.zip?sv=2018-03-28&sr=b&sig=kkrylMGmh360JWetlZ9DBMINRURpX%2Bk150CntKbMs5c%3D&se=2024-11-01T19%3A29%3A51Z&sp=r"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name[1])) %>% 
  rename(LAcode="LAD24CD",
         LAname="LAD24NM")

mapdata <- left_join(shapefile, EWLA %>% select(-LAname), by="LAcode") %>% 
  left_join(NILA, by="LAname") %>% 
  mutate(ASMR=if_else(is.na(ASMR.x), ASMR.y, ASMR.x),
         Deaths=if_else(is.na(Deaths.x), Deaths.y, Deaths.x),
         Country=if_else(is.na(Country.x), Country.y, Country.x)) %>% 
  left_join(ScotLA, by="LAname") %>% 
  mutate(ASMR=if_else(is.na(ASMR.x.x), ASMR.y.y, ASMR.x.x),
         Deaths=if_else(is.na(Deaths.x.x), Deaths.y.y, Deaths.x.x),
         Country=if_else(is.na(Country.x.x), Country.y.y, Country.x.x))

agg_png("Outputs/DRDUKLA.png", units="in", width=8, height=12, res=600)
ggplot(mapdata, aes(fill=ASMR, geometry=geometry))+
  geom_sf(colour="transparent")+
  scale_fill_paletteer_c("viridis::magma", direction=-1, limits=c(0,NA), name="Age-standardised\ndeaths per 100,000")+
  theme_void()+
  labs(title="Drug deaths are much higher in Scotland than the rest of the UK",
       subtitle="Age-standardised rates of drug misuse deaths in Local Authorities across the UK\nGrey areas have too few deaths to calculate a rate. Data is the most recent available for each country",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  theme(plot.title.position="plot", plot.caption.position="plot",
        plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                margin=margin(0,0,5.5,0)), text=element_text(family="Lato"),
        plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
        plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
        legend.text=element_text(colour="Grey40"),
        legend.title=element_text(colour="Grey20"))

dev.off()

agg_png("Outputs/DRDUKLADots.png", units="in", width=10, height=5, res=600)
mapdata %>% arrange(ASMR) %>% 
  mutate(index=1:nrow(.)) %>% 
  ggplot(aes(x=index, y=ASMR, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_point()+
  scale_y_continuous(name="Drug misuse deaths per 100,000\n(age-standardised)")+
  scale_colour_manual(values=c("tomato", "darkred", "navy", "#8FDA04"))+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank())+
  labs(title="One of these countries is not like the others",
       subtitle="Age-standardised drug misuse death rates for every local authority in the UK, coloured by country",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/DRDUKLADotsJitter.png", units="in", width=10, height=5, res=600)
mapdata %>% group_by(LAname) %>% 
  slice(rep(1:n(), each=Deaths)) %>% 
  st_centroid(st_geometry(.)) %>% 
  st_jitter(., 30000) %>% 
  ggplot(aes(geometry=geometry))+
  geom_sf(size=0.001)+
  theme_void()
  
dev.off()

agg_png("Outputs/DRDUKLADotsJitter2.png", units="in", width=6, height=10, res=800, background="#fff7bc")
st_sample(mapdata, size=mapdata$Deaths, type="random") %>% 
  ggplot(aes(geometry=geometry))+
  geom_sf(size=0.3, stroke=0, colour="darkred")+
  theme_void()+
  theme(plot.background=element_rect(fill="#fff7bc", colour="#fff7bc"),
        text=element_text(colour="darkred", family="Lato"),
        plot.title=element_text(size=rel(3)),
        plot.subtitle=element_text(size=rel(1.2)),
        plot.caption=element_text(size=rel(1)),
        plot.caption.position="plot", plot.title.position="plot")+
  labs(title="A national crisis",
       subtitle="Each dot represents one drug misuse death each year in the UK",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()

#Repeat for alcohol-specific deaths
#England & Wales
temp2 <- tempfile()
source2 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/alcoholspecificdeathsinenglandandwalesbylocalauthority/current/alcoholspecificdeathsbylocalauthority2022.xlsx"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")

ladata <- read_excel(temp2, sheet="Table_2", range="A8:CZ367", col_names=FALSE) %>% 
  set_names("Lacode", "Region", "UA", "LAD", 
            paste0(rep(c("2020-22", "2019-21", "2018-20", "2017-19", "2016-18",
                         "2015-17", "2014-16", "2013-15", "2012-14", "2011-13",
                         "2010-12", "2009-11", "2008-10", "2007-09", "2006-08",
                         "2005-07", "2004-06", "2003-05", "2002-04", "2001-03"),
                       each=5), 
                   rep(c("_Deaths", "_Rate", "_Marker", "_Lower", "_Upper"),
                       times=20))) %>% 
  mutate(across(c(5:104), ~as.numeric(.x))) %>% 
  pivot_longer(cols=c(5:104), names_to=c("Year", "Metric"), names_sep="_", values_to="Value")

#Scotland
temp3 <- tempfile()
source3 <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2023/alcohol-specific-deaths-23-all-tabs.xlsx"
temp3 <- curl_download(url=source3, destfile=temp3, quiet=FALSE, mode="wb")

scotladata <- read_excel(temp3, sheet="Table_4B", range="A5:G665") %>% 
  set_names("Year", "Laname", "Value", "Lower", "Upper", "CIs", "Deaths")

#Northern Ireland
temp4 <- tempfile()
source4 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Alcohol_Tables_2022%20Final%20-%20Error%20updated_1.xlsx"
temp4 <- curl_download(url=source4, destfile=temp4, quiet=FALSE, mode="wb")

#Take 3-year average
NIladata <- read_excel(temp4, sheet="Table 7", range="A5:L16") %>% 
  rename("Laname"="ASMR per 100,000 population by LGD") %>% 
  mutate(Value=(`2020`+`2021`+`2022`)/3,
         Laname=gsub("&", "and", Laname))

#Combine country-level data
UKladata <- ladata %>% 
  filter(Metric=="Rate" & Year=="2020-22") %>% 
  select(Lacode, Value) %>% 
  bind_rows(scotladata %>% 
              filter(Year=="2019to2023") %>% 
              select(Laname, Value),
            NIladata %>% select(Laname, Value))

#Download Carl Baker's lovely hex map template
ltla <- tempfile()
ltlasource <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier-2023.gpkg")
ltla <- curl_download(url=ltlasource, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="LocalAuthorities-lowertier 2023 version — 8 Background")

ltlarates <- st_read(ltla, layer="LocalAuthorities-lowertier 2023 version — 7 LTLA 2023") 

#Generate LA lookup
lalookup <- ltlarates %>% 
  st_drop_geometry() %>% 
  select("Lacode", "Laname") 

ladatafull <- bind_rows(merge(UKladata, lalookup, by="Lacode") %>% 
                          mutate(Laname=coalesce(`Laname.x`, `Laname.y`)) %>% 
                          select(Lacode, Laname, Value),
                        merge(UKladata, lalookup, by="Laname") %>% 
                          mutate(Lacode=coalesce(`Lacode.x`, `Lacode.y`)) %>% 
                          select(Lacode, Laname, Value))

ltlarates2 <- left_join(ltlarates, ladatafull)

Groups <- st_read(ltla, layer="LocalAuthorities-lowertier 2023 version — 2 Groups-2023")

Group_labels <- st_read(ltla, layer="LocalAuthorities-lowertier 2023 version — 1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

agg_png("Outputs/ASDCartogramUK.png", units="in", width=7, height=9, res=600)
ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=ltlarates2, aes(geometry=geom, fill=Value), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe, hjust=just), size=rel(2.4),
               colour="Black")+
  scale_fill_paletteer_c("viridis::turbo", limits=c(0,max(ltlarates2$Value)),
                         name="Deaths per 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Alcohol-specific deaths in the UK",
       subtitle="Age-standardised mortality rates for causes that are 100% attributable to alcohol.\nGrey areas have too few deaths to robustly calculate these rates.\nData reflects latest available figures for each country.\n",
       caption="Data from ONS, NRS & NISRA, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

agg_png("Outputs/ASDUKLADots.png", units="in", width=10, height=5, res=600)
ltlarates2 %>% arrange(Value) %>% 
  mutate(index=1:nrow(.),
         Country=case_when(
           substr(Lacode,1,1)=="E" ~ "England",
           substr(Lacode,1,1)=="W" ~ "Wales",
           substr(Lacode,1,1)=="S" ~ "Scotland",
           substr(Lacode,1,1)=="N" ~ "Northern Ireland")) %>% 
  ggplot(aes(x=index, y=Value, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_point()+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000\n(age-standardised)")+
  scale_colour_manual(values=c("tomato", "darkred", "navy", "#8FDA04"))+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank())+
  labs(title="One of these countries is not like the others",
       subtitle="Age-standardised alcohol-specific death rates for every local authority in the UK, coloured by country",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()
