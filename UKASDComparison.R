rm(list=ls())

#If not installed, need to install Nomisr package from Github
#remotes::install_github("ropensci/nomisr", auth_token=gitcreds::gitcreds_get())

library(tidyverse)
library(nomisr)
library(scales)
library(extrafont)
library(ragg)
library(paletteer)
library(curl)
library(readxl)

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

#Pull out list of cause codes
causes <- as.data.frame(nomis_get_metadata("NM_161_1", concept="CAUSE_OF_DEATH"))

#Extract data (can't work out how to get the API to filter on measure, so have to extract them all)
rawdataASD <- as.data.frame(nomis_get_data("NM_161_1",
                                           date = c(2013:2024),
                                           geography = c("2092957699", "2092957700", "2092957703"),
                                           sex=c(0:2),
                                           #Causes of death to make up alcohol-specific as per ONS definition
                                           #https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/methodologies/alcoholrelateddeathsintheukqmi
                                           cause_of_death = c(105244, 10610, 107312, 107621, 107721, 109426, 111292,
                                                              11170, 111852, 111860, 117860, 118780, 12445, 12465, 12515))) %>% 
  filter(MEASURE %in% c(1,2)) %>% 
  select(DATE, GEOGRAPHY_NAME, CAUSE_OF_DEATH_CODE, GENDER_NAME, AGE_NAME, MEASURE_NAME, OBS_VALUE) %>% 
  set_names(c("Year", "Country", "Cause", "Sex", "Age", "Metric", "Value"))

data <- rawdataASD %>% 
  group_by(Year, Country, Sex, Age, Metric) %>% 
  summarise(Value=sum(Value, na.rm=TRUE), .groups="drop") %>% 
  spread(Metric, Value) %>% 
  rename("ASMR"="Age-standardised mortality rate") %>% 
  mutate(Age=case_when(
    Age=="total (all ages)" ~ "All ages",
    Age=="Aged under 1" ~ "<1", Age=="Aged 1 to 4" ~ "1-4", Age=="Aged 5 to 9" ~ "5-9", 
    Age=="Aged 90 and over" ~ "90+",
    TRUE ~ gsub("Aged ", "", Age)),
    Age=factor(Age, levels=c("All ages", "<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                             "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                             "70-74", "75-79", "80-84", "85-89", "90+")))

agg_png("Outputs/NOMISASDxAgeEng.png", units="in", width=9, height=6, res=800)
ggplot(data %>% filter(Sex=="Total" & !Age %in% c("All ages", "<1", "1-4", "5-9", "10-14", "15-19") & 
                         Country=="England"),
       aes(x=Year, y=ASMR))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_area(fill="skyblue")+
  geom_path(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())+
  labs(title="Alcohol-specific deaths have fallen in most age groups",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2013 and 2024 in England",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/NOMISASDxAgeWal.png", units="in", width=9, height=6, res=800)
ggplot(data %>% filter(Sex=="Total" & !Age %in% c("All ages", "<1", "1-4", "5-9", "10-14", "15-19") & 
                         Country=="Wales"),
       aes(x=Year, y=ASMR))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_area(fill="skyblue")+
  geom_path(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())+
  labs(title="Alcohol-specific deaths have fallen in most age groups",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2013 and 2024 in Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/NOMISASDxAgeEW.png", units="in", width=9, height=6, res=800)
ggplot(data %>% filter(Sex=="Total" & !Age %in% c("All ages", "<1", "1-4", "5-9", "10-14", "15-19") & 
                         Country=="England and Wales"),
       aes(x=Year, y=ASMR))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_area(fill="skyblue")+
  geom_path(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())+
  labs(title="Alcohol-specific deaths have fallen in most age groups",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2013 and 2024 in England & Wales\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/NOMISASDEW.png", units="in", width=8, height=5, res=800)
ggplot(data %>% filter(Sex=="Total" & Age=="All ages" & Country!="England and Wales"),
       aes(x=Year, y=ASMR, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="", breaks=c(2014, 2016, 2018, 2020, 2022, 2024))+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_paletteer_d("colorblindr::OkabeIto",
                           guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  labs(title="Alcohol-specific deaths have fallen,but remain well above 2019 levels",
       subtitle="Trends in deaths from conditions caused only by alcohol between 2013 and 2024 in England & Wales\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/NOMISxSexEng.png", units="in", width=7, height=5, res=800)
ggplot(data %>% filter(Sex!="Total" & Age=="All ages" & Country=="England"),
       aes(x=Year, y=ASMR, colour=Sex))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="", breaks=c(2014, 2016, 2018, 2020, 2022, 2024))+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99"),
                      guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  labs(title="Alcohol-specific deaths have fallen, for both men and women",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2013 and 2024 in England",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/NOMISxSexWal.png", units="in", width=7, height=5, res=800)
ggplot(data %>% filter(Sex!="Total" & Age=="All ages" & Country=="Wales"),
       aes(x=Year, y=ASMR, colour=Sex))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="", breaks=c(2014, 2016, 2018, 2020, 2022, 2024))+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99"),
                      guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  labs(title="Alcohol-specific deaths have fallen, for both men and women",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2013 and 2024 in Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/NOMISxSexEW.png", units="in", width=7, height=5, res=800)
ggplot(data %>% filter(Sex!="Total" & Age=="All ages" & Country=="England and Wales"),
       aes(x=Year, y=ASMR, colour=Sex))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="", breaks=c(2014, 2016, 2018, 2020, 2022, 2024))+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99"),
                      guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  labs(title="Alcohol-specific deaths have fallen, for both men and women",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2013 and 2024\nin England & Wales\n",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Import data from Scotland
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/media/ip1crfbe/alcohol-specific-deaths-2024-all-tables.xlsx"
curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ScotData <- read_excel(temp, sheet="Table_1", range="A6:D144") %>% 
  set_names("Year", "Sex", "Deaths", "ASMR") %>% 
  mutate(Sex=case_when(
    Sex=="Males" ~ "Male", 
    Sex=="Females" ~ "Female",
    Sex=="Persons" ~ "Total"),
    Country="Scotland", Age="All ages",
    ASMR=as.numeric(ASMR), Year=as.integer(Year))

#Import data from NI
temp <- tempfile()
url <- "https://www.nisra.gov.uk/system/files/statistics/2026-02/Alcohol-specific_deaths_NI_2024%20-%20FOR%20RELEASE.xlsx"
curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

NIdata_count <- read_excel(temp, sheet="Table_1", range="A4:V7") %>% 
  gather(Year, Deaths, c(`2004`:`2024`)) %>% 
  mutate(Sex=if_else(Sex=="All", "Total", Sex), Country="Northern Ireland",
         Age="All ages")

NIdata_rate <- read_excel(temp, sheet="Table_1", range="A26:V29") %>% 
  gather(Year, ASMR, c(`2004`:`2024`)) %>% 
  mutate(Sex=if_else(Sex=="All", "Total", Sex), Country="Northern Ireland",
         Age="All ages")

NIdata <- merge(NIdata_count, NIdata_rate) %>% 
  mutate(Year=as.integer(Year))

#Combine and plot
AllData <- bind_rows(data %>% filter(Country!="England and Wales" & Age=="All ages"), ScotData, NIdata)

agg_png("Outputs/ASDCountry2024.png", units="in", width=9, height=6, res=800)
ggplot(AllData %>% filter(Sex=="Total" & Year>=2013), aes(x=Year, y=ASMR, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="", breaks=c(2012, 2014, 2016, 2018, 2020, 2022, 2024))+
  scale_y_continuous(name="Age-standardised rate of alcohol-specific deaths")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  theme_custom()+
  labs(title="Northern Ireland had the highest rate of alcohol-specific deaths in 2024",
       subtitle="Age-standardised mortality rates from causes that are 100% attributable to alcohol\n",
       caption="Data from NOMIS/ONS, NRS and NISRA | Plot by Colin Angus")

dev.off()

agg_png("Outputs/ASDCountry2024xSex.png", units="in", width=9, height=6, res=800)
ggplot(AllData %>% filter(Sex!="Total" & Year>=2013), aes(x=Year, y=ASMR, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="", breaks=c(2012, 2014, 2016, 2018, 2020, 2022, 2024))+
  scale_y_continuous(name="Age-standardised rate of alcohol-specific deaths")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="Northern Ireland had the highest rate of alcohol-specific deaths in 2024",
       subtitle="Age-standardised mortality rates from causes that are 100% attributable to alcohol\n",
       caption="Data from NOMIS/ONS, NRS and NISRA | Plot by Colin Angus")

dev.off()
