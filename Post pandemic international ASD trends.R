#Protocol for this analysis registered with OSF: https://osf.io/evmkw

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
library(ggrepel)
library(data.table)
library(HMDHFDplus)
library(keyring)
library(gt)

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
#This code is weirdly janky and falls over sometimes for no reason at all 
#restarting R and/or rerunning it piecemeal seems to work *shrugs*
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

#2013-2016
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part4.zip?sfvrsn=259c5c23_30&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData4 <- data.table::fread(file.path(temp2, "Morticd10_part4"))

#2017-2020
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part5.zip?sfvrsn=ad970d0b_34&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData5 <- data.table::fread(file.path(temp2, "Morticd10_part5"))

#2021-
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part6.zip?sfvrsn=ec801a61_4"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

RawData6 <- data.table::fread(file.path(temp2, "Morticd10_part6"))

#Read in country codes
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/mort_country_codes.zip?sfvrsn=800faac2_5&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

CountryCodes <- data.table::fread(file.path(temp2, "country_codes"))

#Read in population estimates
temp <- tempfile()
temp2 <- tempfile()
source <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/mort_pop.zip?sfvrsn=937039fc_26&ua=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop <- data.table::fread(file.path(temp2, "pop")) %>% 
  merge(CountryCodes, by.x="Country", by.y="country", all.x=TRUE) %>% 
  filter(Frmat<7) %>% 
  #Convert age groups to match Frmat=2
  gather(AgeIndicator, Pop, c(Pop1:Pop26)) %>% 
  mutate(Age=case_when(
    AgeIndicator=="Pop1" ~ "All ages",
    AgeIndicator=="Pop2" ~ "0",
    AgeIndicator %in% c("Pop3", "Pop4", "Pop5", "Pop6") ~ "1-4",
    AgeIndicator=="Pop7" ~ "5-9",
    AgeIndicator=="Pop8" ~ "10-14",
    AgeIndicator=="Pop9" ~ "15-19",
    AgeIndicator=="Pop10" ~ "20-24",
    AgeIndicator=="Pop11" ~ "25-29",
    AgeIndicator=="Pop12" ~ "30-34",
    AgeIndicator=="Pop13" ~ "35-39",
    AgeIndicator=="Pop14" ~ "40-44",
    AgeIndicator=="Pop15" ~ "45-49",
    AgeIndicator=="Pop16" ~ "50-54",
    AgeIndicator=="Pop17" ~ "55-59",
    AgeIndicator=="Pop18" ~ "60-64",
    AgeIndicator=="Pop19" ~ "65-69",
    AgeIndicator=="Pop20" ~ "70-74",
    AgeIndicator %in% c("Pop21", "Pop22", "Pop23", "Pop24", "Pop25") ~ "75+",
    AgeIndicator=="Pop26" ~ "Unknown")) %>% 
  group_by(Year, name, Sex, Age) %>% 
  summarise(Pop=sum(Pop, na.rm=TRUE), .groups="drop")

RawData <- bind_rows(RawData1 %>% mutate(List=as.numeric(List)), 
                     RawData2 %>% mutate(List=as.numeric(List)), 
                     RawData3, RawData4, RawData5, RawData6) %>% 
  merge(CountryCodes, by.x="Country", by.y="country", all.x=TRUE) %>% 
  #Remove sub-administrative districts of a small number of countries that are reported in addition
  #to the country totals for mystery reasons
  filter(is.na(Admin1))

#Define ASD
ASD <- RawData %>% 
  select(Year, Country, Cause, name, Sex, Frmat, Deaths1:Deaths26) %>% 
  mutate(ICD10Upper=substr(Cause, 1, 3),
         CoD=case_when(
           Cause %in% c("E244", "G312", "G621", "G721", "I426", "K292", "K852", "K860", "Q860", "R780") |
             ICD10Upper %in% c("F10", "K70", "X45", "X65", "Y15") ~ "Alcohol-specific",
           TRUE ~ "Other")) %>% 
  group_by(Year, Country, name, Frmat, CoD, Sex) %>% 
  summarise(across(c(Deaths1:Deaths26), ~sum(.x))) %>% 
  ungroup() %>% 
  #Remove countries with Frmat=7 or 9 as these all have populations well below 1m so are excluded from the analysis
  filter(Frmat<7) %>% 
  #Convert age groups to match Frmat=2
  gather(AgeIndicator, Deaths, c(Deaths1:Deaths26)) %>% 
  mutate(Age=case_when(
    AgeIndicator=="Deaths1" ~ "All ages",
    AgeIndicator=="Deaths2" ~ "0",
    AgeIndicator %in% c("Deaths3", "Deaths4", "Deaths5", "Deaths6") ~ "1-4",
    AgeIndicator=="Deaths7" ~ "5-9",
    AgeIndicator=="Deaths8" ~ "10-14",
    AgeIndicator=="Deaths9" ~ "15-19",
    AgeIndicator=="Deaths10" ~ "20-24",
    AgeIndicator=="Deaths11" ~ "25-29",
    AgeIndicator=="Deaths12" ~ "30-34",
    AgeIndicator=="Deaths13" ~ "35-39",
    AgeIndicator=="Deaths14" ~ "40-44",
    AgeIndicator=="Deaths15" ~ "45-49",
    AgeIndicator=="Deaths16" ~ "50-54",
    AgeIndicator=="Deaths17" ~ "55-59",
    AgeIndicator=="Deaths18" ~ "60-64",
    AgeIndicator=="Deaths19" ~ "65-69",
    AgeIndicator=="Deaths20" ~ "70-74",
    AgeIndicator %in% c("Deaths21", "Deaths22", "Deaths23", "Deaths24", "Deaths25") ~ "75+",
    AgeIndicator=="Deaths26" ~ "Unknown")) %>% 
  group_by(Year, name, CoD, Sex, Age) %>% 
  summarise(Deaths=sum(Deaths, na.rm=TRUE), .groups="drop")

#Define simpler definition
ASD_Simple <- RawData %>% 
  select(Year, Country, Cause, name, Sex, Frmat, Deaths1:Deaths26) %>% 
  mutate(ICD10Upper=substr(Cause, 1, 3),
         CoD=case_when(
           ICD10Upper %in% c("F10", "K70", "X45", "X65", "K73", "K74") ~ "Alcohol-specific (simple)",
           TRUE ~ "Other")) %>% 
  group_by(Year, Country, name, Frmat, CoD, Sex) %>% 
  summarise(across(c(Deaths1:Deaths26), ~sum(.x))) %>% 
  ungroup() %>% 
  #Remove countries with Frmat=7 or 9 as these all have populations well below 1m so are excluded from the analysis
  filter(Frmat<7) %>% 
  #Convert age groups to match Frmat=2
  gather(AgeIndicator, Deaths, c(Deaths1:Deaths26)) %>% 
  mutate(Age=case_when(
    AgeIndicator=="Deaths1" ~ "All ages",
    AgeIndicator=="Deaths2" ~ "0",
    AgeIndicator %in% c("Deaths3", "Deaths4", "Deaths5", "Deaths6") ~ "1-4",
    AgeIndicator=="Deaths7" ~ "5-9",
    AgeIndicator=="Deaths8" ~ "10-14",
    AgeIndicator=="Deaths9" ~ "15-19",
    AgeIndicator=="Deaths10" ~ "20-24",
    AgeIndicator=="Deaths11" ~ "25-29",
    AgeIndicator=="Deaths12" ~ "30-34",
    AgeIndicator=="Deaths13" ~ "35-39",
    AgeIndicator=="Deaths14" ~ "40-44",
    AgeIndicator=="Deaths15" ~ "45-49",
    AgeIndicator=="Deaths16" ~ "50-54",
    AgeIndicator=="Deaths17" ~ "55-59",
    AgeIndicator=="Deaths18" ~ "60-64",
    AgeIndicator=="Deaths19" ~ "65-69",
    AgeIndicator=="Deaths20" ~ "70-74",
    AgeIndicator %in% c("Deaths21", "Deaths22", "Deaths23", "Deaths24", "Deaths25") ~ "75+",
    AgeIndicator=="Deaths26" ~ "Unknown")) %>% 
  group_by(Year, name, CoD, Sex, Age) %>% 
  summarise(Deaths=sum(Deaths, na.rm=TRUE), .groups="drop")

#Go through a bit of a palaver now to work out which countries we are including, or not, based on
#total (i.e. not age-specific) alcohol deaths
Inc1 <- ASD %>% 
  filter(CoD=="Alcohol-specific") %>% 
  #Faff about to set ASD to 0 where we have deaths for one sex, but not the other
  spread(Sex, Deaths) %>% 
  mutate(`2`=if_else(!is.na(`1`) & is.na(`2`), 0, `2`),
         `1`=if_else(!is.na(`2`) & is.na(`1`), 0, `1`)) %>% 
  gather(Sex, Deaths, c(`1`: `9`)) %>% 
  merge(Pop, all.x=TRUE) %>% 
  #exclude countries where population data is missing, but population is known to be well below 1m
  filter(!name %in% c("Virgin Islands (USA)", "Belize", "Grenada", "Montserrat",
                      "Saint Kitts and Nevis", "Saint Vincent and Grenadines")) 

Inc2 <- Inc1 %>% 
  filter(Age=="All ages" & Sex!="9") %>% 
  group_by(name, Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  group_by(name) %>% 
  filter(max(Year)>=2022 & min(Year)<=2010) %>% 
  filter(is.na(Pop[Year==max(Year)]) | Pop[Year==max(Year)]>1000000)

MissingPops <- Inc2 %>% 
  select(-c(Deaths)) %>% 
  spread(Year, Pop)

MissingDeaths <- Inc2 %>% 
  select(-c(Pop)) %>% 
  spread(Year, Deaths)

#exclude countries where deaths data exists but has missing years in our period of interest
Inc3 <- Inc2 %>% filter(!name %in% c("Aruba", "Croatia", "Dominica", "Georgia", "Jordan", "Kuwait",
                                     "Monsterrat", "North Macedonia", "Oman", "Panama", 
                                     "Portugal", "Puerto Rico", "Saudi Arabia", "Slovakia",
                                     "Thailand"))

#Bring in missing populations that exist in mortality.org
AgeMatch <- data.frame(Age=c(0:110), Ageband=c("0", "1-4", "1-4", "1-4", "1-4", 
                                               rep(c("5-9", "10-14", "15-19", "20-24", "25-29",
                                                     "30-34", "35-39", "40-44", "45-49", "50-54",
                                                     "55-59", "60-64", "65-69", "70-74"), each=5),
                                               rep("75+", times=36)))

AustriaPop <- readHMDweb(CNTRY="AUT", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Austria", Year=Year+1)

CanadaPop <- readHMDweb(CNTRY="CAN", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Canada", Year=Year+1)

ChilePop <- readHMDweb(CNTRY="CHL", "Population", key_list("mortality.org")[1,2], 
                        key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Chile", Year=Year+1)

DenmarkPop <- readHMDweb(CNTRY="DNK", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Denmark", Year=Year+1)

EstoniaPop <- readHMDweb(CNTRY="EST", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Estonia", Year=Year+1)

FinlandPop <- readHMDweb(CNTRY="FIN", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Finland", Year=Year+1)

FrancePop <- readHMDweb(CNTRY="FRATNP", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="France", Year=Year+1)

IrelandPop <- readHMDweb(CNTRY="IRL", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Ireland", Year=Year+1)

ItalyPop <- readHMDweb(CNTRY="ITA", "Population", key_list("mortality.org")[1,2], 
                         key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Italy", Year=Year+1)

SwitzerlandPop <- readHMDweb(CNTRY="CHE", "Population", key_list("mortality.org")[1,2], 
                        key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="Switzerland", Year=Year+1)

ScotlandPop <- readHMDweb(CNTRY="GBR_SCO", "Population", key_list("mortality.org")[1,2], 
                             key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="United Kingdom, Scotland", Year=Year+1)

USAPop <- readHMDweb(CNTRY="USA", "Population", key_list("mortality.org")[1,2], 
                          key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  merge(AgeMatch) %>% 
  group_by(Year, Ageband) %>% 
  summarise(`1`=sum(Male2), `2`=sum(Female2), .groups="drop") %>% 
  gather(Sex, Pop, c(`1`, `2`)) %>% 
  mutate(name="United States of America", Year=Year+1)

Inc4 <- Inc3 %>% 
  merge(AustriaPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(CanadaPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(ChilePop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(DenmarkPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(EstoniaPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(FinlandPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(FrancePop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(IrelandPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(ItalyPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(SwitzerlandPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(ScotlandPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  merge(USAPop %>% group_by(Year, name) %>% 
          summarise(Pop2=sum(Pop), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2)

MissingPops2 <- Inc4 %>% 
  select(-c(Deaths)) %>% 
  spread(Year, Pop)

#TODO Australia is missing 2005 deaths data for some reason...

#Bring in other WHO population data to plug remaining holes
temp <- tempfile()
source <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/2_Population/WPP2024_POP_F02_2_POPULATION_5-YEAR_AGE_GROUPS_MALE.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

MalePopWHO <- read_excel(temp, sheet="Estimates", range="A17:AF22000") %>% 
  select(c(`Region, subregion, country or area *`, Year, `0-4`:`100+`)) %>% 
  rename("name"="Region, subregion, country or area *") %>% 
  filter(name %in% c("Argentina", "Armenia", "Brazil", "Colombia", "Costa Rica", "Cuba",
                     "Dominican Republic", "Ecuador", "Guatemala", "Kyrgyzstan",
                     "Mauritius", "Mexico", "Nicaragua", "Paraguay", "Peru", "Slovenia",
                     "Uruguay", "Uzbekistan", "Qatar")) %>% 
  gather(Ageband, Pop, c(`0-4`:`100+`)) %>% 
  mutate(Pop=as.numeric(Pop)*1000, Sex=1)

temp <- tempfile()
source <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/2_Population/WPP2024_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

FemalePopWHO <- read_excel(temp, sheet="Estimates", range="A17:AF22000") %>% 
  select(c(`Region, subregion, country or area *`, Year, `0-4`:`100+`)) %>% 
  rename("name"="Region, subregion, country or area *") %>% 
  filter(name %in% c("Argentina", "Armenia", "Brazil", "Colombia", "Costa Rica", "Cuba",
                     "Dominican Republic", "Ecuador", "Guatemala", "Kyrgyzstan",
                     "Mauritius", "Mexico", "Nicaragua", "Paraguay", "Peru", "Slovenia",
                     "Uruguay", "Uzbekistan", "Qatar")) %>% 
  gather(Ageband, Pop, c(`0-4`:`100+`)) %>% 
  mutate(Pop=as.numeric(Pop)*1000, Sex=2)  

PopWHO <- bind_rows(MalePopWHO, FemalePopWHO) %>% 
  #Resolve lack of age 0 in this data, there will no ASD in this age group,
  #so happy to do this simplistically
  filter(Ageband=="0-4") %>% 
  mutate(Pop=Pop/5, Ageband="0") %>% 
  bind_rows(bind_rows(MalePopWHO, FemalePopWHO) %>% 
              filter(Ageband=="0-4") %>% 
              mutate(Pop=4*Pop/5, Ageband="1-4")) %>% 
  bind_rows(bind_rows(MalePopWHO, FemalePopWHO) %>% filter(Ageband!="0-4")) %>% 
  mutate(Age=case_when(
    Ageband %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+") ~ "75+",
    TRUE ~ Ageband)) %>% 
  group_by(Age, name, Year, Sex) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#Add all ages group
PopWHO <- PopWHO %>% group_by(name, Year, Sex) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  mutate(Age="All ages") %>% 
  bind_rows(PopWHO) 

PopWHOTotal <- PopWHO %>% 
  filter(Age=="All ages") %>% 
  group_by(name, Year) %>% 
  summarise(Pop=sum(Pop), .groups="drop")
  
Inc5 <- Inc4 %>% merge(PopWHOTotal %>% rename("Pop2"="Pop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2)

#We are still missing 2024 population data for Ecuador, Scotland and Uzbekistan
#NRS have 2024 population estimates for Scotland, so bring those in
#Get 2024 population estimates from NRS
#No similar data exists for Ecuador or Uzbekistan
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/media/txvdnee4/data-mid-year-population-estimates-2024.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ScotlandPop24 <- read_excel(temp, sheet="Table 1", range="D6:CR7", col_names = FALSE) %>% 
  set_names("Sex", "All ages", as.character(c(0:90))) %>% 
  select(-`All ages`) %>% 
  gather(Age, Pop, c(`0`:`90`)) %>% 
  mutate(Age=as.numeric(Age),
         Age=case_when(Age==0 ~ "0", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14",
                       Age<20 ~ "15-19", Age<25 ~ "20-24", Age<30 ~ "25-29", Age<35 ~ "30-34",
                       Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
                       Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74",
                       TRUE ~ "75+")) %>% 
  group_by(Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  mutate(Sex=if_else(Sex=="Males", "1", "2"), Year=2024,
         name="United Kingdom, Scotland") 

#Merge in 2024 Scottish pop and remove countries with gaps in the data that still qualify otherwise 
#(i.e. gaps are before 2010)
#Also the overall UK data, since we're analysing by nation (w/ England & Wales combined)
Inc6 <- Inc5 %>% merge(ScotlandPop24 %>% group_by(Year) %>% 
                         summarise(Pop2=sum(Pop), name=unique(name), .groups="drop"), all.x=TRUE) %>% 
  mutate(Pop=if_else(is.na(Pop), Pop2, Pop)) %>% 
  select(-Pop2) %>% 
  filter(!(name=="Uzbekistan" & (Year<2009 | Year==2024)) & name!="United Kingdom" )

#Remove countries with ASD rates that never exceed 1/100,000 as well as Qatar,
#which does just squeak over this threshold in some years, but has very low and very 
#volatile rates, suggesting pure noise.
Inc7 <- Inc6 %>% 
  #filter(!is.na(Pop) & !is.na(ASD)) %>% 
  group_by(name) %>% 
  mutate(ASDrate=Deaths*100000/Pop) %>% 
  summarise(ASDrate=max(ASDrate, na.rm=TRUE), .groups="drop") %>% 
  filter(ASDrate>1) %>% 
  filter(name!="Qatar")

IncludedCountries <- unique(Inc7$name)

MissingPops3 <- Inc6 %>% 
  filter(name %in% IncludedCountries) %>% 
  select(-c(Deaths)) %>% 
  spread(Year, Pop)

FinalTotalData <- Inc6 %>% 
  filter(name %in% IncludedCountries)

FinalTotalData %>% 
  ggplot(aes(x=Year, y=Deaths))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits=c(0,NA))+
  facet_wrap(~name, scales="free_y")+
  theme_custom()

FinalTotalData %>% 
  ggplot(aes(x=Year, y=Pop))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits=c(0,NA))+
  facet_wrap(~name, scales="free_y")+
  theme_custom()

FinalTotalData %>% 
  mutate(ASDrate=Deaths*100000/Pop) %>% 
  ggplot(aes(x=Year, y=ASDrate, colour=name))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits=c(0,NA))+
  facet_wrap(~name, scales="free_y")+
  guides(colour="none")+
  theme_custom()

#Report numbers of countries excluded at each stage
#Countries in Raw WHO deaths data
length(unique(RawData$name))
#Countries that have data from 2005 and up to 2022
temp1 <- Inc1 %>% 
  filter(Age=="All ages" & Sex!="9") %>% 
  group_by(name, Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  group_by(name) %>% 
  filter(max(Year)>=2022 & min(Year)<=2010)
length(unique(temp1$name))-7
#Countries with a population over 1m
length(unique(Inc2$name))-7
#Included countries
length(IncludedCountries)

#Compile final dataset of included countries only, by age and sex
HMDPops <- bind_rows(AustriaPop, CanadaPop, ChilePop, DenmarkPop, EstoniaPop, FinlandPop, FrancePop,
                     IrelandPop, ItalyPop, ScotlandPop, SwitzerlandPop, USAPop)

MinYears <- FinalTotalData %>% group_by(name) %>% 
  summarise(MinYear=max(min(Year), 2000))

FinalWHOData <- ASD %>% 
  filter(name %in% IncludedCountries) %>% 
  merge(MinYears) %>% 
  filter(Year>=MinYear) %>% 
  select(-MinYear) %>% 
  #Reallocate unknown sex data in proportion to sex ratio of known deaths
  filter(CoD=="Alcohol-specific") %>% 
  spread(Sex, Deaths) %>% 
  mutate(`9`=if_else(is.na(`9`), 0, `9`),
         `1`=if_else(is.na(`1`), 0, `1`),
         `2`=if_else(is.na(`2`), 0, `2`),
    Male=if_else(`1`+`2`==0, 0, `1`+`9`*`1`/(`1`+`2`)),
    Female=if_else(`1`+`2`==0, 0, `2`+`9`*`2`/(`1`+`2`))) %>% 
  select(-c(`1`, `2`, `9`)) %>% 
  gather(Sex, Deaths, c(Male, Female)) %>% 
  spread(Age, Deaths) %>% 
  #Repeat for unknown age data
  mutate(New_0=if_else(`All ages`==0, 0, `0`+Unknown*`0`/(`All ages`-Unknown)),
         `New_1-4`=if_else(`All ages`==0, 0, `1-4`+Unknown*`1-4`/(`All ages`-Unknown)),
         `New_5-9`=if_else(`All ages`==0, 0, `5-9`+Unknown*`5-9`/(`All ages`-Unknown)),
         `New_10-14`=if_else(`All ages`==0, 0, `10-14`+Unknown*`10-14`/(`All ages`-Unknown)),
         `New_15-19`=if_else(`All ages`==0, 0, `15-19`+Unknown*`15-19`/(`All ages`-Unknown)),
         `New_20-24`=if_else(`All ages`==0, 0, `20-24`+Unknown*`20-24`/(`All ages`-Unknown)),
         `New_25-29`=if_else(`All ages`==0, 0, `25-29`+Unknown*`25-29`/(`All ages`-Unknown)),
         `New_30-34`=if_else(`All ages`==0, 0, `30-34`+Unknown*`30-34`/(`All ages`-Unknown)),
         `New_35-39`=if_else(`All ages`==0, 0, `35-39`+Unknown*`35-39`/(`All ages`-Unknown)),
         `New_40-44`=if_else(`All ages`==0, 0, `40-44`+Unknown*`40-44`/(`All ages`-Unknown)),
         `New_45-49`=if_else(`All ages`==0, 0, `45-49`+Unknown*`45-49`/(`All ages`-Unknown)),
         `New_50-54`=if_else(`All ages`==0, 0, `50-54`+Unknown*`50-54`/(`All ages`-Unknown)),
         `New_55-59`=if_else(`All ages`==0, 0, `55-59`+Unknown*`55-59`/(`All ages`-Unknown)),
         `New_60-64`=if_else(`All ages`==0, 0, `60-64`+Unknown*`60-64`/(`All ages`-Unknown)),
         `New_65-69`=if_else(`All ages`==0, 0, `65-69`+Unknown*`65-69`/(`All ages`-Unknown)),
         `New_70-74`=if_else(`All ages`==0, 0, `70-74`+Unknown*`70-74`/(`All ages`-Unknown)),
         `New_75+`=if_else(`All ages`==0, 0, `75+`+Unknown*`75+`/(`All ages`-Unknown))) %>% 
  select(-c(`0`, `1-4`, `5-9`, `10-14`, `15-19`, `20-24`, `25-29`, `30-34`, `35-39`,
            `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`, `70-74`, `75+`, Unknown)) %>% 
  gather(Age, Deaths, c(`All ages`:`New_75+`)) %>% 
  mutate(Age=if_else(substr(Age,1,3)=="New", substr(Age, 5,9), Age)) %>% 
  merge(Pop %>% mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  merge(HMDPops %>% group_by(Year, Sex, name) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Ageband="All ages") %>% 
          bind_rows(HMDPops)  %>% 
          rename("Pop2"="Pop", "Age"="Ageband") %>% 
          mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  merge(PopWHO %>% group_by(Year, Sex, name) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Age="All ages") %>% 
          bind_rows(PopWHO) %>% 
          rename("Pop3"="Pop") %>% 
          mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  merge(ScotlandPop24 %>% group_by(Year, Sex, name) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Age="All ages") %>% 
          bind_rows(ScotlandPop24) %>% 
          rename("Pop4"="Pop") %>% 
          mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  mutate(Pop=case_when(
    is.na(Pop) & !is.na(Pop2) ~ Pop2,
    is.na(Pop) & is.na(Pop2) & !is.na(Pop3) ~ Pop3,
    is.na(Pop) & is.na(Pop2) & is.na(Pop3) & !is.na(Pop4) ~ Pop4,
    TRUE ~ Pop)) %>%
  mutate(Country=case_when(
    name=="United Kingdom, England and Wales" ~ "England & Wales",
    name=="United Kingdom, Northern Ireland" ~ "Northern Ireland",
    name=="United Kingdom, Scotland" ~ "Scotland",
    name=="United States of America" ~  "USA",
    TRUE ~ name)) %>% 
  select(-c(Pop2, Pop3, Pop4, name)) 

FinalWHOData_Simple <- ASD_Simple %>% 
  filter(name %in% IncludedCountries) %>% 
  merge(MinYears) %>% 
  filter(Year>=MinYear) %>% 
  select(-MinYear) %>% 
  #Reallocate unknown sex data in proportion to sex ratio of known deaths
  filter(CoD=="Alcohol-specific (simple)") %>% 
  spread(Sex, Deaths) %>% 
  mutate(`9`=if_else(is.na(`9`), 0, `9`),
         `1`=if_else(is.na(`1`), 0, `1`),
         `2`=if_else(is.na(`2`), 0, `2`),
         Male=if_else(`1`+`2`==0, 0, `1`+`9`*`1`/(`1`+`2`)),
         Female=if_else(`1`+`2`==0, 0, `2`+`9`*`2`/(`1`+`2`))) %>% 
  select(-c(`1`, `2`, `9`)) %>% 
  gather(Sex, Deaths, c(Male, Female)) %>% 
  spread(Age, Deaths) %>% 
  #Repeat for unknown age data
  mutate(New_0=if_else(`All ages`==0, 0, `0`+Unknown*`0`/(`All ages`-Unknown)),
         `New_1-4`=if_else(`All ages`==0, 0, `1-4`+Unknown*`1-4`/(`All ages`-Unknown)),
         `New_5-9`=if_else(`All ages`==0, 0, `5-9`+Unknown*`5-9`/(`All ages`-Unknown)),
         `New_10-14`=if_else(`All ages`==0, 0, `10-14`+Unknown*`10-14`/(`All ages`-Unknown)),
         `New_15-19`=if_else(`All ages`==0, 0, `15-19`+Unknown*`15-19`/(`All ages`-Unknown)),
         `New_20-24`=if_else(`All ages`==0, 0, `20-24`+Unknown*`20-24`/(`All ages`-Unknown)),
         `New_25-29`=if_else(`All ages`==0, 0, `25-29`+Unknown*`25-29`/(`All ages`-Unknown)),
         `New_30-34`=if_else(`All ages`==0, 0, `30-34`+Unknown*`30-34`/(`All ages`-Unknown)),
         `New_35-39`=if_else(`All ages`==0, 0, `35-39`+Unknown*`35-39`/(`All ages`-Unknown)),
         `New_40-44`=if_else(`All ages`==0, 0, `40-44`+Unknown*`40-44`/(`All ages`-Unknown)),
         `New_45-49`=if_else(`All ages`==0, 0, `45-49`+Unknown*`45-49`/(`All ages`-Unknown)),
         `New_50-54`=if_else(`All ages`==0, 0, `50-54`+Unknown*`50-54`/(`All ages`-Unknown)),
         `New_55-59`=if_else(`All ages`==0, 0, `55-59`+Unknown*`55-59`/(`All ages`-Unknown)),
         `New_60-64`=if_else(`All ages`==0, 0, `60-64`+Unknown*`60-64`/(`All ages`-Unknown)),
         `New_65-69`=if_else(`All ages`==0, 0, `65-69`+Unknown*`65-69`/(`All ages`-Unknown)),
         `New_70-74`=if_else(`All ages`==0, 0, `70-74`+Unknown*`70-74`/(`All ages`-Unknown)),
         `New_75+`=if_else(`All ages`==0, 0, `75+`+Unknown*`75+`/(`All ages`-Unknown))) %>% 
  select(-c(`0`, `1-4`, `5-9`, `10-14`, `15-19`, `20-24`, `25-29`, `30-34`, `35-39`,
            `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`, `70-74`, `75+`, Unknown)) %>% 
  gather(Age, Deaths, c(`All ages`:`New_75+`)) %>% 
  mutate(Age=if_else(substr(Age,1,3)=="New", substr(Age, 5,9), Age)) %>% 
  merge(Pop %>% mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  merge(HMDPops %>% group_by(Year, Sex, name) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Ageband="All ages") %>% 
          bind_rows(HMDPops)  %>% 
          rename("Pop2"="Pop", "Age"="Ageband") %>% 
          mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  merge(PopWHO %>% group_by(Year, Sex, name) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Age="All ages") %>% 
          bind_rows(PopWHO) %>% 
          rename("Pop3"="Pop") %>% 
          mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  merge(ScotlandPop24 %>% group_by(Year, Sex, name) %>% 
          summarise(Pop=sum(Pop), .groups="drop") %>% 
          mutate(Age="All ages") %>% 
          bind_rows(ScotlandPop24) %>% 
          rename("Pop4"="Pop") %>% 
          mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE) %>% 
  mutate(Pop=case_when(
    is.na(Pop) & !is.na(Pop2) ~ Pop2,
    is.na(Pop) & is.na(Pop2) & !is.na(Pop3) ~ Pop3,
    is.na(Pop) & is.na(Pop2) & is.na(Pop3) & !is.na(Pop4) ~ Pop4,
    TRUE ~ Pop)) %>%
  mutate(Country=case_when(
    name=="United Kingdom, England and Wales" ~ "England & Wales",
    name=="United Kingdom, Northern Ireland" ~ "Northern Ireland",
    name=="United Kingdom, Scotland" ~ "Scotland",
    name=="United States of America" ~  "USA",
    TRUE ~ name)) %>% 
  select(-c(Pop2, Pop3, Pop4, name)) 

#Add in newer data for any available countries
MaxYear <- FinalTotalData %>% group_by(name) %>% 
  summarise(MaxYear=max(Year))

#No additional data available for any country as of 4th March 2026

##################################
#Calculate age-standardised rates
ASRates <- FinalWHOData %>% 
  filter(Age!="All ages") %>% 
  merge(ESP %>% mutate(Age=case_when(
    Age=="Under 1" ~ "0",
    Age %in% c("75-79", "80-84", "85-89", "90+") ~ "75+",
    TRUE ~ Age)) %>% 
      group_by(Age) %>% 
      summarise(ESP=sum(ESP), .groups="drop")) %>% 
  group_by(Age, Country, Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), ESP=unique(ESP), .groups="drop") %>% 
  mutate(ASRate=Deaths*100000/Pop) %>% 
  group_by(Country, Year) %>% 
  summarise(ASRate=weighted.mean(ASRate, ESP), .groups="drop")
  
agg_png("X:/HAR_SP/SP/SARG_ADR/WP5/Papers/Melissa's International Pandemic Deaths Paper/DescriptiveFig1FullOnlyMar26.png",
        units="in", width=12, height=6, res=800)
ggplot(ASRates, aes(x=Year, y=ASRate, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  geom_vline(xintercept=2019.2, colour="grey60", linetype=2)+
  scale_x_continuous(name="", breaks=c(2000, 2010, 2020))+
  scale_y_continuous(name="Age-standardised alcohol-specific deaths per 100,000")+
  facet_wrap(~Country, scales="free_y")+
  #facet_wrap(~Country)+
  guides(colour="none")+
  theme_custom()+
  labs(caption="Data from WHO and mortality.org")

dev.off()

#Repeat for simple definition
ASRates_Simple <- FinalWHOData_Simple %>% 
  filter(Age!="All ages") %>% 
  merge(ESP %>% mutate(Age=case_when(
    Age=="Under 1" ~ "0",
    Age %in% c("75-79", "80-84", "85-89", "90+") ~ "75+",
    TRUE ~ Age)) %>% 
      group_by(Age) %>% 
      summarise(ESP=sum(ESP), .groups="drop")) %>% 
  group_by(Age, Country, Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), ESP=unique(ESP), .groups="drop") %>% 
  mutate(ASRate=Deaths*100000/Pop) %>% 
  group_by(Country, Year) %>% 
  summarise(ASRate=weighted.mean(ASRate, ESP), .groups="drop")

ggplot(ASRates_Simple, aes(x=Year, y=ASRate, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  geom_vline(xintercept=2019.2, colour="grey60", linetype=2)+
  facet_wrap(~Country, scales="free_y")+
  guides(colour="none")+
  theme_custom()

#Compare the two
ASRates_All <- bind_rows(ASRates %>% mutate(Definition="Full"),
                         ASRates_Simple %>% mutate(Definition="Simple"))

agg_png("X:/HAR_SP/SP/SARG_ADR/WP5/Papers/Melissa's International Pandemic Deaths Paper/DescriptiveFig1Mar26.png",
        units="in", width=12, height=6, res=500)
ggplot(ASRates_All, aes(x=Year, y=ASRate, colour=Country, linetype=Definition))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  geom_vline(xintercept=2019.2, colour="grey60", linetype=2)+
  scale_x_continuous(name="", breaks=c(2000, 2010, 2020))+
  scale_y_continuous(name="Age-standardised alcohol-specific deaths per 100,000")+
  facet_wrap(~Country, scales="free_y")+
  guides(colour="none")+
  theme_custom()

dev.off()

#Generate final datasets
#Age-standardised rates by definition, year and country
write.csv(ASRates_All, "X:/HAR_SP/SP/SARG_ADR/WP5/Papers/Melissa's International Pandemic Deaths Paper/ASRates_AllMar26.csv")

#Age/sex-specific counts, pops and rates in 10-year age bands by definition, year and country
ASDCounts_All <- FinalWHOData %>% 
  mutate(Definition="Full") %>% 
  bind_rows(FinalWHOData_Simple %>% mutate(Definition="Simple")) %>% 
  mutate(Ageband=case_when(
    Age %in% c("15-19", "20-24") ~ "15-24", Age %in% c("25-29", "30-34") ~ "25-34",
    Age %in% c("35-39", "40-44") ~ "35-44", Age %in% c("45-49", "50-54") ~ "45-54",
    Age %in% c("55-59", "60-64") ~ "55-64", Age %in% c("65-69", "70-74") ~ "65-74",
    Age=="75+" ~ "75+")) %>% 
  filter(!is.na(Ageband)) %>% 
  group_by(Ageband, Sex, Year, Country, Definition) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop")


write.csv(ASDCounts_All, "X:/HAR_SP/SP/SARG_ADR/WP5/Papers/Melissa's International Pandemic Deaths Paper/ASDCounts_AllMar26.csv")

####################################################
#Descriptive analysis

DescriptiveAnalysis <- ASRates_All %>% filter(Year>=2019) %>% 
  group_by(Country, Definition) %>% 
  mutate(AbsChange=ASRate-ASRate[Year==2019],
         RelChange=AbsChange/ASRate[Year==2019]) %>% 
  ungroup() %>% 
  filter(Year!=2019)

DescriptiveAnalysis %>% filter(Definition=="Full") %>% 
  select(-c(ASRate, Definition, RelChange)) %>% 
  spread(Year, AbsChange) %>% 
  merge(DescriptiveAnalysis %>% filter(Definition=="Full") %>% 
          select(-c(ASRate, Definition, AbsChange)) %>% 
          spread(Year, RelChange) %>% 
          rename("2020_rel"="2020", "2021_rel"="2021", "2022_rel"="2022", 
                 "2023_rel"="2023", "2024_rel"="2024")) %>% 
  gt(rowname_col="Country") %>% 
  tab_spanner(label="Absolute change in age-standardised alcohol-specific deaths per 100,000",
              columns=c(`2020`:`2024`)) %>% 
  fmt_number(columns=c(`2020`:`2024`), decimals=2) %>% 
  data_color(columns=c(`2020`:`2024`), method="numeric", palette="PuOr", 
             na_color="white", domain=c(-18, 18)) %>% 
  tab_style(style=cell_text(color="white"),
            locations=cells_body(columns=`2023`, rows=is.na(`2023`))) %>% 
  tab_style(style=cell_text(color="white"),
            locations=cells_body(columns=`2024`, rows=is.na(`2024`))) %>% 
  tab_spanner(label="Relative change in age-standardised alcohol-specific deaths per 100,000",
              columns=c(`2020_rel`:`2024_rel`)) %>% 
  fmt_percent(columns=c(`2020_rel`:`2024_rel`), decimals=1) %>% 
  data_color(columns=c(`2020_rel`:`2024_rel`), method="numeric", palette="PuOr", 
             na_color="white", domain=c(-1.03, 1.03)) %>% 
  tab_style(style=cell_text(color="white"),
            locations=cells_body(columns=`2023_rel`, rows=is.na(`2023_rel`))) %>% 
  tab_style(style=cell_text(color="white"),
            locations=cells_body(columns=`2024_rel`, rows=is.na(`2024_rel`))) %>% 
  cols_label("2020_rel"="2020", "2021_rel"="2021", "2022_rel"="2022", 
             "2023_rel"="2023", "2024_rel"="2024") %>% 
  gtsave("X:/HAR_SP/SP/SARG_ADR/WP5/Papers/Melissa's International Pandemic Deaths Paper/DescriptiveTableMar26.png")
