rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(patchwork)
library(scales)
library(ggtext)

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
          legend.title=element_text(colour="Grey20"))
}


options(scipen=10000)

#################
#England & Wales#
#################

#Read in 2022 death registrations by cause
temp <- tempfile()
url1 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredsummarystatisticsenglandandwales/2022/deathsregisteredsummary2022.xlsx"
rawfile <- curl_download(url=url1, destfile=temp, quiet=FALSE, mode="wb")

raw_22 <- read_excel(rawfile, sheet="3", range="A6:X236") %>% 
  mutate(Year=2022,
         across(.cols=c(5:24), ~as.numeric(.x)))
  

#Read in 2021 death registrations by cause
temp <- tempfile()
url2 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredsummarystatisticsenglandandwales/2021/deathsregisteredsummary20212.xlsx"
rawfile <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

raw_21 <- read_excel(rawfile, sheet="3", range="A6:X236")%>% 
  mutate(Year=2021,
         across(.cols=c(5:24), ~as.numeric(.x))) %>% 
  rename("ICD-10 code"="ICD to 10 code", 
         "Aged 90 years and above"="Aged 90 years and over")

data2122 <- bind_rows(raw_22, raw_21) %>% 
  set_names(c("ICD-10 code", "Cause", "Sex", "Total", "<1", "1-4", "5-9", "10-14",
              "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
              "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+", "Year")) %>% 
  gather(Age, Deaths, c(4:24)) %>% 
  mutate(Age=factor(Age, levels=c("Total", "<1", "1-4", "5-9", "10-14",
                                  "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"))) %>% 
  #identify causes we want to look at related to deaths of despair
  filter(Cause %in% c("All causes, all ages", "Malignant neoplasms",
                      "Mental and behavioural disorders due to psychoactive substance use",
                      "Diseases of the circulatory system", "Influenza [note 8]",
                      "Diseases of the liver", "COVID-19 [note 7]",
                      "External causes of morbidity and mortality", "Accidents",
                      "Accidental poisoning by and exposure to noxious substances",
                      "Intentional self-harm", "Assault",
                      "Intentional self-harm and event of undetermined intent [note 9]")) %>% 
  mutate(Cause=case_when(
    Cause=="All causes, all ages" ~ "All cause",
    Cause=="Malignant neoplasms" ~ "Cancer",
    Cause=="Mental and behavioural disorders due to psychoactive substance use" ~ "Dependence",
    Cause=="Diseases of the circulatory system" ~ "Cardiovascular",
    Cause=="Influenza [note 8]" ~ "Flu",
    Cause=="Diseases of the liver" ~ "Liver disease", 
    Cause=="COVID-19 [note 7]" ~ "COVID-19",
    Cause=="External causes of morbidity and mortality" ~ "All external causes",
    Cause=="Accidental poisoning by and exposure to noxious substances" ~ "Drug/alcohol poisoning",
    Cause=="Intentional self-harm and event of undetermined intent [note 9]" ~ "Suicide (broad)",
    Cause=="Intentional self-harm" ~ "Suicide (narrow)",
    TRUE ~ Cause))

#Read in 2001-2020
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/21stcmortality.xlsx"
rawfile <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data.01 <- read_excel(rawfile, sheet="1", range="A5:E21265")
data.02 <- read_excel(rawfile, sheet="2", range="A5:E20880")
data.03 <- read_excel(rawfile, sheet="3", range="A5:E21251")
data.04 <- read_excel(rawfile, sheet="4", range="A5:E20959")
data.05 <- read_excel(rawfile, sheet="5", range="A5:E20928")
data.06 <- read_excel(rawfile, sheet="6", range="A5:E20866")
data.07 <- read_excel(rawfile, sheet="7", range="A5:E20657")
data.08 <- read_excel(rawfile, sheet="8", range="A5:E20660")
data.09 <- read_excel(rawfile, sheet="9", range="A5:E20792")
data.10 <- read_excel(rawfile, sheet="10", range="A5:E20784")
data.11 <- read_excel(rawfile, sheet="11", range="A5:E20380")
data.12 <- read_excel(rawfile, sheet="12", range="A5:E20211")
data.13 <- read_excel(rawfile, sheet="13", range="A5:E20439")
data.14 <- read_excel(rawfile, sheet="14", range="A5:E20426")
data.15 <- read_excel(rawfile, sheet="15", range="A5:E20198")
data.16 <- read_excel(rawfile, sheet="16", range="A5:E20280")
data.17 <- read_excel(rawfile, sheet="17", range="A5:E20193")
data.18 <- read_excel(rawfile, sheet="18", range="A5:E20481")
data.19 <- read_excel(rawfile, sheet="19", range="A5:E20305")
data.20 <- read_excel(rawfile, sheet="20", range="A5:E19032")

ewdata <- bind_rows(data.01, data.02, data.03, data.04, data.05, data.06, data.07,
                    data.08, data.09, data.10, data.11, data.12, data.13, data.14, 
                    data.15, data.16, data.17, data.18, data.19, data.20) %>% 
  set_names("ICD-10 code", "Year", "Sex", "Age", "Deaths") %>% 
  #Allocate causes to code groups
  mutate(code1=substr(`ICD-10 code`, 1, 1), code2=as.numeric(substr(`ICD-10 code`,2,3)), 
         code3=as.numeric(substr(`ICD-10 code`,4,4)),
         Cause=case_when(
           code1=="U" & code2==7 ~ "COVID-19",
           code1=="C" ~ "Cancer",
           code1=="F" & code2 %in% c(10:19) ~ "Dependence",
           code1=="I" ~ "Cardiovascular",
           code1=="J" & code2 %in% c(10:11) ~ "Flu",
           code1=="K" & code2 %in% c(70:77) ~ "Liver disease",
           code1=="X" & code2 %in% c(40:49) ~ "Drug/alcohol poisoning",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide (narrow)",
           (code1=="X" & code2>=85) | (code1=="Y" & code2<=9) ~ "Assault",
           code1=="Y" & code2 %in% c(10:34) ~ "Suicide (broad) partial",
           code1=="V" | (code1=="X" & code2<=59) ~ "Accidents partial",
           code1 %in% c("V", "X", "Y") ~ "All external causes partial",
           TRUE ~ "Other"),
         Age=case_when(
           Age %in% c("neonates", "neonatal", "Neonates", "<1") ~ "<1",
           TRUE ~ Age)) %>% 
  #Collapse into cause groups
  group_by(Year, Age, Sex, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  #Tidy groups to match 2021/22 data
  spread(Cause, Deaths) %>% 
  mutate(across(c(4:16), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate(`Suicide (broad)`=`Suicide (broad) partial`+ `Suicide (narrow)`,
         Accidents=`Accidents partial`+Assault+`Suicide (broad)`+
           `Drug/alcohol poisoning`,
         `All external causes`=Accidents+`All external causes partial`,
         `All cause`=Other+`COVID-19`+Cancer+Cardiovascular+Flu+`Liver disease`+
           `All external causes`) %>% 
  select(-c("Suicide (broad) partial", "Accidents partial", 
            "All external causes partial")) %>% 
  ungroup() %>% 
  gather(Cause, Deaths, c(4:17)) %>% 
  mutate(Sex=if_else(Sex==1, "Males", "Females"),
         Age=case_when(
           Age=="01-04" ~ "1-4", Age=="05-09" ~ "5-9", TRUE ~ Age)) 
  
fulldata <- data2122 %>% 
  mutate(Age=if_else(Age %in% c("85-89", "90+"), "85+", Age)) %>% 
  group_by(Age, Year, Cause, Sex) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  bind_rows(ewdata) %>% 
  #Add total for older data
  bind_rows(ewdata %>% group_by(Cause, Year, Sex) %>% 
              summarise(Deaths=sum(Deaths), .groups="drop") %>% 
              mutate(Age="Total"))

shortdata <- fulldata %>% 
  group_by(Year, Cause, Age) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")

shortdata %>% filter(Age=="Total" & Cause!="All cause" & Year>2010) %>% 
  ggplot(aes(x=Year, y=Deaths, colour=Cause))+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits=c(0,NA))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

shortdata %>% filter(Age=="Total" & Year >2010 &
                       Cause %in% c("Dependence", "Drug/alcohol poisoning", 
                                    "Liver disease", "Suicide (broad)")) %>% 
  mutate(flag=if_else(Year>=2020, 1, 0)) %>% 
  ggplot(aes(x=Year, y=Deaths))+
  geom_line(colour="grey40")+
  geom_point(aes(colour=as.factor(flag)))+
  scale_x_continuous(breaks=c(2012, 2015, 2018, 2021))+
  scale_y_continuous(limits=c(0,NA))+
  scale_colour_manual(values=c("grey40", "tomato"))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()+
  theme(legend.position="none")

shorterdata <- shortdata %>% 
  mutate(ageband=case_when(
    Age %in% c("15-19", "20-24") ~ "15-24",
    Age %in% c("25-29", "30-34") ~ "25-34",
    Age %in% c("35-39", "40-44") ~ "35-44",
    Age %in% c("45-49", "50-54") ~ "45-54",
    Age %in% c("55-59", "60-64") ~ "55-64",
    Age %in% c("65-69", "70-74") ~ "65-74",
    Age %in% c("75-79", "80-84") ~ "75-84",
    Age=="85+" ~ "85+")) %>% 
  group_by(ageband, Year, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")

agg_png("Outputs/DoDONS2022.png", units="in", width=10, height=6, res=800)
shorterdata %>% filter(Year>2010 & Cause %in% c("Dependence", "Drug/alcohol poisoning", 
                                                "Liver disease", "Suicide (broad)"),
                       !is.na(ageband)) %>% 
  ggplot(aes(x=Year, y=Deaths, colour=Cause))+
  geom_line(alpha=0.3)+
  geom_point(data=. %>% filter(Year==2019))+
  geom_line(data=. %>% filter(Year>=2019), arrow=arrow(angle=25, type="closed", 
                         length=unit(0.13, "cm")))+
  scale_y_continuous(limits=c(0,NA))+
  scale_x_continuous(breaks=c(2012, 2017, 2022))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  facet_wrap(~ageband, nrow=1)+
  theme_custom()+
  theme(legend.position="top")
dev.off()
  
#
#data %>% filter(Cause=="External causes of morbidity and mortality" & Age!="Total") %>% 
#  ggplot(aes(x=Year, y=Deaths, fill=as.factor(Year)))+
#  geom_col()+
#  facet_wrap(~Age)+
#  theme_custom()
#
#data2 <- data %>% 
#  mutate(ageband=case_when(
#    Age %in% c("20-24", "25-29", "30-34", "35-39", "40-44") ~ "20-44",
#    Age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
#    Age %in% c("65-69", "70-74") ~ "65-74",
#    Age %in% c("75-79", "80-84") ~ "75-84")) %>% 
#  group_by(Year, ageband, `ICD-10 code`, Cause) %>% 
#  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
#  spread(Year, Deaths) %>% 
#  mutate(abschange=`2022`-`2021`,
#         relchange=abschange/`2021`)

##########
#Scotland#
##########

#Read in 2022 quarterly data
temp <- tempfile()
url2 <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/22/q1/quarter-1-22-tables.xlsx"
q1raw <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

q1raw <- read_excel(q1raw, sheet="Q4", range="A5:K84") %>% 
  filter(`...2` %in% c("Mental and behavioural disorders due to use of alcohol",
                       "Chronic liver disease")) %>% 
  select(-c(9,10)) %>% 
  set_names(c("ICD-10 code", "Cause", "2016", "2017", "2018", "2019", "2020",
              "2021", "2022")) %>% 
  gather(Year, Deaths, c(3:9)) %>% 
  mutate(Date=as.Date(paste0(Year, "-10-01")), period="Q1")

temp <- tempfile()
url3 <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/22/q2/quarter-2-22-tables.xlsx"
q2raw <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

q2raw <- read_excel(q2raw, sheet="Q4", range="A5:K84") %>% 
  filter(`...2` %in% c("Mental and behavioural disorders due to use of alcohol",
                       "Chronic liver disease")) %>% 
  select(-c(9,10)) %>% 
  set_names(c("ICD-10 code", "Cause", "2016", "2017", "2018", "2019", "2020",
              "2021", "2022")) %>% 
  gather(Year, Deaths, c(3:9)) %>% 
  mutate(Date=as.Date(paste0(Year, "-07-01")), period="Q2")

temp <- tempfile()
url4 <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/22/q3/quarter-3-22-tables.xlsx"
q3raw <- curl_download(url=url4, destfile=temp, quiet=FALSE, mode="wb")

q3raw <- read_excel(q3raw, sheet="Q4", range="A5:K84") %>% 
  filter(`...2` %in% c("Mental and behavioural disorders due to use of alcohol",
                       "Chronic liver disease")) %>% 
  select(-c(9,10)) %>% 
  set_names(c("ICD-10 code", "Cause", "2016", "2017", "2018", "2019", "2020",
              "2021", "2022")) %>% 
  gather(Year, Deaths, c(3:9)) %>% 
  mutate(Date=as.Date(paste0(Year, "-04-01")), period="Q3")

temp <- tempfile()
url5 <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/22/q4/quarter-4-22-tables.xlsx"
q4raw <- curl_download(url=url5, destfile=temp, quiet=FALSE, mode="wb")

q4raw <- read_excel(q4raw, sheet="Q4", range="A5:K84") %>% 
  filter(`...2` %in% c("Mental and behavioural disorders due to use of alcohol",
                       "Chronic liver disease")) %>% 
  select(-c(9,10)) %>% 
  set_names(c("ICD-10 code", "Cause", "2016", "2017", "2018", "2019", "2020",
              "2021", "2022")) %>% 
  gather(Year, Deaths, c(3:9)) %>% 
  mutate(Date=as.Date(paste0(Year, "-01-01")), period="Q4")

scotdata <- bind_rows(q1raw, q2raw, q3raw, q4raw) %>% 
  mutate(Deaths=as.numeric(Deaths),
         Cause=if_else(Cause=="Chronic liver disease", Cause, "Alcohol dependence"))

ggplot(scotdata, aes(x=Date, y=Deaths, colour=Cause))+
  geom_line()+
  scale_y_continuous(limits=c(0,NA))+
  theme_custom()

ggplot(scotdata, aes(x=period, y=Deaths, colour=as.factor(Year), group=as.factor(Year)))+
  geom_line()+
  scale_x_discrete(name="Quarter")+
  scale_y_continuous(limits=c(0,NA))+
  scale_colour_manual(values=c("grey", "grey", "grey", "grey", "orange", 
                               "red", "black"), name="Year")+
  facet_wrap(~Cause)+
  theme_custom()

scotdata_year <- scotdata %>% 
  group_by(Year, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  mutate(flag=if_else(Year>=2019, 1, 0))

agg_png("Outputs/DoDNRS2022.png", units="in", width=10, height=6, res=800)
ggplot(scotdata_year, aes(x=Year, y=Deaths, colour=Cause, group=Cause))+
  geom_line(alpha=0.3)+
  geom_point(data=. %>% filter(Year==2019))+
  geom_line(data=. %>% filter(Year>=2019), arrow=arrow(angle=25, type="closed", 
                                                       length=unit(0.13, "cm")))+
  scale_y_continuous(limits=c(0,NA))+
  scale_colour_manual(values=c("#E69F00", "#009E73"))+
  theme_custom()
dev.off()