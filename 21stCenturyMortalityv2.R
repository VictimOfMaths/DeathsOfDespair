rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(keyring)
library(HMDHFDplus)
library(paletteer)
library(ragg)
library(extrafont)
library(geomtextpath)
library(cowplot)
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

#Read in England & Wales data from
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/21stcmortality.xlsx"
rawfile <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data.01 <- read_excel(rawfile, sheet="1", range="A5:E21265")
data.02 <- read_excel(rawfile, sheet="2", range="A5:E20880")
data.03 <- read_excel(rawfile, sheet="3", range="A5:E21251")
data.04 <- read_excel(rawfile, sheet="4", range="A5:E20959")
data.05 <- read_excel(rawfile, sheet="5", range="A5:E20928")
data.06 <- read_excel(rawfile, sheet="6", range="A5:E20868")
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
data.21 <- read_excel(rawfile, sheet="21", range="A5:E19157")

ewdata <- bind_rows(data.01, data.02, data.03, data.04, data.05, data.06, data.07,
                    data.08, data.09, data.10, data.11, data.12, data.13, data.14, 
                    data.15, data.16, data.17, data.18, data.19, data.20, data.21) %>% 
  set_names("ICD10", "Year", "Sex", "Age", "Deaths") %>% 
  #Allocate causes to code groups
  #ASD definition taken from https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/methodologies/alcoholrelateddeathsintheukqmi#concepts-and-definitions
  mutate(Cause=case_when(
    ICD10 %in% c("E244", "G312", "G621", "G721", "I426", "K292", "K852", "K860", "Q860", "R780") | 
      substr(ICD10,1,3) %in% c("F10", "K70", "X45", "X65", "Y15") ~ "ASD",
    #Drug poisoning definition taken from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/deathsrelatedtodrugpoisoninginenglandandwalesqmi
    substr(ICD10, 1, 3) %in% c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "X40", 
                               "X41", "X42", "X43", "X44", "X60", "X61", "X62", "X63", "X64",
                               "X85", "Y10", "Y11", "Y12", "Y13", "Y14") ~"DRD",
    TRUE ~ "Other"),
    Sex=if_else(Sex==1, "Male", "Female")) %>% 
  na.omit()

#Population data from mortality.org (because ONS don't have a consistent 2001-2021 available
#yet)
ewpop <- readHMDweb(CNTRY="GBRTENW", "Population5", key_list("mortality.org")[1,2], 
                    key_get("mortality.org",  key_list("mortality.org")[1,2]), fixup=FALSE) %>% 
  filter(Year>=2001) %>% 
  mutate(Age=case_when(
    Age %in% c("0", "1-4", "5-9", "10-14", "15-19") ~ "Under 20", 
    Age %in% c("85-89", "90-94", "95-99", "100-104", "105-109", "110+") ~ "85+", TRUE ~ Age)) %>% 
  group_by(Year, Age) %>% 
  summarise(Male=sum(Male), Female=sum(Female), .groups="drop") %>% 
  gather(Sex, Pop, c(Male, Female))

ewdata_short <- ewdata %>% 
  mutate(Age=case_when(
    Age %in% c("Neonates", "<1", "01-04", "05-09", "10-14", "15-19") ~ "Under 20",
    TRUE ~ Age)) %>% 
  group_by(Cause, Year, Sex, Age) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  merge(ewpop) %>% 
  mutate(Age=factor(Age, levels=c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                  "85+")))

plotdata <- ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  mutate(mx=Deaths*100000/Pop)
  
agg_tiff("Outputs/21stCMortalityASDEWLines.tiff", units="in", width=12, height=6, res=600)
ggplot()+
  geom_line(data=plotdata, aes(x=Year, y=mx), colour="#253494", alpha=0.3)+
  geom_line(data=plotdata %>% filter(Year>=2019), aes(x=Year, y=mx), colour="#253494",
            arrow=arrow(angle=25, type="closed", length=unit(0.15, "cm")))+
  geom_point(data=plotdata %>% filter(Year==2019), aes(x=Year, y=mx), colour="#253494")+
  scale_x_continuous(name="Year", breaks=c(2001, 2011, 2021))+
  scale_y_continuous(name="Deaths per 100,000")+
  facet_grid(Sex~Age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown(), axis.text.x=element_text(angle=60, hjust=1, vjust=1))+
  labs(title="Alcohol-specific deaths have risen sharply during the pandemic",
       subtitle="Deaths from causes only attributable to alcohol in England & Wales since 2001. Pandemic years <span style='color:#253494;'>highlighted.<br>",
       caption="Data from ONS | Plot by @VictimOfMaths")
  
dev.off()
