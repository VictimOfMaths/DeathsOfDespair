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
