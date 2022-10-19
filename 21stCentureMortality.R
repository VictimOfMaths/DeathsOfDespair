rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
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

#HMD credentials here
username <- "" 
password <- ""

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
ewpop <- readHMDweb(CNTRY="GBRTENW", "Population5", username, password, fixup=FALSE) %>% 
  filter(Year>=2001) %>% 
  mutate(Age=case_when(
    Age %in% c("0", "1-4", "5-9", "10-14", "15-19") ~ "Under 20",
    Age %in% c("85-89", "90-94", "95-99", "100-104", "105-109", "110+") ~ "85+",
    TRUE ~ Age)) %>% 
  group_by(Year, Age) %>% 
  summarise(Male=sum(Male), Female=sum(Female), .groups="drop") %>% 
  gather(Sex, Pop, c(Male, Female))
         
ewdata_short <- ewdata %>% 
  mutate(Age=case_when(
    Age %in% c("Neonates", "<1", "01-04", "05-09", "10-14", "15-19") ~ "Under 20",
    TRUE ~ Age)) %>% 
  group_by(Cause, Year, Sex, Age) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  merge(ewpop)%>% 
  mutate(Age=factor(Age, levels=c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                  "85+")))

png("Outputs/ASDEW21stC22.png", units="in", width=8, height=6, res=600)
ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  group_by(Year) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  ggplot(aes(x=Year, y=Deaths))+
  geom_line(colour="Tomato")+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA))+
  theme_custom()+
  labs(title="Provisional data suggests a further rise in alcohol deaths in 2021",
       subtitle="Alcohol-specific deaths in England & Wales 2001-2021",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

png("Outputs/ASDEW21stC22xSex.png", units="in", width=8, height=6, res=600)
ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  group_by(Year, Sex) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  ggplot(aes(x=Year, y=Deaths, colour=Sex, label=Sex))+
  geom_textline(show.legend=FALSE, hjust=0.6)+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="")+
  theme_custom()+
  labs(title="Provisional data suggests a further rise in alcohol deaths in 2021",
       subtitle="Alcohol-specific deaths in England & Wales 2001-2021",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

#Lexis surface
png("Outputs/ASDEW21stC22xAgeLexis.png", units="in", width=8, height=6, res=600)
ewdata_short %>% 
  filter(Cause=="ASD" & Age!="Under 20") %>% 
  group_by(Age,Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop) %>% 
  ggplot(aes(x=Year, y=Age, fill=mortrate))+
  geom_tile()+
  scale_fill_paletteer_c("viridis::turbo", limits=c(0,NA), name="Deaths\nper 100,000")+
  coord_equal()+
  theme_custom()+
  labs(title="The pandemic has had a significant impact on alcohol-specific deaths",
       subtitle="Rates of alcohol-specific mortality by age for England & Wales 2001-2021",
       caption="Mortality data from ONS, population data from mortality.org\nPlot by @VictimOfMaths")

dev.off()

#Age-specific lines - simple version
png("Outputs/ASDEW21stC22xAge.png", units="in", width=12, height=6, res=600)
ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  group_by(Age,Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop) %>% 
  ggplot(aes(x=Year, y=mortrate))+
  geom_area(fill="SkyBlue")+
  geom_path(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())

dev.off()

#Age-specific lines - picking out 2021 changes
plotdata <- ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  group_by(Age,Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop)

ASDplot <- ggplot()+
  geom_area(data=plotdata, aes(x=Year, y=mortrate), fill="SkyBlue")+
  geom_path(data=plotdata %>% filter(Year<2021), aes(x=Year, y=mortrate), colour="Grey40", 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
    geom_path(data=plotdata %>% filter(Year>=2020), aes(x=Year, y=mortrate), colour="Tomato", 
              arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
        plot.subtitle=element_markdown())+
  labs(title="Alcohol-specific deaths rose in 2021 in later middle-aged drinkers",
       subtitle="Rates of death from causes attributable only to alcohol by age between 2001 and 2021. Changes in 2021 <span style='color:Tomato;'>shown in red.",
       caption="Mortality data from ONS, population data from mortality.org\nPlot by @VictimOfMaths")

ASDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,14,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1:20), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Grey40")+
  geom_line(aes(x=c(20,21), y=c(10,14)), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), 
            colour="Tomato")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDfull <- ggdraw()+
  draw_plot(ASDplot)+
  draw_plot(ASDinset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10, colour="Grey40")+
  draw_label("2020", x=0.26, y=0.66, size=10, colour="Grey40")+
  draw_label("2021", x=0.27, y=0.8, size=10, colour="Tomato")+
  draw_label("Key", x=0.17, y=0.85, size=11, fontface="bold")

png("Outputs/ASDEW21stC22xAgev2.png", units="in", width=12, height=6, res=600)
ggdraw(ASDfull)
dev.off()

#Age-standardisation
frame <- data.frame(Age=rep(c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                          "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                          "85+"), times=21*2), 
                    weight=rep(c(1000+4000+5500+5500+5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 
                             6500, 6000, 5500, 5000, 4000, 2500, 1500+800+200), times=21*2),
                    Sex=rep(c("Male", "Female"), each=15, times=21),
                    Year=rep(c(2001:2021), each=30)) %>% 
  merge(ewpop)

ASdata_sex <- ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  select(-c(Pop, Cause)) %>% 
  merge(frame, all=T) %>% 
  mutate(Deaths=replace_na(Deaths, 0), mortrate=Deaths*100000/Pop) %>% 
  group_by(Year, Sex) %>% 
  summarise(ASDeaths=sum(mortrate*weight)/sum(weight), .groups="drop")

png("Outputs/ASDEW21stC22xSexRate.png", units="in", width=8, height=6, res=600)
ggplot(data=ASdata_sex, aes(x=Year, y=ASDeaths, colour=Sex, label=Sex))+
  geom_textline(show.legend=FALSE, hjust=0.6)+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA), name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="")+
  theme_custom()+
  labs(title="Alcohol-specific deaths rose again for men and women in 2021",
       subtitle="Age-standardised mortality rates from cases that are wholly attributable to alcohol in England & Wales 2001-2021",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

ASdata_all <- ewdata_short %>% 
  filter(Cause=="ASD") %>% 
  select(-c(Pop, Cause)) %>% 
  merge(frame, all=T) %>% 
  mutate(Deaths=replace_na(Deaths, 0)) %>% 
  group_by(Year, Age) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), weight=sum(weight)/2, .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop) %>% 
  group_by(Year) %>% 
  summarise(ASDeaths=sum(mortrate*weight)/sum(weight), .groups="drop") %>% 
  mutate(abschange=ASDeaths-lag(ASDeaths, 1), relchange=abschange/lag(ASDeaths, 1),
         label=paste0(if_else(relchange<0, "-", "+"), round(abs(relchange)*100, 1), "%"),
         label=if_else(is.na(relchange), "", label))

tiff("Outputs/ASDEW21stC22.tiff", units="in", width=8, height=6, res=600)
ggplot(ASdata_all, aes(x=Year, y=ASDeaths))+
  geom_line(colour="Tomato")+
  geom_text(aes(label=label, vjust=c(0,0.8,1,1,1,1,0.8,-0.2,1,0.8,-0.2,1,0.8,-0.5,0.8,0.8,-0.5,1,
                                     0.8,0.8,0.8)), size=rel(2.5), colour="Grey40", hjust=-0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA), name="Age-standardised deaths per 100,000")+
  theme_custom()+
  labs(title="Alcohol-specific deaths rose again in 2021",
       subtitle="Age-standardised mortality rates from cases that are wholly attributable to alcohol in England & Wales 2001-2021",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

#By condition
tiff("Outputs/ASDEW21stC22xCause.tiff", units="in", width=8, height=6, res=600)
ewdata %>% 
  filter(Cause=="ASD") %>% 
  mutate(Condition=case_when(
    substr(ICD10,1,3)=="F10" ~ "Dependence-related",
    substr(ICD10, 1, 3) %in% c("X45", "Y65", "Y15") ~ "Poisoning",
    substr(ICD10,1,3)=="K70" ~ "Alcoholic liver disease",
    TRUE ~ "Other")) %>% 
  group_by(Condition, Year) %>% 
  summarise(Deaths=sum(Deaths)) %>% 
  mutate(Condition=factor(Condition, levels=c("Alcoholic liver disease", "Dependence-related",
                                              "Poisoning", "Other"))) %>% 
  ggplot(aes(x=Year, y=Deaths, fill=Condition))+
  geom_col(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of alcohol-specific deaths", 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("unikn::pal_unikn_pref", name="")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The vast majority of alcohol-specific deaths are from alcoholic liver disease",
       subtitle="Alcohol-specific deaths in England & Wales by underlying cause",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

###########################################
#Drug poisoning deaths

#Lexis surface
png("Outputs/DRDEW21stC22xAgeLexis.png", units="in", width=8, height=6, res=600)
ewdata_short %>% 
  filter(Cause=="DRD") %>% 
  group_by(Age,Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop) %>% 
  ggplot(aes(x=Year, y=Age, fill=mortrate))+
  geom_tile()+
  scale_fill_paletteer_c("viridis::turbo", limits=c(0,NA), name="Deaths\nper 100,000")+
  coord_equal()+
  theme_custom()+
  labs(title="Drug poisoning deaths have risen during the pandemic",
       subtitle="Rates of drug poisoning deaths by age for England & Wales 2001-2021",
       caption="Mortality data from ONS, population data from mortality.org\nPlot by @VictimOfMaths")

dev.off()

#Age-specific lines - simple version
png("Outputs/DRDEW21stC22xAge.png", units="in", width=12, height=6, res=600)
ewdata_short %>% 
  filter(Cause=="DRD") %>% 
  group_by(Age,Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop) %>% 
  ggplot(aes(x=Year, y=mortrate))+
  geom_area(fill="SkyBlue")+
  geom_path(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())

dev.off()

#Age-specific lines - picking out 2021 changes
plotdata2 <- ewdata_short %>% 
  filter(Cause=="DRD") %>% 
  group_by(Age,Year) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop)

DRDplot <- ggplot()+
  geom_area(data=plotdata2, aes(x=Year, y=mortrate), fill="Tomato")+
  geom_path(data=plotdata2 %>% filter(Year<2021), aes(x=Year, y=mortrate), colour="Grey40", 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  geom_path(data=plotdata2 %>% filter(Year>=2020), aes(x=Year, y=mortrate), colour="#2c7fb8", 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
        plot.subtitle=element_markdown())+
  labs(title="Alcohol-specific deaths rose in 2021 in later middle-aged drinkers",
       subtitle="Rates of death from causes attributable only to alcohol by age between 2001 and 2021. Changes in 2021 <span style='color:Tomato;'>shown in red.",
       caption="Mortality data from ONS, population data from mortality.org\nPlot by @VictimOfMaths")

DRDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,14,0)), 
               fill="Tomato")+
  geom_line(aes(x=c(1:20), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Grey40")+
  geom_line(aes(x=c(20,21), y=c(10,14)), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), 
            colour="#2c7fb8")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

DRDfull <- ggdraw()+
  draw_plot(DRDplot)+
  draw_plot(DRDinset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10, colour="Grey40")+
  draw_label("2020", x=0.26, y=0.66, size=10, colour="Grey40")+
  draw_label("2021", x=0.27, y=0.8, size=10, colour="#2c7fb8")+
  draw_label("Key", x=0.17, y=0.85, size=11, fontface="bold")

png("Outputs/DRDEW21stC22xAgev2.png", units="in", width=12, height=6, res=600)
ggdraw(DRDfull)
dev.off()

#Age-standardisation
ASdataD_sex <- ewdata_short %>% 
  filter(Cause=="DRD") %>% 
  select(-c(Pop, Cause)) %>% 
  merge(frame, all=T) %>% 
  mutate(Deaths=replace_na(Deaths, 0), mortrate=Deaths*100000/Pop) %>% 
  group_by(Year, Sex) %>% 
  summarise(ASDeaths=sum(mortrate*weight)/sum(weight), .groups="drop")

png("Outputs/DRDEW21stC22xSexRate.png", units="in", width=8, height=6, res=600)
ggplot(data=ASdataD_sex, aes(x=Year, y=ASDeaths, colour=Sex, label=Sex))+
  geom_textline(show.legend=FALSE, hjust=0.6)+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA), name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="")+
  theme_custom()+
  labs(title="Drug poisoning deaths rose again for men and women in 2021",
       subtitle="Age-standardised mortality rates from drug poisoning in England & Wales 2001-2021",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

ASdataD_all <- ewdata_short %>% 
  filter(Cause=="DRD") %>% 
  select(-c(Pop, Cause)) %>% 
  merge(frame, all=T) %>% 
  mutate(Deaths=replace_na(Deaths, 0)) %>% 
  group_by(Year, Age) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), weight=sum(weight)/2, .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop) %>% 
  group_by(Year) %>% 
  summarise(ASDeaths=sum(mortrate*weight)/sum(weight), .groups="drop") %>% 
  mutate(abschange=ASDeaths-lag(ASDeaths, 1), relchange=abschange/lag(ASDeaths, 1),
         label=paste0(if_else(relchange<0, "-", "+"), round(abs(relchange)*100, 1), "%"),
         label=if_else(is.na(relchange), "", label))

tiff("Outputs/DRDEW21stC22.tiff", units="in", width=8, height=6, res=600)
ggplot(ASdataD_all, aes(x=Year, y=ASDeaths))+
  geom_line(colour="Tomato")+
  geom_text(aes(label=label, vjust=c(0,0.8,1,1,1,1,0.8,-0.2,1,0.8,-0.2,1,0.8,-0.5,0.8,0.8,-0.5,1,
                                     0.8,0.8,0.8)), size=rel(2.5), colour="Grey40", hjust=-0.1)+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA), name="Age-standardised deaths per 100,000")+
  theme_custom()+
  labs(title="Drug poisoning deaths rose again in 2021",
       subtitle="Age-standardised mortality rates from drug poisoning in England & Wales 2001-2021",
       caption="Data from ONS' 21st Century Mortality dataset | Plot by @VictimOfMaths")

dev.off()

#Combined plot
plotdata3 <- ewdata_short %>% 
  filter(Cause %in% c("ASD", "DRD")) %>% 
  group_by(Age,Year, Cause) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mortrate=Deaths*100000/Pop)

ASDDRDplot <- ggplot()+
  geom_area(data=plotdata3 %>% filter(Cause=="ASD"), aes(x=Year, y=mortrate), fill="SkyBlue",
            alpha=0.5)+
  geom_area(data=plotdata3 %>% filter(Cause=="DRD"), aes(x=Year, y=mortrate), fill="Tomato",
            alpha=0.5)+
  geom_path(data=plotdata3 %>% filter(Cause=="ASD"), aes(x=Year, y=mortrate), colour="#0c2c84",
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  geom_path(data=plotdata3 %>% filter(Cause=="DRD"), aes(x=Year, y=mortrate), colour="#990000",
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
        plot.subtitle=element_markdown())+
  labs(title="Deaths from both alcohol and drugs have risen in recent years",
       subtitle="Rates of death from <span style='color:#0c2c84;'>alcohol-specific causes</span> and <span style='color:#990000;'>drug poisoning</span> in England and Wales by age between 2001 and 2021",
       caption="Mortality data from ONS, population data from mortality.org\nPlot by @VictimOfMaths")

ASDDRDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,14,0)), 
               fill="Grey70")+
  geom_line(aes(x=c(1:21), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,14)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Black")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDDRDfull <- ggdraw()+
  draw_plot(ASDDRDplot)+
  draw_plot(ASDDRDinset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10, colour="Black")+
  draw_label("2021", x=0.27, y=0.66, size=10, colour="Black")+
  draw_label("Key", x=0.17, y=0.85, size=11, fontface="bold")

tiff("Outputs/ASDDRDEW21stC22xAge.tiff", units="in", width=12, height=6, res=600)
ggdraw(ASDDRDfull)
dev.off()




####################################
#Validation of ASD figures using older version of the data
#July 2021 version
temp <- tempfile()
url4 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/previous/v9/21stcenturymortality202005072021135441.xls"
temp <- curl_download(url=url4, destfile=temp, quiet=FALSE, mode="wb")

hist_2020 <- read_excel(temp, sheet="2020", range="A2:E19029") %>% 
  set_names("ICD10", "Year", "Sex", "Age", "Deaths") %>% 
  #Allocate causes to code groups
  mutate(ASD=if_else(ICD10 %in% c("E244", "G312", "G621", "G721", "I426", "K292", "K852", "K860",
                                  "Q860", "R780") | 
                       substr(ICD10,1,3) %in% c("F10", "K70", "X45", "X65", "Y15"), TRUE, FALSE),
         Sex=if_else(Sex==1, "Male", "Female")) %>% 
  na.omit() %>% 
  group_by(ASD, Sex) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")

#Figures match the 2020 official Alcohol-Specific Deaths data *exactly*, which is reassuring
#https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/bulletins/alcoholrelateddeathsintheunitedkingdom/registeredin2020


