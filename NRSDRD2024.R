rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(readxl)
library(ragg)
library(cowplot)
library(extrafont)
library(ggtext)
library(ggrepel)
library(scales)
library(readODS)
library(geomtextpath)
library(paletteer)
library(gtools)

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


#Download NRS data on drug-related deaths in Scotland
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/media/ht1lxdad/drug-related-deaths-data.xlsx"
rawdata <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in DRD rates by age
DRD.age.rate <- read_excel(rawdata, sheet="Table_5", range="A6:W81") %>% 
  select(-3) %>% 
  set_names("Year", "Sex", "0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
            "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+") %>% 
  gather(age, deaths, c(3:22)) %>% 
  mutate(age=factor(age, levels=c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))

#Plot of DRD rates by age
#generate inset key
Inset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:24, 24), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8,12,10,8,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1:24), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8,12,10,8)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="grey30")+
  theme_custom()+
  theme(axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank(), axis.line=element_blank())

Plot1 <- ggplot(DRD.age.rate %>% filter(Sex=="Persons"), aes(x=Year, y=deaths))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_area(fill="skyblue")+
  geom_line(arrow=arrow(angle=25, type="closed", 
                        length=unit(0.13, "cm")))+
  scale_x_continuous(name="Age group")+
  scale_y_continuous(name="Drug-related deaths per 100,000")+
  facet_wrap(~age, nrow=1, strip.position="bottom")+
  theme_custom()+
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  labs(title="Drug deaths in Scotland have fallen sharply in people in their 40s",
       subtitle="Age-specific rates of drug-related deaths in Scotland 2000-2024\n",
       caption="Data from National Records of Scotland\nPlot by @VictimOfMaths")

agg_png("Outputs/ScotlandDRDxAge.png", units="in", width=12, height=6, res=500)
ggdraw()+
  draw_plot(Plot1)+
  draw_plot(Inset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2000", x=0.16, y=0.65, size=10, fontfamily="Lato", colour="grey30")+
  draw_label("2024", x=0.27, y=0.65, size=10, fontfamily="Lato", colour="grey30")+
  draw_label("Key", x=0.16, y=0.87, size=10, fontface="bold", fontfamily="Lato", colour="grey30")

dev.off()

#Read in DRD counts by age
DRD.age.count <- read_excel(rawdata, sheet="Table_4", range="A5:W80") %>% 
  select(-3) %>% 
  set_names("Year", "Sex", "0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
            "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+") %>% 
  gather(age, deaths, c(3:22)) %>% 
  mutate(age=case_when(
    age %in% c("0", "1-4", "5-9", "10-14") ~ "<15",
    age %in% c("70-74", "75-79", "80-84", "85-89", "90+") ~ "70+",
    TRUE ~ age)) %>% 
  group_by(age, Year, Sex) %>% 
  summarise(deaths=sum(deaths), .,groups="drop") %>% 
  mutate(age=factor(age, levels=c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                                  "45-49", "50-54", "55-59", "60-64", "65-69", "70+")))

#Plot proportions by age group
agg_png("Outputs/ScotlandDRDxAgeProp.png", units="in", width=9, height=6, res=500)
ggplot(DRD.age.count %>% filter(Sex=="Persons"), aes(x=Year, y=deaths, fill=age))+
  geom_col(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of drug-related deaths", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("ggthemes::Classic_Cyclic", name="Age")+
  theme_custom()+
  theme(axis.line=element_blank())+
  labs(title="The age distribution of drug deaths in Scotland has changed dramatically",
       subtitle="Proportion of drug-related deaths by age group 2000-2024\n",
       caption="Data fron National Records of Scotland\nPlot by @Victim Of Maths")

dev.off()

#Read in age-standardised DRD rates by sex
DRD.sex.AS <- read_excel(rawdata, sheet="Table_1", range="A6:J85") %>% 
  mutate(across(c(4:10), ~as.numeric(.x)))

lab.f <- DRD.sex.AS$`Age-standardised rate (per 100,000 population)`[DRD.sex.AS$Sex=="Females" & DRD.sex.AS$Year==2024]/DRD.sex.AS$`Age-standardised rate (per 100,000 population)`[DRD.sex.AS$Sex=="Females" & DRD.sex.AS$Year==2000]-1
lab.m <- DRD.sex.AS$`Age-standardised rate (per 100,000 population)`[DRD.sex.AS$Sex=="Males" & DRD.sex.AS$Year==2024]/DRD.sex.AS$`Age-standardised rate (per 100,000 population)`[DRD.sex.AS$Sex=="Males" & DRD.sex.AS$Year==2000]-1

lab.f <- paste0("+",round(lab.f*100,0),"%")
lab.m <- paste0("+",round(lab.m*100,0),"%")

agg_png("Outputs/ScotlandDRDxSex.png", units="in", width=10, height=6, res=500)
ggplot(DRD.sex.AS %>% filter(Sex!="Persons"))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_ribbon(aes(ymin=`Lower 95% confidence interval`, ymax=`Upper 95% confidence interval`,
                  x=Year, fill=Sex), alpha=0.2)+
  geom_textline(aes(x=Year, y=`Age-standardised rate (per 100,000 population)`, colour=Sex, label=Sex), 
                show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual drug-related deaths", limits=c(0,NA))+
  scale_colour_manual(name="", values=c("#00cc99", "#6600cc"))+
  scale_fill_manual(name="", values=c("#00cc99", "#6600cc"))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Women have seen a bigger relative increase in drug-related deaths in Scotland",
       subtitle="Since 2000, age-standardised rates of drug-related deaths <span style='color:#00cc99;'>in women</span> have increased sixfold, while they have tripled <span style='color:#6600cc;'>in men</span><br>",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")+
  annotate("text", x=2024, y=35, label=lab.m, colour="#6600cc", family="Lato")+
  annotate("text", x=2024, y=16, label=lab.f, colour="#00cc99", family="Lato")+
  theme(axis.line.x=element_blank())

dev.off()

#DRDs by drug type
DRD.drg <- read_excel(rawdata, sheet="Table_3", range="A7:T35", col_names=FALSE) %>% 
  gather(drug, deaths, c(3:20)) %>% 
  rename(Year=`...1`, Total=`...2`) %>% 
  mutate(drug=case_when(
    #drug=="...3" ~ "Any opiate/opioid",
    drug=="...4" ~ "Heroin/morphine",
    drug=="...5" ~ "Methadone",
    drug=="...6" ~ "Bupenorphine",
    drug %in% c("...7", "...8") ~ "Codeine/Dihydrocodeine",
    drug=="...9" ~ "Nitazenes",
    #drug=="...10" ~ "Fentanyls,
    #drug=="...11" ~ "Any benzodiazepine",
    drug=="...12" ~ "'Prescribable' benzodiazepine",
    #drug=="...13" ~ "Diazepam",
    drug=="...14" ~ "'Street' benzodiazepine",
    #drug=="...15" ~ "Etizolam",
    #drug=="...16" ~ "Bromazolam",
    drug=="...17" ~ "Gabapentin/Pregabalin",
    drug=="...18" ~ "Cocaine",
    #drug=="...19" ~ "Ecstasy",
    #drug=="...20" ~ "Amphetamines",
    #drug=="...21" ~ "Alcohol",
  ),
  deaths=as.numeric(deaths),
  Year=as.numeric(if_else(Year=="2008 [b]", "2008", Year))) %>% 
  group_by(Year, drug, Total) %>% 
  summarise(deaths=sum(deaths), .groups="drop") %>% 
  filter(!is.na(drug)) %>% 
  mutate(deathprop=deaths/Total,
         drug=factor(drug, levels=c("Heroin/morphine", "Methadone", "'Prescribable' benzodiazepine",
                                    "'Street' benzodiazepine", "Codeine/Dihydrocodeine", 
                                    "Gabapentin/Pregabalin", "Cocaine", "Bupenorphine", "Nitazenes")))

#Plot of totals
agg_png("Outputs/ScotlandDRDxDrugAbs.png", units="in", width=9, height=6, res=500)
ggplot()+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line(data=DRD.drg, aes(x=Year, y=deaths, colour=drug, group=drug), show.legend=FALSE)+
  geom_text_repel(data=DRD.drg %>% filter(Year==2024),aes(color=drug, label=drug, x=Year, y=deaths),
                  show.legend=FALSE, family="Lato", xlim=(c(2024.1, NA)), segment.color = NA)+
  scale_x_continuous(name="", limits=c(2008, 2026))+
  scale_y_continuous(name="Deaths reported as involving...")+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  coord_cartesian(clip = 'off') +
  theme_custom()+
  theme(plot.margin = unit(c(1,10,1,1), "lines"),
        plot.subtitle=element_markdown(),
        axis.line.x=element_blank())+
  labs(title="Drug deaths in 2024 fell for most substances",
       subtitle="Deaths involving <span style='color:#009F3F;'>cocaine</span> were stable, and <span style='color:#AF6125;'>nitazine</span> deaths rose, but deaths involving all other major drugs fell<br>",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

#Plot of proportions
agg_png("Outputs/ScotlandDRDxDrugProp.png", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line(data=DRD.drg, aes(x=Year, y=deathprop, colour=drug), show.legend=FALSE)+
  geom_text_repel(data=DRD.drg %>% filter(Year==2024),aes(color=drug, label=drug, x=Year, y=deathprop),
                  show.legend=FALSE, family="Lato", xlim=(c(2024.1, NA)), segment.color = NA)+
  scale_x_continuous(name="", limits=c(2008, 2024))+
  scale_y_continuous(name="Proportion of all drug-related deaths which involve...",
                     labels = scales::percent_format(accuracy = 2))+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  coord_cartesian(clip = 'off') +
  theme_custom()+
  theme(plot.margin = unit(c(1,10,1,1), "lines"),
        plot.title=element_markdown(),
        plot.subtitle=element_markdown(),
        axis.line.x=element_blank())+
  labs(title="Deaths involving <span style='color:#009F3F;'>cocaine</span> overtook <span style='color:#54BCD1;'>'street benzos'</span> in 2024",
       subtitle="The proportion of drug-related deaths involving most other substances fell",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

#And by age
DRD.drg.age <- read_excel(rawdata, sheet="Table_7", range="A7:V70", col_names=FALSE) %>% 
  gather(age, deaths, c(4:22)) %>% 
  rename(drug=`...1`, Sex=`...2`) %>% 
  merge(bind_rows(read_excel(rawdata, sheet="Table_7", range="A6:V6", col_names=FALSE) %>% 
                    gather(age, deaths, c(4:22)) %>% 
                    select(c(4:5)) %>% 
                    set_names("age", "Total") %>% mutate(Sex="Persons"),
                  read_excel(rawdata, sheet="Table_7", range="A28:V28", col_names=FALSE) %>% 
                    gather(age, deaths, c(4:22)) %>% 
                    select(c(4:5)) %>% 
                    set_names("age", "Total") %>% mutate(Sex="Females"),
                  read_excel(rawdata, sheet="Table_7", range="A50:V50", col_names=FALSE) %>% 
                    gather(age, deaths, c(4:22)) %>% 
                    select(c(4:5)) %>% 
                    set_names("age", "Total") %>% mutate(Sex="Males"))) %>% 
  mutate(age=as.numeric(substr(age, 4, 5)),
         age=case_when(
           age<10 ~ "Under 25",
           age<12 ~ "25-34",
           age<14 ~ "35-44",
           age<16 ~ "45-54",
           TRUE ~ "55 and over"),
         drug=case_when(
           drug=="Heroin / morphine [note 6]" ~ "Heroin/morphine",
           drug=="Methadone" ~ "Methadone",
           drug=="Bupenorphine" ~ "Bupenorphine",
           drug %in% c("Codeine or a codeine-containing compound", ".Dihydrocodeine or a d.h.c-containing compound") ~ "Codeine/Dihydrocodeine",
           drug=="Any prescribable benzodiazepine [note 7]" ~ "'Prescribable' benzodiazepine",
           drug=="Any street benzodiazepine [note 7]" ~ "'Street' benzodiazepine",
           drug=="Gabapentin and/or Pregabalin" ~ "Gabapentin/Pregabalin",
           drug=="Cocaine" ~ "Cocaine")) %>% 
  filter(!is.na(drug)) %>% 
  group_by(age, drug, Sex) %>% 
  summarise(deaths=sum(deaths), Total=sum(Total), .groups="drop") %>% 
  mutate(deathprop=deaths/Total,
         drug=factor(drug, levels=c("Heroin/morphine", "Methadone", "'Prescribable' benzodiazepine",
                                    "'Street' benzodiazepine", "Codeine/Dihydrocodeine",
                                    "Gabapentin/Pregabalin", "Cocaine", "Bupenorphine")),
         age=factor(age, levels=c("Under 25", "25-34", "35-44", "45-54", "55 and over"))) %>% 
  group_by(drug, Sex) %>% 
  mutate(drugtot=sum(deaths)) %>% 
  ungroup() %>% 
  mutate(deathprop2=deaths/drugtot)

agg_png("Outputs/ScotlandDRDxAgexDrugProp.png", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_textline(data=DRD.drg.age %>% filter(Sex=="Persons"), aes(x=age, y=deathprop, group=drug, colour=drug, label=drug), show.legend=FALSE)+
  geom_hline(yintercept=0, colour="grey20")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Proportion of all drug-related deaths which involve...",
                     labels = scales::percent_format(accuracy = 2))+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  coord_cartesian(clip = 'off') +
  theme_custom()+
  theme(plot.margin = unit(c(1,10,1,1), "lines"),
        plot.title=element_markdown())+
  labs(title="<span style='color:#009F3F;'>Cocaine </span> is implicated in a greater proportion of deaths in younger age groups",
       subtitle="Other drugs are more likely to be involved in deaths at older ages",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

ggplot(DRD.drg.age %>% filter(Sex=="Persons"), aes(x=deathprop2, y=drug, fill=age))+
  geom_col()+
  theme_custom()+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="Age")+
  scale_x_continuous(name="Proportion of all drug-related deaths involving...",label=label_percent(accuracy=1))

agg_png("Outputs/ScotlandDRDxAgexDrugProp2.png", units="in", width=9, height=6.6, res=500)
ggplot(DRD.drg.age %>% filter(Sex=="Persons"), aes(x=deathprop, y=fct_rev(drug), fill=drug))+
  geom_col(position="dodge", show.legend=FALSE)+
  theme_custom()+
  scale_fill_paletteer_d("LaCroixColoR::paired")+
  scale_x_continuous(name="Proportion of all drug-related deaths involving...",
                     label=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  facet_wrap(~age)+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="The substances involved in drug deaths varies a lot with age",
       subtitle="In the under 45s, <span style='color:#009F3F;'>cocaine</span> is involved in more deaths than any other drug.<br><span style='color:#54BCD1;'>'Street benzos'</span> are a factor in many drug-related deaths across all ages.<br>At older ages, <span style='color:#F4B95A;'>gabapentin/pregablin</span> and <span style='color:#FC6882;'>methadone</span> are also significant contributors.<br>Deaths can (and often do) involve multiple drugs, so proportions within each age group sum to more than one.<br>",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

####################################################################
#DRDs in Scotland by ICD-10 codes
DRD.cause <- read_excel(rawdata, sheet="Table_2", range="C22:E35", col_names=FALSE) %>% 
  mutate(year=c(2011:2024)) %>% 
  gather(cause, deaths, c(1:3)) %>% 
  mutate(cause=case_when(
    cause=="...1" ~ "Drug abuse",
    cause=="...2" ~ "Accidental poisoning",
    TRUE ~ "Intentional self-poisoning"))

agg_png("Outputs/DRDScotxCause.png", units="in", width=9, height=6.6, res=500)
ggplot(DRD.cause)+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line(aes(x=year, y=deaths, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(2011:2024))+
  scale_y_continuous(name="Annual drug-related deaths")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  theme(plot.subtitle=element_markdown(), axis.line.x=element_blank())+
  labs(title="The rise in drug-related deaths is entirely driven by accidental overdoses",
       subtitle="Drug-related deaths in Scotland from <span style='color:#E69F00;'>accidental poisoning</span>, <span style='color:#56B4E9;'>drug abuse</span> and <span style='color:#009E73;'>intentional self-poisoning",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

#################################################################################
#DRDs in Scotland by deprivation
DRD.SIMD <- read_excel(rawdata, sheet="Table_10", range="A6:F270") %>% 
  mutate(Area=case_when(
    Area=="SIMD Decile 1 (most deprived)" ~ "SIMD10",
    Area=="SIMD Decile 2" ~ "SIMD9", Area=="SIMD Decile 3" ~ "SIMD8",
    Area=="SIMD Decile 4" ~ "SIMD7", Area=="SIMD Decile 5" ~ "SIMD6",
    Area=="SIMD Decile 6" ~ "SIMD5", Area=="SIMD Decile 7" ~ "SIMD4",
    Area=="SIMD Decile 8" ~ "SIMD3", Area=="SIMD Decile 9" ~ "SIMD2",
    Area=="SIMD Decile 10 (least deprived)" ~ "SIMD1",
    TRUE~"Scotland"),
    SIMD=factor(Area, levels=c("SIMD10", "SIMD9", "SIMD8", "SIMD7", "SIMD6", "SIMD5", "SIMD4", "SIMD3",
                               "SIMD2", "SIMD1", "Scotland")),
    label=case_when(
      SIMD=="SIMD10" ~ "Most deprived decile",
      SIMD=="SIMD1" ~ "Least deprived decile",
      TRUE ~ "")) %>% 
    rename("DRD.rate"="Age-standardised mortality rate") %>% 
  mutate(DRD.rate=as.numeric(DRD.rate),
         `Lower 95% confidence interval`=as.numeric(`Lower 95% confidence interval`),
         `Upper 95% confidence interval`=as.numeric(`Upper 95% confidence interval`))

agg_png("Outputs/ScotlandDRDxSIMD.png", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line(data=DRD.SIMD %>% filter(Area!="Scotland"), aes(x=Year, y=DRD.rate, group=SIMD, colour=SIMD), show.legend=FALSE)+
  geom_text_repel(data=DRD.SIMD %>% filter(Year==2024 & Area!="Scotland"), aes(x=Year, y=DRD.rate, label=label, colour=SIMD),
                  family="Lato", xlim=(c(2024.1, NA)), segment.color = NA, show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised rate of drug-related deaths per 100,000")+
  scale_colour_manual(values=c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb",
                               "#fee0b6", "#fdb863", "#e08214", "#b35806", "#7f3b08"))+
  theme_custom()+
  coord_cartesian(clip = 'off')+
  theme(plot.margin = unit(c(1,10,1,1), "lines"), axis.line.x=element_blank())+
  labs(title="Drug-related deaths in Scotland are *incredibly* unequal",
       subtitle="Age-standardised rates of drug-related deaths by decile of the Scottish Index of Multiple Deprivation.\nValues based on fewer than 10 deaths are censored.",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()  

#Plot of population-level figures
agg_png("Outputs/ScotlandDRDTotal.png", units="in", width=9, height=6.6, res=500)
DRD.SIMD %>% filter(Area=="Scotland") %>% 
  ggplot(aes(x=Year, y=DRD.rate))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_ribbon(aes(ymin=`Lower 95% confidence interval`, ymax=`Upper 95% confidence interval`),
              fill="tomato", alpha=0.2)+
  geom_line(colour="tomato")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Drug-related deaths per 100,000")+
  labs(title="Drug-related deaths in Scotland fell in 2024",
       subtitle="Age-standardised rates of drug misuse deaths in Scotland 2001-2024",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")+
  theme_custom()

dev.off()

#################################################################################
#DRDs in Scotland by HB
DRD.HB <- read_excel(rawdata, sheet="Table_HB1", range="A5:Q20") %>% 
  gather(HB, deaths, c(4:17)) 

#Bring in populations
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/media/lchj5a15/mid-year-population-estimates-time-series-data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

HBpop <- read_excel(temp, sheet="Table 2", range="B7:E1986", col_names=FALSE) %>% 
  set_names("HB", "sex", "Year", "pop") %>% 
  filter(sex=="Persons" & Year>=2010)

#Recycle 2022 populations for 2023
DRD.HB <- merge(DRD.HB, HBpop) %>% 
  mutate(mortrate=deaths*100000/pop)

agg_png("Outputs/ScotlandDRDxHB.png", units="in", width=9, height=6.6, res=500)
ggplot(DRD.HB %>% filter(HB!="Scotland"))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line(aes(x=Year, y=mortrate, colour=HB), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(2010:2024))+
  scale_y_continuous(name="Drug-related deaths per 100,000")+
  scale_colour_manual(values=c(rep("Grey70", 6), "#c51b8a", rep("Grey70", 7)))+
  theme_custom()+
  theme(plot.subtitle=element_markdown(),
        axis.line.x=element_blank())+
  labs(title="Scotland's drug death epidemic is centred on Glasgow",
       subtitle="Drug misuse death rates in <span style='color:#c51b8a;'>Greater Glasgow & Clyde</span> compared to <span style='color:Grey70;'>other Health Board areas",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

#DRDs in Scotland by HB and drug
DRD.HB.drg <- read_excel(rawdata, sheet="Table_HB3", range="A7:T20", col_names=FALSE) %>% 
  gather(drug, deaths, c(3:20)) %>% 
  rename(HB="...1", total="...2") %>% 
  filter(total>=20) %>% 
  mutate(drug=case_when(
    drug=="...4" ~ "Heroin/morphine",
    drug=="...5" ~ "Methadone",
    drug=="...6" ~ "Bupenorphine",
    drug %in% c("...7", "...8") ~ "Codeine/Dihydrocodeine",
    drug=="...11" ~ "'Prescribable' benzodiazepine",
    drug=="...13" ~ "'Street' benzodiazepine",
    drug=="...16" ~ "Gabapentin/Pregabalin",
    drug=="...17" ~ "Cocaine")) %>% 
  group_by(HB, drug, total) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  filter(!is.na(drug)) %>% 
  mutate(deathprop=deaths/total,
         drug=factor(drug, levels=c("Heroin/morphine", "Methadone", "'Prescribable' benzodiazepine",
                                    "'Street' benzodiazepine", "Codeine/Dihydrocodeine",
                                    "Gabapentin/Pregabalin", "Cocaine", "Bupenorphine")))

agg_png("Outputs/ScotlandDRDxHBxdrug.png", units="in", width=10, height=8, res=500)
ggplot(DRD.HB.drg)+
  geom_col(aes(x=deathprop, y=fct_rev(HB), fill=HB), show.legend=FALSE)+
  scale_x_continuous(name="Proportion of drug-related deaths involving...", 
                     labels=scales::percent_format(accuracy=2))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c(rep("Grey70", 3), "#c51b8a", rep("Grey70", 6)))+
  facet_wrap(~drug)+
  theme_custom()+
  labs(title="There's something different about Grampian",
       subtitle="A larger proportion of deaths there are linked to cocaine or 'prescribable' benzodiazepine and a much smaller proportion to 'street' diazepine",
       caption="\n\nHealth boards with fewer than 20 deaths are excluded\nData from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()

#Comparing Scotland to rUK
DRD.UK <- read_excel(rawdata, sheet="Table_12", range="A7:E20") %>% 
  mutate(Area=if_else(Area=="England", "England average", Area)) %>% 
  arrange(`Age-standardised mortality rate`) %>% 
  mutate(Area=fct_inorder(Area))

agg_png("Outputs/ScotlandDRDvsrUK.png", units="in", width=9, height=6.6, res=500)
ggplot(DRD.UK, aes(x=`Age-standardised mortality rate`, y=Area, fill=Area))+
  geom_col()+
  geom_vline(xintercept=0, colour="grey20")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c(rep("Grey70", 12), "#c51b8a"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(colour="grey95"))+
  guides(fill="none")+
  labs(title="Drug deaths in Scotland remain well above the rest of the UK",
       subtitle="Age-standardised rate of deaths from drug poisoning in 2023 by UK region\n",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()
