rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(scales)
library(ggtext)
library(cowplot)

#Download 4 files
#Main Alcohol-specific deaths up to 2019
file.main <- tempfile()
#Additional data for Q1-3 2020
file.2020 <- tempfile()
#Additional liver disease/deprivation data up to 2019
file.extra <- tempfile()
#Additional cause-specific data up to 2019
file.cause <- tempfile()

source.main <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsintheukmaindataset/current/alcoholspecificdeaths2019.xlsx"
source.2020 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/quarterlyalcoholspecificdeathsinenglandandwales/quarter1jantomartoquarter3julytosept2020/2020q1toq3quarterlyalcoholspecificdeaths1.xls"
source.extra <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsintheunitedkingdomsupplementarydatatables/current/supplementaryanalysis201901022021150545.xlsx"
source.cause <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsbysexagegroupandindividualcauseofdeath/current/alcoholspecificdeathsbyindividualcause2019.xlsx"

file.main <- curl_download(url=source.main, destfile=file.main, quiet=FALSE, mode="wb")
file.2020 <- curl_download(url=source.2020, destfile=file.2020, quiet=FALSE, mode="wb")
file.extra <- curl_download(url=source.extra, destfile=file.extra, quiet=FALSE, mode="wb")
file.cause <- curl_download(url=source.cause, destfile=file.cause, quiet=FALSE, mode="wb")

#ASD by deprivation
data.dep <- read_excel(file.extra, sheet="Table 3", range="A6:J50", col_names=FALSE) %>%
  select(-c(3,7)) %>% 
  rename(year=`...1`, IMDQ=`...2`, Mean.F=`...4`, Lower.F=`...5`, Upper.F=`...6`,
         Mean.M=`...8`, Lower.M=`...9`, Upper.M=`...10`)

tiff("Outputs/ASDxIMDEngMale.tiff", units="in", width=8, height=6, res=500)
ggplot(data.dep, aes(x=year))+
  geom_ribbon(aes(ymin=Lower.M, ymax=Upper.M, fill=as.factor(IMDQ)), alpha=0.3)+
  geom_line(aes(y=Mean.M, group=IMDQ, colour=as.factor(IMDQ)))+
  scale_x_continuous(name="", breaks=c(2012, 2014, 2016, 2018, 2020))+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_colour_manual(values=c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0"),
                      name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  scale_fill_manual(values=c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0"),
                    name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Alcohol-specific deaths in men are higher and more unequal than in women",
       subtitle="Age-standardised alcohol-specific mortality rates in England by quintiles of the Index of Multiple Deprivation",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxIMDEngFemale.tiff", units="in", width=8, height=6, res=500)
ggplot(data.dep, aes(x=year))+
  geom_ribbon(aes(ymin=Lower.F, ymax=Upper.F, fill=as.factor(IMDQ)), alpha=0.3)+
  geom_line(aes(y=Mean.F, group=IMDQ, colour=as.factor(IMDQ)))+
  scale_x_continuous(name="", breaks=c(2012, 2014, 2016, 2018, 2020))+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_colour_manual(values=c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0"),
                      name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  scale_fill_manual(values=c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0"),
                    name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Alcohol-specific deaths in women are much higher in more deprived groups",
       subtitle="Age-standardised alcohol-specific mortality rates in England by quintiles of the Index of Multiple Deprivation",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#ASD in 2020
data.2020 <- read_excel(file.2020, sheet="Table 1", range="B23:AC82", col_names=FALSE) %>% 
  select(c(1,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28)) %>% 
  rename(sex=`...1`, year=`...4`, Q1.deaths=`...6`, Q1.rate=`...7`, Q1.lower=`...9`,
         Q1.upper=`...10`, Q2.deaths=`...12`, Q2.rate=`...13`, Q2.lower=`...15`,
         Q2.upper=`...16`, Q3.deaths=`...18`, Q3.rate=`...19`, Q3.lower=`...21`,
         Q3.upper=`...22`, Q4.deaths=`...24`, Q4.rate=`...25`, Q4.lower=`...27`,
         Q4.upper=`...28`) %>% 
  pivot_longer(c(3:18), names_to=c("quarter", "metric"), names_sep="\\.", values_to=c("value")) %>% 
  mutate(index=(year-2001)*4,
         index=index+as.numeric(substr(quarter, 2, 3))-1) %>% 
  spread(metric, value)

tiff("Outputs/ASDxSexWith2020.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data.2020, sex!="Persons"))+
  geom_ribbon(aes(x=index, ymin=lower, ymax=upper, fill=sex), alpha=0.2, show.legend=FALSE)+
  geom_line(aes(x=index, y=rate, colour=sex), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(4, 12, 20, 28, 36, 44, 52, 60, 68, 76),
                     labels=c("2002", "2004", "2006", "2008", "2010", "2012",
                              "2014", "2016", "2018", "2020"))+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="Sex")+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="Sex")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown())+
  labs(title="Alcohol-specific deaths have risen in 2020",
       subtitle="Quarterly age-standardised alcohol-specific mortality rates in England & Wales for <span style='color:#6600cc;'>men </span>and <span style='color:#00cc99;'>women",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Quarterly overlay
tiff("Outputs/ASDxSexWithout2020Quarterly.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_line(data=subset(data.2020, year<2020 & sex=="Persons"), 
            aes(x=quarter, y=rate, group=year, colour=year))+
  geom_point(data=subset(data.2020, year<2020 & sex=="Persons"), 
             aes(x=quarter, y=rate, colour=year))+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_x_discrete(name="Quarter")+
  scale_colour_paletteer_c("scico::vik", name="Year")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Alcohol-specific deaths are highest in October - March",
       subtitle="Quarterly age-standardised alcohol-specific mortality rates in England & Wales in 2001-2019",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxSexWith2020Quarterly.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  #geom_ribbon(data=subset(data.2020, year>=2010 & sex=="Persons"), 
  #            aes(x=quarter, ymin=lower, ymax=upper, group=year), fill="Grey70", alpha=0.3)+
  geom_line(data=subset(data.2020, year>=2010 & sex=="Persons"), 
            aes(x=quarter, y=rate, group=year), colour="Grey70")+
  geom_point(data=subset(data.2020, year>=2010 & sex=="Persons"), 
            aes(x=quarter, y=rate), colour="Grey70")+
  geom_line(data=subset(data.2020, year==2020 & sex=="Persons"), 
            aes(x=quarter, y=rate, group=year), colour="#FF4E86")+
  geom_point(data=subset(data.2020, year==2020 & sex=="Persons"), 
            aes(x=quarter, y=rate), colour="#FF4E86")+
  scale_x_discrete(name="Quarter")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown())+
  labs(title="Alcohol-specific deaths in January-September 2020 were higher than recent years",
       subtitle="Quarterly age-standardised alcohol-specific mortality rates in England & Wales in <span style='color:#FF4E86;'>2020</span>, compared to 2010-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#ASD by age in 2020
data.age <- read_excel(file.2020, sheet="Table 2", range="B24:AB383", col_names=FALSE) %>% 
  select(1:3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27) %>% 
  rename(sex=`...1`, year=`...2`, age=`...3`, Q1.deaths=`...5`, Q1.rate=`...6`,
         Q1.lower=`...8`, Q1.upper=`...9`, Q2.deaths=`...11`, Q2.rate=`...12`,
         Q2.lower=`...14`, Q2.upper=`...15`, Q3.deaths=`...17`, Q3.rate=`...18`,
         Q3.lower=`...20`, Q3.upper=`...21`, Q4.deaths=`...23`, Q4.rate=`...24`,
         Q4.lower=`...26`, Q4.upper=`...27`) %>% 
  mutate(Q1.deaths=as.character(Q1.deaths),
         Q2.deaths=as.character(Q2.deaths),
         Q3.deaths=as.character(Q3.deaths),
         Q4.deaths=as.character(Q4.deaths)) %>% 
  pivot_longer(c(4:19), names_to=c("quarter", "metric"), names_sep="\\.", values_to=c("value")) %>% 
  mutate(value=as.numeric(str_replace(value, ":", "0")),
         age=factor(age, levels=c("Under 20", "20-29", "30-39", "40-49", 
                                  "50-69", "70 and over"))) %>% 
  mutate(index=(year-2001)*4,
         index=index+as.numeric(substr(quarter, 2, 3))-1) %>% 
  spread(metric, value)
  
tiff("Outputs/ASDxAgeWith2020.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data.age, sex=="Persons"))+
  geom_ribbon(aes(x=index, ymin=lower, ymax=upper, group=age, fill=age), alpha=0.3)+
  geom_line(aes(x=index, y=rate, group=age, colour=age))+
  scale_x_continuous(name="", breaks=c(4, 12, 20, 28, 36, 44, 52, 60, 68, 76),
                     labels=c("2002", "2004", "2006", "2008", "2010", "2012",
                              "2014", "2016", "2018", "2020"))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_fill_paletteer_d("awtools::a_palette", name="Age")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Alcohol-specific deaths haven't risen across all age groups in 2020",
       subtitle="Quarterly alcohol-specific mortality rates in England & Wales by age",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxAgeWith2020Quarterly.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_line(data=subset(data.age, year>=2010 & sex=="Persons"), 
            aes(x=quarter, y=rate, group=year), colour="Grey70")+
  geom_point(data=subset(data.age, year>=2010 & sex=="Persons"), 
             aes(x=quarter, y=rate), colour="Grey70")+
  geom_line(data=subset(data.age, year==2020 & sex=="Persons"), 
            aes(x=quarter, y=rate, group=year), colour="#FF4E86")+
  geom_point(data=subset(data.age, year==2020 & sex=="Persons"), 
             aes(x=quarter, y=rate), colour="#FF4E86")+
  facet_wrap(~age)+
  scale_x_discrete(name="Quarter")+
  scale_y_continuous(name="Deaths per 100,000", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Alcohol-specific deaths in 2020 have risen in 40-69 year olds",
       subtitle="Quarterly age-standardised alcohol-specific mortality rates in England & Wales in <span style='color:#FF4E86;'>2020</span>, compared to 2010-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#ASD by region in 2020
data.region <- read_excel(file.2020, sheet="Table 3", range="B26:AC289", col_names=FALSE) %>% 
  select(c(1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28)) %>% 
  rename(sex=`...1`, region=`...3`, year=`...4`, Q1.deaths=`...6`, Q1.rate=`...7`, 
         Q1.lower=`...9`, Q1.upper=`...10`, Q2.deaths=`...12`, Q2.rate=`...13`, 
         Q2.lower=`...15`, Q2.upper=`...16`, Q3.deaths=`...18`, Q3.rate=`...19`, 
         Q3.lower=`...21`, Q3.upper=`...22`, Q4.deaths=`...24`, Q4.rate=`...25`, 
         Q4.lower=`...27`, Q4.upper=`...28`) %>% 
  pivot_longer(c(4:19), names_to=c("quarter", "metric"), names_sep="\\.", values_to=c("value")) %>% 
  mutate(index=(year-2001)*4,
         index=index+as.numeric(substr(quarter, 2, 3))-1) %>% 
  spread(metric, value)

tiff("Outputs/ASDxRegionWith2020.tiff", units="in", width=10, height=7, res=500)
ggplot(subset(data.region, sex=="Persons" & region!="England"))+
  geom_ribbon(aes(x=index, ymin=lower, ymax=upper, group=region, fill=region), alpha=0.3,
              show.legend=FALSE)+
  geom_line(aes(x=index, y=rate, group=region, colour=region), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(4, 12, 20, 28, 36, 44, 52, 60, 68, 76),
                     labels=c("2002", "2004", "2006", "2008", "2010", "2012",
                              "2014", "2016", "2018", "2020"))+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_fill_paletteer_d("rcartocolor::Vivid", name="Age")+
  scale_colour_paletteer_d("rcartocolor::Vivid", name="Age")+
  facet_wrap(~region)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The rise in alcohol-specific deaths has been greatest in the North East",
       subtitle="Quarterly age-standardised alcohol-specific mortality rates in England & Wales by region",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxRegionWith2020Quarterly.tiff", units="in", width=10, height=7, res=500)
ggplot()+
  geom_line(data=subset(data.region, year>=2010 & sex=="Persons" & region!="England"), 
            aes(x=quarter, y=rate, group=year), colour="Grey70")+
  geom_point(data=subset(data.region, year>=2010 & sex=="Persons" & region!="England"), 
             aes(x=quarter, y=rate), colour="Grey70")+
  geom_line(data=subset(data.region, year==2020 & sex=="Persons" & region!="England"), 
            aes(x=quarter, y=rate, group=year), colour="#FF4E86")+
  geom_point(data=subset(data.region, year==2020 & sex=="Persons" & region!="England"), 
             aes(x=quarter, y=rate), colour="#FF4E86")+
  facet_wrap(~region)+
  scale_x_discrete(name="Quarter")+
  scale_y_continuous(name="Deaths per 100,000", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Not all regions have seen an increase in alcohol-specific deaths",
       subtitle="Quarterly age-standardised alcohol-specific mortality rates in England & Wales in <span style='color:#FF4E86;'>2020</span>, compared to 2010-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Longer-term country-level trends
data.country <- read_excel(file.main, sheet="Table 1 data", range="A1:H856")

tiff("Outputs/ASDOverallUK.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data.country, `Area name`=="United Kingdom" & Sex=="Persons"))+
  geom_ribbon(aes(x=Year, ymin=`CI LOWER`, ymax=`CI UPPER`), alpha=0.3, fill="SkyBlue3")+
  geom_line(aes(x=Year, y=Rate), colour="Blue")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Rates of alcohol-specific deaths have remained stable ",
       subtitle="Annual age-standardised alcohol-specific mortality rates in the UK from 2001-2019",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxSexUK.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data.country, `Area name`=="United Kingdom" & Sex!="Persons"))+
  geom_ribbon(aes(x=Year, ymin=`CI LOWER`, ymax=`CI UPPER`, group=Sex, fill=Sex), 
              alpha=0.3, show.legend=FALSE)+
  geom_line(aes(x=Year, y=Rate, colour=Sex), show.legend=FALSE)+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="Sex")+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="Sex")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown())+
  labs(title="Rates of alcohol-specific death are much higher among men",
       subtitle="Annual age-standardised alcohol-specific mortality rates in the UK for <span style='color:#6600cc;'>men </span>and <span style='color:#00cc99;'>women</span> from 2001-2019",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxCountryUK.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data.country, `Area name` %in% c("England", "Wales", "Scotland", 
                                               "Northern Ireland") & Sex=="Persons"))+
  geom_ribbon(aes(x=Year, ymin=`CI LOWER`, ymax=`CI UPPER`, group=`Area name`, fill=`Area name`),
              alpha=0.3)+
  geom_line(aes(x=Year, y=Rate, colour=`Area name`))+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Alcohol-specific deaths have fallen in Scotland and risen in Northern Ireland",
       subtitle="Annual age-standardised alcohol-specific mortality rates in the UK from 2001-2019",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#ASD by cause
data.cause <- read_excel(file.cause, sheet="Table 1 - UK", range="A13:BP316") %>% 
  select(-c(25,47)) %>% 
  fill(Year, .direction="down") %>% 
  na.omit() %>% 
  mutate(cause=case_when(
    `ICD-10 code`=="F10" ~ "Conditions related to dependence",
    `ICD-10 code`=="K70" ~ "Alcoholic liver disease",
    `ICD-10 code` %in% c("X45", "X65", "Y15") ~ "Alcohol poisoning",
    TRUE ~ "Other causes")) %>% 
  group_by(Year, cause) %>% 
  summarise(across(c(3:64), sum)) %>% 
  gather(group, deaths, c(3:64)) %>% 
  separate(group, into=c("age", "sex"),sep="\\...") %>% 
  mutate(sex=as.numeric(sex), sex=case_when(
    sex<25 ~ "Persons",
    sex<47 ~ "Male",
    TRUE ~ "Female"))

#2019 deaths by cause
tiff("Outputs/ASDxCauseUK.tiff", units="in", width=8, height=6, res=500)
data.cause %>% 
  filter(sex!="Persons" & Year==2019 & age!="All ages") %>% 
  ggplot(aes(y=age, x=deaths, fill=cause))+
  geom_col()+
  scale_x_continuous(name="Deaths")+
  scale_y_discrete(name="Age group")+
  scale_fill_paletteer_d("rcartocolor::Safe", name="Cause of death")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Most alcohol-specific deaths are from alcoholic liver disease",
       subtitle="Alcohol-specific deaths in 2019 in England & Wales by cause",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Trends in deaths by cause
tiff("Outputs/ASDxCauseTrendsUK.tiff", units="in", width=8, height=6, res=500)
data.cause %>% 
  filter(sex=="Persons" & age=="All ages") %>% 
  ggplot(aes(x=Year, y=deaths, colour=cause))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths")+
  scale_colour_paletteer_d("rcartocolor::Safe", name="Cause of death")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Deaths from alcoholic liver disease have been rising for several years",
       subtitle="Alcohol-specific deaths in England & Wales by cause",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#UK-level age patterns
data.main <- read_excel(file.main, sheet="Table 2", range="C6:V366", col_names=FALSE) %>% 
  select(1,2,4,5,7,8,10,11,13,14,16,17,19,20) %>% 
  rename(year=`...1`, age=`...2`, deaths.Person=`...4`, rate.Person=`...5`,
         lower.Person=`...7`, upper.Person=`...8`, deaths.Male=`...10`, rate.Male=`...11`,
         lower.Male=`...13`, upper.Male=`...14`, deaths.Female=`...16`, rate.Female=`...17`,
         lower.Female=`...19`, upper.Female=`...20`) %>% 
  mutate(deaths.Person=as.character(deaths.Person),
         deaths.Male=as.character(deaths.Male),
         deaths.Female=as.character(deaths.Female)) %>% 
  pivot_longer(c(3:14), names_to=c("metric", "sex"), names_sep="\\.", values_to=c("value")) %>% 
  mutate(value=as.numeric(str_replace(value, "z", "0"))) %>% 
  spread(metric, value)

#Bring in populations
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

pop <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Persons", range="E6:CQ6", 
                                  col_names=FALSE))) %>% 
  mutate(age.sgl=0:90,
         age=case_when(
           age.sgl<1 ~ "<1",
           age.sgl<5 ~ "01-04",
           age.sgl<10 ~ "05-09",
           age.sgl<15 ~ "10-14",
           age.sgl<20 ~ "15-19",
           age.sgl<25 ~ "20-24",
           age.sgl<30 ~ "25-29",
           age.sgl<35 ~ "30-34",
           age.sgl<40 ~ "35-39",
           age.sgl<45 ~ "40-44",
           age.sgl<50 ~ "45-49",
           age.sgl<55 ~ "50-54",
           age.sgl<60 ~ "55-59",
           age.sgl<65 ~ "60-64",
           age.sgl<70 ~ "65-69",
           age.sgl<75 ~ "70-74",
           age.sgl<80 ~ "75-79",
           age.sgl<85 ~ "80-84",
           TRUE ~ "85+")) %>% 
  group_by(age) %>% 
  summarise(pop=sum(V1)) %>% 
  ungroup()

#Collapse age bands
data.main <- data.main %>% 
  merge(pop) %>% 
  mutate(age=case_when(
           age %in% c("<1", "01-04", "05-09", "10-14", "15-19") ~ "Under 20",
           age %in% c("20-24", "25-29") ~ "20-29",
           age %in% c("30-34", "35-39") ~ "30-39",
           age %in% c("40-44", "45-49") ~ "40-49",
           age %in% c("50-54", "55-59") ~ "50-59",
           age %in% c("60-64", "65-69") ~ "60-69",
           age %in% c("70-74", "75-79") ~ "70-79",
           TRUE ~ "80+"),
         age=factor(age, levels=c("Under 20", "20-29", "30-39", "40-49", "50-59",
                                  "60-69", "70-79", "80+"))) %>% 
  group_by(year, age) %>% 
  summarise(deaths=sum(deaths), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(rate=deaths*100000/pop) %>% 
  arrange(age, year) %>% 
  mutate(index=1:152)

#grouped path of ASD
x1 <- c(0, data.main$rate[data.main$age=="Under 20" & data.main$year==2001],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2002],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2003],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2004],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2005],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2006],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2007],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2008],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2009],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2010],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2011],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2012],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2013],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2014],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2015],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2016],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2017],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2018],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2019],
        data.main$rate[data.main$age=="Under 20" & data.main$year==2020],0)

x2 <- c(0, data.main$rate[data.main$age=="20-29" & data.main$year==2001],
        data.main$rate[data.main$age=="20-29" & data.main$year==2002],
        data.main$rate[data.main$age=="20-29" & data.main$year==2003],
        data.main$rate[data.main$age=="20-29" & data.main$year==2004],
        data.main$rate[data.main$age=="20-29" & data.main$year==2005],
        data.main$rate[data.main$age=="20-29" & data.main$year==2006],
        data.main$rate[data.main$age=="20-29" & data.main$year==2007],
        data.main$rate[data.main$age=="20-29" & data.main$year==2008],
        data.main$rate[data.main$age=="20-29" & data.main$year==2009],
        data.main$rate[data.main$age=="20-29" & data.main$year==2010],
        data.main$rate[data.main$age=="20-29" & data.main$year==2011],
        data.main$rate[data.main$age=="20-29" & data.main$year==2012],
        data.main$rate[data.main$age=="20-29" & data.main$year==2013],
        data.main$rate[data.main$age=="20-29" & data.main$year==2014],
        data.main$rate[data.main$age=="20-29" & data.main$year==2015],
        data.main$rate[data.main$age=="20-29" & data.main$year==2016],
        data.main$rate[data.main$age=="20-29" & data.main$year==2017],
        data.main$rate[data.main$age=="20-29" & data.main$year==2018],
        data.main$rate[data.main$age=="20-29" & data.main$year==2019],
        data.main$rate[data.main$age=="20-29" & data.main$year==2020],0)

x3 <- c(0, data.main$rate[data.main$age=="30-39" & data.main$year==2001],
        data.main$rate[data.main$age=="30-39" & data.main$year==2002],
        data.main$rate[data.main$age=="30-39" & data.main$year==2003],
        data.main$rate[data.main$age=="30-39" & data.main$year==2004],
        data.main$rate[data.main$age=="30-39" & data.main$year==2005],
        data.main$rate[data.main$age=="30-39" & data.main$year==2006],
        data.main$rate[data.main$age=="30-39" & data.main$year==2007],
        data.main$rate[data.main$age=="30-39" & data.main$year==2008],
        data.main$rate[data.main$age=="30-39" & data.main$year==2009],
        data.main$rate[data.main$age=="30-39" & data.main$year==2010],
        data.main$rate[data.main$age=="30-39" & data.main$year==2011],
        data.main$rate[data.main$age=="30-39" & data.main$year==2012],
        data.main$rate[data.main$age=="30-39" & data.main$year==2013],
        data.main$rate[data.main$age=="30-39" & data.main$year==2014],
        data.main$rate[data.main$age=="30-39" & data.main$year==2015],
        data.main$rate[data.main$age=="30-39" & data.main$year==2016],
        data.main$rate[data.main$age=="30-39" & data.main$year==2017],
        data.main$rate[data.main$age=="30-39" & data.main$year==2018],
        data.main$rate[data.main$age=="30-39" & data.main$year==2019],
        data.main$rate[data.main$age=="30-39" & data.main$year==2020],0)

x4 <- c(0, data.main$rate[data.main$age=="40-49" & data.main$year==2001],
        data.main$rate[data.main$age=="40-49" & data.main$year==2002],
        data.main$rate[data.main$age=="40-49" & data.main$year==2003],
        data.main$rate[data.main$age=="40-49" & data.main$year==2004],
        data.main$rate[data.main$age=="40-49" & data.main$year==2005],
        data.main$rate[data.main$age=="40-49" & data.main$year==2006],
        data.main$rate[data.main$age=="40-49" & data.main$year==2007],
        data.main$rate[data.main$age=="40-49" & data.main$year==2008],
        data.main$rate[data.main$age=="40-49" & data.main$year==2009],
        data.main$rate[data.main$age=="40-49" & data.main$year==2010],
        data.main$rate[data.main$age=="40-49" & data.main$year==2011],
        data.main$rate[data.main$age=="40-49" & data.main$year==2012],
        data.main$rate[data.main$age=="40-49" & data.main$year==2013],
        data.main$rate[data.main$age=="40-49" & data.main$year==2014],
        data.main$rate[data.main$age=="40-49" & data.main$year==2015],
        data.main$rate[data.main$age=="40-49" & data.main$year==2016],
        data.main$rate[data.main$age=="40-49" & data.main$year==2017],
        data.main$rate[data.main$age=="40-49" & data.main$year==2018],
        data.main$rate[data.main$age=="40-49" & data.main$year==2019],
        data.main$rate[data.main$age=="40-49" & data.main$year==2020],0)

x5 <- c(0, data.main$rate[data.main$age=="50-59" & data.main$year==2001],
        data.main$rate[data.main$age=="50-59" & data.main$year==2002],
        data.main$rate[data.main$age=="50-59" & data.main$year==2003],
        data.main$rate[data.main$age=="50-59" & data.main$year==2004],
        data.main$rate[data.main$age=="50-59" & data.main$year==2005],
        data.main$rate[data.main$age=="50-59" & data.main$year==2006],
        data.main$rate[data.main$age=="50-59" & data.main$year==2007],
        data.main$rate[data.main$age=="50-59" & data.main$year==2008],
        data.main$rate[data.main$age=="50-59" & data.main$year==2009],
        data.main$rate[data.main$age=="50-59" & data.main$year==2010],
        data.main$rate[data.main$age=="50-59" & data.main$year==2011],
        data.main$rate[data.main$age=="50-59" & data.main$year==2012],
        data.main$rate[data.main$age=="50-59" & data.main$year==2013],
        data.main$rate[data.main$age=="50-59" & data.main$year==2014],
        data.main$rate[data.main$age=="50-59" & data.main$year==2015],
        data.main$rate[data.main$age=="50-59" & data.main$year==2016],
        data.main$rate[data.main$age=="50-59" & data.main$year==2017],
        data.main$rate[data.main$age=="50-59" & data.main$year==2018],
        data.main$rate[data.main$age=="50-59" & data.main$year==2019],
        data.main$rate[data.main$age=="50-59" & data.main$year==2020],0)

x6 <- c(0, data.main$rate[data.main$age=="60-69" & data.main$year==2001],
        data.main$rate[data.main$age=="60-69" & data.main$year==2002],
        data.main$rate[data.main$age=="60-69" & data.main$year==2003],
        data.main$rate[data.main$age=="60-69" & data.main$year==2004],
        data.main$rate[data.main$age=="60-69" & data.main$year==2005],
        data.main$rate[data.main$age=="60-69" & data.main$year==2006],
        data.main$rate[data.main$age=="60-69" & data.main$year==2007],
        data.main$rate[data.main$age=="60-69" & data.main$year==2008],
        data.main$rate[data.main$age=="60-69" & data.main$year==2009],
        data.main$rate[data.main$age=="60-69" & data.main$year==2010],
        data.main$rate[data.main$age=="60-69" & data.main$year==2011],
        data.main$rate[data.main$age=="60-69" & data.main$year==2012],
        data.main$rate[data.main$age=="60-69" & data.main$year==2013],
        data.main$rate[data.main$age=="60-69" & data.main$year==2014],
        data.main$rate[data.main$age=="60-69" & data.main$year==2015],
        data.main$rate[data.main$age=="60-69" & data.main$year==2016],
        data.main$rate[data.main$age=="60-69" & data.main$year==2017],
        data.main$rate[data.main$age=="60-69" & data.main$year==2018],
        data.main$rate[data.main$age=="60-69" & data.main$year==2019],
        data.main$rate[data.main$age=="60-69" & data.main$year==2020],0)

x7 <- c(0, data.main$rate[data.main$age=="70-79" & data.main$year==2001],
        data.main$rate[data.main$age=="70-79" & data.main$year==2002],
        data.main$rate[data.main$age=="70-79" & data.main$year==2003],
        data.main$rate[data.main$age=="70-79" & data.main$year==2004],
        data.main$rate[data.main$age=="70-79" & data.main$year==2005],
        data.main$rate[data.main$age=="70-79" & data.main$year==2006],
        data.main$rate[data.main$age=="70-79" & data.main$year==2007],
        data.main$rate[data.main$age=="70-79" & data.main$year==2008],
        data.main$rate[data.main$age=="70-79" & data.main$year==2009],
        data.main$rate[data.main$age=="70-79" & data.main$year==2010],
        data.main$rate[data.main$age=="70-79" & data.main$year==2011],
        data.main$rate[data.main$age=="70-79" & data.main$year==2012],
        data.main$rate[data.main$age=="70-79" & data.main$year==2013],
        data.main$rate[data.main$age=="70-79" & data.main$year==2014],
        data.main$rate[data.main$age=="70-79" & data.main$year==2015],
        data.main$rate[data.main$age=="70-79" & data.main$year==2016],
        data.main$rate[data.main$age=="70-79" & data.main$year==2017],
        data.main$rate[data.main$age=="70-79" & data.main$year==2018],
        data.main$rate[data.main$age=="70-79" & data.main$year==2019],
        data.main$rate[data.main$age=="70-79" & data.main$year==2020],0)

x8 <- c(0, data.main$rate[data.main$age=="80+" & data.main$year==2001],
        data.main$rate[data.main$age=="80+" & data.main$year==2002],
        data.main$rate[data.main$age=="80+" & data.main$year==2003],
        data.main$rate[data.main$age=="80+" & data.main$year==2004],
        data.main$rate[data.main$age=="80+" & data.main$year==2005],
        data.main$rate[data.main$age=="80+" & data.main$year==2006],
        data.main$rate[data.main$age=="80+" & data.main$year==2007],
        data.main$rate[data.main$age=="80+" & data.main$year==2008],
        data.main$rate[data.main$age=="80+" & data.main$year==2009],
        data.main$rate[data.main$age=="80+" & data.main$year==2010],
        data.main$rate[data.main$age=="80+" & data.main$year==2011],
        data.main$rate[data.main$age=="80+" & data.main$year==2012],
        data.main$rate[data.main$age=="80+" & data.main$year==2013],
        data.main$rate[data.main$age=="80+" & data.main$year==2014],
        data.main$rate[data.main$age=="80+" & data.main$year==2015],
        data.main$rate[data.main$age=="80+" & data.main$year==2016],
        data.main$rate[data.main$age=="80+" & data.main$year==2017],
        data.main$rate[data.main$age=="80+" & data.main$year==2018],
        data.main$rate[data.main$age=="80+" & data.main$year==2019],
        data.main$rate[data.main$age=="80+" & data.main$year==2020],0)


ASDplot <- ggplot()+
  geom_polygon(aes(x=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,19), y=x1), fill="SkyBlue")+
  geom_polygon(aes(x=c(20,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,38), y=x2), fill="SkyBlue")+
  geom_polygon(aes(x=c(39,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,57), y=x3), fill="SkyBlue")+
  geom_polygon(aes(x=c(58,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,76), y=x4), fill="SkyBlue")+
  geom_polygon(aes(x=c(77,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,95), y=x5), fill="SkyBlue")+
  geom_polygon(aes(x=c(96,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,114), y=x6), fill="SkyBlue")+
  geom_polygon(aes(x=c(115,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,133), y=x7), fill="SkyBlue")+
  geom_polygon(aes(x=c(134,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,152), y=x8), fill="SkyBlue")+
  geom_path(data=data.main,
            aes(x=index, y=rate, group=age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_x_continuous(breaks=c(10,29,48,67,86,105,124,143), 
                     labels=c("under 20", "20-29", "30-39", "40-49", "50-59",
                              "60-69", "70-79", "80+"),name="Age")+
  scale_y_continuous(name="Annual alcohol-specific deaths per 100,000")+
  labs(title="Rates of alcohol-specific deaths in the UK have risen in the over 50s",
       subtitle="Trends in alcohol-specific death rates by age in the UK between 2001 and 2019",
       caption="Data from ONS | Plot by @VictimOfMaths")

ASDinset <- ggplot()+
  geom_polygon(aes(x=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,19), 
                   y=c(0,15,18,16,20,16,13,11,10,15,12,9,8,10,7,9,6,12,15,9,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), 
                y=c(15,18,16,20,16,13,11,10,15,12,9,8,10,7,9,6,12,15,9)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDfull <- ggdraw()+
  draw_plot(ASDplot)+
  draw_plot(ASDinset, x=0.15, y=0.65, width=0.1, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10)+
  draw_label("2019", x=0.24, y=0.66, size=10)+
  draw_label("Key", x=0.18, y=0.85, size=10, fontface="bold")

tiff("Outputs/ASDUK2001to19.tiff", units="in", width=9, height=6.6, res=500)
ggdraw(ASDfull)
dev.off()

#####################################################################
#Try and get at similar 2020 analysis for Scotland
q1file <- tempfile()
q2file <- tempfile()
q3file <- tempfile()
q4file <- tempfile()

q1url <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/20/q1/quarter-1-20-tables.xlsx"
q2url <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/20/q2/quarter-2-20-tables.xlsx"
q3url <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/20/q3/quarter-3-20-tables.xls"
q4url <- "https://www.nrscotland.gov.uk/files//statistics/births-marriages-deaths-quarterly/19/q4/quarter-4-19-tables.xlsx"

q1file <- curl_download(url=q1url, destfile=q1file, quiet=FALSE, mode="wb")
q2file <- curl_download(url=q2url, destfile=q2file, quiet=FALSE, mode="wb")
q3file <- curl_download(url=q3url, destfile=q3file, quiet=FALSE, mode="wb")
q4file <- curl_download(url=q4url, destfile=q4file, quiet=FALSE, mode="wb")

q1data <- read_excel(q1file, sheet="Q4", range="A9:I89", col_names=FALSE) %>% 
  filter(`...1` %in% c("F10", "K70, K73-74"))

colnames(q1data) <- c("ICD-10", "cause", "2015", "2016", "2017", "2018", "2019", 
                       "2015-19", "2020")

q1data <- q1data %>% 
  select(-`2015-19`) %>% 
  gather(year, deaths, c(3:8)) %>% 
  mutate(quarter="Q1")

q2data <- read_excel(q2file, sheet="Q4", range="A9:I89", col_names=FALSE) %>% 
  filter(`...1` %in% c("F10", "K70, K73-74"))

colnames(q2data) <- c("ICD-10", "cause", "2015", "2016", "2017", "2018", "2019", 
                      "2015-19", "2020")

q2data <- q2data %>% 
  select(-`2015-19`) %>% 
  gather(year, deaths, c(3:8)) %>% 
  mutate(quarter="Q2")

q3data <- read_excel(q3file, sheet="Q4", range="A9:I89", col_names=FALSE) %>% 
  filter(`...1` %in% c("F10", "K70, K73-74"))

colnames(q3data) <- c("ICD-10", "cause", "2015", "2016", "2017", "2018", "2019", 
                      "2015-19", "2020")

q3data <- q3data %>% 
  select(-`2015-19`) %>% 
  gather(year, deaths, c(3:8)) %>% 
  mutate(quarter="Q3")

q4data <- read_excel(q1file, sheet="Q4", range="A9:I92", col_names=FALSE) %>% 
  filter(`...1` %in% c("F10", "K70, K73-74"))

colnames(q4data) <- c("ICD-10", "cause", "2014", "2015", "2016", "2017", "2018", 
                      "2014-18", "2019")

q4data <- q4data %>% 
  select(-`2014-18`) %>% 
  gather(year, deaths, c(3:8)) %>% 
  mutate(quarter="Q4")

#Merge quarters
scotdata <- bind_rows(q1data, q2data, q3data, q4data) %>% 
  mutate(year=as.numeric(year), deaths=as.numeric(deaths))

#Add totals
scotdata <- scotdata %>% 
  group_by(quarter, year) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  mutate(cause="Total") %>% 
  bind_rows(scotdata) %>% 
  filter(year>2014) %>% 
  mutate(index=(year-2015)*4,
         index=index+as.numeric(substr(quarter, 2, 3))-1)

#Time series data
tiff("Outputs/ASDxCauseScot.tiff", units="in", width=9, height=7, res=500)
ggplot(subset(scotdata, cause!="Total"))+
  geom_line(aes(x=index, y=deaths, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(0,4,8,12,16,20),
                    labels=c("2015", "2016", "2017", "2018", "2019", "2020"))+
  scale_y_continuous(name="Number of deaths", limits=c(0,NA))+
  scale_colour_paletteer_d("NineteenEightyR::malibu")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Deaths in Scotland from causes closely linked to alcohol don't seem to have risen in 2020",
       subtitle="Quarterly deaths from <span style='color:#FF4E86;'>liver disease</span> and <span style='color:#FF9E44;'>mental & behavioural disorders due to alcohol</span> in Scotland",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Quarterly comparison
tiff("Outputs/ASDxCauseScotWith2020Quarterly.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_line(data=subset(scotdata, cause=="Total"), 
            aes(x=quarter, y=deaths, group=year), colour="Grey70")+
  geom_point(data=subset(scotdata, cause=="Total"), 
             aes(x=quarter, y=deaths), colour="Grey70")+
  geom_line(data=subset(scotdata, cause=="Total" & year==2020),
            aes(x=quarter, y=deaths, group=year), colour="#FF4E86")+
  geom_point(data=subset(scotdata, cause=="Total" & year==2020),
            aes(x=quarter, y=deaths), colour="#FF4E86")+
  scale_x_discrete(name="Quarter")+
  scale_y_continuous(name="Number of deaths", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown())+
  labs(title="Deaths linked to alcohol don't seem to have risen in Scotland in 2020",
       subtitle="Quarterly mortality rates from liver disease and mental & behavioural disorders due to alcohol<br>in Scotland in <span style='color:#FF4E86;'>2020</span>, compared to 2015-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ASDxCauseScotWith2020Quarterly.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  geom_line(data=subset(scotdata, cause!="Total"), 
            aes(x=quarter, y=deaths, group=year), colour="Grey70")+
  geom_point(data=subset(scotdata, cause!="Total"), 
             aes(x=quarter, y=deaths), colour="Grey70")+
  geom_line(data=subset(scotdata, cause!="Total" & year==2020),
            aes(x=quarter, y=deaths, group=year), colour="#FF4E86")+
  geom_point(data=subset(scotdata, cause!="Total" & year==2020),
             aes(x=quarter, y=deaths), colour="#FF4E86")+
  scale_x_discrete(name="Quarter")+
  scale_y_continuous(name="Number of deaths", limits=c(0,NA))+
  facet_wrap(~cause)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Deaths linked to alcohol don't seem to have risen in Scotland in 2020",
       subtitle="Quarterly mortality rates from selected causes linked to alcohol<br>in Scotland in <span style='color:#FF4E86;'>2020</span>, compared to 2015-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
