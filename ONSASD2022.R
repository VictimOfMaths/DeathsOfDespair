rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(lubridate)
library(ggrepel)
library(sf)
library(geomtextpath)

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

#Read in alcohol-specific deaths data from ONS website
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/alcoholspecificdeathsintheuk/current/alcoholspecificdeaths2022.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Country-specific figures
UKraw <- read_excel(temp, sheet="Table_1", range="A6:O27", col_names=FALSE) %>% 
  set_names("code", "Country", "Year", "Deaths_Total", "Rate_Total", "Lower_Total", 
            "Upper_Total", "Deaths_Male", "Rate_Male", "Lower_Male", "Upper_Male",
            "Deaths_Female", "Rate_Female", "Lower_Female", "Upper_Female")

Engraw <- read_excel(temp, sheet="Table_2", range="A6:O27", col_names=FALSE) %>% 
  set_names("code", "Country", "Year", "Deaths_Total", "Rate_Total", "Lower_Total", 
            "Upper_Total", "Deaths_Male", "Rate_Male", "Lower_Male", "Upper_Male",
            "Deaths_Female", "Rate_Female", "Lower_Female", "Upper_Female")

Walraw <- read_excel(temp, sheet="Table_3", range="A6:O27", col_names=FALSE) %>% 
  set_names("code", "Country", "Year", "Deaths_Total", "Rate_Total", "Lower_Total", 
            "Upper_Total", "Deaths_Male", "Rate_Male", "Lower_Male", "Upper_Male",
            "Deaths_Female", "Rate_Female", "Lower_Female", "Upper_Female")  

Scoraw <- read_excel(temp, sheet="Table_4", range="A6:O27", col_names=FALSE) %>% 
  set_names("code", "Country", "Year", "Deaths_Total", "Rate_Total", "Lower_Total", 
            "Upper_Total", "Deaths_Male", "Rate_Male", "Lower_Male", "Upper_Male",
            "Deaths_Female", "Rate_Female", "Lower_Female", "Upper_Female")

NIraw <- read_excel(temp, sheet="Table_5", range="A6:O27", col_names=FALSE) %>% 
  set_names("code", "Country", "Year", "Deaths_Total", "Rate_Total", "Lower_Total", 
            "Upper_Total", "Deaths_Male", "Rate_Male", "Lower_Male", "Upper_Male",
            "Deaths_Female", "Rate_Female", "Lower_Female", "Upper_Female")

Engraw <- read_excel(temp, sheet="Table_2", range="A6:O27", col_names=FALSE) %>% 
  set_names("code", "Country", "Year", "Deaths_Total", "Rate_Total", "Lower_Total", 
            "Upper_Total", "Deaths_Male", "Rate_Male", "Lower_Male", "Upper_Male",
            "Deaths_Female", "Rate_Female", "Lower_Female", "Upper_Female")

data <- bind_rows(UKraw, Engraw, Walraw, Scoraw, NIraw) %>% 
  pivot_longer(cols=c(4:15), names_to=c("Metric", "Sex"), names_sep="_", values_to="Value") %>% 
  spread(Metric, Value)

#Add in 2023 figures where available
data23 <- data %>% bind_rows(data,
  #English data from LAPE
  data.frame(Country="England", Sex="Total", Year=2023, Deaths=8274, Rate=15, Lower=14.7, Upper=15.3),
  #Scottish data from NRS
  data.frame(Country=rep("Scotland", times=3), Sex=c("Total", "Male", "Female"), Year=rep(2023, times=3), 
             Deaths=c(1277, 861, 416), Rate=c(22.7,32, 14.2), Lower=c(21.4, 29.8, 12.8), Upper=c(23.9, 34.1, 15.5))
)

#Plot UK total
agg_png("Outputs/ASDUK.png", units="in", width=8, height=6, res=800)
ggplot(data23 %>% filter(Country=="United Kingdom" & Sex=="Total"), 
       aes(x=Year, y=Deaths))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(colour="tomato")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Alcohol-specific deaths")+
  theme_custom()+
  labs(title="Alcohol-specific deaths have risen again",
       subtitle="Number of deaths in the UK from conditions caused only by alcohol",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Plot country-specific rates
agg_png("Outputs/ASDxCountry.png", units="in", width=8, height=6, res=800)
ggplot(data23 %>% filter(Country!="United Kingdom" & Sex=="Total") %>% 
         mutate(Country=factor(Country, levels=c("England", "Wales", "Northern Ireland", "Scotland"))),
       aes(x=Year, y=Rate, ymin=Lower, ymax=Upper))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(fill=Country), alpha=0.1)+
  geom_line(aes(colour=Country))+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(name="", values=c("#017a4a", "#FFCE4E", "#3d98d3", "#ff363c"),
                      guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(name="", values=c("#017a4a", "#FFCE4E", "#3d98d3", "#ff363c"),
                    guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  labs(title="Alcohol-specific deaths rates are still rising",
       subtitle="Age-standardised mortality rates from conditions caused only by alcohol.\nShaded areas represent 95% Confidence Intervals.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Plot England only
agg_png("Outputs/ASDEng.png", units="in", width=8, height=6, res=800)
ggplot(data23 %>% filter(Country=="England" & Sex=="Total"), 
       aes(x=Year, y=Deaths))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(colour="tomato")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Alcohol-specific deaths")+
  theme_custom()+
  labs(title="Alcohol-specific deaths have risen 42% since 2019",
       subtitle="Number of deaths in England from conditions caused only by alcohol",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Changes since 2019
changes <- data %>% 
  group_by(Sex, Country) %>% 
  summarise(RateChange=(Rate[Year==2022]-Rate[Year==2019])/Rate[Year==2019], .groups="drop")

agg_png("Outputs/ASDChangexCountryxSex.png", units="in", width=8, height=6, res=800)
ggplot(changes %>% filter(Country!="United Kingdom" & Sex!="Total") %>% 
         mutate(Country=factor(Country, levels=c("England", "Wales", "Northern Ireland", "Scotland")),
                dodge=if_else(RateChange<0, 1.1, -0.1),
                barlab=if_else(RateChange>0, paste0("+", round(RateChange*100, 1), "%"),
                               paste0(round(RateChange*100, 1), "%"))),
       aes(x=RateChange, y=Country, fill=Sex))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="grey30")+
  geom_text(aes(label=barlab, hjust=dodge),
            position=position_dodge(width=0.9))+
  scale_x_continuous(name="Change 2019-2022", labels=label_percent(accuracy=1),
                     limits=c(-0.1, 0.5))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#6600cc", "#00cc99"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.major.x=element_line(colour="grey95"))+
  labs(title="Alcohol-specific deaths have risen fastest in women in England",
       subtitle="Change in age-standardised mortality rates from conditions caused only by alcohol between 2019 and 2022 by sex\n ",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/ASDxSex.png", units="in", width=11, height=5, res=800)
bind_rows(Engraw, Walraw) %>% 
  group_by(Year) %>% 
  summarise(Male=sum(Deaths_Male), Female=sum(Deaths_Female)) %>% 
  ggplot(aes(x=Year))+
  geom_textpath(aes(y=Male), colour="#00cc99", size=4, label="Male", hjust=0.55)+
  geom_textpath(aes(y=Female), colour="#6600cc", size=4, label="Female", hjust=0.55)+
  geom_hline(yintercept=0, colour="grey30")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths", limits=c(0,NA))+
  theme_custom()

dev.off()

#By region
regdata <- read_excel(temp, sheet="Table_6", range="A6:H599", col_names=FALSE) %>% 
  set_names("Year", "Sex", "Code", "Region", "Deaths", "Rate", "Lower", "Upper") %>% 
  group_by(Region, Sex) %>% 
  mutate(RateChange=(Rate[Year==2022]-Rate[Year==2019])/Rate[Year==2019]) %>% 
  ungroup()

agg_png("Outputs/ASDxRegion.png", units="in", width=8, height=6, res=800)
ggplot(regdata %>% filter(Sex=="Persons"),
       aes(x=Year, y=Rate))+
  geom_rect(ymin=0, ymax=23, xmin=2019.2, xmax=2022.5, colour="grey90", fill="grey90")+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2, fill="tomato")+
  geom_line(colour="tomato")+
  geom_text(data=. %>% filter(Year==2022), aes(label=paste0("+", round(RateChange*100, 0), "%")), 
            hjust=0.9,vjust=4, size=rel(3), family=font)+
  scale_x_continuous(name="")+
  scale_y_continuous(limits=c(0,NA))+
  facet_wrap(~Region)+
  theme_custom()+
  labs(title="Alcohol deaths have risen sharply across all regions of England",
       subtitle="Age-standardised mortality rates from conditions that are only caused by alcohol by English region.\nGrey shaded areas represent the pandemic period and the percentage change 2019-2022.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By condition
conddata <- read_excel(temp, sheet="Table_9", range="A4:AA994") %>% 
  rename("Code"="Area code \r\n[note 2]", 
         "Country"="Area of usual residence \r\n[note 2]", 
         "Year"="Year of death registration \r\n[note 3]",
         "Cause"="Individual cause of death description") %>% 
  mutate(Cause=case_when(
    Cause=="Alcoholic liver disease" ~ Cause,
    Cause=="Accidental poisoning by and exposure to alcohol" ~ "Alcohol poisoning",
    `ICD-10 Code`=="F10" ~ "Conditions related to dependence",
    TRUE ~ "Other")) %>% 
  group_by(Cause, Year, Sex) %>% 
  summarise(across(c(`<1`:`Total`), ~sum(.x)), .groups="drop")

agg_png("Outputs/ASDxCause.png", units="in", width=8, height=6, res=800)
ggplot(conddata %>% filter(Sex=="Persons"),
       aes(x=Year, y=Total, fill=Cause))+
  geom_area(position="fill")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of all alcohol-specific deaths",
                     label=label_percent(accuracy=1))+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  labs(title="Most alcohol-specific deaths are due to liver disease",
       subtitle="Causes of death from conditions that are only caused by alcohol inthe UK",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

ggplot(conddata %>% filter(Year==2022 & Sex!="Persons") %>% 
         gather(Age, Deaths, c(`<1`:`90+`)),
       aes(x=Age, y=Deaths, fill=Cause))+
  geom_col(position="stack")+
  facet_wrap(~Sex)+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()

#By age
agedata <- read_excel(temp, sheet="Table_7", range="A6:CZ71", col_names=FALSE) %>% 
  set_names("Code", "Country", "Year", "Sex", 
            paste0(rep(c("<1", "1-4","5-9", "10-14", "15-19", "20-24", "25-29", 
                         "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                         "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                       each=5), rep(c("_Deaths", "_Rate", "_Marker", "_Lower", "_Upper"),
                                    times=20))) %>% 
  mutate(across(c(6:104), ~as.numeric(.x))) %>% 
  pivot_longer(cols=c(6:104), names_to=c("Age", "Metric"), names_sep="_", values_to="Value") %>% 
  mutate(Age=factor(Age, levels=c("<1", "1-4","5-9", "10-14", "15-19", "20-24", "25-29", 
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                                  "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))


agg_png("Outputs/ASDUKxAge.png", units="in", width=12, height=6, res=800)
ggplot(agedata %>% filter(Sex=="Persons" & Metric=="Rate") %>% arrange(Year),
       aes(x=Year, y=Value))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_area(fill="skyblue")+
  geom_path(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())+
  labs(title="Alcohol-specific deaths have risen sharply in older ages",
       subtitle="Age-specific trends in deaths from conditions caused only by alcohol between 2001 and 2022 in the UK",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

##################################
#Read in Local Authority data
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
source3 <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2022/alcohol-specific-deaths-22-all-tabs.xlsx"
temp3 <- curl_download(url=source3, destfile=temp3, quiet=FALSE, mode="wb")

scotladata <- read_excel(temp3, sheet="Table_4B", range="A5:G632") %>% 
  set_names("Year", "Laname", "Value", "Lower", "Upper", "CIs", "Deaths")

#Northern Ireland
temp4 <- tempfile()
source4 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Alcohol_Tables_2022%20Final%20updated.xlsx"
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
              filter(Year=="2018to2022") %>% 
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

#Bar chart
ltlarates2 %>% st_drop_geometry %>% 
  mutate(Country=substr(Lacode, 1, 1)) %>% 
  arrange(Value) %>% 
  mutate(Laname=fct_inorder(Laname)) %>% 
  ggplot(aes(x=Value, y=Laname, fill=Country))+
  geom_col()+
  theme_custom()

#Read in English and Welst drug-related death rates
temp5 <- tempfile()
source5 <- ("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority/current/2022localauthorities.xlsx")
temp5 <- curl_download(url=source5, destfile=temp5, quiet=FALSE, mode="wb")
