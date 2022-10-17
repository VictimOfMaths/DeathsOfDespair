rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(readxl)
library(ragg)
library(extrafont)
library(ggtext)
library(scales)
library(geomtextpath)
library(paletteer)
library(cowplot)

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

#Download ASD data for Northern Ireland from NISRA
#2011-21
temp <- tempfile()
url1 <- "https://www.nisra.gov.uk/system/files/statistics/Alcohol_Tables_2021%20Final.xlsx"
niraw <- curl_download(url=url1, destfile=temp, quiet=FALSE, mode="wb")

#2001-11
temp <- tempfile()
url2 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Alcohol_Tables_11.xls"
niraw_old <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

#Get population data to derive rates as older data is counts only
temp <- tempfile()
url3 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/MYE20-AGE-BANDS.xlsx"
nipopraw <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

#Grouped path data for NI
nidata_age_new <- read_excel(niraw, sheet="Table 2", range="B15:M21", col_names=FALSE) %>% 
  gather(Year, mortrate, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(substr(Year, 4, 5))+2009) %>% 
  rename("Age"="...1")

nidata_age_old <- read_excel(niraw_old, sheet="Table 2", range="C4:I14") %>% 
  mutate(Year=c(2001:2010)) %>% 
  gather(Age, Deaths, c(1:7)) %>% 
  mutate(Deaths=if_else(Deaths=="-", "0", Deaths),
         Deaths=as.numeric(Deaths))

nipop <- read_excel(nipopraw, sheet="Tabular (Age_5)", range="D16:BB35") %>% 
  select(age_5, as.character(2001:2010)) %>% 
  gather(Year, pop, c(2:11)) %>% 
  mutate(Age=case_when(
    age_5 %in% c("00-04", "05-09", "10-14", "15-19", "20-24") ~ "Under 25",
    age_5 %in% c("25-29", "30-34") ~ "25-34",
    age_5 %in% c("35-39", "40-44") ~ "35-44",
    age_5 %in% c("45-49", "50-54") ~ "45-54",
    age_5 %in% c("55-59", "60-64") ~ "55-64",
    age_5 %in% c("65-69", "70-74") ~ "65-74",
    TRUE ~ "75 and over")) %>% 
  group_by(Year, Age) %>% 
  summarise(pop=sum(pop), .groups="drop")

nidata_age <- nidata_age_old %>% 
  merge(nipop) %>% 
  mutate(mortrate=Deaths*100000/pop) %>% 
  select(Year, Age, mortrate) %>% 
  bind_rows(nidata_age_new) %>% 
  mutate(Age=factor(Age, levels=c("Under 25", "25-34", "35-44", "45-54", "55-64", "65-74", 
                                  "75 and over"))) %>% 
  arrange(Age, Year) %>% 
  #mutate(index=c(1:21, 23:148))
  mutate(index=c(1:21, 23:43, 45:65, 67:87, 89:109, 111:131, 133:153))

#Grouped path plot
#grouped path of DRD
x1 <- c(0, nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="Under 25" & nidata_age$Year==2021],
        0)

x2 <- c(0, nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="25-34" & nidata_age$Year==2021],
        0)
x3 <- c(0, nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="35-44" & nidata_age$Year==2021],
        0)

x4 <- c(0, nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="45-54" & nidata_age$Year==2021],
        0)

x5 <- c(0, nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="55-64" & nidata_age$Year==2021],
        0)

x6 <- c(0, nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="65-74" & nidata_age$Year==2021],
        0)

x7 <- c(0, nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2001],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2002],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2003],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2004],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2005],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2006],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2007],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2008],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2009],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2010],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2011],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2012],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2013],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2014],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2015],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2016],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2017],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2018],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2019],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2020],
        nidata_age$mortrate[nidata_age$Age=="75 and over" & nidata_age$Year==2021],
        0)

ASDplot <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), y=x1), fill="SkyBlue")+
  geom_polygon(aes(x=c(23, 23:43, 43), y=x2), fill="SkyBlue")+
  geom_polygon(aes(x=c(45, 45:65, 65), y=x3), fill="SkyBlue")+
  geom_polygon(aes(x=c(67, 67:87, 87), y=x4), fill="SkyBlue")+
  geom_polygon(aes(x=c(89, 89:109, 109), y=x5), fill="SkyBlue")+
  geom_polygon(aes(x=c(111, 111:131, 131), y=x6), fill="SkyBlue")+
  geom_polygon(aes(x=c(133, 133:153, 153), y=x7), fill="SkyBlue")+
  geom_path(data=nidata_age, aes(x=index, y=mortrate, group=Age), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_custom()+
  scale_x_continuous(breaks=c(10,33,55,77,99, 121, 143), 
                     labels=c("under 25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
                     name="Age")+
  scale_y_continuous(name="Annual alcohol-specific deaths per 100,000")+
  labs(title="Recent rises in alcohol-specific deaths in NI are largely in the over 55s",
       subtitle="Annual rates of alcohol-specific deaths by age group in Northern Ireland 2001-2021",
       caption="Data from NISRA | Plot by @VictimOfMaths")

ASDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1:21), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDfull <- ggdraw()+
  draw_plot(ASDplot)+
  draw_plot(ASDinset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10)+
  draw_label("2021", x=0.27, y=0.66, size=10)+
  draw_label("Key", x=0.18, y=0.85, size=10, fontface="bold")

tiff("Outputs/ASDNI2022.tiff", units="in", width=9, height=6.6, res=500)
ggdraw(ASDfull)
dev.off()

##################################
#Comparison of UK nations in age-standardised ASD rates
#UK data up to 2020
temp <- tempfile()
url4 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsintheukmaindataset/current/alcoholspecificdeaths2020.xlsx"
ukraw <- curl_download(url=url4, destfile=temp, quiet=FALSE, mode="wb")

ukdata <- read_excel(ukraw, sheet="Table 1 data", range="A1:F901") %>% 
  set_names("Code", "Area", "Sex", "Year", "Deaths", "ASrate")

#NI data for 2021
nidata <- read_excel(niraw, sheet="Table 2", range="B29:M31", col_names=FALSE) %>% 
  select(`...1`, `...12`) %>% 
  set_names("Sex", "ASrate") %>%
  mutate(Area="Northern Ireland", Year=2021, Sex=c("Persons", "Males", "Females"))

#Scottish data for 2021
temp <- tempfile()
url5 <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2021/alcohol-specific-deaths-21-all-tabs.xlsx"
scotraw <- curl_download(url=url5, destfile=temp, quiet=FALSE, mode="wb")

scotdata <- read_excel(scotraw, sheet="Table_1", range="A5:P48") %>% 
  select(Year, `Age-standardised mortality rate\r\nPersons`, 
         `Age-standardised mortality rate\r\nFemales`,
         `Age-standardised mortality rate\r\nMales`) %>% 
  set_names("Year", "Persons", "Females", "Males") %>% 
  gather(Sex, ASrate, c(2:4)) %>% 
  filter(Year==2021) %>% 
  mutate(Area="Scotland")
  
ukdata_full <- ukdata %>% 
  bind_rows(nidata, scotdata) 

#Population plot
tiff("Outputs/ASDxCountry22.tiff", units="in", width=9, height=6.6, res=600)
ukdata_full %>% 
  filter(Area %in% c("England", "Wales", "Scotland", "Northern Ireland") & Sex=="Persons") %>% 
  mutate(Area=factor(Area, levels=c("Scotland", "Northern Ireland", "Wales", "England"))) %>% 
ggplot(aes(x=Year, y=ASrate, colour=Area))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_colour_paletteer_d("khroma::vibrant", name="")+
  theme_custom()+
  labs(title="Scotland & Northern Ireland have the highest alcohol-specific death rates",
       subtitle="Age-standardised mortality rates from causes that are only attributable to alcohol",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()

#Plot by sex
tiff("Outputs/ASDxCountryxSex22.tiff", units="in", width=10, height=6.6, res=600)
ukdata_full %>% 
  filter(Area %in% c("England", "Wales", "Scotland", "Northern Ireland") & Sex!="Persons") %>% 
  mutate(Area=factor(Area, levels=c("Scotland", "Northern Ireland", "Wales", "England"))) %>% 
  ggplot(aes(x=Year, y=ASrate, colour=Area))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000", limits=c(0,NA))+
  scale_colour_paletteer_d("khroma::vibrant", name="")+
  theme_custom()+
  facet_wrap(~Sex)+
  labs(title="Scotland & Northern Ireland have the highest alcohol-specific death rates",
       subtitle="Age-standardised mortality rates from causes that are only attributable to alcohol",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()
