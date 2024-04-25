rm(list=ls())

#library(remotes)
#install_github("timriffe/TR1/TR1/HMDHFDplus")

library(curl)
library(readxl)
library(keyring)
library(tidyverse)
library(HMDHFDplus)
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
          strip.clip="off",
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

#Read in cause-specific mortality data for England by age from ONS
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1355deathsbysexsingleyearofageunderlyingcauseicd10codeanddeprivationdecileengland2001to2022/deathsbyimd20012022final.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

rawpersons <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                         sheet="1", range=cell_limits(c(6,1), c(567283, 25))) %>% 
  mutate(Sex="Total")

rawmale <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                      sheet="2", range=cell_limits(c(6,1), c(415178, 25))) %>% 
  mutate(Sex="Male")

rawfemale <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                        sheet="3", range=cell_limits(c(6,1), c(379521, 25))) %>% 
  mutate(Sex="Female")

rawdata <- bind_rows(rawpersons, rawmale, rawfemale) %>% 
  gather(Year, Deaths, c(4:25)) %>% 
  mutate(Year=as.numeric(Year),
         IMD=11-`IMD decile`,
         IMD=case_when(
           IMD=="1" ~ "1 (least deprived)",
           IMD=="10" ~ "10 (most deprived)",
           TRUE ~ as.character(IMD)),
         IMD=factor(IMD, levels=c("1 (least deprived)", "2", "3", "4", "5", "6",
                                  "7", "8", "9", "10 (most deprived)")))

ASdata <- rawdata %>% 
  mutate(Cause=if_else(`ICD-10 code` %in% c("E244", "G312", "G621", "G721", "I426",
                                            "K292", "K852", "K860", "Q860", "R780") |
                         substr(`ICD-10 code`,1,3) %in% c("F10", "K70", "X45", "X65", "Y15"),
                       "Alcohol", "Other")) %>% 
  group_by(Age, Sex, IMD, Cause, Year) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  spread(Age, Deaths) %>% 
  mutate(across(.cols=c(`0`:`118`), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(Age, Deaths, c(`0`:`118`)) %>% 
  mutate(Age=as.numeric(Age))

agg_png("Outputs/ASDLexissxIMD.png", units="in", width=12, height=6, res=500)
ggplot(ASdata %>% filter(Sex=="Total" & Cause=="Alcohol"), aes(x=Year, y=Age, fill=Deaths))+
  geom_tile()+
  facet_grid(~IMD)+
  scale_x_continuous(name="", breaks=c(2005, 2015))+
  scale_fill_paletteer_c("viridis::turbo", name="Annual deaths")+
  coord_equal()+
  theme_custom()+
  theme(axis.line=element_blank())+
  labs(title="The burden of alcohol deaths falls disproportionately on the most vulnerable",
       subtitle="Rates of mortality from conditions only caused by alcohol in England by decile of the Index of Multiple Deprivation\n" ,
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

ASdata %>% group_by(Age, Sex, IMD, Year) %>% 
  mutate(ASProp=Deaths[Cause=="Alcohol"]/sum(Deaths)) %>% 
  ungroup() %>% 
  filter(ASProp<0.3) %>% 
  ggplot(aes(x=Year, y=Age, fill=ASProp))+
  geom_tile()+
  facet_grid(~IMD)+
  scale_fill_paletteer_c("viridis::turbo")+
  coord_equal()+
  theme_custom()

shortdata <- ASdata %>% 
  group_by(Year, Sex, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")

#Bring in ONS population estimates (which is unreasonably hard for some reason)
#Not separated by IMD decile
#2001-2020
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2020detailedtimeseries/ukdetailedtimeseries2001to2020.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop0120 <- read.csv(file.path(temp2, "MYEB2_detailed_components_of_change_series_EW_(2020_geog21).csv")) %>% 
  select(c(1:25)) %>% 
  gather(Year, Pop, c(6:25)) %>% 
  mutate(Year=as.numeric(substr(Year, 12,15))) %>% 
  filter(country=="E") %>% 
  group_by(age, sex, Year) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  mutate(sex=if_else(sex==1, "Male", "Female")) %>% 
  rename("Sex"="sex", "Age"="age")

#2021
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2021/ukpopestimatesmid2021on2021geographyfinal.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop21 <- bind_rows(read_excel(temp, sheet="MYE2 - Males", range="E12:CQ12",
                              col_names=FALSE) %>% 
                     set_names(0:90) %>% 
                     mutate(Sex="Male", Year=2021) %>% 
                     gather(Age, Pop, c(1:91)),
                   read_excel(temp, sheet="MYE2 - Females", range="E12:CQ12",
                              col_names=FALSE) %>% 
                     set_names(0:90) %>% 
                     mutate(Sex="Female", Year=2021) %>% 
                     gather(Age, Pop, c(1:91))) %>% 
  mutate(Age=as.numeric(Age))

#2022
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2022/mye22final.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop22 <- bind_rows(read_excel(temp, sheet="MYE2 - Males", range="E12:CQ12",
                              col_names=FALSE) %>% 
                     set_names(0:90) %>% 
                     mutate(Sex="Male", Year=2022) %>% 
                     gather(Age, Pop, c(1:91)),
                   read_excel(temp, sheet="MYE2 - Females", range="E12:CQ12",
                              col_names=FALSE) %>% 
                     set_names(0:90) %>% 
                     mutate(Sex="Female", Year=2022) %>% 
                     gather(Age, Pop, c(1:91))) %>% 
  mutate(Age=as.numeric(Age))

#Combine and add totals
Pop <- bind_rows(pop0120, pop21, pop22) %>% 
  spread(Sex, Pop) %>% 
  mutate(Total=Male+Female) %>% 
  gather(Sex, Pop, c(3:5))

#Aggregate deaths data across IMD deciles, merge in pops and calculate rates
AllASData <- ASdata %>% 
  mutate(Age=if_else(Age<90, Age, 90)) %>% 
  group_by(Sex, Year, Cause, IMD, Age) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  group_by(Age, Sex, Year, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  merge(Pop) %>% 
  mutate(mx=Deaths*100000/Pop)

agg_png("Outputs/ASDEngLexis.png", units="in", width=7, height=9, res=800)
ggplot(AllASData %>% filter(Cause=="Alcohol" & Sex!="Total"),
       aes(x=Year, y=Age, fill=mx))+
  geom_tile()+
  scale_x_continuous(name="", breaks=c(2000, 2010, 2020))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_wrap(~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.line=element_blank())+
  labs(title="Alcohol-specific deaths have risen across 50 & 60 year olds",
       subtitle="Mortality rates from conditions which are only caused by alcohol in England 2001-2022\n ",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
  
#Analyse by cohort
Cohorts <- AllASData %>% 
  mutate(BirthYear=Year-Age,
         Cohort=case_when(
           BirthYear %in% c(1913:1917) ~ "1913-17",
           BirthYear %in% c(1918:1927) ~ "1918-22",
           BirthYear %in% c(1923:1927) ~ "1923-27",
           BirthYear %in% c(1928:1937) ~ "1928-32",
           BirthYear %in% c(1933:1937) ~ "1933-37",
           BirthYear %in% c(1938:1942) ~ "1938-42",
           BirthYear %in% c(1943:1947) ~ "1943-47",
           BirthYear %in% c(1948:1952) ~ "1948-52",
           BirthYear %in% c(1953:1957) ~ "1953-57",
           BirthYear %in% c(1958:1962) ~ "1958-62",
           BirthYear %in% c(1963:1967) ~ "1963-67",
           BirthYear %in% c(1968:1972) ~ "1968-72",
           BirthYear %in% c(1973:1977) ~ "1973-77",
           BirthYear %in% c(1978:1982) ~ "1978-82",
           BirthYear %in% c(1983:1987) ~ "1983-87",
           BirthYear %in% c(1988:1992) ~ "1988-92",
           BirthYear %in% c(1993:1997) ~ "1993-97",
           BirthYear %in% c(1998:2002) ~ "1998-2002",
           BirthYear %in% c(2003:2007) ~ "2003-07",
           BirthYear %in% c(2008:2012) ~ "2008-12",
           BirthYear %in% c(2013:2017) ~ "2013-17",
           BirthYear %in% c(2018:2022) ~ "2018-22"),
         CohortAge=Year-as.numeric(substr(Cohort,1,4))-3) %>% 
  group_by(Sex, Cohort, CohortAge, Year, Cause) %>% 
  summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
  mutate(mx=Deaths*100000/Pop)
  
agg_png("Outputs/ASDEngCohorts.png", units="in", width=11, height=6, res=800)
ggplot(Cohorts %>% filter(Cause=="Alcohol" & Sex!="Total" & !is.na(Cohort) & CohortAge>=18,
                            !Cohort %in% c("2003-07", "2008-12", "2013-17", "2018-22")), 
       aes(x=CohortAge, y=mx, colour=Cohort))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  facet_wrap(~Sex)+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000")+
  scale_colour_paletteer_d("ggthemes::Hue_Circle")+
  theme_custom()+
  theme(axis.line.x=element_blank())

dev.off()

agg_png("Outputs/ASDEngCohortsPandemic.png", units="in", width=11, height=6, res=800)
ggplot(Cohorts %>% filter(Cause=="Alcohol" & Sex=="Total" & !is.na(Cohort) & CohortAge>=18 & CohortAge<90 &
                          !Cohort %in% c("2003-07", "2008-12", "2013-17", "2018-22")), 
       aes(x=CohortAge, y=mx, colour=Cohort))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(alpha=0.3)+
  geom_path(data=. %>% filter(Year>=2019), alpha=1,
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")),
            show.legend = FALSE)+
  #facet_wrap(~Sex)+
  scale_x_continuous(name="Age", breaks=c(20, 30, 40, 50, 60, 70, 80))+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000")+
  scale_colour_paletteer_d("ggthemes::Hue_Circle", name="Birth\ncohort")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="The pandemic has driven rises in alcohol deaths across all cohorts",
       subtitle="Mortality rates from conditions that are only caused by alcohol by birth cohort. Bold lines represent changes since 2019.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()






