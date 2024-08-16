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
library(gt)
library(MortalitySmooth)
library(RcppRoll)

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
data.21 <- read_excel(rawfile, sheet="21", range="A5:E19157")
data.22 <- read_excel(rawfile, sheet="22", range="A5:E19483")

ewdata <- bind_rows(data.01, data.02, data.03, data.04, data.05, data.06, data.07,
                    data.08, data.09, data.10, data.11, data.12, data.13, data.14, 
                    data.15, data.16, data.17, data.18, data.19, data.20, data.21,
                    data.22) %>% 
  set_names("ICD10", "Year", "Sex", "Age", "Deaths") %>% 
  #Allocate causes to code groups
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)), 
         code3=as.numeric(substr(ICD10,4,4)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="Y" & code2==15 ~ "Alcohol", #Difference from the Masters defns as Y15 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",           
           code1=="Y" & code2 %in% c(10:14) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           TRUE ~ "Other"),
         Age=case_when(
           Age %in% c("neonates", "neonatal", "Neonates", "<1") ~ "<1",
           TRUE ~ Age)) %>% 
  #Collapse into cause groups
  group_by(Year, Age, Sex, Cause) %>% 
  summarise(Dx=sum(Deaths), .groups="drop")

#Pull out alcohol data specifically for validation at detailed ICD-10 code level
ewalcvalidate <- bind_rows(data.01, data.02, data.03, data.04, data.05, data.06, data.07,
                           data.08, data.09, data.10, data.11, data.12, data.13, data.14, 
                           data.15, data.16, data.17, data.18, data.19, data.20, data.21,
                           data.22) %>% 
  set_names("ICD10", "Year", "Sex", "Age", "Deaths") %>% 
  #Allocate causes to code groups
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)), 
         code3=as.numeric(substr(ICD10,4,4))) %>% 
  filter(code1=="K" & code2 %in% c(70, 73, 74) | code1=="F" & code2==10 | code1=="X" & code2==45 |
           code1=="Y" & code2==15) %>% 
  mutate(ICD10group=paste0(code1, code2)) %>% 
  group_by(Year, Age, Sex, ICD10group) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")

#Set up framework
years <- length(unique(ewdata$Year))
ages <- length(unique(ewdata$Age))
causes <- length(unique(ewdata$Cause))
frame <- data.frame(Year=rep(2001:(2001+years-1), times=1, each=2*ages*causes),
                    Sex=rep(1:2, times=years, each=ages*causes),
                    Age=rep(unique(ewdata$Age), times=2*years, each=causes),
                    Cause=rep(unique(ewdata$Cause), times=2*years*ages, each=1))

#Widen for smoothing
ewdata.wide <- ewdata %>% 
  merge(frame, all.y=TRUE) %>% 
  mutate(Dx=replace_na(Dx, 0)) %>% 
  spread(Year, Dx) 

#Add all-cause deaths group
ewdata.wide <- ewdata.wide %>% 
  group_by(Age, Sex) %>% 
  summarise(across(c(`2001`:`2022`), sum)) %>% 
  ungroup() %>% 
  mutate(Cause="Total") %>% 
  bind_rows(ewdata.wide)%>% 
  #Initiate start of age groups
  mutate(agestart=case_when(
    Age %in% c("neonatal", "neonates", "Neonates", "<1") ~ 0,
    Age=="01-04" ~ 1, Age=="05-09" ~ 5, Age=="10-14" ~ 10, Age=="15-19" ~ 15,
    Age=="20-24" ~ 20, Age=="25-29" ~ 25, Age=="30-34" ~ 30, Age=="35-39" ~ 35,
    Age=="40-44" ~ 40, Age=="45-49" ~ 45, Age=="50-54" ~ 50, Age=="55-59" ~ 55,
    Age=="60-64" ~ 60, Age=="65-69" ~ 65, Age=="70-74" ~ 70, Age=="75-79" ~ 75,
    Age=="80-84" ~ 80, TRUE ~ 85),
    Age=case_when(
      Age=="<1" ~ "0", Age=="01-04" ~ "1-4", TRUE ~ Age)) %>% 
  arrange(agestart) %>% 
  relocate(Cause, agestart)

#Download populations/exposures from HMD
#Note that you will need to register with mortality.org and set this 
#username and password up with {keyring} for this to work
ewpop <- readHMDweb(CNTRY="GBRTENW", "Population", key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) %>% 
  filter(Year>=2001) 

ewpop <- bind_rows(ewpop %>% filter(Year==2021) %>% 
                     select("Year", "Age", "Male2", "Female2") %>% 
                     mutate(Year=2022) %>% 
                     set_names(c("Year", "Age", "Male", "Female")),
                   ewpop %>% select(c("Year", "Age", "Male1", "Female1")) %>% 
                     set_names(c("Year", "Age", "Male", "Female"))) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2)) 

#Group populations to match deaths age groups
ewpop.grouped <- ewpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2022`, \(x) sum(x, na.rm = TRUE))) %>% 
  ungroup()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "username", "password", 
                        "font", "theme_custom", "ewalcvalidate")))

##########
#Scotland#
##########
#Get Scottish data
scotfile.2023 <- tempfile()
scoturl.2023 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2023/vital-events-23-ref-tabs-6.xlsx"
scotfile.2023 <- curl_download(url=scoturl.2023, destfile=scotfile.2023, quiet=FALSE, mode="wb")

scotdata.2023 <- read_excel(scotfile.2023, sheet="6.04", range=c("B9:Y1729"), col_names=FALSE) %>% 
  mutate(Year=2023)

scotfile.2022 <- tempfile()
scoturl.2022 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2022/vital-events-22-ref-tabs-6.xlsx"
scotfile.2022 <- curl_download(url=scoturl.2022, destfile=scotfile.2022, quiet=FALSE, mode="wb")

scotdata.2022 <- read_excel(scotfile.2022, sheet="6.04", range=c("B10:Y1730"), col_names=FALSE) %>% 
  mutate(Year=2022)

scotfile.2021 <- tempfile()
scoturl.2021 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2021/vital-events-21-ref-tabs-6.xlsx"
scotfile.2021 <- curl_download(url=scoturl.2021, destfile=scotfile.2021, quiet=FALSE, mode="wb")

scotdata.2021 <- read_excel(scotfile.2021, sheet="6.04", range=c("B10:Y1726"), col_names=FALSE) %>% 
  mutate(Year=2021)

scotfile.2020 <- tempfile()
scoturl.2020 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2020/vital-events-20-ref-tabs-6.xlsx"
scotfile.2020 <- curl_download(url=scoturl.2020, destfile=scotfile.2020, quiet=FALSE, mode="wb")

scotdata.2020 <- read_excel(scotfile.2020, sheet="6.04", range=c("A9:X1789"), col_names=FALSE) %>% 
  mutate(Year=2020)

scotfile.2019 <- tempfile()
scoturl.2019 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2019/vital-events-19-ref-tabs-6.xlsx"
scotfile.2019 <- curl_download(url=scoturl.2019, destfile=scotfile.2019, quiet=FALSE, mode="wb")

scotdata.2019 <- read_excel(scotfile.2019, sheet="6.04", range=c("A9:X1739"), col_names=FALSE) %>% 
  mutate(Year=2019)

scotfile.2018 <- tempfile()
scoturl.2018 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2018/vital-events-18-ref-tabs-6.xlsx"
scotfile.2018 <- curl_download(url=scoturl.2018, destfile=scotfile.2018, quiet=FALSE, mode="wb")

scotdata.2018 <- read_excel(scotfile.2018, sheet="6.04", range=c("A9:X1698"), col_names=FALSE) %>% 
  mutate(Year=2018)

scotfile.2017 <- tempfile()
scoturl.2017 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2017/vital-events-17-ref-tabs-6-corrected.xlsx"
scotfile.2017 <- curl_download(url=scoturl.2017, destfile=scotfile.2017, quiet=FALSE, mode="wb")

scotdata.2017 <- read_excel(scotfile.2017, sheet="6.04", range=c("A9:X1767"), col_names=FALSE) %>% 
  mutate(Year=2017)

scotfile.2016 <- tempfile()
scoturl.2016 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/16/6-d-cause/ve-ref-tabs-16-tab6.04.xlsx"
scotfile.2016 <- curl_download(url=scoturl.2016, destfile=scotfile.2016, quiet=FALSE, mode="wb")

scotdata.2016 <- read_excel(scotfile.2016, sheet="6.04", range=c("A9:X1776"), col_names=FALSE) %>% 
  mutate(Year=2016)

scotfile.2015 <- tempfile()
scoturl.2015 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2015/section6/15-vital-events-ref-tabs-6-4.xlsx"
scotfile.2015 <- curl_download(url=scoturl.2015, destfile=scotfile.2015, quiet=FALSE, mode="wb")

scotdata.2015 <- read_excel(scotfile.2015, sheet="6.4", range=c("A9:W1784"), col_names=FALSE) %>% 
  mutate(Year=2015)

scotfile.2014 <- tempfile()
scoturl.2014 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2014/section-6/14-vital-events-ref-tabs-6-4.xlsx"
scotfile.2014 <- curl_download(url=scoturl.2014, destfile=scotfile.2014, quiet=FALSE, mode="wb")

scotdata.2014 <- read_excel(scotfile.2014, sheet="6.4", range=c("A9:W1683"), col_names=FALSE) %>% 
  mutate(Year=2014)

scotfile.2013 <- tempfile()
scoturl.2013 <- "https://www.nrscotland.gov.uk/files//statistics/ve-ref-tables-2013/2013-ref-tabs-6-4.xls"
scotfile.2013 <- curl_download(url=scoturl.2013, destfile=scotfile.2013, quiet=FALSE, mode="wb")

scotdata.2013 <- read_excel(scotfile.2013, sheet="6.4", range=c("A9:W1741"), col_names=FALSE) %>% 
  mutate(Year=2013)

scotfile.2012 <- tempfile()
scoturl.2012 <- "https://www.nrscotland.gov.uk/files/statistics/ve-ref-tables-2012/ve-12-t6-4.xls"
scotfile.2012 <- curl_download(url=scoturl.2012, destfile=scotfile.2012, quiet=FALSE, mode="wb")

scotdata.2012 <- read_excel(scotfile.2012, sheet="6.4", range=c("A9:W1736"), col_names=FALSE) %>% 
  mutate(Year=2012)

scotfile.2011 <- tempfile()
scoturl.2011 <- "https://www.nrscotland.gov.uk/files/statistics/ve-reftables-2011/ve-2011-t6.4.xls"
scotfile.2011 <- curl_download(url=scoturl.2011, destfile=scotfile.2011, quiet=FALSE, mode="wb")

scotdata.2011 <- read_excel(scotfile.2011, sheet="6.4", range=c("A9:W1791"), col_names=FALSE) %>% 
  mutate(Year=2011)

scotfile.2010 <- tempfile()
scoturl.2010 <- "https://www.nrscotland.gov.uk/files/statistics/ve-reftables-2010/ve10-t6-4.xls"
scotfile.2010 <- curl_download(url=scoturl.2010, destfile=scotfile.2010, quiet=FALSE, mode="wb")

scotdata.2010 <- read_excel(scotfile.2010, range=c("A9:W1779"), col_names=FALSE) %>% 
  mutate(Year=2010)

scotfile.2009 <- tempfile()
scoturl.2009 <- "https://www.nrscotland.gov.uk/files/statistics/ve-reftables-09/ve09-t6-4.xls"
scotfile.2009 <- curl_download(url=scoturl.2009, destfile=scotfile.2009, quiet=FALSE, mode="wb")

scotdata.2009 <- read_excel(scotfile.2009, range=c("A9:W1794"), col_names=FALSE) %>% 
  mutate(Year=2009)

scotfile.2008 <- tempfile()
scoturl.2008 <- "https://www.nrscotland.gov.uk/files/statistics/vital-events-ref-tables-2008/ve-2008-t6-4.xls"
scotfile.2008 <- curl_download(url=scoturl.2008, destfile=scotfile.2008, quiet=FALSE, mode="wb")

scotdata.2008 <- read_excel(scotfile.2008, range=c("A9:W1811"), col_names=FALSE) %>% 
  mutate(Year=2008)

scotfile.2007 <- tempfile()
scoturl.2007 <- "https://www.nrscotland.gov.uk/files/statistics/07t6-4.xls"
scotfile.2007 <- curl_download(url=scoturl.2007, destfile=scotfile.2007, quiet=FALSE, mode="wb")

scotdata.2007 <- read_excel(scotfile.2007, range=c("A9:W1867"), col_names=FALSE) %>% 
  mutate(Year=2007)

scotfile.2006 <- tempfile()
scoturl.2006 <- "https://www.nrscotland.gov.uk/files/statistics/06t6-4%20rev.xls"
scotfile.2006 <- curl_download(url=scoturl.2006, destfile=scotfile.2006, quiet=FALSE, mode="wb")

scotdata.2006 <- read_excel(scotfile.2006, range=c("A8:W1984"), col_names=FALSE) %>% 
  mutate(Year=2006)

scotfile.2005 <- tempfile()
scoturl.2005 <- "https://www.nrscotland.gov.uk/files/statistics/old/05t6-4.xls"
scotfile.2005 <- curl_download(url=scoturl.2005, destfile=scotfile.2005, quiet=FALSE, mode="wb")

scotdata.2005 <- read_excel(scotfile.2005, range=c("A9:X2017"), col_names=FALSE) %>% 
  mutate(Year=2005)

scotfile.2004 <- tempfile()
scoturl.2004 <- "https://www.nrscotland.gov.uk/files/statistics/old/04t6-4.xls"
scotfile.2004 <- curl_download(url=scoturl.2004, destfile=scotfile.2004, quiet=FALSE, mode="wb")

scotdata.2004 <- read_excel(scotfile.2004, range=c("A9:X2033"), col_names=FALSE) %>% 
  mutate(Year=2004)

scotfile.2003 <- tempfile()
scoturl.2003 <- "https://www.nrscotland.gov.uk/files/statistics/old/03t6-4.xls"
scotfile.2003 <- curl_download(url=scoturl.2003, destfile=scotfile.2003, quiet=FALSE, mode="wb")

scotdata.2003 <- read_excel(scotfile.2003, range=c("A9:X2081"), col_names=FALSE) %>% 
  mutate(Year=2003)

scotfile.2002 <- tempfile()
scoturl.2002 <- "https://www.nrscotland.gov.uk/files/statistics/old/02t6-4.xls"
scotfile.2002 <- curl_download(url=scoturl.2002, destfile=scotfile.2002, quiet=FALSE, mode="wb")

scotdata.2002 <- read_excel(scotfile.2002, range=c("A9:X2067"), col_names=FALSE) %>% 
  mutate(Year=2002)

scotfile.2001 <- tempfile()
scoturl.2001 <- "https://www.nrscotland.gov.uk/files/statistics/old/01t6_4.xls"
scotfile.2001 <- curl_download(url=scoturl.2001, destfile=scotfile.2001, quiet=FALSE, mode="wb")

scotdata.2001 <- read_excel(scotfile.2001, range=c("A9:X2056"), col_names=FALSE) %>% 
  mutate(Year=2001)

#Bring together older data, with infuriatingly slightly different formatting
scotdata <- bind_rows(scotdata.2001, scotdata.2002, scotdata.2003, scotdata.2004, scotdata.2005) %>% 
  filter(!is.na(`...4`)) %>% 
  rename(ICD10=`...1`, Sex=`...4`) %>% 
  gather(Age, Dx, c(6:24)) %>% 
  fill(ICD10) %>% 
  filter(nchar(ICD10)<=3) %>% 
  #Stupid faff because of *horrible* formatting choices in the data
  mutate(`...5`=as.numeric(`...5`)) %>% 
  arrange(Year, Sex, Age, ICD10, `...5`) %>% 
  distinct(Year, Sex, Age, ICD10, .keep_all=TRUE) %>% 
  select(-c(`...2`, `...3`, `...5`)) %>% 
  mutate(Dx=as.numeric(if_else(Dx %in% c("-", ".", NA), "0", Dx)),
         Age=case_when(Age=="...6" ~ "0", Age=="...7" ~ "1-4",Age=="...8" ~ "5-9",
                       Age=="...9" ~ "10-14", Age=="...10" ~ "15-19", Age=="...11" ~ "20-24",
                       Age=="...12" ~ "25-29", Age=="...13" ~ "30-34", Age=="...14" ~ "35-39",
                       Age=="...15" ~ "40-44", Age=="...16" ~ "45-49", Age=="...17" ~ "50-54",
                       Age=="...18" ~ "55-59", Age=="...19" ~ "60-64", Age=="...20" ~ "65-69",
                       Age=="...21" ~ "70-74", Age=="...22" ~ "75-79", Age=="...23" ~ "80-84",
                       Age=="...24" ~ "85+"),
         Sex=if_else(Sex=="M", 1, 2))

#Age groups are 90+ in 2016 onwards, but 85+ before then
scotdata <- bind_rows(scotdata.2006, scotdata.2007, scotdata.2008, scotdata.2009, scotdata.2010, 
                      scotdata.2011, scotdata.2012, scotdata.2013, scotdata.2014, scotdata.2015, 
                      scotdata.2016, scotdata.2017, scotdata.2018, scotdata.2019, scotdata.2020) %>% 
  filter(!is.na(`...3`)) %>% 
  rename(ICD10=`...1`, Sex=`...3`) %>% 
  gather(Age, Dx, c(5:23, 25)) %>% 
  fill(ICD10) %>% 
  filter(nchar(ICD10)<=3) %>% 
  #Stupid faff because of *horrible* formatting choices in the data
  mutate(`...4`=as.numeric(`...4`)) %>% 
  arrange(Year, Sex, Age, ICD10, `...4`) %>% 
  distinct(Year, Sex, Age, ICD10, .keep_all=TRUE) %>% 
  select(-c(`...2`, `...4`)) %>% 
  mutate(Dx=as.numeric(if_else(Dx %in% c("-", ".", NA), "0", Dx)),
         Age=case_when(Age=="...5" ~ "0", Age=="...6" ~ "1-4",Age=="...7" ~ "5-9",
                       Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                       Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                       Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                       Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                       Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                       TRUE ~ "85+"),
         Sex=if_else(Sex=="M", 1, 2)) %>% 
  #Add in 2021 data, which was formatted differently (yay)
  bind_rows(scotdata.2021 %>% 
              #Sort out first 26 rows which are extra borked for some reason
              slice_head(n=26) %>% 
              mutate(lag1=lag(`...1`, n=1), lag2=lag(`...2`, n=1),
                     `...1`=coalesce(`...1`, lag1), `...2`=coalesce(`...2`, lag2)) %>% 
              bind_rows(scotdata.2021 %>% slice_tail(n=(nrow(.)-26))) %>% 
              filter(!is.na(`...2`)) %>% 
              mutate(ICD10=substr(`...2`, 1, 3)) %>% 
              select(-`...4`) %>% 
              gather(Age, Dx, c(4:23)) %>% 
              select(Year, `...3`, Age, ICD10, Dx) %>% 
              rename(Sex=`...3`) %>% 
              mutate(Dx=replace_na(as.numeric(Dx),0),
                     Age=case_when(Age=="...5" ~ "0", Age=="...6" ~ "1-4",Age=="...7" ~ "5-9",
                                   Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                                   Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                                   Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                                   Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                                   Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                                   TRUE ~ "85+"),
                     Sex=if_else(Sex=="M", 1, 2))) %>% 
  #Add in 2022 & 2023 data, which was formatted differently again (double yay)
  bind_rows(bind_rows(scotdata.2022, scotdata.2023) %>% 
              filter(`...2`!="Total") %>% 
              rename(ICD10=`...2`, Sex=`...3`) %>% 
              gather(Age, Dx, c(5:24)) %>% 
              mutate(Dx=replace_na(as.numeric(Dx),0),
                     Age=case_when(Age=="...5" ~ "0", Age=="...6" ~ "1-4",Age=="...7" ~ "5-9",
                                   Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                                   Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                                   Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                                   Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                                   Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                                   TRUE ~ "85+"),
                     Sex=if_else(Sex=="M", 1, 2),
                     ICD10=substr(ICD10, 1, 3)) %>% 
              select(-c(`...1`, `...4`))) %>% 
  #Collapse into age groups we actually want
  group_by(Age, Sex, Year, ICD10) %>% 
  summarise(Dx=sum(Dx), .groups="drop") %>% 
  bind_rows(scotdata) %>% 
  #Remove stuborn codes that remain due to bad formatting in the data
  filter(!ICD10 %in% c("E90", "F99", "G99", "J99", "K93", "L99", "M99", "N99", "Y98")) %>% 
  filter(!(ICD10=="I99" & Year==2006)) %>% 
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="Y" & code2==15 ~ "Alcohol", #Difference from the Masters defns as Y15 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",
           code1=="Y" & code2 %in% c(10:14) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           TRUE ~ "Other")) %>% 
  group_by(Sex, Age, Cause, Year) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup()

#Add all-cause deaths group
scotdata.wide <- scotdata %>% 
  spread(Year, Dx) %>% 
  group_by(Age, Sex) %>% 
  summarise(across(c(`2001`:`2023`), sum)) %>% 
  ungroup() %>% 
  mutate(Cause="Total") %>% 
  bind_rows(scotdata %>% spread(Year, Dx) )%>% 
  #Initiate start of age groups
  mutate(agestart=case_when(
    Age=="0" ~ 0,
    Age=="1-4" ~ 1, Age=="5-9" ~ 5, Age=="10-14" ~ 10, Age=="15-19" ~ 15,
    Age=="20-24" ~ 20, Age=="25-29" ~ 25, Age=="30-34" ~ 30, Age=="35-39" ~ 35,
    Age=="40-44" ~ 40, Age=="45-49" ~ 45, Age=="50-54" ~ 50, Age=="55-59" ~ 55,
    Age=="60-64" ~ 60, Age=="65-69" ~ 65, Age=="70-74" ~ 70, Age=="75-79" ~ 75,
    Age=="80-84" ~ 80, TRUE ~ 85)) %>% 
  arrange(agestart) %>% 
  relocate(Cause, agestart)

#Download populations
scotpop <- readHMDweb(CNTRY="GBR_SCO", "Population",  key_list("mortality.org")[1,2], 
                      key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=FALSE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) %>% 
  filter(Year>=2001) 

scotpop <- bind_rows(scotpop %>% filter(Year==2021) %>% 
                       select("Year", "Age", "Male2", "Female2") %>% 
                       mutate(Year=2022) %>% 
                       set_names(c("Year", "Age", "Male", "Female")),
                     #recycle 2022 populations for 2023 until HMD produce 2023 pops
                     scotpop %>% filter(Year==2021) %>% 
                       select("Year", "Age", "Male2", "Female2") %>% 
                       mutate(Year=2023) %>% 
                       set_names(c("Year", "Age", "Male", "Female")),
                     scotpop %>% select("Year", "Age", "Male1", "Female1") %>% 
                       set_names(c("Year", "Age", "Male", "Female"))) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2))

#Group populations to match deaths age groups
scotpop.grouped <- scotpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2023`, sum, na.rm=TRUE)) %>% 
  ungroup()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "username", "password", "font", "theme_custom",
                        "cleanedpop", "ewalcvalidate")))

##################
#Northern Ireland#
##################
#Get NI data
nifile.2022 <- tempfile()
niurl.2022 <- "https://www.nisra.gov.uk/system/files/statistics/Section%206%20-%20Cause_Death_Tables_2022_revised_final.xlsx"
nifile.2022 <- curl_download(url=niurl.2022, destfile=nifile.2022, quiet=FALSE, mode="wb")

nidata.2022 <- read_excel(nifile.2022, sheet="Table 6.4", range=c("A7:V2010"), col_names=FALSE) %>% 
  mutate(Year=2022,
         #Formatting of various conditions is wonky and unhelpful
         `...1`=gsub("\\(G60) ", "\\(G60):", `...1`)) %>% 
  separate(`...1`, into=c("Condition", "Sex"), sep=":")%>% 
  mutate(Condition=gsub("\\(chickenpox", "chickenpox", Condition)) %>% 
  mutate(Condition=gsub("\\(HIV", "HIV", Condition)) %>% 
  mutate(Condition=gsub("\\(nodular", "nodular", Condition)) %>% 
  mutate(Condition=gsub("\\(primary", "primary", Condition)) %>% 
  mutate(Condition=gsub("\\(hyperthyroidism", "hyperthyroidism", Condition)) %>% 
  mutate(Condition=gsub("\\(affective", "affective", Condition)) %>% 
  mutate(Condition=gsub("\\(suicide", "suicide", Condition)) %>% 
  mutate(Condition=gsub("I06", "\\(I06", Condition)) %>% 
  mutate(Condition=gsub(" K20", "\\(K20)", Condition)) %>% 
  mutate(Condition=gsub("M00-M25", "\\(M00-M25", Condition),
         Sex=str_trim(Sex, side="left")) %>% 
  separate(Condition, c("Cause", "ICD10"), sep="\\(") %>% 
  mutate(ICD10=gsub("\\)", "", ICD10))

niallcause.2022 <- nidata.2022 %>% slice_head(n=2) 

nidata.2022 <- nidata.2022 %>% slice(3:nrow(.))

nifile.2021 <- tempfile()
niurl.2021 <- "https://www.nisra.gov.uk/system/files/statistics/Section%206%20-%20Cause_Death_Tables_2021_Final.xlsx"
nifile.2021 <- curl_download(url=niurl.2021, destfile=nifile.2021, quiet=FALSE, mode="wb")

nidata.2021 <- read_excel(nifile.2021, sheet="Table 6.4", range=c("A7:V1944"), col_names=FALSE) %>% 
  mutate(Year=2021,
         #Formatting of various conditions is wonky and unhelpful
         `...1`=gsub("\\(G60) ", "\\(G60):", `...1`)) %>% 
  separate(`...1`, into=c("Condition", "Sex"), sep=":")%>% 
  mutate(Condition=gsub("\\(chickenpox", "chickenpox", Condition)) %>% 
  mutate(Condition=gsub("\\(HIV", "HIV", Condition)) %>% 
  mutate(Condition=gsub("\\(nodular", "nodular", Condition)) %>% 
  mutate(Condition=gsub("\\(primary", "primary", Condition)) %>% 
  mutate(Condition=gsub("\\(hyperthyroidism", "hyperthyroidism", Condition)) %>% 
  mutate(Condition=gsub("\\(affective", "affective", Condition)) %>% 
  mutate(Condition=gsub("\\(suicide", "suicide", Condition)) %>% 
  mutate(Condition=gsub("I06", "\\(I06", Condition)) %>% 
  mutate(Condition=gsub(" K20", "\\(K20)", Condition)) %>% 
  mutate(Condition=gsub("M00-M25", "\\(M00-M25", Condition),
         Sex=str_trim(Sex, side="left")) %>% 
  separate(Condition, c("Cause", "ICD10"), sep="\\(") %>% 
  mutate(ICD10=gsub("\\)", "", ICD10))

niallcause.2021 <- nidata.2021 %>% slice_head(n=2) 

nidata.2021 <- nidata.2021 %>% slice(3:nrow(.))

nifile.2020 <- tempfile()
niurl.2020 <- "https://www.nisra.gov.uk/system/files/statistics/Section%206%20-%20Cause_Death_Tables_2020_Revised%20June%202022.xlsx"
nifile.2020 <- curl_download(url=niurl.2020, destfile=nifile.2020, quiet=FALSE, mode="wb")

nidata.2020 <- read_excel(nifile.2020, sheet="Table 6.4", range=c("A7:V1904"), col_names=FALSE) %>% 
  mutate(Year=2020,
         #Formatting of various conditions is wonky and unhelpful
         `...1`=gsub("\\(G60) ", "\\(G60):", `...1`)) %>% 
  separate(`...1`, into=c("Condition", "Sex"), sep=":")%>% 
  mutate(Condition=gsub("\\(chickenpox", "chickenpox", Condition)) %>% 
  mutate(Condition=gsub("\\(HIV", "HIV", Condition)) %>% 
  mutate(Condition=gsub("\\(nodular", "nodular", Condition)) %>% 
  mutate(Condition=gsub("\\(primary", "primary", Condition)) %>% 
  mutate(Condition=gsub("\\(hyperthyroidism", "hyperthyroidism", Condition)) %>% 
  mutate(Condition=gsub("\\(affective", "affective", Condition)) %>% 
  mutate(Condition=gsub("\\(suicide", "suicide", Condition)) %>% 
  mutate(Condition=gsub("I06", "\\(I06", Condition)) %>% 
  mutate(Condition=gsub(" K20", "\\(K20)", Condition)) %>% 
  mutate(Condition=gsub("M00-M25", "\\(M00-M25", Condition),
         Sex=str_trim(Sex, side="left")) %>% 
  separate(Condition, c("Cause", "ICD10"), sep="\\(")%>% 
  mutate(ICD10=gsub("\\)", "", ICD10))

niallcause.2020 <- nidata.2020 %>% slice_head(n=2) 

nidata.2020 <- nidata.2020 %>% slice(3:nrow(.))

nifile.2019 <- tempfile()
niurl.2019 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Cause_Death_Tables_2019.xlsx"
nifile.2019 <- curl_download(url=niurl.2019, destfile=nifile.2019, quiet=FALSE, mode="wb")

nidata.2019 <- read_excel(nifile.2019, sheet="Table 6.4", range=c("A8:X2950"), col_names=FALSE) %>% 
  mutate(Year=2019)

niallcause.2019 <- nidata.2019 %>% slice_head(n=2) 

nidata.2019 <- nidata.2019 %>% slice(4:nrow(.))

temp <- tempfile()
nifile.2018 <- tempfile()
niurl.2018 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/RG_tables_2018.zip"
temp <- curl_download(url=niurl.2018, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2018)

nidata.2018 <- read_excel(file.path(nifile.2018, "Section 6 - Cause of Death.xlsx"), sheet="Table 6.4", 
                          range=c("A8:X2882"), col_names=FALSE) %>% 
  mutate(Year=2018)

niallcause.2018 <- nidata.2018 %>% slice_head(n=2)

nidata.2018 <- nidata.2018 %>% slice(4:nrow(.))

nifile.2017 <- tempfile()
niurl.2017 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/RG_tables_2017.zip"
temp <- curl_download(url=niurl.2017, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2017)

nidata.2017 <- read_excel(file.path(nifile.2017, "CauseOfDeath_2017.xls"), sheet="Table 6.4", 
                          range=c("A8:X2817"), col_names=FALSE) %>% 
  mutate(Year=2017)

niallcause.2017 <- nidata.2017 %>% slice_head(n=2) 

nidata.2017 <- nidata.2017 %>% slice(3:nrow(.))

nifile.2016 <- tempfile()
niurl.2016 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/RG_tables_2016.zip"
temp <- curl_download(url=niurl.2016, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2016)

nidata.2016 <- read_excel(file.path(nifile.2016, "CauseOfDeath_2016.xls"), sheet="Table 6.4", 
                          range=c("A8:X2150"), col_names=FALSE) %>% 
  mutate(Year=2016)

niallcause.2016 <- nidata.2016 %>% slice_head(n=2)

nidata.2016 <- nidata.2016 %>% slice(4:nrow(.))

nifile.2015 <- tempfile()
niurl.2015 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2015.zip"
temp <- curl_download(url=niurl.2015, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2015)

nidata.2015 <- read_excel(file.path(nifile.2015, "Chpt6_2015.xls"), sheet="Table6.4", 
                          range=c("A6:X5623"), col_names=FALSE) %>% 
  mutate(Year=2015)

niallcause.2015 <- nidata.2015 %>% slice_head(n=2)

nidata.2015 <- nidata.2015 %>% slice(4:nrow(.))

nifile.2014 <- tempfile()
niurl.2014 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2014.zip"
temp <- curl_download(url=niurl.2014, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2014)

nidata.2014 <- read_excel(file.path(nifile.2014, "Chpt6_2014.xls"), sheet="Table6.4", 
                          range=c("A6:X5607"), col_names=FALSE) %>% 
  mutate(Year=2014)

niallcause.2014 <- nidata.2014 %>% slice_head(n=2) 

nidata.2014 <- nidata.2014 %>% slice(4:nrow(.))

nifile.2013 <- tempfile()
niurl.2013 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2013.zip"
temp <- curl_download(url=niurl.2013, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2013)

nidata.2013 <- read_excel(file.path(nifile.2013, "Chpt6_2013.xls"), sheet="Table 6.4", 
                          range=c("A8:X2540"), col_names=FALSE) %>% 
  mutate(Year=2013)

niallcause.2013 <- nidata.2013 %>% slice_head(n=2) 

nidata.2013 <- nidata.2013 %>% slice(4:nrow(.))

nifile.2012 <- tempfile()
niurl.2012 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2012.zip"
temp <- curl_download(url=niurl.2012, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2012)

nidata.2012 <- read_excel(file.path(nifile.2012, "Chpt6_2012.xls"), sheet="Table 6.4", 
                          range=c("A8:X2292"), col_names=FALSE) %>% 
  mutate(Year=2012)

niallcause.2012 <- nidata.2012 %>% slice_head(n=2)

nidata.2012 <- nidata.2012 %>% slice(4:nrow(.))

nifile.2011 <- tempfile()
niurl.2011 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2011.zip"
temp <- curl_download(url=niurl.2011, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2011)

nidata.2011 <- read_excel(file.path(nifile.2011, "Chpt6_2011.xls"), sheet="Table 6.4", 
                          range=c("A8:X2289"), col_names=FALSE) %>% 
  mutate(Year=2011)

niallcause.2011 <- nidata.2011 %>% slice_head(n=2) 

nidata.2011 <- nidata.2011 %>% slice(4:nrow(.))

nifile.2010 <- tempfile()
niurl.2010 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2010.zip"
temp <- curl_download(url=niurl.2010, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2010)

nidata.2010 <- read_excel(file.path(nifile.2010, "Chpt6_2010.xls"), sheet="Table 6.4", 
                          range=c("A9:X2075"), col_names=FALSE) %>% 
  mutate(Year=2010)

niallcause.2010 <- nidata.2010 %>% slice_head(n=2) 

nidata.2010 <- nidata.2010 %>% slice(4:nrow(.))

nifile.2009 <- tempfile()
niurl.2009 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2009.zip"
temp <- curl_download(url=niurl.2009, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2009)

nidata.2009 <- read_excel(file.path(nifile.2009, "Chpt6_2009.xls"), sheet="Table 6.4", 
                          range=c("A9:X2075"), col_names=FALSE) %>% 
  mutate(Year=2009)

niallcause.2009 <- nidata.2009 %>% slice_head(n=2) 

nidata.2009 <- nidata.2009 %>% slice(4:nrow(.))

nifile.2008 <- tempfile()
niurl.2008 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2008.zip"
temp <- curl_download(url=niurl.2008, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2008)

nidata.2008 <- read_excel(file.path(nifile.2008, "Chpt6_2008.xls"), sheet="Table 6.4", 
                          range=c("A9:X2100"), col_names=FALSE) %>% 
  mutate(Year=2008)

niallcause.2008 <- nidata.2008 %>% slice_head(n=2) 

nidata.2008 <- nidata.2008 %>% slice(4:nrow(.))

nifile.2007 <- tempfile()
niurl.2007 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2007.zip"
temp <- curl_download(url=niurl.2007, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2007)

nidata.2007 <- read_excel(file.path(nifile.2007, "Chpt6_2007.xls"), sheet="Table 6.4", 
                          range=c("A8:X2200"), col_names=FALSE) %>% 
  mutate(Year=2007)

niallcause.2007 <- nidata.2007 %>% slice_head(n=2) 

nidata.2007 <- nidata.2007 %>% slice(4:nrow(.))

nifile.2006 <- tempfile()
niurl.2006 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2006.zip"
temp <- curl_download(url=niurl.2006, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2006)

nidata.2006 <- read_excel(file.path(nifile.2006, "Chpt6_2006.xls"), sheet="Table 6.4", 
                          range=c("A8:X2200"), col_names=FALSE) %>% 
  mutate(Year=2006)

niallcause.2006 <- nidata.2006 %>% slice_head(n=2) 

nidata.2006 <- nidata.2006 %>% slice(4:nrow(.))

nifile.2005 <- tempfile()
niurl.2005 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2005.zip"
temp <- curl_download(url=niurl.2005, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2005)

nidata.2005 <- read_excel(file.path(nifile.2005, "Chpt6_2005.xls"), sheet="Table 6.4", 
                          range=c("A8:X2300"), col_names=FALSE) %>% 
  mutate(Year=2005)

niallcause.2005 <- nidata.2005 %>% slice_head(n=2) 

nidata.2005 <- nidata.2005 %>% slice(4:nrow(.))

nifile.2004 <- tempfile()
niurl.2004 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Tables_2004.zip"
temp <- curl_download(url=niurl.2004, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=nifile.2004)

nidata.2004 <- read_excel(file.path(nifile.2004, "AnnRep6_2004.xls"), sheet="Table 6.4", 
                          range=c("A8:X2300"), col_names=FALSE) %>% 
  mutate(Year=2004)

niallcause.2004 <- nidata.2004 %>% slice_head(n=2) 

nidata.2004 <- nidata.2004 %>% slice(4:nrow(.))

#2003 data and earlier is only available as pdfs
#https://www.nisra.gov.uk/publications/registrar-general-annual-reports-2001-2010
#Many thanks to Irina Kolegova for digitising these
niurl.0103 <- "https://github.com/VictimOfMaths/DeathsOfDespair/raw/master/Paper/NITables6_4_2001_2003.xlsx"
temp <- curl_download(url=niurl.0103, destfile=temp, quiet=FALSE, mode="wb")

nidata.2003 <- read_excel(temp, sheet="2003_no_sum", range=c("A6:X1295"), col_names=FALSE) %>% 
  mutate(Year=2003)

niallcause.2003 <- nidata.2003 %>% slice_head(n=2) 

nidata.2003 <- nidata.2003 %>% slice(3:nrow(.))

nidata.2002 <- read_excel(temp, sheet="2002_no_sum", range=c("A6:X1295"), col_names=FALSE) %>% 
  mutate(Year=2002)

niallcause.2002 <- nidata.2002 %>% slice_head(n=2) 

nidata.2002 <- nidata.2002 %>% slice(3:nrow(.))

nidata.2001 <- read_excel(temp, sheet="2001_no_sum", range=c("A6:X1258"), col_names=FALSE) %>% 
  mutate(Year=2001,
         across(c(4:24), as.character))

niallcause.2001 <- nidata.2001 %>% slice_head(n=2) 

nidata.2001 <- nidata.2001 %>% slice(3:nrow(.))

#Stitch them all together
nidata <- bind_rows(nidata.2001, nidata.2002, nidata.2003, nidata.2004, nidata.2005, nidata.2006, 
                    nidata.2007, nidata.2008, nidata.2009, nidata.2010, nidata.2011) %>% 
  fill(`...1`) %>% 
  filter(!is.na(`...4`)) %>% 
  select(-4) %>% 
  gather(Age, Dx, c(4:23)) %>% 
  mutate(Age=case_when(
    Age=="...5" ~ "0", Age=="...6" ~ "1-4", Age=="...7" ~ "5-9", Age=="...8" ~ "10-14",
    Age=="...9" ~ "15-19", Age=="...10" ~ "20-24", Age=="...11" ~ "25-29", Age=="...12" ~ "30-34",
    Age=="...13" ~ "35-39", Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
    Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69", Age=="...20" ~ "70-74",
    Age=="...21" ~ "75-79", Age=="...22" ~ "80-84", Age=="...23" ~ "85-89", Age=="...24" ~ "90+"),
    Dx=as.numeric(Dx), Dx=if_else(is.na(Dx), 0, Dx)) %>% 
  set_names(c("ICD10", "Cause", "Sex", "Year", "Age", "Dx")) %>% 
  bind_rows(bind_rows(nidata.2012, nidata.2013, nidata.2014, nidata.2015, nidata.2016,
                      nidata.2017 ,nidata.2018 ,nidata.2019)  %>% 
              fill(`...1`) %>% 
              filter(!is.na(`...4`)) %>% 
              select(-4) %>% 
              gather(Age, Dx, c(4:23)) %>% 
              mutate(Age=case_when(
                Age=="...5" ~ "0", Age=="...6" ~ "1-4", Age=="...7" ~ "5-9", Age=="...8" ~ "10-14",
                Age=="...9" ~ "15-19", Age=="...10" ~ "20-24", Age=="...11" ~ "25-29", Age=="...12" ~ "30-34",
                Age=="...13" ~ "35-39", Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69", Age=="...20" ~ "70-74",
                Age=="...21" ~ "75-79", Age=="...22" ~ "80-84", Age=="...23" ~ "85-89", Age=="...24" ~ "90+"),
                Dx=as.numeric(Dx), Dx=if_else(is.na(Dx), 0, Dx)) %>% 
              set_names(c("ICD10", "Cause", "Sex", "Year", "Age", "Dx"))) %>% 
  bind_rows(bind_rows(nidata.2020, nidata.2021, nidata.2022) %>% gather(Age, Dx, c(5:24)) %>% 
              select(-`...4`) %>% 
              mutate(Age=case_when(
                Age=="...5" ~ "0", Age=="...6" ~ "1-4", Age=="...7" ~ "5-9", Age=="...8" ~ "10-14",
                Age=="...9" ~ "15-19", Age=="...10" ~ "20-24", Age=="...11" ~ "25-29", Age=="...12" ~ "30-34",
                Age=="...13" ~ "35-39", Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69", Age=="...20" ~ "70-74",
                Age=="...21" ~ "75-79", Age=="...22" ~ "80-84", Age=="...23" ~ "85-89", Age=="...24" ~ "90+"),
                ICD10=gsub("\\)", "", ICD10))) %>% 
  #Fill down ICD-10 codes and remove rows we don't want (e.g. summary rows)
  filter(nchar(ICD10)<=3) %>% 
  filter(!Sex %in% c("P", "All")) %>% 
  filter(!ICD10 %in% c("B99", "D48", "D89", "E90", "F99", "G99", "I99", "J99", "K93", "L99", "M99",
                       "N99", "O99", "P96", "Q99", "R99", "Y98", "X59", "Y09")) %>% 
  #Assign codes to DoD groupings
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="Y" & code2==15 ~ "Alcohol", #Difference from the Masters defns as Y15 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",
           code1=="Y" & code2 %in% c(10:14) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           TRUE ~ "Other"),
         #FOR THE LOVE OF GOD HOW MANY WAYS TO WRITE "Female" ARE THERE?
         Sex=as.numeric(if_else(Sex %in% c("Female", "F", "Fales", "females", "FEMALES", "Females"), "2", "1")),
         Age=if_else(Age %in% c("85-89", "90+"), "85+", Age)) %>% 
  group_by(Sex, Age, Cause, Year) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup()

#Add all-cause total
niallcause <- bind_rows(niallcause.2001, niallcause.2002, niallcause.2003, niallcause.2004, 
                        niallcause.2005, niallcause.2006, niallcause.2007, niallcause.2008, 
                        niallcause.2009, niallcause.2010, niallcause.2011) %>% 
  select(-c(`...1`,`...2`, `...4`)) %>% 
  gather(Age, Dx, c(2:21)) %>% 
  mutate(Age=case_when(
    Age=="...5" ~ "0", Age=="...6" ~ "1-4", Age=="...7" ~ "5-9", Age=="...8" ~ "10-14",
    Age=="...9" ~ "15-19", Age=="...10" ~ "20-24", Age=="...11" ~ "25-29", Age=="...12" ~ "30-34",
    Age=="...13" ~ "35-39", Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
    Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69", Age=="...20" ~ "70-74",
    Age=="...21" ~ "75-79", Age=="...22" ~ "80-84", Age=="...23" ~ "85-89", Age=="...24" ~ "90+"),
    Dx=as.numeric(Dx), Dx=if_else(is.na(Dx), 0, Dx),
    Cause="Total") %>% 
  set_names(c("Sex", "Year", "Age", "Dx", "Cause")) %>% 
  bind_rows(bind_rows(niallcause.2012, niallcause.2013, niallcause.2014, niallcause.2015,
                      niallcause.2016, niallcause.2017, niallcause.2018, niallcause.2019) %>% 
              select(-c(`...1`,`...2`, `...4`)) %>% 
              gather(Age, Dx, c(2:21)) %>% 
              mutate(Age=case_when(
                Age=="...5" ~ "0", Age=="...6" ~ "1-4", Age=="...7" ~ "5-9", Age=="...8" ~ "10-14",
                Age=="...9" ~ "15-19", Age=="...10" ~ "20-24", Age=="...11" ~ "25-29", Age=="...12" ~ "30-34",
                Age=="...13" ~ "35-39", Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69", Age=="...20" ~ "70-74",
                Age=="...21" ~ "75-79", Age=="...22" ~ "80-84", Age=="...23" ~ "85-89", Age=="...24" ~ "90+"),
                Dx=as.numeric(Dx), Dx=if_else(is.na(Dx), 0, Dx),
                Cause="Total") %>% 
              set_names(c("Sex", "Year", "Age", "Dx", "Cause"))) %>% 
  bind_rows(bind_rows(niallcause.2020, niallcause.2021, niallcause.2022) %>% select(-c(ICD10, Cause, `...4`)) %>% 
              gather(Age, Dx, c(2:21)) %>% 
              mutate(Age=case_when(
                Age=="...5" ~ "0", Age=="...6" ~ "1-4", Age=="...7" ~ "5-9", Age=="...8" ~ "10-14",
                Age=="...9" ~ "15-19", Age=="...10" ~ "20-24", Age=="...11" ~ "25-29", Age=="...12" ~ "30-34",
                Age=="...13" ~ "35-39", Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69", Age=="...20" ~ "70-74",
                Age=="...21" ~ "75-79", Age=="...22" ~ "80-84", Age=="...23" ~ "85-89", Age=="...24" ~ "90+"),
                Cause="Total")) %>% 
  mutate(Sex=as.numeric(if_else(Sex %in% c("M", "Male", "Males"), "1", "2")),
         Age=if_else(Age %in% c("85-89", "90+"), "85+", Age)) %>% 
  group_by(Sex, Age, Cause, Year) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup()

nidata.wide <- nidata %>% 
  bind_rows(niallcause) %>% 
  spread(Year, Dx) %>% 
  #Initiate start of age groups
  mutate(agestart=case_when(
    Age=="0" ~ 0,
    Age=="1-4" ~ 1, Age=="5-9" ~ 5, Age=="10-14" ~ 10, Age=="15-19" ~ 15,
    Age=="20-24" ~ 20, Age=="25-29" ~ 25, Age=="30-34" ~ 30, Age=="35-39" ~ 35,
    Age=="40-44" ~ 40, Age=="45-49" ~ 45, Age=="50-54" ~ 50, Age=="55-59" ~ 55,
    Age=="60-64" ~ 60, Age=="65-69" ~ 65, Age=="70-74" ~ 70, Age=="75-79" ~ 75,
    Age=="80-84" ~ 80, TRUE ~ 85)) %>% 
  arrange(agestart) %>% 
  relocate(Cause, agestart)

#Download populations
nipop <- readHMDweb(CNTRY="GBR_NIR", "Population",  key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=FALSE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) %>% 
  filter(Year>=2001) 

nipop <- bind_rows(nipop %>% filter(Year==2021) %>% 
                     select("Year", "Age", "Male2", "Female2") %>% 
                     mutate(Year=2022) %>% 
                     set_names(c("Year", "Age", "Male", "Female")),
                   nipop %>% select("Year", "Age", "Male1", "Female1") %>% 
                     set_names(c("Year", "Age", "Male", "Female"))) %>%  
  gather(Sex, Ex, c("Male", "Female")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2)) 

#Group populations to match deaths age groups
nipop.grouped <- nipop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2022`, sum, na.rm=TRUE)) %>% 
  ungroup()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "nidata.wide", "nipop", "nipop.grouped", "password",
                        "username", "font", "theme_custom", "ewalcvalidate")))

#Combine EW, Scotland & NI
UKdata <- ewdata.wide %>% 
  gather(Year, Dx, c(5:26)) %>% 
  select(Cause, agestart, Sex, Year, Dx) %>% 
  mutate(Country="England & Wales") %>%
  merge(ewpop.grouped %>% gather(Year, pop, c(3:24))) %>% 
  mutate(mx=Dx*100000/pop) %>% 
  bind_rows(scotdata.wide %>% 
              gather(Year, Dx, c(5:27)) %>% 
              select(Cause, agestart, Sex, Year, Dx) %>% 
              mutate(Country="Scotland") %>%
              merge(scotpop.grouped %>% gather(Year, pop, c(3:25))) %>% 
              mutate(mx=Dx*100000/pop)) %>% 
  bind_rows(nidata.wide %>% 
              gather(Year, Dx, c(5:26)) %>% 
              select(Cause, agestart, Sex, Year, Dx) %>% 
              mutate(Country="Northern Ireland") %>%
              merge(nipop.grouped %>% gather(Year, pop, c(3:24))) %>% 
              mutate(mx=Dx*100000/pop)) %>% 
  rename(Ex=pop)

#########
#US data#
#########

#All downloaded from the CDC wonder database using the same ICD-10 definitions as above
temp <- tempfile()
US.DoD0021 <- "https://raw.githubusercontent.com/VictimOfMaths/Publications/master/DoDPandemic/CDCWonderDoD2020.txt"
temp <- curl_download(url=US.DoD0021, destfile=temp, quiet=FALSE, mode="wb")

US.DoD0021 <- read.csv(temp, sep="\t") 

temp <- tempfile()
US.AllCause0021 <- "https://raw.githubusercontent.com/VictimOfMaths/Publications/master/DoDPandemic/CDCWonderAllCause2020.txt"
temp <- curl_download(url=US.AllCause0021, destfile=temp, quiet=FALSE, mode="wb")

US.AllCause0021 <- read.csv(temp, sep="\t") 

#More recent provisional data
US.DoD2123 <- read.csv("CDC Data/CDCWonderDOD2123.txt", sep="\t") 

US.AllCause2123 <- read.csv("CDC Data/CDCWonderAllCause2123.txt", sep="\t") 

USdata <- bind_rows(US.DoD0021, US.AllCause0021) %>% 
  filter(Notes!="Total" & !is.na(Deaths)) %>% 
  select(Year.Code, Five.Year.Age.Groups.Code, Gender, Cause.of.death.Code, Deaths) %>% 
  set_names("Year", "Age", "Sex", "ICD10", "Dx") %>% 
  bind_rows(bind_rows(US.DoD2123 %>% mutate(Deaths=as.numeric(Deaths)), US.AllCause2123) %>% 
              filter(Notes!="Total" & !is.na(Deaths)) %>% 
              select(Year.Code, Five.Year.Age.Groups.Code, Gender, Underlying.Cause.of.death.Code, Deaths) %>% 
              set_names("Year", "Age", "Sex", "ICD10", "Dx")) %>% 
  mutate(Cause=case_when(
    is.na(ICD10) ~ "Total",
    substr(ICD10, 1, 3) %in% c("F10", "K70", "K73", "K74", "X45", "Y15") ~ "Alcohol",
    substr(ICD10, 1, 3) %in% c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "X40", "X41", "X42",
                               "X43", "X44", "X85", "Y10", "Y11", "Y12", "Y13", "Y14") ~ "Drugs",
    substr(ICD10, 1, 3) %in% c("U03", "Y87") | substr(ICD10, 1, 2) %in% c("X6", "X7", "X8") ~ "Suicide")) %>% 
  group_by(Year, Age, Sex, Cause) %>% 
  summarise(Dx=sum(Dx), .groups="drop") %>% 
  #Calculate other cause deaths
  spread(Cause, Dx) %>% 
  mutate(Other=Total-Alcohol-Drugs-Suicide) %>% 
  gather(Cause, Dx, c(4:8)) %>% 
  mutate(Dx=replace_na(Dx, 0), 
         agestart=case_when(Age=="1" ~ 0,
                            Age=="1-4" ~ 1, Age=="5-9" ~ 5, Age=="10-14" ~ 10, Age=="15-19" ~ 15,
                            Age=="20-24" ~ 20, Age=="25-29" ~ 25, Age=="30-34" ~ 30, Age=="35-39" ~ 35,
                            Age=="40-44" ~ 40, Age=="45-49" ~ 45, Age=="50-54" ~ 50, Age=="55-59" ~ 55,
                            Age=="60-64" ~ 60, Age=="65-69" ~ 65, Age=="70-74" ~ 70, Age=="75-79" ~ 75,
                            Age=="80-84" ~ 80, TRUE ~ 85)) %>% 
  group_by(Year, agestart, Sex, Cause) %>% 
  summarise(Dx=sum(Dx), .groups="drop")

#Bring in exposures/populations from HMD as CDC data is missing populations for 85+
USpop <- readHMDweb(CNTRY="USA", "Population",  key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=FALSE) %>% 
  filter(Year>=1999) %>% 
  select("Age", "Year", "Male1", "Female1") %>% 
  set_names(c("Age", "Year", "Male", "Female")) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  mutate(Age=if_else(Age=="110+", 110, as.double(Age))) %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(Ex=sum(Ex)) %>% 
  ungroup() 

USpop.grouped <- USpop %>%  
  filter(Age>=10) %>% 
  mutate(agestart=case_when(
    Age<15 ~ 10, Age<20 ~ 15, Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, 
    Age<50 ~ 45, Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Year, Sex, agestart) %>% 
  summarise(Ex=sum(Ex, na.rm=TRUE), .groups="drop") 

#Hackily add 2022 & 23 pops assuming they are just 2021 pops
USpop.grouped <- bind_rows(USpop.grouped, USpop.grouped %>% filter(Year==2021) %>% 
                     mutate(Year=2022), USpop.grouped %>% filter(Year==2021) %>% 
                       mutate(Year=2023))

USpop <- bind_rows(USpop, USpop %>% filter(Year==2021) %>% 
                             mutate(Year=2022), 
                   USpop %>% filter(Year==2021) %>% 
                     mutate(Year=2023))

USdata <- merge(USdata, USpop.grouped) %>% 
  mutate(mx=Dx*100000/Ex, Country="USA")

###############
#Canadian data#
###############

#Read in Canadian data
#Cancer data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310014201
#(not using cancer data any more, but this file has the all cause data in it)
Can.can <- read.csv("Data/StatCan Data/StatCanCancer0022.csv")

#Mental and behavioural data from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014301
Can.men <- read.csv("Data/StatCan Data/StatCanICDF0022.csv")

#Liver disease from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014801
Can.dig <- read.csv("Data/StatCan Data/StatCanICDK0022.csv")

#External cause data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310015601
Can.ext <- read.csv("Data/StatCan Data/StatCanExternal0022.csv")

#Combine and separate out into causes of interest
Candata <- bind_rows(Can.can, Can.men, Can.dig, Can.ext) %>% 
  select(`REF_DATE`, `Age.group`, Sex, Cause.of.death..ICD.10., VALUE) %>% 
  set_names(c("Year", "Age", "Sex", "CoD", "Dx")) %>% 
  filter(Age!="Age, not stated") %>% 
  #Extract the actual ICD-10 codes from the causes of death 
  #(using a regex I don't understand) plus some faffery to handly the "[hallucinogens]"
  #that appear in some of the cause of death descriptions
  mutate(ICD10v1=str_match(CoD, "(?<=\\[).+?(?=\\])"),
         ICD10v2=str_match(CoD, "(?<=\\[X).+?(?=\\])"),
         ICD10v3=str_match(CoD, "(?<=\\[Y).+?(?=\\])"),
         ICD10=case_when(
           ICD10v1=="hallucinogens" & !is.na(ICD10v2) ~
             paste0("X", ICD10v2),
           ICD10v1=="hallucinogens" & !is.na(ICD10v3) ~
             paste0("Y", ICD10v3),
           TRUE ~ ICD10v1),
         #Group
         Cause=case_when(
           ICD10=="A00-Y89" ~ "Total",
           ICD10 %in% c("K70", "K73", "K74", "F10", "X45", "Y15") ~ "Alcohol",
           ICD10 %in% c("F11", "F12", "F13", "F14", "F17", "F18",
                        "F19", "X40", "X41", "X42", "X43", "X44",
                        "X85", "Y10", "Y11", "Y12", "Y13", "Y14") ~ "Drugs",
           ICD10 %in% c("X60-X84", "Y87") ~ "Suicide")) %>% 
  group_by(Year, Age, Sex, Cause) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup() %>% 
  mutate(agestart=case_when(
    Age=="Under 1 year" ~ 0, Age=="1 to 4 years" ~ 1, Age=="5 to 9 years" ~ 5, 
    Age=="10 to 14 years" ~ 10, Age=="15 to 19 years" ~ 15, Age=="20 to 24 years" ~ 20, 
    Age=="25 to 29 years" ~ 25, Age=="30 to 34 years" ~ 30, Age=="35 to 39 years" ~ 35, 
    Age=="40 to 44 years" ~ 40, Age=="45 to 49 years" ~ 45, Age=="50 to 54 years" ~ 50, 
    Age=="55 to 59 years" ~ 55, Age=="60 to 64 years" ~ 60, Age=="65 to 69 years" ~ 65, 
    Age=="70 to 74 years" ~ 70, Age=="75 to 79 years"~ 75, Age=="80 to 84 years" ~ 80, 
    TRUE ~ 85),
    Sex=if_else(Sex=="Males", 1, 2),
    Country="Canada") %>% 
  group_by(Year, agestart, Sex, Cause, Country) %>% 
  summarise(Dx=sum(Dx), .groups="drop")

#Bring in exposures/populations from HMD 

#Download populations
Canpop <- readHMDweb(CNTRY="CAN", "Exposures_1x1",  key_list("mortality.org")[1,2], 
                     key_get("mortality.org", key_list("mortality.org")[1,2])) %>% 
  filter(Year>=2000) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2),
         #Hackily assumpe populations in 2021 & 22 = 2020
         `2021`=`2020`, `2022`=`2021`)

#Group populations to match deaths age groups
Canpop.grouped <- Canpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15, Age<25 ~ 20, 
    Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2000`:`2022`, sum)) %>% 
  ungroup() %>% 
  arrange(agestart)

Candata <- Candata %>% 
  merge(Canpop.grouped %>% gather(Year, pop, c(3:25))) %>% 
  mutate(mx=Dx*100000/pop) %>% 
  rename("Ex"="pop") %>% 
  filter(!is.na(Cause))

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "nidata.wide", "nipop", "nipop.grouped", "UKdata", 
                        "USdata", "USpop", "USpop.grouped", "Candata", "Canpop", "Canpop.grouped",
                        "font", "theme_custom", "ewalcvalidate")))

Raw <- USdata %>% 
  bind_rows(Candata %>% mutate(Sex=if_else(Sex==1, "Male", "Female"))) %>% 
  bind_rows(UKdata %>% 
              mutate(Year=as.numeric(Year),
                     Sex=if_else(Sex==1, "Male", "Female")))

#############################################
#Smooth out to single year of age

#Apply smoothing based approach suggested by Tim Riffe
#Prediction models fall over if you include <10 year olds, so exclude them as not relevant to analysis

ShortCountries <- c("Canada", "Northern Ireland", "England & Wales")
LongCountries <- c("USA", "Scotland")

#First do countries with data only up to 2022
x <- seq(10,85, by=5)
Smoothing1 <- Raw %>% filter(agestart>=10 & Country %in% ShortCountries & Year>=2001)
y <- 2001:2022
z <- Smoothing1 %>% select(-c(Ex, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Dx) %>% 
  arrange(agestart)

offset <- Smoothing1 %>% select(-c(Dx, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Ex) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx_smoothed1D1 <- data.frame(Country=character(), Cause=character(), Sex=character(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

for(i in ShortCountries){
  for(j in c("Alcohol", "Drugs", "Suicide", "Total")){
    for(k in c("Male", "Female")){
      for(l in 2001:2022){
        y <- z %>% filter(Country==i & Cause==j & Sex==k) %>% 
          select(-c(agestart, Sex, Country, Cause, Sex)) %>% 
          select(c(l-2000)) %>% 
          unlist() %>% 
          as.vector()
        
        offset_i <- offset %>% filter(Country==i & Cause==j & Sex==k& agestart>=10) %>% 
          select(-c(agestart, Sex, Country, Cause, Sex)) %>% 
          select(c(l-2000)) %>% 
          log() %>% 
          unlist() %>% 
          as.vector()
        
        mod <- Mort1Dsmooth(x, y, offset=offset_i)
        
        mx_smoothed1D1 <- predict(mod, newdata=c(10:85)) %>% 
          exp() %>% 
          as.data.frame() %>% 
          rename(mx_smt1D=1) %>% 
          mutate(Age=c(10:85), Country=i, Cause=j, Sex=k, Year=l) %>% 
          bind_rows(mx_smoothed1D1)
      }
    }
  }
}

#Then countries with data up to 2023
x <- seq(10,85, by=5)
Smoothing2 <- Raw %>% filter(agestart>=10 & Country %in% LongCountries & Year>=2001)
y <- 2001:2023
z <- Smoothing2 %>% select(-c(Ex, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Dx) %>% 
  arrange(agestart)

offset <- Smoothing2 %>% select(-c(Dx, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Ex) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx_smoothed1D2 <- data.frame(Country=character(), Cause=character(), Sex=character(), Age=integer(),
                             Year=integer(), mx_smt1D=double())

for(i in LongCountries){
  for(j in c("Alcohol", "Drugs", "Suicide", "Total")){
    for(k in c("Male", "Female")){
      for(l in 2001:2023){
        y <- z %>% filter(Country==i & Cause==j & Sex==k) %>% 
          select(-c(agestart, Sex, Country, Cause, Sex)) %>% 
          select(c(l-2000)) %>% 
          unlist() %>% 
          as.vector()
        
        offset_i <- offset %>% filter(Country==i & Cause==j & Sex==k& agestart>=10) %>% 
          select(-c(agestart, Sex, Country, Cause, Sex)) %>% 
          select(c(l-2000)) %>% 
          log() %>% 
          unlist() %>% 
          as.vector()
        
        mod <- Mort1Dsmooth(x, y, offset=offset_i)
        
        mx_smoothed1D2 <- predict(mod, newdata=c(10:85)) %>% 
          exp() %>% 
          as.data.frame() %>% 
          rename(mx_smt1D=1) %>% 
          mutate(Age=c(10:85), Country=i, Cause=j, Sex=k, Year=l) %>% 
          bind_rows(mx_smoothed1D2)
      }
    }
  }
}

Rawsmoothed <- bind_rows(mx_smoothed1D1, mx_smoothed1D2) %>% 
  #mx_smoothed1D2 %>% 
  merge(scotpop %>% gather(Year, pop.s, c(3:25)) %>% 
           merge(ewpop %>% gather(Year, pop.ew, c(3:24)), all.x=TRUE) %>% 
           merge(nipop %>% gather(Year, pop.ni, c(3:24)), all.x=TRUE) %>%
           mutate(Sex=if_else(Sex==1, "Male", "Female")) %>% 
           merge(USpop %>% rename(pop.us=Ex), all.x=TRUE) %>% 
          merge(Canpop %>% gather(Year, pop.can, c(3:25)) %>%
                  mutate(Sex=if_else(Sex==1, "Male", "Female")), all.x=TRUE)) %>%
           mutate(pop=case_when(
             Country=="Scotland" ~ pop.s, 
             Country=="England & Wales" ~ pop.ew,
             Country=="USA" ~ pop.us,
             Country=="Canada" ~ pop.can,
             TRUE ~ pop.ni)) %>% 
  select(-c(pop.s, pop.ew, pop.ni, pop.us, pop.can)) %>% 
  mutate(Dx_smt1D=mx_smt1D*pop)

############################
#Analysis#
##########

#Derive age-standardised deaths for each cause/country
ASdata <- Raw %>% 
  mutate(stdpop=case_when(
    agestart==10 ~ 5500, agestart==15 ~ 5500, agestart==20 ~ 6000, agestart==25 ~ 6000, 
    agestart==30 ~ 6500, agestart==35 ~ 7000, agestart==40 ~ 7000, agestart==45 ~ 7000, 
    agestart==50 ~ 7000, agestart==55 ~ 6500, agestart==60 ~ 6000, agestart==65 ~ 5500, 
    agestart==70 ~ 5000, agestart==75 ~ 4000, agestart==80 ~ 2500, agestart==85 ~ 1500, 
    TRUE ~ 1000)) %>% 
  group_by(Country, Cause, Sex, Year) %>% 
  summarise(Dx=sum(Dx), Ex=sum(Ex), 
            mx_std=weighted.mean(mx, stdpop), .groups="drop") %>% 
  group_by(Country, Cause, Sex) 

#Plot
agg_tiff("Outputs/DoDPandemicPaperFig1Updated.tiff", units="in", width=10, height=6, res=600)
ggplot(ASdata %>% filter(!Cause %in% c("Total", "Other") & Year>=2000) %>% 
         mutate(Sex=factor(Sex, levels=c("Male", "Female"))), aes(x=Year, y=mx_std, colour=Cause))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=56), fill="Grey92", colour=NA)+
  geom_line()+
  geom_point()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="")+
  facet_grid(Sex~Country)+
  theme_custom()+
  theme(legend.position="top", axis.line.x=element_blank(),
        panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="Trends in deaths due to alcohol, drugs and suicide in the UK, USA and Canada", 
       subtitle="Grey shaded areas represent the pandemic period",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/DoDPandemicPaperFig1AltUpdated.tiff", units="in", width=10, height=6, res=600)
ggplot(ASdata %>% filter(!Cause %in% c("Total", "Other") & Year>=2000), aes(x=Year, y=mx_std, colour=Cause))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line(alpha=0.3, show.legend=FALSE)+
  geom_line(data=ASdata %>% filter(!Cause %in% c("Total", "Other") & Year>=2019),
            aes(x=Year, y=mx_std, colour=Cause), arrow=arrow(angle=25, type="closed", 
                                                             length=unit(0.13, "cm")))+
  geom_point(data=ASdata %>% filter(!Cause %in% c("Total", "Other") & Year==2019),
             aes(x=Year, y=mx_std, colour=Cause), show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="")+
  facet_grid(Sex~Country)+
  theme_custom()+
  theme(legend.position="top", axis.line.x=element_blank(),
        panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="Trends in deaths due to alcohol, drugs and suicide in the UK, USA and Canada", 
       subtitle="Bolder line segments represent the pandemic period",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/DoDPandemicPaperFig1Altv2Updated.tiff", units="in", width=10, height=7.5, res=600)
ggplot(ASdata %>% filter(!Cause %in% c("Total", "Other") & Year>=2000) %>% 
         mutate(Sex=factor(Sex, levels=c("Male", "Female"))), aes(x=Year, y=mx_std, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=56), fill="Grey92", colour=NA)+
  geom_line()+
  geom_point()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#017a4a", "#FFCE4E", "#3d98d3", "#ff363c", "#7559a2"), name="")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  theme(legend.position="top", axis.line.x=element_blank(),
        panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="Trends in deaths due to alcohol, drugs and suicide in the UK, USA and Canada", 
       subtitle="Grey shaded areas represent the pandemic period",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")

dev.off()

#Lexis surfaces
agg_tiff("Outputs/DoDLexisMale.tiff", units="in", width=8, height=13, res=800)
Rawsmoothed %>% filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()
dev.off()

agg_tiff("Outputs/DoDLexisFemale.tiff", units="in", width=8, height=13, res=800)
Rawsmoothed %>% filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Female" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()
dev.off()

#Just compare drugs in Scotland & the USA
Rawsmoothed %>% filter(Cause=="Drugs" & Age<80 & Country %in% c("Scotland", "USA")) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~Country)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()

#Cohortiness

Cohorts <-  Rawsmoothed %>% 
  mutate(YOB=Year-Age,
         Cohort=case_when(
           YOB %in% c(1916:1925) ~ "1916-25", YOB %in% c(1926:1935) ~ "1926-35",
           YOB %in% c(1936:1945) ~ "1936-45", YOB %in% c(1946:1955) ~ "1946-55",
           YOB %in% c(1956:1965) ~ "1956-65", YOB %in% c(1966:1975) ~ "1966-75",
           YOB %in% c(1976:1985) ~ "1976-85", YOB %in% c(1986:1995) ~ "1986-95",
           YOB %in% c(1996:2005) ~ "1996-2005")) %>% 
  group_by(Year, Cohort, Cause, Country, Sex) %>% 
  summarise(Dx_smt1D=sum(Dx_smt1D), pop=sum(pop), .groups="drop") %>% 
  mutate(mx_smt=Dx_smt1D*100000/pop, MeanAge=Year-as.numeric(substr(Cohort, 1, 4))+4)

agg_tiff("Outputs/DoDCohortsMale.tiff", units="in", width=12, height=8, res=600)
ggplot(Cohorts %>% filter(Sex=="Male" & Cause!="Total" & !is.na(Cohort)), 
       aes(x=MeanAge, y=mx_smt, colour=Cohort))+
  geom_line()+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_manual(values=palettes_d$khroma$sunset[-c(5,7)], name="Birth cohort")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey97"))+
  labs(title="Cohort patterns in male mortality",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/DoDCohortsFemale.tiff", units="in", width=12, height=8, res=600)
ggplot(Cohorts %>% filter(Sex=="Female" & Cause!="Total" & !is.na(Cohort)), 
       aes(x=MeanAge, y=mx_smt, colour=Cohort))+
  geom_line()+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_manual(values=palettes_d$khroma$sunset[-c(5,7)], name="Birth cohort")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey97"))+
  labs(title="Cohort patterns in female mortality",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")
dev.off()

####################
#Actually replicate analysis from Jenn's BMJ Open paper
Combined_short <- Rawsmoothed %>% 
  filter(Year>2000) %>% 
  mutate(ageband=case_when(
    Age<35 ~ "<35",
    Age<45 ~ "35-44",
    Age<55 ~ "45-54",
    Age<65 ~ "55-64",
    TRUE ~ "65+"),
    stdpop=case_when(
      Age<15 ~ 5500/5, Age<20 ~ 5500/5, Age<25 ~ 6000/5, Age<30 ~ 6000/5, Age<35 ~ 6500/5,
      Age<40 ~ 7000/5, Age<45 ~ 7000/5, Age<50 ~ 7000/5, Age<55 ~ 7000/5, Age<60 ~ 6500/5,
      Age<65 ~ 6000/5, Age<70 ~ 5500/5, Age<75 ~ 5000/5, Age<80 ~ 4000/5, Age<85 ~ 2500/5,
      Age<90 ~ 1500/5, TRUE ~ 1000
    )) %>% 
  group_by(ageband, Country, Cause, Sex, Year) %>% 
  summarise(Dx_smt1D=sum(Dx_smt1D), Ex=sum(pop), mx_std=weighted.mean(mx_smt1D, stdpop)*100000) %>% 
  ungroup() %>% 
  mutate(mx_smt1D=Dx_smt1D*100000/Ex) %>% 
  #calculate 3-year rolling averages
  group_by(ageband, Country, Cause, Sex) %>% 
  arrange(Year) %>% 
  mutate(mx_smt1D_roll=roll_mean(mx_smt1D, 3, align="center", fill=NA),
         mx_std_roll=roll_mean(mx_std, 3, align="center", fill=NA))

agg_tiff("Outputs/DoDAlcoholxAge2022.tiff", units="in", width=8, height=6, res=800)
Combined_short %>% 
  filter(Cause=="Alcohol" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_hline(yintercept=0, colour="grey20")+
  geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=118), fill="Grey92", colour=NA)+
  geom_line(aes(x=Year, y=mx_std, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        panel.grid.major.y=element_line(colour="grey95"),
        axis.line.x=element_blank())+
  labs(title="Alcohol-specific deaths",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/DoDDrugsxAge2022.tiff", units="in", width=8, height=6, res=800)
Combined_short %>% 
  filter(Cause=="Drugs" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_hline(yintercept=0, colour="grey20")+
  geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=110), fill="Grey92", colour=NA)+
  geom_line(aes(x=Year, y=mx_std, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        panel.grid.major.y=element_line(colour="grey95"),
        axis.line.x=element_blank())+
  labs(title="Drug-related deaths",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/DoDSuicidexAge2022.tiff", units="in", width=8, height=6, res=800)
Combined_short %>% 
  filter(Cause=="Suicide" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_hline(yintercept=0, colour="grey20")+
  geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=40), fill="Grey92", colour=NA)+
  geom_line(aes(x=Year, y=mx_std, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        panel.grid.major.y=element_line(colour="grey95"),
        axis.line.x=element_blank())+
  labs(title="Deaths by suicide",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nPlot by @VictimOfMaths")
dev.off()

agg_png("Outputs/RCGPFigure3.png", units="in", width=11, height=5, res=800)

ASdata %>% filter(Cause=="Alcohol" & Year>=2001 & Year<=2020) %>% 
  mutate(Sex=factor(Sex, levels=c("Male", "Female"))) %>% 
  group_by(Country, Year) %>% 
  summarise(mx_std=weighted.mean(mx_std, Ex), .groups="drop") %>% 
ggplot(aes(x=Year, y=mx_std, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_rect(aes(xmin=2019.5, xmax=2020.5, ymin=0, ymax=40), fill="Grey92", colour=NA)+
  geom_line()+
  geom_point()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#017a4a", "#FFCE4E", "#3d98d3", "#ff363c", "#7559a2"), name="")+
  theme_custom()+
  theme(legend.position="top", axis.line.x=element_blank(),
        panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="Alcohol-specific deaths rose everywhere in 2020", 
       subtitle="Grey shaded areas represent the pandemic period",
       caption="Data from StatCan, ONS, NISRA, NRS, CDC Wonder & HMD\nAnalysis by Colin Angus")

dev.off()