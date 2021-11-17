rm(list=ls())

#Mortality smooth is currently off CRAN, so take mirrored version from Tim Riffe's GitHub
#remotes::install_github("timriffe/MortalitySmooth")

library(curl)
library(readxl)
library(tidyverse)
library(HMDHFDplus)
library(MortalitySmooth)
library(paletteer)
library(ragg)
library(extrafont)
library(RcppRoll)

options(scipen=10000)

#Read in England & Wales data from
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset
ewfile <- tempfile()
ewurl <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/21stcenturymortality2019final.xls"
ewfile <- curl_download(url=ewurl, destfile=ewfile, quiet=FALSE, mode="wb")

ewdata.01 <- read_excel(ewfile, sheet="2001", range="A2:E21262")
ewdata.02 <- read_excel(ewfile, sheet="2002", range="A2:E20877")
ewdata.03 <- read_excel(ewfile, sheet="2003", range="A2:E21248")
ewdata.04 <- read_excel(ewfile, sheet="2004", range="A2:E20956")
ewdata.05 <- read_excel(ewfile, sheet="2005", range="A2:E20925")
ewdata.06 <- read_excel(ewfile, sheet="2006", range="A2:E20863")
ewdata.07 <- read_excel(ewfile, sheet="2007", range="A2:E20654")
ewdata.08 <- read_excel(ewfile, sheet="2008", range="A2:E20657")
ewdata.09 <- read_excel(ewfile, sheet="2009", range="A2:E20789")
ewdata.10 <- read_excel(ewfile, sheet="2010", range="A2:E20781")
ewdata.11 <- read_excel(ewfile, sheet="2011", range="A2:E20377")
ewdata.12 <- read_excel(ewfile, sheet="2012", range="A2:E20208")
ewdata.13 <- read_excel(ewfile, sheet="2013", range="A2:E20436")
ewdata.14 <- read_excel(ewfile, sheet="2014", range="A2:E20423")
ewdata.15 <- read_excel(ewfile, sheet="2015", range="A2:E20195")
ewdata.16 <- read_excel(ewfile, sheet="2016", range="A2:E20277")
ewdata.17 <- read_excel(ewfile, sheet="2017", range="A2:E20190")
ewdata.18 <- read_excel(ewfile, sheet="2018", range="A2:E20478")
ewdata.19 <- read_excel(ewfile, sheet="2019", range="A2:E20302")

ewdata <- bind_rows(ewdata.01, ewdata.02, ewdata.03, ewdata.04, ewdata.05, ewdata.06, ewdata.07,
                    ewdata.08, ewdata.09, ewdata.10, ewdata.11, ewdata.12, ewdata.13, ewdata.14, 
                    ewdata.15, ewdata.16, ewdata.17, ewdata.18, ewdata.19) %>% 
  mutate(Year=coalesce(Year, YR),
         Age=coalesce(Age, AGE),
         Sex=coalesce(Sex, SEX),
         ICD10=coalesce(ICD10, `ICD-10`)) %>% 
  select(-c(YR, AGE, SEX, `ICD-10`)) %>% 
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
           code1=="C" ~ "Cancer",
           code1=="E" & code2 %in% c(10:14, 65:67) ~ "Metabolic",
           code1=="I" & code2 %in% c(0:13, 20:51) ~ "Metabolic",
           TRUE ~ "Other"),
         Age=case_when(
           Age %in% c("neonates", "neonatal", "Neonates", "<1") ~ "<1",
           TRUE ~ Age)) %>% 
  #Collapse into cause groups
  group_by(Year, Age, Sex, Cause) %>% 
  summarise(Dx=sum(NDTHS)) %>% 
  ungroup()

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
  summarise(across(c(`2001`:`2019`), sum)) %>% 
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
    Age=="80-84" ~ 80, TRUE ~ 85)) %>% 
  arrange(agestart) %>% 
  relocate(Cause, agestart)

#Download populations/exposures from HMD
username <- "c.r.angus@sheffield.ac.uk" 
password <- "1574553541"

ewpop <- readHMDweb(CNTRY="GBRTENW", "Exposures_1x1", username, password) %>% 
  filter(Year>=2001) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
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
  summarise(across(`2001`:`2018`, sum)) %>% 
  ungroup()

#######################################################################
#Get Scottish data
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
                      scotdata.2016, scotdata.2017, scotdata.2018, scotdata.2019) %>% 
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
  group_by(Age, Sex, Year, ICD10) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup() %>% 
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
           code1=="C" ~ "Cancer",
           code1=="E" & code2 %in% c(10:14, 65:67) ~ "Metabolic",
           code1=="I" & code2 %in% c(0:13, 20:51) ~ "Metabolic",
           TRUE ~ "Other")) %>% 
  group_by(Sex, Age, Cause, Year) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup()

#Add all-cause deaths group
scotdata.wide <- scotdata %>% 
  spread(Year, Dx) %>% 
  group_by(Age, Sex) %>% 
  summarise(across(c(`2001`:`2019`), sum)) %>% 
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
scotpop <- readHMDweb(CNTRY="GBR_SCO", "Exposures_1x1", username, password) %>% 
  filter(Year>=2001) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
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
  summarise(across(`2001`:`2018`, sum)) %>% 
  ungroup()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped")))

#Combine EW and Scotland
ewsdata <- ewdata.wide %>% 
  gather(Year, Dx, c(5:22)) %>% 
  select(Cause, agestart, Sex, Year, Dx) %>% 
  mutate(Country="England & Wales") %>%
  merge(ewpop.grouped %>% gather(Year, pop, c(3:20))) %>% 
  mutate(mx=Dx*100000/pop)

ewsdata <- scotdata.wide %>% 
  gather(Year, Dx, c(5:22)) %>% 
  select(Cause, agestart, Sex, Year, Dx) %>% 
  mutate(Country="Scotland") %>%
  merge(scotpop.grouped %>% gather(Year, pop, c(3:20))) %>% 
  mutate(mx=Dx*100000/pop) %>% 
  bind_rows(ewsdata)

#Plot raw mx
agg_tiff("Outputs/DoDEWSRaw.tiff", units="in", width=6, height=10, res=500)
ewsdata %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1) %>% 
ggplot()+
  geom_raster(aes(x=Year, y=as.factor(agestart), fill=mx))+
  scale_x_discrete(breaks=c(2005, 2010, 2015, 2020))+
  scale_y_discrete(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Country~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))
dev.off()

#Apply 2D smoothing based on Tim Riffe's suggested approach
#Prediction models fall over if you include <10 year olds, so exclude them as not relevant to analysis
x <- seq(10,85, by=5)
ewsdata <- ewsdata %>% filter(agestart>=10)
y <- 2001:2018
z <- ewsdata %>% select(-c(pop, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Dx) %>% 
  arrange(agestart)

offset <- ewsdata %>% select(-c(Dx, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, pop) %>% 
  arrange(agestart)

mx_smoothed2D <- data.frame(Country=character(), Cause=character(), Sex=integer(), Age=integer())

#Fit smoothing models across 2 dimensions
for(i in unique(ewsdata$Country)){
  for(j in c("Alcohol", "Drugs", "Suicide", "Cancer", "Metabolic", "Total")){
    for(k in 1:2){
      z_i <- z %>% filter(Country==i & Cause==j & Sex==k) %>% 
        select(-c(agestart, Sex, Country, Cause, Sex)) %>% 
        as.matrix()
      
      offset_i <- offset %>% filter(Country==i & Cause==j & Sex==k) %>% 
        select(-c(agestart, Sex, Country, Cause, Sex)) %>% 
        as.matrix() %>% 
        log()
      
      mod <- Mort2Dsmooth(x, y, z_i, offset=offset_i, ndx=c(6,6))
      
      rates_1x1 <- expand.grid(y=y, x=10:85)
      
      mx_smoothed2D <- predict(mod, newdata=rates_1x1) %>% 
        exp() %>% 
        as.data.frame() %>% 
        mutate(Age=c(10:85), Country=i, Cause=j, Sex=k) %>% 
        bind_rows(mx_smoothed2D)

    }
  }
}

ewssmoothed <- gather(mx_smoothed2D, Year, mx_smt2D, c(1:18)) %>% 
  merge(ewpop %>% gather(Year, pop.ew, c(3:20))) %>% 
  merge(scotpop %>% gather(Year, pop.s, c(3:20))) %>% 
  mutate(pop=if_else(Country=="Scotland", pop.s, pop.ew)) %>% 
  select(-c(pop.s, pop.ew)) %>% 
  mutate(Dx_smt2D=mx_smt2D*pop)

agg_tiff("Outputs/DoDEWS2Dsmt.tiff", units="in", width=6, height=10, res=500)
ewssmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt2D*100000))+
  scale_x_discrete(breaks=c(2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Country~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  coord_equal()
dev.off()

#Fit smoothing models within years only
mx_smoothed1D <- data.frame(Country=character(), Cause=character(), Sex=integer(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

for(i in unique(ewsdata$Country)){
  for(j in c("Alcohol", "Drugs", "Suicide", "Cancer", "Metabolic", "Total")){
    for(k in 1:2){
      for(l in 2001:2018){
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
      
            mx_smoothed1D <- predict(mod, newdata=c(10:85)) %>% 
        exp() %>% 
        as.data.frame() %>% 
        rename(mx_smt1D=1) %>% 
        mutate(Age=c(10:85), Country=i, Cause=j, Sex=k, Year=l) %>% 
        bind_rows(mx_smoothed1D)
      }
    }
  }
}

ewssmoothed <- mx_smoothed1D %>% 
  merge(ewpop %>% gather(Year, pop.ew, c(3:20))) %>% 
  merge(scotpop %>% gather(Year, pop.s, c(3:20))) %>% 
  mutate(pop=if_else(Country=="Scotland", pop.s, pop.ew)) %>% 
  select(-c(pop.s, pop.ew)) %>% 
  mutate(Dx_smt1D=mx_smt1D*pop) %>% 
  merge(ewssmoothed)

agg_tiff("Outputs/DoDEWS1Dsmt.tiff", units="in", width=6, height=10, res=500)
ewssmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_discrete(breaks=c(2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Country~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  coord_equal()
dev.off()

#################################################################################################
#It would be nice to use {ungroup} instead for the smoothing,  but there is a bug with 
#the package which returns negative smoothed mortality rates for relatively low prevalence causes 
#(e.g. all deaths of despair)
#https://github.com/mpascariu/ungroup/issues/8
#################################################################################################

#Read in Canadian data
#Cancer data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310014201
Can.can <- read.csv("Data/StatCan Data/StatCan Cancer.csv")
#Mental and behavioural data from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014301
Can.men <- read.csv("Data/StatCan Data/StatCan Mental.csv")
#Metabolic pt 1 from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014401
Can.met1 <- read.csv("Data/StatCan Data/StatCan Metabolic1.csv")
#Metabolic pt 2 from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014701
Can.met2 <- read.csv("Data/StatCan Data/StatCan Metabolic2.csv")
#Liver disease from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014801
Can.dig <- read.csv("Data/StatCan Data/StatCan Digestive.csv")
#External cause data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310015601
Can.ext <- read.csv("Data/StatCan Data/StatCan External.csv")

#Combine and separate out into causes of interest
Candata <- bind_rows(Can.can, Can.men, Can.met1 %>% mutate(DGUID=as.character(DGUID)), 
                     Can.met2, Can.dig, Can.ext) %>% 
  select(`ï..REF_DATE`, `Age.group`, Sex, Cause.of.death..ICD.10., VALUE) %>% 
  set_names(c("Year", "Age", "Sex", "CoD", "Dx")) %>% 
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
    ICD10 %in% c("X60-X84", "Y87") ~ "Suicide",
    ICD10=="C00-C97" ~ "Cancer",
    ICD10 %in% c("E10-E14", "E66", "E67", "I00-I02", 
                 "I05-I09", "I10", "I11", "I12", "I13",
                 "I20-I25", "I26-I28", "I30-I51") ~ "Metabolic")) %>% 
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
    Age=="85 to 89 years" ~ 85, TRUE ~ 90),
    Sex=if_else(Sex=="Males", 1, 2))
  
#Bring in exposures/populations from HMD as CDC data is missing populations for 85+
username <- "c.r.angus@sheffield.ac.uk" 
password <- "1574553541"

#Download populations
Canpop <- readHMDweb(CNTRY="CAN", "Exposures_1x1", username, password) %>% 
  filter(Year>=2000) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2))

#Group populations to match deaths age groups
Canpop.grouped <- Canpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15, Age<25 ~ 20, 
    Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, Age<90 ~ 85, TRUE ~ 90)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2000`:`2019`, sum)) %>% 
  ungroup() %>% 
  arrange(agestart)

Candata <- Candata %>% 
  merge(Canpop.grouped %>% gather(Year, pop, c(3:22))) %>% 
  mutate(mx=Dx*100000/pop)

#Plot raw mx
agg_tiff("Outputs/DoDCanRaw.tiff", units="in", width=6, height=10, res=500)
Candata %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=as.factor(agestart), fill=mx))+
  #scale_x_discrete(breaks=c(2005, 2010, 2015, 2020))+
  scale_y_discrete(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))
dev.off()

#Apply 2D smoothing based on Tim Riffe's suggested approach
#Prediction models fall over if you include <10 year olds, so exclude them as not relevant to analysis
x <- seq(10,90, by=5)
Candata <- Candata %>% filter(agestart>=10)
y <- 2000:2019
z <- Candata %>% select(-c(pop, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Dx) %>% 
  arrange(agestart)

offset <- Candata %>% select(-c(Dx, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, pop) %>% 
  arrange(agestart)

mx_smoothed2D <- data.frame(Cause=character(), Sex=integer(), Age=integer())

#Fit smoothing models across 2 dimensions
  for(i in c("Alcohol", "Drugs", "Suicide", "Cancer", "Metabolic", "Total")){
    for(j in 1:2){
      z_i <- z %>% filter(Cause==i & Sex==j) %>% 
        select(-c(agestart, Sex, Cause, Sex, Age)) %>% 
        as.matrix()
      
      offset_i <- offset %>% filter(Cause==i & Sex==j) %>% 
        select(-c(agestart, Sex, Cause, Sex, Age)) %>% 
        as.matrix() %>% 
        log()
      
      mod <- Mort2Dsmooth(x, y, z_i, offset=offset_i, ndx=c(6,6))
      
      rates_1x1 <- expand.grid(y=y, x=10:90)
      
      mx_smoothed2D <- predict(mod, newdata=rates_1x1) %>% 
        exp() %>% 
        as.data.frame() %>% 
        mutate(Age=c(10:90), Cause=i, Sex=j) %>% 
        bind_rows(mx_smoothed2D)
      
    }
  }


Cansmoothed <- gather(mx_smoothed2D, Year, mx_smt2D, c(1:20)) %>% 
  merge(Canpop %>% gather(Year, pop, c(3:22))) %>% 
  mutate(Dx_smt2D=mx_smt2D*pop, Country="Canada")

agg_tiff("Outputs/DoDCan2Dsmt.tiff", units="in", width=6, height=10, res=500)
Cansmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt2D*100000))+
  scale_x_discrete(breaks=c(2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  coord_equal()
dev.off()

#Fit smoothing models within years only
mx_smoothed1D <- data.frame(Cause=character(), Sex=integer(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

  for(i in c("Alcohol", "Drugs", "Suicide", "Cancer", "Metabolic", "Total")){
    for(j in 1:2){
      for(k in 2000:2019){
        y <- z %>% filter(Cause==i & Sex==j) %>% 
          select(-c(agestart, Sex, Cause, Age)) %>% 
          select(c(k-1999)) %>% 
          unlist() %>% 
          as.vector()
        
        offset_i <- offset %>% filter(Cause==i & Sex==j & agestart>=10) %>% 
          select(-c(agestart, Sex, Cause, Age)) %>% 
          select(c(k-1999)) %>% 
          log() %>% 
          unlist() %>% 
          as.vector()
        
        mod <- Mort1Dsmooth(x, y, offset=offset_i)
        
        mx_smoothed1D <- predict(mod, newdata=c(10:90)) %>% 
          exp() %>% 
          as.data.frame() %>% 
          rename(mx_smt1D=1) %>% 
          mutate(Age=c(10:90), Cause=i, Sex=j, Year=k) %>% 
          bind_rows(mx_smoothed1D)
      
    }
  }
}

Cansmoothed <- mx_smoothed1D %>% 
  merge(Canpop %>% gather(Year, pop, c(3:22))) %>% 
  mutate(Dx_smt1D=mx_smt1D*pop, Country="Canada") %>% 
  merge(Cansmoothed)

agg_tiff("Outputs/DoDCan1Dsmt.tiff", units="in", width=6, height=10, res=500)
Cansmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_discrete(breaks=c(2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  coord_equal()
dev.off()

#Read in US data
#All downloaded from the CDC wonder database using the same ICD-10 definitions as above
US.alc <- read.csv("CDC Data/CDCAlcoholDeaths.txt", sep="\t")%>% 
  mutate(Cause="Alcohol")

US.drg <- read.csv("CDC Data/CDCDrugDeaths.txt", sep="\t") %>% 
  mutate(Cause="Drugs")

US.scd <- read.csv("CDC Data/CDCSuicideDeaths.txt", sep="\t") %>% 
  mutate(Cause="Suicide")

US.can <- read.csv("CDC Data/CDCCancerDeaths.txt", sep="\t") %>% 
  mutate(Cause="Cancer")

US.met <- read.csv("CDC Data/CDCMetabolicDeaths.txt", sep="\t") %>% 
  mutate(Cause="Metabolic")

US.tot <- read.csv("CDC Data/CDCAllCauseDeaths.txt", sep="\t") %>% 
  mutate(Cause="Total")

USdata <- bind_rows(US.alc, US.drg, US.scd, US.can, US.met, US.tot) %>% 
  filter(Notes!="Total" & !is.na(Deaths)) %>% 
  mutate(Age=as.integer(as.character(Single.Year.Ages.Code))) %>% 
  select(Year, Cause, Gender, Age, Deaths) %>% 
  rename(Dx=Deaths, Sex=Gender) %>% 
  #Calculate other cause deaths
  spread(Cause, Dx) %>% 
  mutate(Other=Total-Alcohol-Drugs-Suicide-Cancer-Metabolic) %>% 
  gather(Cause, Dx, c(4:10))

#Bring in exposures/populations from HMD as CDC data is missing populations for 85+
username <- "c.r.angus@sheffield.ac.uk" 
password <- "1574553541"

USpop <- readHMDweb(CNTRY="USA", "Exposures_1x1", username, password) %>% 
  filter(Year>=1999) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  mutate(Age=if_else(Age>=100, 100, as.double(Age))) %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(Ex=sum(Ex)) %>% 
  ungroup()

USdata <- merge(USdata, USpop) %>% 
  mutate(mx=Dx*100000/Ex)

#Plot lexis surface
agg_tiff("Outputs/DoDUSRaw.tiff", units="in", width=6, height=8, res=500)
ggplot(USdata %>% filter(Age<=85 & !Cause %in% c("Other", "Total", "Cancer", "Metabolic")), 
       aes(x=Year, y=Age, fill=mx))+ 
  geom_raster()+
  scale_fill_paletteer_c("viridis::magma", name="Deaths per 100,000")+
  facet_grid(Sex~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Roboto"), axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        plot.title=element_text(face="bold", size=rel(1.4)))+
  coord_equal()+
  labs(title="Deaths of despair in the USA",
       subtitle="Annual mortality rates by single year of age",
       caption="Data from Centre for Disease Control | Plot by @VictimOfMaths")
dev.off()

#2D smoothing
x <- c(10:85)
USdata <- USdata %>% filter(Age %in% c(10:85))
y <- 1999:2019
z <- USdata %>% select(-c(Ex, mx)) %>% 
  spread(Year, Dx) %>% 
  arrange(Age)

offset <- USdata %>% select(-c(Dx, mx)) %>% 
  spread(Year, Ex) %>% 
  arrange(Age)

mx_smoothed2DUS <- data.frame(Cause=character(), Sex=character(), Age=integer())

#Fit smoothing models across 2 dimensions
  for(j in c("Alcohol", "Drugs", "Suicide", "Cancer", "Metabolic", "Total")){
    for(k in c("Male", "Female")){
      z_i <- z %>% filter(Cause==j & Sex==k) %>% 
        select(-c(Age, Sex, Cause, Sex)) %>% 
        as.matrix()
      
      offset_i <- offset %>% filter(Cause==j & Sex==k) %>% 
        select(-c(Age, Sex, Cause, Sex)) %>% 
        as.matrix() %>% 
        log()
      
      mod <- Mort2Dsmooth(x, y, z_i, offset=offset_i, ndx=c(6,6))
      
      rates_1x1 <- expand.grid(y=y, x=10:85)
      
      mx_smoothed2DUS <- predict(mod, newdata=rates_1x1) %>% 
        exp() %>% 
        as.data.frame() %>% 
        mutate(Age=c(10:85), Cause=j, Sex=k) %>% 
        bind_rows(mx_smoothed2DUS)
      
    }
  }

USsmoothed <- gather(mx_smoothed2DUS, Year, mx_smt2D, c(1:21)) %>% 
  merge(USdata) %>% 
  mutate(Dx_smt2D=mx_smt2D*Ex)

agg_tiff("Outputs/DoDUS2Dsmt.tiff", units="in", width=6, height=8, res=500)
USsmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt2D*100000))+
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  coord_equal()
dev.off()

#Fit smoothing models within years only
mx_smoothed1DUS <- data.frame(Cause=character(), Sex=character(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

  for(j in c("Alcohol", "Drugs", "Suicide", "Cancer", "Metabolic", "Total")){
    for(k in c("Male", "Female")){
      for(l in 1999:2019){
        y <- z %>% filter(Cause==j & Sex==k) %>% 
          select(-c(Age, Sex, Cause, Sex)) %>% 
          select(c(l-1998)) %>% 
          unlist() %>% 
          as.vector()
        
        offset_i <- offset %>% filter(Cause==j & Sex==k) %>% 
          select(-c(Age, Sex, Cause, Sex)) %>% 
          select(c(l-1998)) %>% 
          log() %>% 
          unlist() %>% 
          as.vector()
        
        mod <- Mort1Dsmooth(x, y, offset=offset_i)
        
        mx_smoothed1DUS <- predict(mod, newdata=c(10:85)) %>% 
          exp() %>% 
          as.data.frame() %>% 
          rename(mx_smt1D=1) %>% 
          mutate(Age=c(10:85), Cause=j, Sex=k, Year=l) %>% 
          bind_rows(mx_smoothed1DUS)
      }
    }
  }

USsmoothed <- mx_smoothed1DUS %>% 
  merge(USsmoothed) %>% 
  mutate(Dx_smt1D=mx_smt1D*Ex)

agg_tiff("Outputs/DoDUS1Dsmt.tiff", units="in", width=6, height=10, res=500)
USsmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  coord_equal()
dev.off()

#############
#Merge everything together
Combined <- USsmoothed %>% 
  mutate(Country="USA") %>% 
  bind_rows(ewssmoothed %>% rename(Ex=pop) %>% mutate(Sex=if_else(Sex==1, "Male", "Female"))) %>% 
  bind_rows(Cansmoothed%>% rename(Ex=pop) %>% mutate(Sex=if_else(Sex==1, "Male", "Female")))

agg_tiff("Outputs/DoDCombined1D.tiff", units="in", width=8, height=13, res=500)
Combined %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.4)), text=element_text(family="Lato"),
        plot.title.position = "plot", plot.caption.position="plot")+
  coord_equal()+
  labs(title="Deaths of Despair in men in Canada, the UK and US",
       caption="Data from StatCan, ONS, NRS and CDC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/DoDCombined2D.tiff", units="in", width=8, height=13, res=500)
Combined %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt2D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.4)), text=element_text(family="Lato"))+
  coord_equal()+
  labs(title="Deaths of Despair in men in Canada, the UK and US",
       caption="Data from StatCan, ONS, NRS and CDC | Plot by @VictimOfMaths")
dev.off()

#Compress to age groups of interest
Combined_short <- Combined %>% 
  mutate(ageband=case_when(
    Age<35 ~ "<35",
    Age<45 ~ "35-44",
    Age<55 ~ "45-54",
    Age<65 ~ "55-64",
    TRUE ~ "65+")) %>% 
  group_by(ageband, Country, Cause, Sex, Year) %>% 
  summarise(Dx_smt1D=sum(Dx_smt1D), Dx_smt2D=sum(Dx_smt2D), Ex=sum(Ex)) %>% 
  ungroup() %>% 
  mutate(mx_smt1D=Dx_smt1D*100000/Ex, mx_smt2D=Dx_smt2D*100000/Ex) %>% 
  #calculate 3-year rolling averages
  group_by(ageband, Country, Cause, Sex) %>% 
  arrange(Year) %>% 
  mutate(mx_smt1D_roll=roll_mean(mx_smt1D, 3, align="center", fill=NA))

#TODO DOD only

agg_tiff("Outputs/DoDCombinedTotal.tiff", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Total" & ageband %in% c("35-44")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_smt1D_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband, scales="free")+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.5)), text=element_text(family="Lato"),
        plot.title.position="plot")+
  labs(title="All-cause mortality rates")
dev.off()

agg_tiff("Outputs/DoDCombinedAlcohol.tiff", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Alcohol" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_smt1D_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.5)), text=element_text(family="Lato"),
        plot.title.position="plot")+
  labs(title="Alcohol-specific mortality rates")
dev.off()

agg_tiff("Outputs/DoDCombinedDrugs.tiff", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Drugs" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_smt1D_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.5)), text=element_text(family="Lato"),
        plot.title.position="plot")+
  labs(title="Drug-specific mortality rates")
dev.off()

agg_tiff("Outputs/DoDCombinedSuicide.tiff", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Suicide" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_smt1D_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.5)), text=element_text(family="Lato"),
        plot.title.position="plot")+
  labs(title="Suicide rates")
dev.off()

agg_tiff("Outputs/DoDCombinedCancer.tiff", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Cancer" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_smt1D_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.5)), text=element_text(family="Lato"),
        plot.title.position="plot")+
  labs(title="Cancer mortality rates")
dev.off()

agg_tiff("Outputs/DoDCombinedMetabolic.tiff", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Metabolic" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_smt1D_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.5)), text=element_text(family="Lato"),
        plot.title.position="plot")+
  labs(title="Metabolic mortality rates")
dev.off()

#############################
#APC curvature plots
APCcurve <- Combined %>%
  filter(Age<=85) %>% 
  group_by(Year, Cause, Country, Sex) %>%
  summarise(mean=weighted.mean(Age, Dx_smt1D), 
            meanDx=Dx_smt1D[which(Age==round(mean, digits=0))] ,
            maxDx=max(Dx_smt1D), 
            mode=Age[which(Dx_smt1D==maxDx)][1], 
            moderate=mx_smt1D[which(Age==mode)],
            spread=sd(Dx_smt1D)) %>% 
  ungroup()

ann_text <- data.frame(mode=seq(29, 79, by=5), Year=rep(2022.5, times=11), label=as.character(seq(1995, 1945, by=-5)))

agg_tiff("Outputs/DoDCombinedModalAges.tiff", units="in", width=10, height=10, res=800)
APCcurve %>% 
  filter(!Cause %in% c("Total", "Cancer", "Metabolic")) %>% 
ggplot(aes(x=Year, y=mode))+
  geom_point(aes(colour=Cause, size=moderate*100000), alpha=0.7)+
  geom_point(shape=21, colour="Black", aes(size=moderate*100000))+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family="Roboto")+
  annotate("text", x = 2022, y = 25, label = "Cohort", size=3.2, angle = 45, color="black",
           family="Roboto")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause", labels=c("Alcohol", "Drugs", "Suicide"))+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024))+
  scale_y_continuous(name="Age", breaks=c(seq(20,90, by=10)))+
  facet_grid(Sex ~ Country)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="There is a strong cohort effect in 'Deaths of despair'", 
       subtitle="APC curvature plot showing modal age of death by cause",
       caption="Data from Statistics Canada, Office for National Statistics, National Records of Scotland & Centre for Disease Control \n Plot by @VictimOfMaths")

dev.off()

#US APC curvature plot based on raw data
USAPCcurve <- Combined %>%
  filter(Country=="USA") %>% 
  group_by(Year, Cause, Sex) %>%
  summarise(mean=weighted.mean(Age, Dx), 
            meanDx=Dx[which(Age==round(mean, digits=0))] ,
            maxDx=max(Dx), 
            mode=Age[which(Dx==maxDx)][1], 
            moderate=mx[which(Age==mode)],
            spread=sd(Dx)) %>% 
  ungroup()

agg_tiff("Outputs/DoDUSAModalAges.tiff", units="in", width=8, height=8, res=500)
USAPCcurve %>% 
  filter(!Cause %in% c("Total", "Cancer", "Metabolic")) %>% 
  ggplot(aes(x=Year, y=mode))+
  geom_point(aes(colour=Cause, size=moderate), alpha=0.7)+
  geom_point(shape=21, colour="Black", aes(size=moderate))+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family="Roboto")+
  annotate("text", x = 2022, y = 25, label = "Cohort", size=3.2, angle = 45, color="black",
           family="Roboto")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause", labels=c("Alcohol", "Drugs", "Suicide"))+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024))+
  scale_y_continuous(name="Age", breaks=c(seq(20,90, by=10)))+
  facet_grid(~Sex)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Age patterns in 'Deaths of despair' in the USA", 
       subtitle="APC curvature plot showing modal age of death by cause",
       caption="Data from Centre for Disease Control \n Plot by @VictimOfMaths")

dev.off()

########################
#Alternative ways of visualising cohort effects
cohort <- Combined %>% 
  mutate(cohort=Year-Age, 
         cohort_band=cut(cohort, seq(1910, 2010, 5), right=FALSE)) %>% 
  group_by(cohort_band, Country, Cause, Sex, Year) %>% 
  summarise(Dx_smt1D=sum(Dx_smt1D), Dx_smt2D=sum(Dx_smt2D), Ex=sum(Ex)) %>% 
  ungroup() %>% 
  mutate(mx_smt1D=Dx_smt1D*100000/Ex, mx_smt2D=Dx_smt2D*100000/Ex,
         bandstart=as.numeric(substr(cohort_band, 2, 5)),
         age=Year-bandstart+2.5) 

agg_tiff("Outputs/DoDCohortAlcohol.tiff", units="in", width=10, height=7, res=500)
ggplot(cohort %>% filter(Cause=="Alcohol"), 
       aes(x=age, y=mx_smt1D, group=cohort_band, colour=bandstart))+
  geom_line()+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Deaths per 100,000")+
  facet_grid(Sex~Country)+
  scale_colour_paletteer_c("pals::ocean.curl", direction=-1, name="Birth cohort")+
  theme_classic()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position = "plot")+
  labs(title="Cohort effects in alcohol deaths")
dev.off()

agg_tiff("Outputs/DoDCohortDrugs.tiff", units="in", width=10, height=7, res=500)
ggplot(cohort %>% filter(Cause=="Drugs"), 
       aes(x=age, y=mx_smt1D, group=cohort_band, colour=bandstart))+
  geom_line()+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Deaths per 100,000")+
  facet_grid(Sex~Country)+
  scale_colour_paletteer_c("pals::ocean.curl", direction=-1, name="Birth cohort")+
  theme_classic()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position = "plot")+
  labs(title="Cohort effects in drug deaths")
dev.off()

agg_tiff("Outputs/DoDCohortSuicide.tiff", units="in", width=10, height=7, res=500)
ggplot(cohort %>% filter(Cause=="Suicide"), 
       aes(x=age, y=mx_smt1D, group=cohort_band, colour=bandstart))+
  geom_line()+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Deaths per 100,000")+
  facet_grid(Sex~Country)+
  scale_colour_paletteer_c("pals::ocean.curl", direction=-1, name="Birth cohort")+
  theme_classic()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position = "plot")+
  labs(title="Cohort effects in deaths by suicide")
dev.off()

Combined %>% 
  mutate(cohort=Year-Age) %>% 
  filter(Year=="2018" & Sex=="Male" & Country=="USA" & 
           Cause %in% c("Alcohol", "Drugs", "Suicide") & Age>15) %>% 
  ggplot(aes(x=cohort, y=Dx_smt1D*100000/Ex, colour=Cause))+
  geom_line()

