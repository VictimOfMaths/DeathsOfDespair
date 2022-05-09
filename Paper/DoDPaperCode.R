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
library(pdftools)
library(gt)

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
username <- "c.r.angus@sheffield.ac.uk" 
password <- "1574553541"

#########################################################
#Start by pulling together all the data and tidying it up

#Get 2019 populations for the UK nations, as not in HMD yet
#Thanks to Luyin Zhang for the code to tidy this data up
ukpop <- tempfile()
ukpopurl <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
ukpop <- curl_download(url=ukpopurl, destfile=ukpop, quiet=FALSE, mode="wb")

rawpop <- list()
rawpop$male <- read_excel(ukpop, sheet = 'MYE2 - Males', skip = 4)
rawpop$female <- read_excel(ukpop, sheet = 'MYE2 - Females', skip = 4)

#Clean data
cleanedpop <- rawpop %>% bind_rows(.id='sex') %>% 
  filter(Name %in% c('ENGLAND AND WALES', 'SCOTLAND', 'NORTHERN IRELAND') ) %>%
  pivot_longer(
    cols = c(`0`:`90+`),
    names_to = 'age',
    values_to = 'pop'
  ) %>% 
  mutate(
    country = case_when(
      Name=='ENGLAND AND WALES' ~ 1,
      Name=='SCOTLAND' ~ 2,
      Name=='NORTHERN IRELAND' ~ 3
    ) %>% factor(labels = c('ENW','SCO','NIR')),
    Sex = ifelse(sex=='male',1,2),
    Age = parse_number(age)
  ) %>%
  select(country, Sex, Age, pop) %>%
  arrange(country, Sex, Age, pop)
  

#################
#England & Wales#
#################

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
    Age=="80-84" ~ 80, TRUE ~ 85),
    Age=case_when(
      Age=="<1" ~ "0", Age=="01-04" ~ "1-4", TRUE ~ Age)) %>% 
  arrange(agestart) %>% 
  relocate(Cause, agestart)

#Download populations/exposures from HMD
ewpop <- readHMDweb(CNTRY="GBRTENW", "Exposures_1x1", username, password) %>% 
  filter(Year>=2001) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2)) %>% 
  #Add in 2019 data from ONS
  merge(cleanedpop %>% filter(country=="ENW") %>% 
          select(-country), all.x=TRUE) %>% 
  rename("2019"="pop")

#Group populations to match deaths age groups
ewpop.grouped <- ewpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2019`, sum, na.rm=TRUE)) %>% 
  ungroup()

##########
#Scotland#
##########
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
  mutate(Sex=if_else(Sex=="Male", 1, 2)) %>% 
  #Add in 2019 data from ONS
  merge(cleanedpop %>% filter(country=="SCO") %>% 
        select(-country), all.x=TRUE) %>% 
  rename("2019"="pop")

#Group populations to match deaths age groups
scotpop.grouped <- scotpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2019`, sum, na.rm=TRUE)) %>% 
  ungroup()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "username", "password", "font", "theme_custom",
                        "cleanedpop")))

##################
#Northern Ireland#
##################
#Get NI data
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
niurl.2016 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/2016.zip"
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
         Sex=as.numeric(if_else(Sex %in% c("Female", "F"), "2", "1")),
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
  mutate(Sex=as.numeric(if_else(Sex %in% c("M", "Male"), "1", "2")),
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
nipop <- readHMDweb(CNTRY="GBR_NIR", "Exposures_1x1", username, password) %>% 
  filter(Year>=2001) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male", 1, 2)) %>% 
  #Add in 2019 data from ONS
  merge(cleanedpop %>% filter(country=="NIR") %>% 
          select(-country), all.x=TRUE) %>% 
  rename("2019"="pop")

#Group populations to match deaths age groups
nipop.grouped <- nipop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2019`, sum, na.rm=TRUE)) %>% 
  ungroup()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "nidata.wide", "nipop", "nipop.grouped", "password",
                        "username", "font", "theme_custom")))

#Combine EW, Scotland & NI
UKdata <- ewdata.wide %>% 
  gather(Year, Dx, c(5:23)) %>% 
  select(Cause, agestart, Sex, Year, Dx) %>% 
  mutate(Country="England & Wales") %>%
  merge(ewpop.grouped %>% gather(Year, pop, c(3:21))) %>% 
  mutate(mx=Dx*100000/pop) %>% 
  bind_rows(scotdata.wide %>% 
              gather(Year, Dx, c(5:23)) %>% 
              select(Cause, agestart, Sex, Year, Dx) %>% 
              mutate(Country="Scotland") %>%
              merge(scotpop.grouped %>% gather(Year, pop, c(3:21))) %>% 
              mutate(mx=Dx*100000/pop)) %>% 
  bind_rows(nidata.wide %>% 
              gather(Year, Dx, c(5:23)) %>% 
              select(Cause, agestart, Sex, Year, Dx) %>% 
              mutate(Country="Northern Ireland") %>%
              merge(nipop.grouped %>% gather(Year, pop, c(3:21))) %>% 
              mutate(mx=Dx*100000/pop))

#Plot raw mx
agg_png("Outputs/DoDUKRawMale.png", units="in", width=7, height=10, res=500)
UKdata %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=as.factor(agestart), fill=mx))+
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_discrete(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Raw mortality rates from 'Deaths of Despair' in UK males",
       subtitle="Lexis surfaces showing mortality rates in 5-year age bands between 2001-2019*",
       caption="Data from Office for National Statistics, Northern Ireland Statistics\nand Research Agency and National Records of Scotland\n\n*Northern Irish data from 2004-2019 only")

dev.off()

agg_png("Outputs/DoDUKRawFemale.png", units="in", width=7, height=10, res=500)
UKdata %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==2) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=as.factor(agestart), fill=mx))+
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_discrete(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+  
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Raw mortality rates from 'Deaths of Despair' in UK females",
       subtitle="Lexis surfaces showing mortality rates in 5-year age bands between 2001-2019*",
       caption="Data from Office for National Statistics, Northern Ireland Statistics\nand Research Agency and National Records of Scotland\n\n*Northern Irish data from 2004-2019 only")

dev.off()

#Apply smoothing based on Tim Riffe's suggested approach
#Prediction models fall over if you include <10 year olds, so exclude them as not relevant to analysis
x <- seq(10,85, by=5)
UKdata <- UKdata %>% filter(agestart>=10)
y <- 2001:2019
z <- UKdata %>% select(-c(pop, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, Dx) %>% 
  arrange(agestart)

offset <- UKdata %>% select(-c(Dx, mx)) %>% 
  filter(agestart>=10) %>% 
  spread(Year, pop) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx_smoothed1D <- data.frame(Country=character(), Cause=character(), Sex=integer(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

for(i in c("England & Wales", "Northern Ireland", "Scotland")){
  for(j in c("Alcohol", "Drugs", "Suicide", "Total")){
    for(k in 1:2){
      for(l in 2001:2019){
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

UKsmoothed <- mx_smoothed1D %>% 
  merge(ewpop %>% gather(Year, pop.ew, c(3:21))) %>% 
  merge(scotpop %>% gather(Year, pop.s, c(3:21))) %>% 
  merge(nipop %>% gather(Year, pop.ni, c(3:21)), all.x=TRUE) %>% 
  mutate(pop=case_when(
    Country=="Scotland" ~ pop.s, 
    Country=="England & Wales" ~ pop.ew,
    TRUE ~ pop.ni)) %>% 
  select(-c(pop.s, pop.ew, pop.ni)) %>% 
  mutate(Dx_smt1D=mx_smt1D*pop)

agg_png("Outputs/DoDUK1DsmtMale.png", units="in", width=6, height=12, res=500)
UKsmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(strip.text=element_text(size=rel(0.7)), 
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Smoothed mortality rates from 'Deaths of Despair'\nin UK males",
       subtitle="Lexis surfaces showing mortality rates by age between 2001-2019",
       caption="Data from Office for National Statistics, Northern Ireland Statistics\nand Research Agency and National Records of Scotland\n")

dev.off()

agg_png("Outputs/DoDUK1DsmtFemale.png", units="in", width=6, height=12, res=500)
UKsmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==2 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_discrete(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(strip.text=element_text(size=rel(0.7)), 
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Smoothed mortality rates from 'Deaths of Despair'\nin UK females",
       subtitle="Lexis surfaces showing mortality rates by age between 2001-2019",
       caption="Data from Office for National Statistics, Northern Ireland Statistics\nand Research Agency and National Records of Scotland\n")

dev.off()

#################################################################################################
#It would be nice to use {ungroup} instead for the smoothing,  but there is a bug with 
#the package which returns negative smoothed mortality rates for relatively low prevalence causes 
#(e.g. all deaths of despair)
#https://github.com/mpascariu/ungroup/issues/8
#################################################################################################

#Read in Canadian data
#Cancer data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310014201
#(not using cancer data any more, but this file has the all cause data in it)
temp <- tempfile()
Can.canurl <- "https://raw.githubusercontent.com/VictimOfMaths/DeathsOfDespair/master/Paper/StatCan%20Cancer.csv"
temp <- curl_download(url=Can.canurl, destfile=temp, quiet=FALSE, mode="wb")

Can.can <- read.csv(temp)

#Mental and behavioural data from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014301
Can.menurl <- "https://github.com/VictimOfMaths/DeathsOfDespair/raw/master/Paper/StatCan%20Mental.csv"
temp <- curl_download(url=Can.menurl, destfile=temp, quiet=FALSE, mode="wb")

Can.men <- read.csv(temp)

#Liver disease from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310014801
Can.digurl <- "https://raw.githubusercontent.com/VictimOfMaths/DeathsOfDespair/master/Paper/StatCan%20Digestive.csv"
temp <- curl_download(url=Can.digurl, destfile=temp, quiet=FALSE, mode="wb")

Can.dig <- read.csv(temp)

#External cause data from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310015601
Can.exturl <- "https://github.com/VictimOfMaths/DeathsOfDespair/raw/master/Paper/StatCan%20External.csv"
temp <- curl_download(url=Can.exturl, destfile=temp, quiet=FALSE, mode="wb")

Can.ext <- read.csv(temp)

#Combine and separate out into causes of interest
Candata <- bind_rows(Can.can, Can.men, Can.dig, Can.ext) %>% 
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
    Age=="85 to 89 years" ~ 85, TRUE ~ 90),
    Sex=if_else(Sex=="Males", 1, 2))

#Bring in exposures/populations from HMD 

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
agg_png("Outputs/DoDCanRawMale.png", units="in", width=7, height=10, res=500)
Candata %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=as.factor(agestart), fill=mx))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_discrete(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Raw mortality rates from 'Deaths of Despair' in Canadian males",
       subtitle="Lexis surfaces showing mortality rates in 5-year age bands between 2001-2019",
       caption="Data from Statistics Canada")

dev.off()

agg_png("Outputs/DoDCanRawFemale.png", units="in", width=7, height=10, res=500)
Candata %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==2) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=as.factor(agestart), fill=mx))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_discrete(name="Age (5 year bands)", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Raw mortality rates from 'Deaths of Despair' in Canadian females",
       subtitle="Lexis surfaces showing mortality rates in 5-year age bands between 2001-2019",
       caption="Data from Statistics Canada")

dev.off()

#Apply 1D smoothing based on Tim Riffe's suggested approach
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

#Fit smoothing models within years only
mx_smoothed1D <- data.frame(Cause=character(), Sex=integer(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

for(i in c("Alcohol", "Drugs", "Suicide", "Total")){
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
  mutate(Dx_smt1D=mx_smt1D*pop, Country="Canada") 

agg_png("Outputs/DoDCan1DsmtMale.png", units="in", width=6, height=8, res=500)
Cansmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==1 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Smoothed mortality rates from 'Deaths of Despair'\nin Canadian males",
       subtitle="Lexis surfaces showing mortality rates by age between 2001-2019",
       caption="Data from Statistics Canada")
dev.off()

agg_png("Outputs/DoDCan1DsmtFemale.png", units="in", width=6, height=8, res=500)
Cansmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex==2 & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Smoothed mortality rates from 'Deaths of Despair'\nin Canadian females",
       subtitle="Lexis surfaces showing mortality rates by age between 2001-2019",
       caption="Data from Statistics Canada")
dev.off()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "nidata.wide", "nipop", "nipop.grouped", "UKsmoothed",
                        "Candata", "Canpop", "Canpop.grouped", "Cansmoothed",  "password", 
                        "username", "font", "theme_custom")))


#########################################################################
#Read in US data
#All downloaded from the CDC wonder database using the same ICD-10 definitions as above
temp <- tempfile()
US.alcurl <- "https://raw.githubusercontent.com/VictimOfMaths/DeathsOfDespair/master/Paper/CDCAlcoholDeaths.txt"
temp <- curl_download(url=US.alcurl, destfile=temp, quiet=FALSE, mode="wb")

US.alc <- read.csv(temp, sep="\t") %>% 
  mutate(Cause="Alcohol")

US.drgurl <- "https://raw.githubusercontent.com/VictimOfMaths/DeathsOfDespair/master/Paper/CDCDrugDeaths.txt"
temp <- curl_download(url=US.drgurl, destfile=temp, quiet=FALSE, mode="wb")

US.drg <- read.csv(temp, sep="\t") %>% 
  mutate(Cause="Drugs")

US.scdurl <- "https://raw.githubusercontent.com/VictimOfMaths/DeathsOfDespair/master/Paper/CDCSuicideDeaths.txt"
temp <- curl_download(url=US.scdurl, destfile=temp, quiet=FALSE, mode="wb")

US.scd <- read.csv(temp, sep="\t") %>% 
  mutate(Cause="Suicide")

US.toturl <- "https://raw.githubusercontent.com/VictimOfMaths/DeathsOfDespair/master/Paper/CDCAllCauseDeaths.txt"
temp <- curl_download(url=US.toturl, destfile=temp, quiet=FALSE, mode="wb")

US.tot <- read.csv(temp, sep="\t") %>% 
  mutate(Cause="Total")

USdata <- bind_rows(US.alc, US.drg, US.scd, US.tot) %>% 
  filter(Notes!="Total" & !is.na(Deaths)) %>% 
  mutate(Age=as.integer(as.character(Single.Year.Ages.Code))) %>% 
  select(Year, Cause, Gender, Age, Deaths) %>% 
  rename(Dx=Deaths, Sex=Gender) %>% 
  #Calculate other cause deaths
  spread(Cause, Dx) %>% 
  mutate(Other=Total-Alcohol-Drugs-Suicide) %>% 
  gather(Cause, Dx, c(4:8))

#Bring in exposures/populations from HMD as CDC data is missing populations for 85+
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
agg_png("Outputs/DoDUSRaw.png", units="in", width=7, height=8, res=500)
ggplot(USdata %>% filter(Age<=85 & !Cause %in% c("Other", "Total")), 
       aes(x=Year, y=Age, fill=mx))+ 
  geom_raster()+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Raw mortality rates from 'Deaths of Despair' in the USA",
       subtitle="Annual mortality rates by single year of age 2001-2019",
       caption="Data from Center for Disease Control")
dev.off()

#smoothing
x <- c(10:85)
USdata <- USdata %>% filter(Age %in% c(10:85))
y <- 1999:2019
z <- USdata %>% select(-c(Ex, mx)) %>% 
  spread(Year, Dx) %>% 
  arrange(Age)

offset <- USdata %>% select(-c(Dx, mx)) %>% 
  spread(Year, Ex) %>% 
  arrange(Age)

#Fit smoothing models within years only
mx_smoothed1DUS <- data.frame(Cause=character(), Sex=character(), Age=integer(),
                              Year=integer(), mx_smt1D=double())

for(j in c("Alcohol", "Drugs", "Suicide", "Total")){
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
  merge(USdata) %>% 
  mutate(Dx_smt1D=mx_smt1D*Ex)

agg_png("Outputs/DoDUS1Dsmt.png", units="in", width=6, height=8, res=500)
USsmoothed %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Smoothed mortality rates from 'Deaths of Despair'\nin the USA",
       subtitle="Lexis surfaces showing mortality rates by age between 2001-2019",
       caption="Data from Center for Disease Control")
dev.off()

rm(list=setdiff(ls(), c("ewdata.wide", "ewpop", "ewpop.grouped", "scotdata.wide", "scotpop", 
                        "scotpop.grouped", "nidata.wide", "nipop", "nipop.grouped", "UKsmoothed",
                        "Candata", "Canpop", "Canpop.grouped", "Cansmoothed", "USsmoothed", 
                        "password", "username", "font", "theme_custom")))

#############
#Merge everything together
#Note that mx_smt1D is already in per 100,000 units
Combined <- USsmoothed %>% 
  mutate(Country="USA") %>% 
  bind_rows(UKsmoothed %>% rename(Ex=pop) %>% mutate(Sex=if_else(Sex==1, "Male", "Female"))) %>% 
  bind_rows(Cansmoothed%>% rename(Ex=pop) %>% mutate(Sex=if_else(Sex==1, "Male", "Female"))) 

  #Add in combined deaths of despair cause
Combined <- Combined %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide")) %>% 
  group_by(Age, Sex, Year, Country) %>% 
  summarise(Dx=sum(Dx, na.rm=TRUE), Ex=unique(Ex), Dx_smt1D=sum(Dx_smt1D)) %>% 
  ungroup() %>% 
  mutate(mx_smt1D=Dx_smt1D/Ex,
         Cause="DoD") %>% 
  bind_rows(Combined) 
    
agg_png("Outputs/DoDCombined1DMale.png", units="in", width=8, height=13, res=500)
Combined %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Deaths of Despair in men in Canada, the UK and US",
       subtitle="Mortality rates by age 2001-2019",
       caption="Data from StatCan, ONS, NRS, NISRA and CDC")
dev.off()

agg_png("Outputs/DoDCombined1DFemale.png", units="in", width=8, height=13, res=500)
Combined %>% 
  filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Female" & Age<80) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths\nper 100,000")+
  facet_grid(Cause~Country)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Deaths of Despair in women in Canada, the UK and US",
       subtitle="Mortality rates by age 2001-2019",
       caption="Data from StatCan, ONS, NRS, NISRA and CDC")
dev.off()

#Compress to age groups of interest
#Age-standardising within each age-group to European Standard Population
Combined_short <- Combined %>% 
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
  summarise(Dx_smt1D=sum(Dx_smt1D), Ex=sum(Ex), mx_std=weighted.mean(mx_smt1D, stdpop)*100000) %>% 
  ungroup() %>% 
  mutate(mx_smt1D=Dx_smt1D*100000/Ex) %>% 
  #calculate 3-year rolling averages
  group_by(ageband, Country, Cause, Sex) %>% 
  arrange(Year) %>% 
  mutate(mx_smt1D_roll=roll_mean(mx_smt1D, 3, align="center", fill=NA),
         mx_std_roll=roll_mean(mx_std, 3, align="center", fill=NA))

agg_png("Outputs/DoDCombinedTotal.png", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Total" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  mutate(ymax=case_when(
    ageband=="35-44" ~ 300, ageband=="45-54" ~ 700, ageband=="55-64" ~ 1800)) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_std_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_wrap(Sex~ageband, scales="free_y")+
  geom_blank(aes(y=ymax))+
  theme_custom()+
  theme(panel.background=element_rect(fill="Grey95"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Midlife mortality from all causes in Canada, the UK and the US, 2001-2019\n ")

dev.off()

agg_png("Outputs/DoDCombinedDoD.png", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="DoD" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_std_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(panel.background=element_rect(fill="Grey95"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+  
  labs(title="Midlife mortality from combined 'Deaths of Despair' in Canada, the UK and the US\n2001-2019")

dev.off()

agg_png("Outputs/DoDCombinedAlcohol.png", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Alcohol" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_std_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(panel.background=element_rect(fill="Grey95"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+  
  labs(title="Midlife alcohol-specific mortality in Canada, the UK and the US 2001-2019")

dev.off()

agg_png("Outputs/DoDCombinedDrugs.png", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Drugs" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_std_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(panel.background=element_rect(fill="Grey95"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+  
  labs(title="Midlife drug-related mortality in Canada, the UK and the US 2001-2019")

dev.off()

agg_png("Outputs/DoDCombinedSuicide.png", units="in", width=10, height=6, res=500)
Combined_short %>% 
  filter(Cause=="Suicide" & ageband %in% c("35-44", "45-54", "55-64")) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=mx_std_roll, colour=Country))+
  scale_y_continuous(name="Deaths per 100,000\n(Age-standardised)", limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::mpalette", name="")+
  facet_grid(Sex~ageband)+
  theme_custom()+
  theme(panel.background=element_rect(fill="Grey95"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+  
  labs(title="Midlife suicide mortality in Canada, the UK and the US 2001-2019")

dev.off()

###########################
#Generate tables of mortality rates
tabledata_f <- Combined_short %>% 
  ungroup() %>% 
  select(ageband, Country, Sex, Year, Cause, mx_std) %>% 
  mutate(mx_std=round(mx_std, digits=1),
         Cause=case_when(
           Cause=="DoD" ~ "Combined 'Deaths of Despair'",
           Cause=="Total" ~ "All-Cause",
           TRUE ~ Cause)) %>% 
  filter(Year %in% c(2001, 2010, 2019) & ageband %in% c("35-44", "45-54", "55-64") &
           Sex=="Female") %>% 
  select(-Sex) %>% 
  arrange(Country, Year) %>% 
  pivot_wider(names_from=c(Country, Year), values_from=c(mx_std)) %>% 
  mutate(Cause=factor(Cause, levels=c("All-Cause", "Combined 'Deaths of Despair'",
                                      "Alcohol", "Drugs", "Suicide"))) %>% 
  group_by(Cause)

tabledata_m <- Combined_short %>% 
  ungroup() %>% 
  select(ageband, Country, Sex, Year, Cause, mx_std) %>% 
  mutate(mx_std=round(mx_std, digits=1),
         Cause=case_when(
           Cause=="DoD" ~ "Combined 'Deaths of Despair'",
           Cause=="Total" ~ "All-Cause",
           TRUE ~ Cause)) %>% 
  filter(Year %in% c(2001, 2010, 2019) & ageband %in% c("35-44", "45-54", "55-64") &
           Sex=="Male") %>% 
  select(-Sex) %>% 
  arrange(Country, Year) %>% 
  pivot_wider(names_from=c(Country, Year), values_from=c(mx_std)) %>% 
  mutate(Cause=factor(Cause, levels=c("All-Cause", "Combined 'Deaths of Despair'",
                                      "Alcohol", "Drugs", "Suicide"))) %>% 
  group_by(Cause)

table_f <- gt(tabledata_f, rowname_col = "ageband") %>% 
  tab_header(title=md("**Female mortality rates for 'Deaths of Despair'**"),
             subtitle="Age-standardised death rates among women by year, country, cause and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  tab_spanner(label="Canada", columns=c(Canada_2001, Canada_2010,
                                                Canada_2019)) %>% 
 tab_spanner(label="England & Wales", columns=c(`England & Wales_2001`, 
                                                       `England & Wales_2010`,
                                                       `England & Wales_2019`)) %>% 
  tab_spanner(label="Northern Ireland", columns=c(`Northern Ireland_2001`, 
                                                  `Northern Ireland_2010`,
                                                        `Northern Ireland_2019`)) %>% 
  tab_spanner(label="Scotland", columns=c(Scotland_2001, Scotland_2010,
                                                Scotland_2019)) %>% 
  tab_spanner(label="USA", columns=c(USA_2001, USA_2010,
                                           USA_2019)) %>% 
  cols_label(Canada_2001="2001",
             Canada_2010="2010",
             Canada_2019="2019",
             `England & Wales_2001`="2001",
             `England & Wales_2010`="2010",
             `England & Wales_2019`="2019",
             `Northern Ireland_2001`="2001",
             `Northern Ireland_2010`="2010",
             `Northern Ireland_2019`="2019",
             Scotland_2001="2001",
             Scotland_2010="2010",
             Scotland_2019="2019",        
             USA_2001="2001",
             USA_2010="2010",
             USA_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(Canada_2001, Canada_2010, Canada_2019), colors=c("#017a4a"), alpha=0.1,
             apply_to=c("fill")) %>% 
  data_color(columns=c(`England & Wales_2001`, `England & Wales_2010`, `England & Wales_2019`), 
             colors=c("#FFCE4E"), alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(`Northern Ireland_2001`, `Northern Ireland_2010`, `Northern Ireland_2019`), 
             colors=c("#3d98d3"), alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(Scotland_2001, Scotland_2010, Scotland_2019), colors=c("#ff363c"), alpha=0.1,
             apply_to=c("fill")) %>% 
  data_color(columns=c(USA_2001, USA_2010, USA_2019), colors=c("#7559a2"), alpha=0.1,
             apply_to=c("fill")) %>% 
  data_color(columns=c(Canada_2001, Canada_2010, Canada_2019, `England & Wales_2001`, 
                       `England & Wales_2010`, `England & Wales_2019`, `Northern Ireland_2001`,
                       `Northern Ireland_2010`, `Northern Ireland_2019`, Scotland_2001, 
                       Scotland_2010, Scotland_2019, USA_2001, USA_2010, USA_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

table_m <- gt(tabledata_m, rowname_col = "ageband") %>% 
  tab_header(title=md("**Male mortality rates for 'Deaths of Despair'**"),
             subtitle="Age-standardised death rates among men by year, country, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  tab_spanner(label="Canada", columns=c(Canada_2001, Canada_2010,
                                            Canada_2019)) %>% 
  tab_spanner(label="England & Wales", columns=c(`England & Wales_2001`, 
                                                     `England & Wales_2010`,
                                                     `England & Wales_2019`)) %>% 
  tab_spanner(label="Northern Ireland", columns=c(`Northern Ireland_2001`, 
                                                  `Northern Ireland_2010`,
                                                  `Northern Ireland_2019`)) %>% 
  tab_spanner(label="Scotland", columns=c(Scotland_2001, Scotland_2010,
                                              Scotland_2019)) %>% 
  tab_spanner(label="USA", columns=c(USA_2001, USA_2010,
                                         USA_2019)) %>% 
  cols_label(Canada_2001="2001",
             Canada_2010="2010",
             Canada_2019="2019",
             `England & Wales_2001`="2001",
             `England & Wales_2010`="2010",
             `England & Wales_2019`="2019",
             `Northern Ireland_2001`="2001",
             `Northern Ireland_2010`="2010",
             `Northern Ireland_2019`="2019",  
             Scotland_2001="2001",
             Scotland_2010="2010",
             Scotland_2019="2019",
             USA_2001="2001",
             USA_2010="2010",
             USA_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts()))%>% 
  data_color(columns=c(Canada_2001, Canada_2010, Canada_2019), colors=c("#017a4a"), alpha=0.1,
             apply_to=c("fill")) %>% 
  data_color(columns=c(`England & Wales_2001`, `England & Wales_2010`, `England & Wales_2019`), 
             colors=c("#FFCE4E"), alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(`Northern Ireland_2001`, `Northern Ireland_2010`, `Northern Ireland_2019`), 
             colors=c("#3d98d3"), alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(Scotland_2001, Scotland_2010, Scotland_2019), colors=c("#ff363c"), alpha=0.1,
             apply_to=c("fill")) %>% 
  data_color(columns=c(USA_2001, USA_2010, USA_2019), colors=c("#7559a2"), alpha=0.1,
             apply_to=c("fill")) %>% 
  data_color(columns=c(Canada_2001, Canada_2010, Canada_2019, `England & Wales_2001`, 
                       `England & Wales_2010`, `England & Wales_2019`, `Northern Ireland_2001`,
                       `Northern Ireland_2010`, `Northern Ireland_2019`, Scotland_2001, 
                       Scotland_2010, Scotland_2019, USA_2001, USA_2010, USA_2019), 
             colors=c("Black"), 
             apply_to=c("text"))


gtsave(table_f, "Outputs/DoDTableFemale.png")
gtsave(table_m, "Outputs/DoDTableMale.png")

#############################
#APC curvature plots

#Start with a diagnostic plot showing mortality rates by age
Combined %>% filter(Cause %in% c("Alcohol", "Drugs", "Suicide") & Sex=="Male") %>% 
  ggplot(aes(x=Age, y=mx_smt1D, colour=Year, group=Year))+
  geom_line()+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_paletteer_c("scico::cork")+
  facet_grid(Cause ~ Country)+
  theme_custom()

#Pick out modal age of death for each cause, in each country in each year
APCcurve <- Combined %>%
  filter(Age<=65 & Age>=20) %>% 
  group_by(Year, Cause, Country, Sex) %>%
  summarise(maxDx=max(Dx_smt1D),
            maxmx=max(mx_smt1D),
            mode=Age[which(Dx_smt1D==maxDx)][1], 
            mode_mx=Age[which(mx_smt1D==maxmx)][1], 
            moderate=mx_smt1D[which(Age==mode)],
            moderate_mx=mx_smt1D[which(Age==mode_mx)],
            meanage=weighted.mean(Age, Dx_smt1D)) %>% 
  ungroup()

ann_text <- data.frame(mode_mx=seq(29, 64, by=5), Year=rep(2022.5, times=8), 
                       label=as.character(seq(1995, 1960, by=-5)))

agg_png("Outputs/DoDCombinedModalAges.png", units="in", width=10, height=7, res=800)
APCcurve %>% 
  filter(!Cause %in% c("Total", "DoD")) %>% 
  ggplot(aes(x=Year, y=mode_mx))+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=-Inf, ymax=35), fill="Grey80", colour=NA)+
  #geom_rect(aes(xmin=1999, xmax=2020, ymin=65, ymax=Inf), fill="Grey80", colour=NA)+
  geom_point(aes(colour=Cause, size=moderate_mx*100000), alpha=0.7)+
  geom_point(shape=21, colour="Black", aes(size=moderate_mx*100000))+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2021, y = 22, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause", 
                      labels=c("Alcohol", "Drugs", "Suicide"))+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024), breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(seq(20,60, by=10)), limits=c(18, 66))+
  facet_grid(Sex ~ Country)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family=font), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Temporal patterns in the age with the highest death rates", 
       subtitle="APC curvature plot for 'Deaths of Despair'",
       caption="Circles represent the age within each year with the highest mortality rate for each of the three causes, restricted to ages 20-65, sized according to the mortality rate.")
       
dev.off()

agg_png("Outputs/DoDCombinedModalAgesAlternate.png", units="in", width=10, height=7, res=800)
APCcurve %>% 
  filter(!Cause %in% c("Total", "DoD")) %>% 
  ggplot(aes(x=Year, y=mode_mx))+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=-Inf, ymax=35), fill="Grey80", colour=NA)+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=65, ymax=Inf), fill="Grey80", colour=NA)+
  geom_point(aes(colour=Country, size=moderate_mx*100000), alpha=0.7)+
  geom_point(shape=21, colour="Black", aes(size=moderate_mx*100000))+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2022, y = 25, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  scale_colour_paletteer_d("awtools::mpalette")+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024), breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(seq(20,90, by=10)))+
  facet_grid(Sex ~ Cause)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family=font), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Temporal patterns in the age with the highest death rates", 
       subtitle="APC curvature plot for 'Deaths of Despair'",
       caption="Circles represent the age within each year with the highest mortality rate for each of the three causes, restricted to ages 20-85,\nsized according to the mortality rate.")

dev.off()

#Plot of *mean* ages of death instead
ann_text2 <- data.frame(meanage=seq(29, 70, by=5), Year=rep(2022.5, times=9), 
                       label=as.character(seq(1995, 1955, by=-5)))

agg_png("Outputs/DoDCombinedMeanAges.png", units="in", width=10, height=9, res=800)
APCcurve %>% 
  filter(!Cause %in% c("Total", "DoD")) %>% 
  ggplot(aes(x=Year, y=meanage))+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=-Inf, ymax=35), fill="Grey80", colour=NA)+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=65, ymax=Inf), fill="Grey80", colour=NA)+
  geom_line(aes(colour=Cause), size=1.2)+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text2,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2021, y = 22, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause", 
                      labels=c("Alcohol", "Drugs", "Suicide"))+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024), breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(seq(20,90, by=10)), limits=c(18, 70))+
  facet_grid(Sex ~ Country)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family=font), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Temporal patterns in average ages of 'Deaths of Despair", 
       subtitle="Mean ages of death by cause, country, year and sex")

dev.off()

agg_png("Outputs/DoDCombinedMeanAgesAlternate.png", units="in", width=8, height=9, res=800)
APCcurve %>% 
  filter(!Cause %in% c("Total", "DoD")) %>% 
  ggplot(aes(x=Year, y=meanage))+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=-Inf, ymax=35), fill="Grey80", colour=NA)+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=65, ymax=Inf), fill="Grey80", colour=NA)+
  geom_line(aes(colour=Country), size=1.2)+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text2,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2022, y = 25, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  scale_colour_paletteer_d("awtools::mpalette")+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024), breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(seq(20,90, by=10)), limits=c(18, 70))+
  facet_grid(Sex ~ Cause)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family=font), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Temporal patterns in average ages of 'Deaths of Despair", 
       subtitle="Mean ages of death by cause, country, year and sex")

dev.off()

#US APC curvature plot based on raw data
USAPCcurve <- Combined %>%
  filter(Country=="USA") %>% 
  group_by(Year, Cause, Sex) %>%
  summarise(maxDx=max(Dx_smt1D),
            maxmx=max(mx_smt1D),
            mode=Age[which(Dx_smt1D==maxDx)][1], 
            mode_mx=Age[which(mx_smt1D==maxmx)][1], 
            moderate=mx_smt1D[which(Age==mode)],
            moderate_mx=mx_smt1D[which(Age==mode_mx)]) %>% 
  ungroup()

agg_png("Outputs/DoDUSAModalAges.png", units="in", width=8, height=8, res=500)
USAPCcurve %>% 
  filter(!Cause %in% c("Total", "Cancer", "Metabolic", "DoD")) %>% 
  ggplot(aes(x=Year, y=mode_mx))+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=-Inf, ymax=34), fill="Grey80", colour=NA)+
  geom_rect(aes(xmin=1999, xmax=2020, ymin=65, ymax=Inf), fill="Grey80", colour=NA)+
  geom_point(aes(colour=Cause, size=moderate_mx*100000), alpha=0.7)+
  geom_point(shape=21, colour="Black", aes(size=moderate_mx*100000))+
  theme_classic()+
  geom_vline(xintercept = seq(2000, 2020, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(20, 75, by=5), linetype="dashed", color="grey30", size=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-1995, -1860, by=5), slope = 1, linetype="dashed", color="grey30", size=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2022, y = 25, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause", labels=c("Alcohol", "Drugs", "Suicide"))+
  scale_size(name="Deaths per 100,000")+
  scale_x_continuous(name="Period", limits=c(1999, 2024), breaks=c(2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Age", breaks=c(seq(20,90, by=10)))+
  facet_grid(~Sex)+
  coord_equal()+
  theme(panel.spacing=unit(2, "lines"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        text=element_text(family=font), plot.title.position="plot",
        plot.caption.position = "plot")+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Age patterns in 'Deaths of despair' in the USA", 
       subtitle="APC curvature plot showing modal age of death by cause")

dev.off()

########################
#Alternative ways of visualising cohort effects
cohort <- Combined %>% 
  mutate(cohort=Year-Age, 
         cohort_band=cut(cohort, seq(1910, 2010, 5), right=FALSE)) %>% 
  group_by(cohort_band, Country, Cause, Sex, Year) %>% 
  summarise(Dx_smt1D=sum(Dx_smt1D), Ex=sum(Ex)) %>% 
  ungroup() %>% 
  mutate(mx_smt1D=Dx_smt1D*100000/Ex, 
         bandstart=as.numeric(substr(cohort_band, 2, 5)),
         age=Year-bandstart+2.5) 

agg_png("Outputs/DoDCohortAlcohol.png", units="in", width=10, height=7, res=500)
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

agg_png("Outputs/DoDCohortDrugs.png", units="in", width=10, height=7, res=500)
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

agg_png("Outputs/DoDCohortSuicide.png", units="in", width=10, height=7, res=500)
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

##################
#Supplemental tables
#Generate tables of mortality rates
tabledata_f_supp <- Combined_short %>% 
  ungroup() %>% 
  select(ageband, Country, Sex, Year, Cause, mx_std) %>% 
  mutate(mx_std=round(mx_std, digits=1),
         Cause=case_when(
           Cause=="DoD" ~ "Combined 'Deaths of Despair'",
           Cause=="Total" ~ "All-Cause",
           TRUE ~ Cause)) %>% 
  filter(ageband %in% c("35-44", "45-54", "55-64") &
           Sex=="Female") %>% 
  select(-Sex) %>% 
  arrange(Country, Year) %>% 
  pivot_wider(names_from=c(Country, Year), values_from=c(mx_std)) %>% 
  mutate(Cause=factor(Cause, levels=c("All-Cause", "Combined 'Deaths of Despair'",
                                      "Alcohol", "Drugs", "Suicide"))) %>% 
  group_by(Cause)

tabledata_m_supp <- Combined_short %>% 
  ungroup() %>% 
  select(ageband, Country, Sex, Year, Cause, mx_std) %>% 
  mutate(mx_std=round(mx_std, digits=1),
         Cause=case_when(
           Cause=="DoD" ~ "Combined 'Deaths of Despair'",
           Cause=="Total" ~ "All-Cause",
           TRUE ~ Cause)) %>% 
  filter(ageband %in% c("35-44", "45-54", "55-64") &
           Sex=="Male") %>% 
  select(-Sex) %>% 
  arrange(Country, Year) %>% 
  pivot_wider(names_from=c(Country, Year), values_from=c(mx_std)) %>% 
  mutate(Cause=factor(Cause, levels=c("All-Cause", "Combined 'Deaths of Despair'",
                                      "Alcohol", "Drugs", "Suicide"))) %>% 
  group_by(Cause)

#Canada
table_f_supp_can <- gt(tabledata_f_supp %>% select(ageband, Cause, starts_with("Canada")), 
                      rowname_col = "ageband") %>% 
  tab_header(title=md("**Female mortality rates for 'Deaths of Despair' in Canada**"),
             subtitle="Age-standardised death rates among women by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(Canada_2001="2001", Canada_2002="2002", Canada_2003="2003", Canada_2004="2004",
             Canada_2005="2005", Canada_2006="2006", Canada_2007="2007", Canada_2008="2008",
             Canada_2009="2009", Canada_2010="2010", Canada_2011="2011", Canada_2012="2012",
             Canada_2013="2013", Canada_2014="2014", Canada_2015="2015", Canada_2016="2016",
             Canada_2017="2017", Canada_2018="2018", Canada_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(Canada_2001, Canada_2002, Canada_2003, Canada_2004, Canada_2005,
                       Canada_2006, Canada_2007, Canada_2008, Canada_2009, Canada_2010, 
                       Canada_2011, Canada_2012, Canada_2013, Canada_2014, Canada_2015,
                       Canada_2016, Canada_2017, Canada_2018, Canada_2019), colors=c("#017a4a"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(Canada_2001, Canada_2002, Canada_2003, Canada_2004, Canada_2005,
                       Canada_2006, Canada_2007, Canada_2008, Canada_2009, Canada_2010, 
                       Canada_2011, Canada_2012, Canada_2013, Canada_2014, Canada_2015,
                       Canada_2016, Canada_2017, Canada_2018, Canada_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

table_m_supp_can <- gt(tabledata_m_supp %>% select(ageband, Cause, starts_with("Canada")), 
                      rowname_col = "ageband") %>% 
  tab_header(title=md("**Male mortality rates for 'Deaths of Despair' in Canada**"),
             subtitle="Age-standardised death rates among men by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(Canada_2001="2001", Canada_2002="2002", Canada_2003="2003", Canada_2004="2004",
             Canada_2005="2005", Canada_2006="2006", Canada_2007="2007", Canada_2008="2008",
             Canada_2009="2009", Canada_2010="2010", Canada_2011="2011", Canada_2012="2012",
             Canada_2013="2013", Canada_2014="2014", Canada_2015="2015", Canada_2016="2016",
             Canada_2017="2017", Canada_2018="2018", Canada_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(Canada_2001, Canada_2002, Canada_2003, Canada_2004, Canada_2005,
                       Canada_2006, Canada_2007, Canada_2008, Canada_2009, Canada_2010, 
                       Canada_2011, Canada_2012, Canada_2013, Canada_2014, Canada_2015,
                       Canada_2016, Canada_2017, Canada_2018, Canada_2019), colors=c("#017a4a"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(Canada_2001, Canada_2002, Canada_2003, Canada_2004, Canada_2005,
                       Canada_2006, Canada_2007, Canada_2008, Canada_2009, Canada_2010, 
                       Canada_2011, Canada_2012, Canada_2013, Canada_2014, Canada_2015,
                       Canada_2016, Canada_2017, Canada_2018, Canada_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

gtsave(table_f_supp_can, "Outputs/DoDTableFemale_supp_can.png")
gtsave(table_m_supp_can, "Outputs/DoDTableMale_supp_can.png")

#England & Wales
table_f_supp_ew <- gt(tabledata_f_supp %>% select(ageband, Cause, starts_with("England")), 
                       rowname_col = "ageband") %>% 
  tab_header(title=md("**Female mortality rates for 'Deaths of Despair' in England & Wales**"),
             subtitle="Age-standardised death rates among women by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(`England & Wales_2001`="2001", `England & Wales_2002`="2002", `England & Wales_2003`="2003", `England & Wales_2004`="2004",
             `England & Wales_2005`="2005", `England & Wales_2006`="2006", `England & Wales_2007`="2007", `England & Wales_2008`="2008",
             `England & Wales_2009`="2009", `England & Wales_2010`="2010", `England & Wales_2011`="2011", `England & Wales_2012`="2012",
             `England & Wales_2013`="2013", `England & Wales_2014`="2014", `England & Wales_2015`="2015", `England & Wales_2016`="2016",
             `England & Wales_2017`="2017", `England & Wales_2018`="2018", `England & Wales_2019`="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(`England & Wales_2001`, `England & Wales_2002`, `England & Wales_2003`, `England & Wales_2004`, `England & Wales_2005`,
                       `England & Wales_2006`, `England & Wales_2007`, `England & Wales_2008`, `England & Wales_2009`, `England & Wales_2010`, 
                       `England & Wales_2011`, `England & Wales_2012`, `England & Wales_2013`, `England & Wales_2014`, `England & Wales_2015`,
                       `England & Wales_2016`, `England & Wales_2017`, `England & Wales_2018`, `England & Wales_2019`), colors=c("#FFCE4E"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(`England & Wales_2001`, `England & Wales_2002`, `England & Wales_2003`, `England & Wales_2004`, `England & Wales_2005`,
                       `England & Wales_2006`, `England & Wales_2007`, `England & Wales_2008`, `England & Wales_2009`, `England & Wales_2010`, 
                       `England & Wales_2011`, `England & Wales_2012`, `England & Wales_2013`, `England & Wales_2014`, `England & Wales_2015`,
                       `England & Wales_2016`, `England & Wales_2017`, `England & Wales_2018`, `England & Wales_2019`), 
             colors=c("Black"), 
             apply_to=c("text"))

table_m_supp_ew <- gt(tabledata_m_supp %>% select(ageband, Cause, starts_with("England")), 
                       rowname_col = "ageband") %>% 
  tab_header(title=md("**Male mortality rates for 'Deaths of Despair' in England & Wales**"),
             subtitle="Age-standardised death rates among men by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(`England & Wales_2001`="2001", `England & Wales_2002`="2002", `England & Wales_2003`="2003", `England & Wales_2004`="2004",
             `England & Wales_2005`="2005", `England & Wales_2006`="2006", `England & Wales_2007`="2007", `England & Wales_2008`="2008",
             `England & Wales_2009`="2009", `England & Wales_2010`="2010", `England & Wales_2011`="2011", `England & Wales_2012`="2012",
             `England & Wales_2013`="2013", `England & Wales_2014`="2014", `England & Wales_2015`="2015", `England & Wales_2016`="2016",
             `England & Wales_2017`="2017", `England & Wales_2018`="2018", `England & Wales_2019`="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(`England & Wales_2001`, `England & Wales_2002`, `England & Wales_2003`, `England & Wales_2004`, `England & Wales_2005`,
                       `England & Wales_2006`, `England & Wales_2007`, `England & Wales_2008`, `England & Wales_2009`, `England & Wales_2010`, 
                       `England & Wales_2011`, `England & Wales_2012`, `England & Wales_2013`, `England & Wales_2014`, `England & Wales_2015`,
                       `England & Wales_2016`, `England & Wales_2017`, `England & Wales_2018`, `England & Wales_2019`), colors=c("#FFCE4E"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(`England & Wales_2001`, `England & Wales_2002`, `England & Wales_2003`, `England & Wales_2004`, `England & Wales_2005`,
                       `England & Wales_2006`, `England & Wales_2007`, `England & Wales_2008`, `England & Wales_2009`, `England & Wales_2010`, 
                       `England & Wales_2011`, `England & Wales_2012`, `England & Wales_2013`, `England & Wales_2014`, `England & Wales_2015`,
                       `England & Wales_2016`, `England & Wales_2017`, `England & Wales_2018`, `England & Wales_2019`), 
             colors=c("Black"), 
             apply_to=c("text"))

gtsave(table_f_supp_ew, "Outputs/DoDTableFemale_supp_ew.png")
gtsave(table_m_supp_ew, "Outputs/DoDTableMale_supp_ew.png")

#Northern Ireland
table_f_supp_ni <- gt(tabledata_f_supp %>% select(ageband, Cause, starts_with("Northern")), 
                      rowname_col = "ageband") %>% 
  tab_header(title=md("**Female mortality rates for 'Deaths of Despair' in Northern Ireland**"),
             subtitle="Age-standardised death rates among women by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(`Northern Ireland_2001`="2001", `Northern Ireland_2002`="2002", `Northern Ireland_2003`="2003", `Northern Ireland_2004`="2004",
             `Northern Ireland_2005`="2005", `Northern Ireland_2006`="2006", `Northern Ireland_2007`="2007", `Northern Ireland_2008`="2008",
             `Northern Ireland_2009`="2009", `Northern Ireland_2010`="2010", `Northern Ireland_2011`="2011", `Northern Ireland_2012`="2012",
             `Northern Ireland_2013`="2013", `Northern Ireland_2014`="2014", `Northern Ireland_2015`="2015", `Northern Ireland_2016`="2016",
             `Northern Ireland_2017`="2017", `Northern Ireland_2018`="2018", `Northern Ireland_2019`="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(`Northern Ireland_2001`, `Northern Ireland_2002`, `Northern Ireland_2003`, `Northern Ireland_2004`, `Northern Ireland_2005`,
                       `Northern Ireland_2006`, `Northern Ireland_2007`, `Northern Ireland_2008`, `Northern Ireland_2009`, `Northern Ireland_2010`, 
                       `Northern Ireland_2011`, `Northern Ireland_2012`, `Northern Ireland_2013`, `Northern Ireland_2014`, `Northern Ireland_2015`,
                       `Northern Ireland_2016`, `Northern Ireland_2017`, `Northern Ireland_2018`, `Northern Ireland_2019`), colors=c("#3d98d3"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(`Northern Ireland_2001`, `Northern Ireland_2002`, `Northern Ireland_2003`, `Northern Ireland_2004`, `Northern Ireland_2005`,
                       `Northern Ireland_2006`, `Northern Ireland_2007`, `Northern Ireland_2008`, `Northern Ireland_2009`, `Northern Ireland_2010`, 
                       `Northern Ireland_2011`, `Northern Ireland_2012`, `Northern Ireland_2013`, `Northern Ireland_2014`, `Northern Ireland_2015`,
                       `Northern Ireland_2016`, `Northern Ireland_2017`, `Northern Ireland_2018`, `Northern Ireland_2019`), 
             colors=c("Black"), 
             apply_to=c("text"))

table_m_supp_ni <- gt(tabledata_m_supp %>% select(ageband, Cause, starts_with("Northern")), 
                      rowname_col = "ageband") %>% 
  tab_header(title=md("**Male mortality rates for 'Deaths of Despair' in Northern Ireland**"),
             subtitle="Age-standardised death rates among men by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(`Northern Ireland_2001`="2001", `Northern Ireland_2002`="2002", `Northern Ireland_2003`="2003", `Northern Ireland_2004`="2004",
             `Northern Ireland_2005`="2005", `Northern Ireland_2006`="2006", `Northern Ireland_2007`="2007", `Northern Ireland_2008`="2008",
             `Northern Ireland_2009`="2009", `Northern Ireland_2010`="2010", `Northern Ireland_2011`="2011", `Northern Ireland_2012`="2012",
             `Northern Ireland_2013`="2013", `Northern Ireland_2014`="2014", `Northern Ireland_2015`="2015", `Northern Ireland_2016`="2016",
             `Northern Ireland_2017`="2017", `Northern Ireland_2018`="2018", `Northern Ireland_2019`="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(`Northern Ireland_2001`, `Northern Ireland_2002`, `Northern Ireland_2003`, `Northern Ireland_2004`, `Northern Ireland_2005`,
                       `Northern Ireland_2006`, `Northern Ireland_2007`, `Northern Ireland_2008`, `Northern Ireland_2009`, `Northern Ireland_2010`, 
                       `Northern Ireland_2011`, `Northern Ireland_2012`, `Northern Ireland_2013`, `Northern Ireland_2014`, `Northern Ireland_2015`,
                       `Northern Ireland_2016`, `Northern Ireland_2017`, `Northern Ireland_2018`, `Northern Ireland_2019`), colors=c("#3d98d3"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(`Northern Ireland_2001`, `Northern Ireland_2002`, `Northern Ireland_2003`, `Northern Ireland_2004`, `Northern Ireland_2005`,
                       `Northern Ireland_2006`, `Northern Ireland_2007`, `Northern Ireland_2008`, `Northern Ireland_2009`, `Northern Ireland_2010`, 
                       `Northern Ireland_2011`, `Northern Ireland_2012`, `Northern Ireland_2013`, `Northern Ireland_2014`, `Northern Ireland_2015`,
                       `Northern Ireland_2016`, `Northern Ireland_2017`, `Northern Ireland_2018`, `Northern Ireland_2019`), 
             colors=c("Black"), 
             apply_to=c("text"))

gtsave(table_f_supp_ni, "Outputs/DoDTableFemale_supp_ni.png")
gtsave(table_m_supp_ni, "Outputs/DoDTableMale_supp_ni.png")

#Scotland
table_f_supp_sco <- gt(tabledata_f_supp %>% select(ageband, Cause, starts_with("Scotland")), 
                       rowname_col = "ageband") %>% 
  tab_header(title=md("**Female mortality rates for 'Deaths of Despair' in Scotland**"),
             subtitle="Age-standardised death rates among women by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(Scotland_2001="2001", Scotland_2002="2002", Scotland_2003="2003", Scotland_2004="2004",
             Scotland_2005="2005", Scotland_2006="2006", Scotland_2007="2007", Scotland_2008="2008",
             Scotland_2009="2009", Scotland_2010="2010", Scotland_2011="2011", Scotland_2012="2012",
             Scotland_2013="2013", Scotland_2014="2014", Scotland_2015="2015", Scotland_2016="2016",
             Scotland_2017="2017", Scotland_2018="2018", Scotland_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(Scotland_2001, Scotland_2002, Scotland_2003, Scotland_2004, Scotland_2005,
                       Scotland_2006, Scotland_2007, Scotland_2008, Scotland_2009, Scotland_2010, 
                       Scotland_2011, Scotland_2012, Scotland_2013, Scotland_2014, Scotland_2015,
                       Scotland_2016, Scotland_2017, Scotland_2018, Scotland_2019), colors=c("#ff363c"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(Scotland_2001, Scotland_2002, Scotland_2003, Scotland_2004, Scotland_2005,
                       Scotland_2006, Scotland_2007, Scotland_2008, Scotland_2009, Scotland_2010, 
                       Scotland_2011, Scotland_2012, Scotland_2013, Scotland_2014, Scotland_2015,
                       Scotland_2016, Scotland_2017, Scotland_2018, Scotland_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

table_m_supp_sco <- gt(tabledata_m_supp %>% select(ageband, Cause, starts_with("Scotland")), 
                       rowname_col = "ageband") %>% 
  tab_header(title=md("**Male mortality rates for 'Deaths of Despair' in Scotland**"),
             subtitle="Age-standardised death rates among men by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(Scotland_2001="2001", Scotland_2002="2002", Scotland_2003="2003", Scotland_2004="2004",
             Scotland_2005="2005", Scotland_2006="2006", Scotland_2007="2007", Scotland_2008="2008",
             Scotland_2009="2009", Scotland_2010="2010", Scotland_2011="2011", Scotland_2012="2012",
             Scotland_2013="2013", Scotland_2014="2014", Scotland_2015="2015", Scotland_2016="2016",
             Scotland_2017="2017", Scotland_2018="2018", Scotland_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(Scotland_2001, Scotland_2002, Scotland_2003, Scotland_2004, Scotland_2005,
                       Scotland_2006, Scotland_2007, Scotland_2008, Scotland_2009, Scotland_2010, 
                       Scotland_2011, Scotland_2012, Scotland_2013, Scotland_2014, Scotland_2015,
                       Scotland_2016, Scotland_2017, Scotland_2018, Scotland_2019), colors=c("#ff363c"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(Scotland_2001, Scotland_2002, Scotland_2003, Scotland_2004, Scotland_2005,
                       Scotland_2006, Scotland_2007, Scotland_2008, Scotland_2009, Scotland_2010, 
                       Scotland_2011, Scotland_2012, Scotland_2013, Scotland_2014, Scotland_2015,
                       Scotland_2016, Scotland_2017, Scotland_2018, Scotland_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

gtsave(table_f_supp_sco, "Outputs/DoDTableFemale_supp_sco.png")
gtsave(table_m_supp_sco, "Outputs/DoDTableMale_supp_sco.png", vwidth=1200)

#USA
table_f_supp_us <- gt(tabledata_f_supp %>% select(ageband, Cause, starts_with("USA")), 
                       rowname_col = "ageband") %>% 
  tab_header(title=md("**Female mortality rates for 'Deaths of Despair' in USA**"),
             subtitle="Age-standardised death rates among women by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(USA_2001="2001", USA_2002="2002", USA_2003="2003", USA_2004="2004",
             USA_2005="2005", USA_2006="2006", USA_2007="2007", USA_2008="2008",
             USA_2009="2009", USA_2010="2010", USA_2011="2011", USA_2012="2012",
             USA_2013="2013", USA_2014="2014", USA_2015="2015", USA_2016="2016",
             USA_2017="2017", USA_2018="2018", USA_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(USA_2001, USA_2002, USA_2003, USA_2004, USA_2005,
                       USA_2006, USA_2007, USA_2008, USA_2009, USA_2010, 
                       USA_2011, USA_2012, USA_2013, USA_2014, USA_2015,
                       USA_2016, USA_2017, USA_2018, USA_2019), colors=c("#7559a2"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(USA_2001, USA_2002, USA_2003, USA_2004, USA_2005,
                       USA_2006, USA_2007, USA_2008, USA_2009, USA_2010, 
                       USA_2011, USA_2012, USA_2013, USA_2014, USA_2015,
                       USA_2016, USA_2017, USA_2018, USA_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

table_m_supp_us <- gt(tabledata_m_supp %>% select(ageband, Cause, starts_with("USA")), 
                       rowname_col = "ageband") %>% 
  tab_header(title=md("**Male mortality rates for 'Deaths of Despair' in USA**"),
             subtitle="Age-standardised death rates among men by year, cause, and age group") %>% 
  tab_stubhead(label="Age group") %>% 
  cols_label(USA_2001="2001", USA_2002="2002", USA_2003="2003", USA_2004="2004",
             USA_2005="2005", USA_2006="2006", USA_2007="2007", USA_2008="2008",
             USA_2009="2009", USA_2010="2010", USA_2011="2011", USA_2012="2012",
             USA_2013="2013", USA_2014="2014", USA_2015="2015", USA_2016="2016",
             USA_2017="2017", USA_2018="2018", USA_2019="2019") %>% 
  row_group_order(groups=c("All-Cause", "Combined 'Deaths of Despair'",
                           "Alcohol", "Drugs", "Suicide")) %>% 
  tab_options(table.font.size = "small") %>% 
  opt_table_font(font=c(google_font(name="Lato"), default_fonts())) %>% 
  data_color(columns=c(USA_2001, USA_2002, USA_2003, USA_2004, USA_2005,
                       USA_2006, USA_2007, USA_2008, USA_2009, USA_2010, 
                       USA_2011, USA_2012, USA_2013, USA_2014, USA_2015,
                       USA_2016, USA_2017, USA_2018, USA_2019), colors=c("#7559a2"), 
             alpha=0.1, apply_to=c("fill")) %>% 
  data_color(columns=c(USA_2001, USA_2002, USA_2003, USA_2004, USA_2005,
                       USA_2006, USA_2007, USA_2008, USA_2009, USA_2010, 
                       USA_2011, USA_2012, USA_2013, USA_2014, USA_2015,
                       USA_2016, USA_2017, USA_2018, USA_2019), 
             colors=c("Black"), 
             apply_to=c("text"))

gtsave(table_f_supp_us, "Outputs/DoDTableFemale_supp_us.png")
gtsave(table_m_supp_us, "Outputs/DoDTableMale_supp_us.png", vwidth=1200)

