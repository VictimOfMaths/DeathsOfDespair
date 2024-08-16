rm(list=ls())

library(curl)
library(readxl)
library(keyring)
library(tidyverse)
library(HMDHFDplus)
library(ragg)
library(extrafont)
library(cowplot)
library(scales)
library(ggtext)
library(gt)
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
           code1=="K" & code2 %in% c(70) ~ "Alcohol",
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
  ungroup()%>% 
  mutate(Sex=if_else(Sex==1, "Male", "Female"))


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
  gather(Sex, Ex, c("Male", "Female")) 

#Group populations to match deaths age groups
scotpop.grouped <- scotpop %>% 
  mutate(Age=case_when(
    Age==0 ~ "0", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19",
    Age<25 ~ "20-24", Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49",
    Age<55 ~ "50-54", Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79",
    Age<85 ~ "80-84", TRUE ~ "85+")) %>% 
  group_by(Sex, Age, Year) %>%
  summarise(Ex=sum(Ex, na.rm=TRUE), .groups="drop") 

#Merge together
finaldata <- merge(scotdata, scotpop.grouped, all.x=TRUE) %>% 
  #Add total age group
  bind_rows(merge(scotdata, scotpop.grouped, all.x=TRUE) %>% 
              group_by(Sex, Cause, Year) %>% 
              summarise(Dx=sum(Dx), Ex=sum(Ex), .groups="drop") %>% 
              mutate(Age="All ages")) 

#Add combined sex group
finaldata <- bind_rows(finaldata, finaldata %>% 
                         group_by(Age, Cause, Year) %>% 
                         summarise(Dx=sum(Dx), Ex=sum(Ex), .groups="drop") %>% 
                         mutate(Sex="Both")) %>% 
  mutate(mx=Dx*100000/Ex)

#Age-standardise
ASdata <- finaldata %>% 
  filter(!Age %in% c("0", "1-4", "5-9")) %>% 
  mutate(stdpop=case_when(
    Age=="10-14" ~ 5500, Age=="15-19" ~ 5500, Age=="20-24" ~ 6000, Age=="25-29" ~ 6000, 
    Age=="30-34" ~ 6500, Age=="35-39" ~ 7000, Age=="40-44" ~ 7000, Age=="45-49" ~ 7000, 
    Age=="50-54" ~ 7000, Age=="55-59" ~ 6500, Age=="60-64" ~ 6000, Age=="65-58" ~ 5500, 
    Age=="70-74" ~ 5000, Age=="75-79" ~ 4000, Age=="80-84" ~ 2500, Age=="85+" ~ 1500, 
    TRUE ~ 1000)) %>% 
  group_by(Cause, Sex, Year) %>% 
  summarise(Dx=sum(Dx), Ex=sum(Ex), 
            mx_std=weighted.mean(mx, stdpop), .groups="drop") 

agg_png("Outputs/ScotlandDoD2023Overall.png", units="in", width=9, height=6, res=800)
ggplot(ASdata %>% filter(Cause!="Other" & Sex=="Both"), aes(x=Year, y=mx_std, colour=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  #geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=55), fill="Grey92", colour=NA)+
  geom_line()+
  geom_point()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="")+  theme_custom()+
  theme_custom()+
  theme(axis.line.x=element_blank(), panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="Deaths from drugs and suicide rose, with alcohol deaths stable in Scotland in 2023",
       subtitle="Age-standardised rates of deaths from alcohol-specific causes, drug related causes and suicide in Scotland 2001-2023",
       caption="Data from National Records of Scotland and mortality.org\nPlot by @VictimOfMaths")

dev.off()

#By Sex
agg_png("Outputs/ScotlandDoD2023xSex.png", units="in", width=10, height=6, res=800)
ggplot(ASdata %>% filter(Cause!="Other" & Sex!="Both"), aes(x=Year, y=mx_std, colour=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  #geom_rect(aes(xmin=2019.5, xmax=2023.5, ymin=0, ymax=55), fill="Grey92", colour=NA)+
  geom_line()+
  geom_point()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised deaths per 100,000")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="")+  
  facet_wrap(~Sex)+
  theme_custom()+
  theme(axis.line.x=element_blank(), panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="'Deaths of despair' rose in Scotland in 2023 for men, but not women",
       subtitle="Age-standardised rates of deaths from alcohol-specific causes, drug related causes and suicide in Scotland 2001-2023\n ",
       caption="Data from National Records of Scotland and mortality.org\nPlot by @VictimOfMaths")

dev.off()

#By age

#generate inset key
LineInset <- ggplot()+
  #geom_polygon(aes(x=c(1, 1:23, 23), 
  #                 y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8,12,10,0)), 
  #             fill="SkyBlue")+
  geom_line(aes(x=c(1:23), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,8,12,10)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="grey30")+
  theme_custom()+
  theme(axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank(), axis.line=element_line(colour="grey30"))

Plot1 <- finaldata %>% filter(Sex=="Both" & Cause!="Other" & !Age %in% c("All ages", "0", "1-4", "5-9")) %>% 
  mutate(Age=factor(Age, levels=c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                                  "80-84", "85+"))) %>% 
  ggplot(aes(x=Year, y=mx, colour=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="")+  
  facet_wrap(~Age, nrow=1, strip.position="bottom")+
  theme_custom()+
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())

agg_png("Outputs/ScotlandDoD2023xAge.png", units="in", width=12, height=6, res=800)
ggdraw()+
  draw_plot(Plot1)+
  draw_plot(LineInset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.16, y=0.65, size=10, fontfamily="Lato", colour="grey30")+
  draw_label("2023", x=0.27, y=0.65, size=10, fontfamily="Lato", colour="grey30")+
  draw_label("Key", x=0.16, y=0.87, size=10, fontface="bold", fontfamily="Lato", colour="grey30")

dev.off()

agg_png("Outputs/ScotlandDoD2023xAgev2.png", units="in", width=12, height=6, res=800)
finaldata %>% filter(Sex=="Both" & Cause!="Other" & !Age %in% c("All ages", "0", "1-4", "5-9")) %>% 
  mutate(Age=factor(Age, levels=c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                                  "80-84", "85+"))) %>% 
  ggplot(aes(x=Year, y=mx, colour=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(alpha=0.3)+
  geom_line(data=. %>% filter(Year>=2022), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="")+  
  facet_wrap(~Age, nrow=1, strip.position="bottom")+
  theme_custom()+
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  labs(title="Age patterns in deaths from alcohol, drugs and suicide in Scotland",
       subtitle="Age-specific trends in mortality rates from alcohol-specific causes, drug-related causes and suicide 2001-2023\nChanges from 2022 to 2023 picked out in bold",
       caption="Data from National Records of Scotland and mortality.org\nPlot by @VictimOfMaths")

dev.off()
