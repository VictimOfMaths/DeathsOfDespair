rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(cowplot)
library(sf)
library(paletteer)

#Read in data by age

temp1 <- tempfile()
source1 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsrelatedtodrugpoisoningenglandandwalesreferencetable%2fcurrent/2019maindataset1.xls"
temp1 <- curl_download(url=source1, destfile=temp1, quiet=FALSE, mode="wb")
raw.u20 <- read_excel(temp1, sheet="Table 2", range="L9:R91", col_names=FALSE)[-c(28,56),-c(4)]
colnames(raw.u20) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning", 
                       "Combined_Misuse", "England_Misuse", "Wales_Misuse")
raw.u20$age <- "u20"
raw.u20$sex <- rep(c("Total", "Male", "Female"), each=27)
raw.u20$year <- rep(2019:1993, times=3)

raw.2029 <- read_excel(temp1, sheet="Table 2", range="T9:Z91", col_names=FALSE)[-c(28,56),-c(4)]
colnames(raw.2029) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning", 
                       "Combined_Misuse", "England_Misuse", "Wales_Misuse")
raw.2029$age <- "20-29"
raw.2029$sex <- rep(c("Total", "Male", "Female"), each=27)
raw.2029$year <- rep(2019:1993, times=3)

raw.3039 <- read_excel(temp1, sheet="Table 2", range="AB9:AH91", col_names=FALSE)[-c(28,56),-c(4)]
colnames(raw.3039) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning", 
                       "Combined_Misuse", "England_Misuse", "Wales_Misuse")
raw.3039$age <- "30-39"
raw.3039$sex <- rep(c("Total", "Male", "Female"), each=27)
raw.3039$year <- rep(2019:1993, times=3)

raw.4049 <- read_excel(temp1, sheet="Table 2", range="AJ9:AP91", col_names=FALSE)[-c(28,56),-c(4)]
colnames(raw.4049) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning", 
                       "Combined_Misuse", "England_Misuse", "Wales_Misuse")
raw.4049$age <- "40-49"
raw.4049$sex <- rep(c("Total", "Male", "Female"), each=27)
raw.4049$year <- rep(2019:1993, times=3)

raw.5069 <- read_excel(temp1, sheet="Table 2", range="AR9:AX91", col_names=FALSE)[-c(28,56),-c(4)]
colnames(raw.5069) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning", 
                       "Combined_Misuse", "England_Misuse", "Wales_Misuse")
raw.5069$age <- "50-69"
raw.5069$sex <- rep(c("Total", "Male", "Female"), each=27)
raw.5069$year <- rep(2019:1993, times=3)

raw.70 <- read_excel(temp1, sheet="Table 2", range="AZ9:BF91", col_names=FALSE)[-c(28,56),-c(4)]
colnames(raw.70) <- c("Combined_Poisoning", "England_Poisoning", "Wales_Poisoning", 
                       "Combined_Misuse", "England_Misuse", "Wales_Misuse")
raw.70$age <- "70+"
raw.70$sex <- rep(c("Total", "Male", "Female"), each=27)
raw.70$year <- rep(2019:1993, times=3)

data <- bind_rows(raw.u20, raw.2029, raw.3039, raw.4049, raw.5069, raw.70) %>% 
  pivot_longer(c(1:6), names_to=c("Country", "Definition"), names_sep="_", values_to="Deaths")

data$age <- factor(data$age, levels=c("u20", "20-29", "30-39", "40-49", "50-69", "70+"))

#Look at differences by gender
tiff("Outputs/DRDEWSex.tiff", units="in", width=10, height=7, res=500)
data %>% 
  filter(Country=="Combined" & Definition=="Poisoning" & sex!="Total") %>% 
  ggplot()+
  geom_line(aes(x=year, y=Deaths, colour=sex))+
  scale_x_continuous(name="")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="Sex")+
  facet_wrap(~age)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Most drug-related deaths are among men",
       subtitle="Deaths from drug poisoning in England & Wales",
       caption="Date from Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

#Set up grouped path plot
data_drd <- subset(data, Country=="Combined" & Definition=="Poisoning" & sex=="Total") %>% 
  arrange(age, year) %>% 
  mutate(index=c(1:162))

x1 <- c(0, data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1993],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1994],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1995],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1996],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1997],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1998],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==1999],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2000],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2001],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2002],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2003],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2004],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2005],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2006],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2007],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2008],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2009],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2010],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2011],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2012],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2013],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2014],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2015],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2016],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2017],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2018],
        data_drd$Deaths[data_drd$age=="u20" & data_drd$year==2019],0)


x2 <- c(0, data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1993],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1994],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1995],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1996],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1997],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1998],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==1999],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2000],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2001],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2002],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2003],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2004],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2005],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2006],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2007],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2008],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2009],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2010],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2011],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2012],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2013],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2014],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2015],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2016],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2017],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2018],
        data_drd$Deaths[data_drd$age=="20-29" & data_drd$year==2019],0)


x3 <- c(0, data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1993],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1994],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1995],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1996],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1997],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1998],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==1999],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2000],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2001],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2002],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2003],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2004],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2005],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2006],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2007],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2008],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2009],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2010],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2011],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2012],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2013],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2014],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2015],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2016],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2017],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2018],
        data_drd$Deaths[data_drd$age=="30-39" & data_drd$year==2019],0)


x4 <- c(0, data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1993],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1994],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1995],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1996],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1997],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1998],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==1999],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2000],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2001],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2002],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2003],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2004],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2005],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2006],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2007],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2008],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2009],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2010],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2011],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2012],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2013],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2014],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2015],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2016],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2017],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2018],
        data_drd$Deaths[data_drd$age=="40-49" & data_drd$year==2019],0)


x5 <- c(0, data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1993],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1994],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1995],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1996],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1997],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1998],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==1999],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2000],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2001],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2002],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2003],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2004],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2005],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2006],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2007],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2008],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2009],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2010],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2011],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2012],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2013],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2014],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2015],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2016],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2017],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2018],
        data_drd$Deaths[data_drd$age=="50-69" & data_drd$year==2019],0)


x6 <- c(0, data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1993],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1994],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1995],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1996],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1997],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1998],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==1999],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2000],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2001],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2002],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2003],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2004],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2005],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2006],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2007],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2008],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2009],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2010],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2011],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2012],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2013],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2014],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2015],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2016],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2017],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2018],
        data_drd$Deaths[data_drd$age=="70+" & data_drd$year==2019],0)


plot <- ggplot()+
  geom_polygon(aes(x=c(1, 1:27, 27), y=x1), fill="Tomato")+
  geom_polygon(aes(x=c(28, 28:54, 54), y=x2), fill="Tomato")+
  geom_polygon(aes(x=c(55, 55:81, 81), y=x3), fill="Tomato")+
  geom_polygon(aes(x=c(82, 82:108, 108), y=x4), fill="Tomato")+
  geom_polygon(aes(x=c(109, 109:135, 135), y=x5), fill="Tomato")+
  geom_polygon(aes(x=c(136, 136:162, 162), y=x6), fill="Tomato")+
  geom_path(data=data_drd,aes(x=index, y=Deaths, group=age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.15, "cm")))+
  theme_classic()+
  scale_x_continuous(breaks=c(13, 40,67,94,121,148), labels=c("0-19", "20-29",
                                                          "30-39", "40-49", "50-69",
                                                          "70+"),name="Age")+
  scale_y_continuous(name="Annual drug poisoning deaths", limits=c(0,1500))+
  labs(title="Drug-related deaths are rising sharply in older adults",
       subtitle="Drug poisoning deaths in England & Wales 1993-2019",
       caption="Data from Office for National Statistics | Plot by @VictimOfMaths")

inset <- ggplot()+
  geom_polygon(aes(x=c(0,0,1,2,3,4,5,6,7,8,9,10,10), y=c(0,6,4,3,9,10,12,13,10,16,15,17,0)), 
               fill="Tomato")+
  geom_line(aes(x=c(0,1,2,3,4,5,6,7,8,9,10), y=c(6,4,3,9,10,12,13,10,16,15,17)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.15, "cm")))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

DRDfull <- ggdraw()+
  draw_plot(plot)+
  draw_plot(inset, x=0.85, y=0.75, width=0.1, height=0.2)+
  draw_label("1993", x=0.87, y=0.76, size=10)+
  draw_label("2019", x=0.94, y=0.76, size=10)+
  draw_label("Key", x=0.88, y=0.95, size=10)

tiff("Outputs/DRDfullEW.tiff", units="in", width=10, height=7, res=500)
ggdraw(DRDfull)
dev.off()

#Read in regional data
regdata <- read_excel(temp1, sheet="Table 6", range="A9:F331", col_names=FALSE)[,-c(5)]
colnames(regdata) <- c("year", "code", "name1", "name2", "deaths")

regdata <- regdata %>%
  mutate(name=coalesce(name1, name2)) %>% 
  fill(year) %>% 
  select(year, code, deaths, name) %>% 
  na.omit() %>% 
  filter(name!="England")

tiff("Outputs/DRDEWRegions.tiff", units="in", width=10, height=7, res=500)
ggplot(regdata)+
  geom_line(aes(x=year, y=deaths, colour=name))+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="Region")+
  scale_y_continuous("Deaths per 100,000", limits=c(0,NA))+
  scale_x_continuous(name="")+
  theme_classic()+
  labs(title="Drug-related deaths have risen sharply in the North in recent years",
       subtitle="Age-standardised deaths per 100,000 from drug poisoning in England & Wales",
       caption="Date from Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

#Read in data by Local Authority
temp2 <- tempfile()
source2 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdrugmisusedeathsbylocalauthority%2fcurrent/2019localauthorities1.xls"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")
rawdata <- read_excel(temp2, sheet="Table 3", range="A9:CX438", 
                      col_names=FALSE)[,c(1:4,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,102)]
rawdata$name <- coalesce(rawdata$...2, rawdata$...3, rawdata$...4)
rawdata <- rawdata %>% 
  select(!c(2:4)) %>% 
  filter(complete.cases(.))

colnames(rawdata) <- c("code", "2017-19", "2016-18", "2015-17", "2014-16", "2013-15", "2012-14",
                       "2011-13", "2010-12", "2009-11", "2008-10", "2007-09", "2006-08", "2005-07",
                       "2004-06", "2003-05", "2002-04", "2001-03", "name")

data <- gather(rawdata, year, deaths, c(2:18))
data$deaths <- as.numeric(data$deaths)

#Sort out Buckinghamshire
temp <- subset(data, code=="E06000060")

data$code <- if_else(data$code=="E06000060", "E07000004", as.character(data$code))
data$name <- if_else(data$name=="Buckinghamshire", "Aylesbury Vale", as.character(data$name))

temp1 <- temp
temp1$code <- "E07000005"
temp1$name <- "Chiltern"

temp2 <- temp
temp2$code <- "E07000006"
temp2$name <- "South Bucks"

temp$code <- "E07000007"
temp$name <- "Wycombe"

data <- bind_rows(data, temp, temp1, temp2)


#Bring in shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "lad19cd"] <- "code"

map <- full_join(data, shapefile, by="code", all.y=TRUE)

tiff("Outputs/DRDfullEWmap.tiff", units="in", width=7, height=8, res=500)
ggplot()+
  geom_sf(data=subset(map, year=="2017-19"), aes(geometry=geometry, fill=deaths), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.amp", name="Deaths\nper 100,000", na.value="White")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))+
  labs(title="Drug-related deaths are highest in coastal areas and the North",
       subtitle="Age-standardised deaths per 100,000 population from drug poisoning in 2017-19.\nAreas coloured in white had too few deaths to calculate a rate (<10).",
       caption="Data from Office for National Statistics | Plot by @VictimOfMaths")
dev.off()



