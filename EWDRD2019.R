rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)

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

data %>% 
  filter(Country=="Combined" & Definition=="Poisoning" & sex!="Total") %>% 
  ggplot()+
  geom_line(aes(x=year, y=Deaths, colour=sex))+
  facet_wrap(~age)+
  theme_classic()

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