rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(ragg)
library(extrafont)
library(cowplot)
library(ggtext)

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

#Download Australian ASD and DRD data
temp <- tempfile()
url.aus <- "https://www.abs.gov.au/statistics/health/causes-death/causes-death-australia/2021/2021_13%20Drug%20and%20alcohol-induced%20deaths%20%28Australia%29.xlsx"
rawfile.aus <- curl_download(url=url.aus, destfile=temp, quiet=FALSE, mode="wb")

#Grab alcohol data
rawdata.aus <- read_excel(rawfile.aus, sheet="Table 13.12", range="M25:V29", col_names=FALSE) %>% 
  mutate(Age=c("15-34", "35-44", "45-54", "55-64", "65+")) %>% 
  gather(Year, mortrate, c(1:(ncol(.)-1))) %>% 
  mutate(Year=as.numeric(substr(Year, 4, 5))+2011,
         Cause="Alcohol") %>% 
  #Grab drug data
  bind_rows(read_excel(rawfile.aus, sheet="Table 13.2", range="B27:V32", col_names=FALSE) %>% 
              select(-`...11`) %>% 
              set_names(c(paste(rep(2012:2021, times=2), rep(c("count", "rate"), each=10), sep="_"))) %>% 
              mutate(Age=c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")) %>% 
              pivot_longer(cols=c(1:(ncol(.)-1)), names_to=c("Year", "Metric"), names_sep="_",
                           values_to="Value") %>% 
              #Thanks to ABS for randomly making the age groups different for drugs and alcohol
              spread(Metric, Value) %>% 
              mutate(pop=count/rate*100000,
                     Age=if_else(Age %in% c("15-24", "25-34"), "15-34", Age)) %>%
              group_by(Year, Age) %>% 
              summarise(pop=sum(pop), count=sum(count), .groups="drop") %>% 
              mutate(mortrate=count*100000/pop,
                     Cause="Drugs", Year=as.numeric(Year)) %>% 
              select(-c(pop, count)))

#Plot

ASDDRDplot <- ggplot()+
  geom_area(data=rawdata.aus %>% filter(Cause=="Alcohol"), aes(x=Year, y=mortrate), fill="SkyBlue",
            alpha=0.5)+
  geom_area(data=rawdata.aus %>% filter(Cause=="Drugs"), aes(x=Year, y=mortrate), fill="Tomato",
            alpha=0.5)+
  geom_path(data=rawdata.aus %>% filter(Cause=="Alcohol"), aes(x=Year, y=mortrate), colour="#0c2c84",
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  geom_path(data=rawdata.aus %>% filter(Cause=="Drugs"), aes(x=Year, y=mortrate), colour="#990000",
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_wrap(~Age, strip.position="bottom", nrow=1)+
  theme_custom()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
        plot.subtitle=element_markdown())+
  labs(title="Deaths from alcohol and drugs in Australia have very different age patterns",
       subtitle="Rates of <span style='color:#0c2c84;'>alcohol-induced</span> and <span style='color:#990000;'>drug-induced</span> mortality in Australia by age between 2012 and 2021",
       caption="Data from ABS\nPlot by @VictimOfMaths")

ASDDRDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:10, 10), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,0)), 
               fill="Grey70")+
  geom_line(aes(x=c(1:10), 
                y=c(21,20,18,15,17,21,18,20,16,17)), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Black")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDDRDfull <- ggdraw()+
  draw_plot(ASDDRDplot)+
  draw_plot(ASDDRDinset, x=0.1, y=0.65, width=0.13, height=0.2)+
  draw_label("2012", x=0.1, y=0.66, size=10, colour="Black")+
  draw_label("2021", x=0.2, y=0.66, size=10, colour="Black")+
  draw_label("Key", x=0.1, y=0.85, size=11, fontface="bold")

tiff("Outputs/ASDDRDAusxAge.tiff", units="in", width=12, height=6, res=600)
ggdraw(ASDDRDfull)
dev.off()
