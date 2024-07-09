rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ragg)
library(ggstream)
library(forcats)
library(ggtext)

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

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/impactofregistrationdelaysonmortalitystatistics/2022/registrationdelays2022.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="Table_2", range="A5:J65") %>% 
  mutate(Cause=case_when(
    `Specific causes of death`=="Suicide deaths - National statistics definition" ~ "Suicide",
    `Specific causes of death`=="Drug related deaths - National statistics definition" ~ "Drugs",
    `Specific causes of death`=="Alcohol specific deaths  - National statistics definition" ~ "Alcohol",
    TRUE ~ `Specific causes of death`),
    Cause=factor(Cause, levels=c("All causes", "Alcohol", "Drugs", "Suicide")))

agg_png("Outputs/ONSDoDRegDelaysDuration.png", units="in", width=9, height=6, res=800)
ggplot(data %>% filter(Cause %in% c("All causes", "Suicide", "Drugs", "Alcohol")),
       aes(y=`Median delay in days`, x=`Year of registration`))+
  geom_ribbon(aes(ymin=`Lower quartile of registration delay in days`,
                  ymax=`Upper quartile of registration delay in days`, 
                  fill=Cause), alpha=0.3)+
  geom_line(aes(colour=Cause))+
  geom_hline(yintercept=0, colour="grey30")+
  scale_x_continuous(name="", breaks=c(2014, 2016, 2018, 2020, 2022),
                     labels=c("2014", "2016", "2018", "2020", "2022"))+
  scale_y_continuous(limits=c(0,NA))+
  scale_colour_manual(values=c("black", "#00A1FF", "#E69F00", "#CC5395"))+
  scale_fill_manual(values=c("black", "#00A1FF", "#E69F00", "#CC5395"))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()+
  theme(legend.position="none", axis.line.x=element_blank())+
  labs(title="Delays in death registrations have increased",
       subtitle="Lag between deaths occuring and being registered in England & Wales.\nLines represent median delays, shaded areas the inter-quartile range",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()