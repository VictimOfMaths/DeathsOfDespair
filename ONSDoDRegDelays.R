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
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fimpactofregistrationdelaysonmortalitystatistics%2f2020/registrationdelaysreferencetables.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="2b", range="A6:S127") %>% 
  mutate(Cause=case_when(
    substr(`ICD-10 Chapter`, 1, 3)=="Neo" ~ "Neonatal",
    substr(`ICD-10 Chapter`, 1, 3)=="Pos" ~ "Postneonatal",
    substr(`ICD-10 Chapter`, 1, 3)=="Sui" ~ "Suicide",
    substr(`ICD-10 Chapter`, 1, 3)=="Dru" ~ "Drugs",
    substr(`ICD-10 Chapter`, 1, 3)=="Alc" ~ "Alcohol",
    substr(`ICD-10 Chapter`, 1, 3)=="Due" ~ "COVID-19",
    substr(`ICD-10 Chapter`, 1, 3)=="Tot" ~ "Total")) %>% 
  pivot_longer(c(4:19), names_to=c("Delay", "Metric"), names_sep="\\(", values_to="Value") %>% 
  mutate(Lag=case_when(
    Delay=="Within one week\r\n" ~ "<1 week",
    Delay=="One to two weeks\r\n" ~ "1-2 weeks",
    Delay=="Two to three weeks\r\n" ~ "2-3 weeks",
    Delay=="Three weeks to one month\r\n" ~ "3-4 weeks",
    Delay=="One to three months\r\n" ~ "1-3 months",
    Delay=="Three to six months\r\n" ~ "3-6 months",
    Delay=="Six months to one year\r\n" ~ "6-12 months",
    Delay=="Over one year\r\n" ~ "1+ year"),
    Lag=factor(Lag, levels=c("<1 week","1-2 weeks","2-3 weeks","3-4 weeks","1-3 months",
                             "3-6 months","6-12 months","1+ year")))

agg_png("Outputs/ONSDoDRegDelays.png", units="in", width=12, height=8, res=500)
ggplot(data %>% filter(Cause %in% c("Suicide", "Drugs", "Alcohol") & Metric=="Percentage)"),
                       aes(x=Value/100, y=fct_rev(as.factor(Year)), fill=fct_rev(Lag)))+
  geom_col()+
  scale_x_continuous(name="Proportion of deaths registered", label=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d(name="", "futurevisions::kepler186", guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  theme(legend.position="top")+
  facet_wrap(~Cause)+
  labs(title="Deaths from suicide and drugs take a long time to appear in official statistics",
       subtitle="Lag between deaths occuring and being registered in England & Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
