rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(extrafont)
library(ragg)
library(readxl)
library(ggrepel)

options(scipen=10000)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Read in DRD data
url1 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsrelatedtodrugpoisoningbyselectedsubstances%2f2020registrations/2020substancespivot.xlsx"
temp1 <- tempfile()
temp1 <- curl_download(url=url1, destfile=temp1, quiet=FALSE, mode="wb")

#Have to use a stupid hacky cell_limits thing here because readxl throws an error with too many rows of data
DRDdata <- read_excel(temp1, sheet="All data", range=cell_limits(c(5, 1), c(NA, 7))) %>% 
  filter(`Usual residence name`=="England and Wales" & Mentioned=="On the death certificate" &
           Sex=="Persons") %>% 
  select(c(1,5,7)) %>% 
  set_names(c("Year", "Substance", "Deaths")) %>% 
  mutate(Substance=case_when(
    Substance=="01 All drug poisonings" ~ "All DRDs",
    Substance=="02 Any opiate (includes unspecified opiates, excludes paracetamol compounds)" ~ "Any opiates",
    Substance=="03 Heroin and Morphine" ~ "Heroin/Morphine",
    Substance=="04 Methadone" ~ "Methadone",
    Substance=="05 Tramadol" ~ "Tramadol",
    Substance=="06 Codeine not from compound formulation" ~ "Codeine",
    Substance=="07 Dihydrocodeine not from compound formulation" ~ "Dihydrocodeine",
    Substance=="08 Oxycodone" ~ "Oxycodone",
    Substance=="09 Fentanyl" ~ "Fentanyl",
    Substance=="10 Fentanyl analogues" ~ "Fentanyl analogues",
    Substance=="11 Buprenorphine" ~ "Buprenorphine",
    Substance=="12 Other specified opiate" ~ "Other specified opiate",
    Substance=="13 Unspecified opiate" ~ "Unspecified opiate",
    Substance=="14 Cocaine" ~ "Cocaine",
    Substance=="15 Any amphetamine" ~ "Any amphetamines",
    Substance=="16 Amphetamine" ~ "Amphetamine",
    Substance=="17 Ecstasy/MDMA" ~ "Ecstasy/MDMA",
    Substance=="18 PMA/PMMA" ~ "PMA/PMMA",
    Substance=="19 Cannabis" ~ "Cannabis",
    Substance=="20 New psychoactive substance" ~ "NPS",
    Substance=="21 Any benzodiazepine" ~ "Any benzodiazepine",
    Substance=="22 Diazepam" ~ "Diazepam",
    Substance=="23 Temazepam" ~ "Temazepam",
    Substance=="24 Zopiclone/Zolpidem" ~ "Zopiclone/Zolpidem",
    Substance=="25 Pregabalin" ~ "Pregabalin ",
    Substance=="26 Gabapentin" ~ "Gabapentin",
    Substance=="27 Barbiturates" ~ "Barbiturates",
    Substance=="28 Any antipsychotics" ~ "Any antipsychotics",
    Substance=="29 Quetiapine" ~ "Quetiapine",
    Substance=="30 Clozapine" ~ "Clozapine",
    Substance=="31 Any antidepressant" ~ "Any antidepressant",
    Substance=="32 Any tricyclic antidepressants" ~ "Any tricyclic antidepressants",
    Substance=="33 Amitriptyline" ~ "Amitriptyline",
    Substance=="34 Dothiepin" ~ "Dothiepin",
    Substance=="35 Monoamine-oxidase inhibitors (BNF 4.3.2)" ~ "Monoamine-oxidase inhibitors",
    Substance=="36 Any SSRI" ~ "Any SSRI",
    Substance=="37 Citalopram" ~ "Citalopram",
    Substance=="38 Any other antidepressants" ~ "Any other antidepressants",
    Substance=="39 Mirtazapine" ~ "Mirtazapine",
    Substance=="40 Venlafaxine" ~ "Venlafaxine",
    Substance=="41 Any paracetamol (includes dextropropoxyphene mentioned without paracetamol)" ~ 
      "Any paracetamol",
    Substance=="42 Paracetamol" ~ "Paracetamol",
    Substance=="43 Paracetamol and dextropropoxyphene compound formulation" ~ 
      "Paracetamol + dextropropoxyphene",
    Substance=="44 Paracetamol and codeine compound formulation" ~ "Paracetamol + codeine",
    Substance=="45 Paracetamol and dihydrocodeine compound formulation" ~ "Paracetamol + dihydrocodeine",
    Substance=="46 Paracetamol not from compound formulation" ~ "Paracetamol not from compound"),
    Category="Drug")

#Read in VSA deaths data
url2 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsrelatedtovolatilesubstanceabuseandheliumgreatbritain%2f2001to2016/deathsrelatedtovolatilesubstanceabuseandheliumgreatbritain2001to2016registrationsv2.xls"
temp2 <- tempfile()
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

VSAdata <- read_excel(temp2, sheet="Table 5", range="A6:Q15") %>% 
  rename(Substance=`...1`) %>% 
  filter(complete.cases(.)) %>% 
  gather(Year, Deaths, c(2:17)) %>% 
  mutate(Year=as.numeric(Year), Category="Volatile substance",
         Substance=case_when(
           Substance=="Nitrogen related" ~ "Nitrous Oxide",
           TRUE ~ Substance))

data <- bind_rows(DRDdata, VSAdata)

plotdata <- data %>% 
  filter(Substance %in% c("Any opiates", "Cocaine", "Any amphetamine", "NPS", "Any benzodiazepine",
                          "Zopiclones/Zolpidem", "Pregabalin", "Gabapentin", "Any antipsychotics",
                          "Any antidepressant", "Any paracetamol", "Nitrous Oxide") & Year>=2001)

plotlabs <- plotdata %>% 
  group_by(Substance) %>% 
  filter(Year==max(Year)) %>% 
  ungroup() %>% 
  mutate(label=case_when(
    Substance=="Any opiates" ~ "Opiates",
    Substance=="Any antidepressant" ~ "Antidepressants",
    Substance=="Any benzodiazepine" ~ "Benzodiazepines",
    Substance=="Any paracetemol" ~ "Paracetemol",
    Substance=="Any antipsychotics" ~ "Antipsychotics",
    Substance=="NPS" ~ "New Psychoactive\nSubstances",
    TRUE ~ Substance))

#Nice labels shamelessly borrowed from Cedric Scherer
#http://www.r-graph-gallery.com/web-line-chart-with-labels-at-end-of-line.html
agg_tiff("Outputs/DRDEWxSubstanceIncN2O.tiff", units="in", width=9, height=7, res=800)
ggplot()+
  geom_line(data=plotdata, aes(x=Year, y=Deaths, colour=Substance), show.legend=FALSE)+
  geom_text_repel(data=plotlabs, aes(x=Year, y=Deaths, label=label, colour=Substance), xlim=c(2020.8, NA),
                  show.legend=FALSE, segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = 0.6,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20)+
  coord_cartesian(clip="off")+
  scale_x_continuous(expand=c(0,0), limits=c(2000, 2028.5), breaks=seq(2000, 2020, by=5), name="")+
  scale_y_continuous(name="Deaths")+
  scale_colour_manual(values=c("#1D271CFF", "#274637FF", "#2C715FFF", "#44A57CFF", "#819A7AFF", 
                               "#58A449FF", "Grey10", "#FF4E86", "ForestGreen"))+
  theme_custom()+
  labs(title="Nitrous Oxide is not a leading cause of substance misuse deaths",
       subtitle="Annual deaths involving the use of drugs and/or volatile substances by substance in England & Wales.\nMultiple substances can be recorded for each death. Nitrous Oxide data is only available up to 2016.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/DRDEWN2O.tiff", units="in", width=9, height=7, res=800)
ggplot(plotdata %>% filter(Substance=="Nitrous Oxide"), aes(x=Year, y=Deaths))+
  geom_line(colour="#FF4E86")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths")+
  theme_custom()+
  labs(title="Nitrous Oxide deaths rose sharply between 2012 and 2016",
       subtitle="Annual deaths in England & Wales recording Nitrous Oxide as being related to the death on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(plot.title=element_text(size=rel(2)))
dev.off()