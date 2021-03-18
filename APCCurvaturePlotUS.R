rm(list=ls())

library(readxl)
library(tidyverse)
library(HMDHFDplus)
library(ragg)
library(extrafont)

options(scipen=10000)

#Read in US data
#All downloaded from the CDC wonder database using the ICD-10 definitions based on Masters et al.
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

#APC curvature plots
USAPCcurve <- USdata %>%
  group_by(Year, Cause, Sex) %>%
  summarise(mean=weighted.mean(Age, Dx), 
            meanDx=Dx[which(Age==round(mean, digits=0))] ,
            maxDx=max(Dx), 
            mode=Age[which(Dx==maxDx)][1], 
            moderate=mx[which(Age==mode)],
            spread=sd(Dx)) %>% 
  ungroup()

ann_text <- data.frame(mode=seq(29, 79, by=5), Year=rep(2022.5, times=11), label=as.character(seq(1995, 1945, by=-5)))

agg_tiff("Outputs/DoDUSAModalAges.tiff", units="in", width=8, height=8, res=500)
USAPCcurve %>% 
  filter(!Cause %in% c("Total", "Cancer", "Metabolic", "Other")) %>% 
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
        text=element_text(family="Roboto"))+
  guides(colour=guide_legend(order=1), size=guide_legend(order=2))+
  labs(title="Age patterns in 'Deaths of despair' in the USA", 
       subtitle="APC curvature plot showing modal age of death by cause",
       caption="Data from Centre for Disease Control \n Plot by @VictimOfMaths")

dev.off()

