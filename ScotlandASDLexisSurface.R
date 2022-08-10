rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(MortalitySmooth)
library(paletteer)
library(extrafont)
library(ragg)

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

#Download Scottish Alcohol-specific deaths data from NRS website
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2021/alcohol-specific-deaths-21-all-tabs.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(temp, sheet="Table_2A", range="A5:W134") %>% 
  gather(Age, Dx, c(4:23)) %>% 
  select(-Measure) %>% 
  mutate(Age=gsub("Age ", "", Age))

#Download mid-year population estimates for Scotland
url <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-21/mid-year-pop-est-21-time-series-data.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

popdata_grp <- read_excel(temp, sheet="Table_5", range="A5:W338") %>% 
  filter(Year %in% c(1979:2021)) %>% 
  select(-`All Ages`) %>% 
  gather(Age, pop, c(3:22)) %>% 
  mutate(pop=as.numeric(pop),
         Age=gsub(" to ", "-", Age),
         Age=case_when(
           Age=="85-89 \r\n[note 5]" ~ "85-89",
           Age=="90 and over \r\n[note 5]" ~ "90 or more",
           TRUE ~ Age)) %>% 
  filter(Age!="85 and over")

popdata_sgl <- read_excel(temp, sheet="Table_6", range="A6:CP129") %>% 
  select(-`All Ages`) %>% 
  gather(Age, pop, c(3:93))

#Bring togather
data <- rawdata %>% 
  merge(popdata_grp, all.x=T) %>% 
  mutate(agestart=case_when(
    Age=="90 or more" ~ 90,
    TRUE ~ as.numeric(gsub("\\-.*", "", Age))))

#Set up data for smoothing
x <- seq(10,90, by=5)
smoothdata <- data %>% filter(agestart>=10)
y <- 1979:2021
z <- smoothdata %>% select(c(Sex, Year, agestart, Dx)) %>% 
  spread(Year, Dx) %>% 
  arrange(Sex, agestart)

offset <- smoothdata %>% select(c(Sex, Year, agestart, pop)) %>% 
  spread(Year, pop) %>% 
  arrange(Sex, agestart)

#Fit smoothing models within years only
#Credit to Tim Riffe for help with this approach

mx_smoothed1D <- data.frame(Sex=character(), Age=integer(), Year=integer(), mx_smt1D=double())

for(i in c("Males", "Females", "Persons")){
  for(j in 1979:2021){
        y <- z %>% filter(Sex==i) %>% 
          select(-c(agestart, Sex)) %>% 
          select(c(j-1978)) %>% 
          unlist() %>% 
          as.vector()
        
        offset_i <- offset %>% filter(Sex==i) %>% 
          select(-c(agestart, Sex)) %>% 
          select(c(j-1978)) %>% 
          log() %>% 
          unlist() %>% 
          as.vector()
        
        mod <- Mort1Dsmooth(x, y, offset=offset_i)
        
        mx_smoothed1D <- predict(mod, newdata=c(10:90)) %>% 
          exp() %>% 
          as.data.frame() %>% 
          rename(mx_smt1D=1) %>% 
          mutate(Age=c(10:90), Sex=i, Year=j) %>% 
          bind_rows(mx_smoothed1D)
  }
}

ASD_smoothed <- mx_smoothed1D %>% 
  merge(popdata_sgl) %>% 
  mutate(Dx_smt=mx_smt1D*pop)

#Validate against actual deaths
validate_age <- ASD_smoothed %>% 
  mutate(Age=case_when(
    Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24", Age<30 ~ "25-29", Age<35 ~ "30-34",
    Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54", Age<60 ~ "55-59",
    Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84")) %>% 
  group_by(Age, Sex, Year) %>% 
  summarise(Dx_smt=sum(Dx_smt)) %>% 
  ungroup() %>% 
  merge(smoothdata)

#diagnostic plot
ggplot(validate_age, aes(x=Dx, y=Dx_smt))+
  geom_point()+
  geom_abline()+
  theme_custom()

#Repeat using annual figures
validate_yr <- ASD_smoothed %>% 
  mutate(Age=case_when(
    Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24", Age<30 ~ "25-29", Age<35 ~ "30-34",
    Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54", Age<60 ~ "55-59",
    Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84")) %>% 
  group_by(Sex, Year) %>% 
  summarise(Dx_smt=sum(Dx_smt)) %>% 
  ungroup() %>% 
  merge(smoothdata %>% group_by(Sex, Year) %>% 
          summarise(Dx=sum(Dx)) %>% 
          ungroup())

#diagnostic plot
ggplot(validate_yr, aes(x=Dx, y=Dx_smt))+
  geom_point()+
  geom_abline()+
  theme_custom()

#Final Lexis Surface
agg_tiff("Outputs/ASDScotlandLexis.tiff", units="in", width=9, height=7, res=500)
ggplot(mx_smoothed1D %>% filter(Sex!="Persons"), aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  geom_tile()+
  scale_fill_paletteer_c("viridis::inferno", limits=c(0,NA), name="Deaths\nper 100,000")+
  facet_wrap(~Sex)+
  theme_custom()+
  coord_equal()+
  labs(title="Scotland's alcohol deaths crisis started in the mid-90s",
       subtitle="Rates of alcohol-specific deaths in Scotland 1979-2021. Data is published in 5-year age bands and has been modelled\nout to single years of age using a spline-based approach\n",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

dev.off()
