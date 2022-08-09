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

popdata <- read_excel(temp, sheet="Table_5", range="A5:W338") %>% 
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

#Bring togather
data <- rawdata %>% 
  merge(popdata, all.x=T) %>% 
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
