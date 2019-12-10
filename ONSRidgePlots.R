rm(list=ls())

#devtools::install_github("coolbutuseless/lofi")      # Colour encoding
#devtools::install_github("coolbutuseless/minisvg")   # SVG creation
#devtools::install_github("coolbutuseless/devout")    # Device interface
#devtools::install_github("coolbutuseless/devoutsvg") # This package
#devtools::install_github("coolbutuseless/poissoned") # This package
#devtools::install_github("coolbutuseless/svgpatternsimple") # This package

library(data.table)
library(tidyr)
library(ggplot2)
library(ggridges)
library(dplyr)
library(lofi)
library(minisvg)
library(devout)
library(devoutsvg)
library(svgpatternsimple)
library(poissoned)

#Read in data from ONS website
read.url <- function(url, ...){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile, method = "curl")
  url.data <- read.csv(tmpFile, ...)
  return(url.data)
}
data <- read.url("https://www.ons.gov.uk/visualisations/dvc661/drugs/datadownload.csv")
data2 <- read.url("https://www.ons.gov.uk/visualisations/dvc661/suicides/datadownload.csv")

#Tidying data
colnames(data) <- c("Age", 1993:2017)
colnames(data2) <- c("Age", 1981:2017)

data$Age <- as.character(data$Age)
data2$Age <- as.character(data2$Age)

data$Age <- case_when(
  data$Age=="<10" ~ "9",
  data$Age=="90+" ~ "90",
  TRUE ~ data$Age)

data2$Age <- case_when(
  data2$Age=="<10" ~ "9",
  data2$Age=="90+" ~ "90",
  TRUE ~ data2$Age)

#Convert to long
data_long <- melt(as.data.table(data), id.vars = "Age", variable.name = "Year", value.name = "Deaths")
data_long[ , Year := as.integer(as.vector(Year))]
data_long[,flag:=1]

data2_long <- melt(as.data.table(data2), id.vars = "Age", variable.name = "Year", value.name = "Deaths")
data2_long[ , Year := as.integer(as.vector(Year))]
data2_long[,flag:=1]
  
#Create pattern
gradblue <- create_gradient_pattern(id="p1", angle=90, colour1="White", colour2="#0570b0")

#visualise it
gradblue$show()

#encode it
gradblue <- encode_pattern_params_as_hex_colour(pattern_name="gradient",angle=90, 
                                                colour1="White", colour2="#0570b0")

#create drug death graph
svgout(filename = "Outputs/DrugDeaths.svg", pattern_pkg="svgpatternsimple", width=4, height=6)
ggplot(data_long, aes(x=Age, y=-Year, height=Deaths, group=Year, fill=as.factor(flag)))+
  geom_density_ridges(stat="identity", alpha=1, scale=3, colour=alpha(0.0001))+
  theme_classic()+
  scale_y_continuous(breaks=c(-1993:-2017), labels=c(1993:2017), name="", position="right")+
  scale_x_discrete(breaks=c("10", "20", "30", "40", "50", "60", "70", "80", "90"))+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), text=element_text(family="Georgia"))+
  labs(title="Trends in deaths from drug poisoning", 
       subtitle="Data from England and Wales 1993-2017", 
       caption="Source: Office for National Statistics\nPlot by @VictimOfMaths")+
  scale_fill_manual(values=c("1"=gradblue), guide=FALSE)
invisible(dev.off())

#create suicide graph
svgout(filename = "Outputs/SuicideDeaths.svg", pattern_pkg="svgpatternsimple", width=4, height=6)
ggplot(data2_long, aes(x=Age, y=-Year, height=Deaths, group=Year, fill=as.factor(flag)))+
  geom_density_ridges(stat="identity", alpha=1, scale=3, colour=alpha(0.0001))+
  theme_classic()+
  scale_y_continuous(breaks=c(-1981:-2017), labels=c(1981:2017), name="", position="right")+
  scale_x_discrete(breaks=c("10", "20", "30", "40", "50", "60", "70", "80", "90"))+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), text=element_text(family="Georgia"))+
  labs(title="Trends in deaths by suicide", subtitle="Data from England and Wales 1981-2017",
       caption="Source: Office for National Statistics\nPlot by @VictimOfMaths")+
  scale_fill_manual(values=c("1"=gradblue), guide=FALSE)
invisible(dev.off())