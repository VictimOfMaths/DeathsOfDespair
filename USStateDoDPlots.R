rm(list=ls())

library(tidyverse)
library(data.table)
library(forcats)
library(viridis)

data1 <- fread("Data/CDC Data/AlcDrg.txt")
data2 <- fread("Data/CDC Data/Xs.txt")

data2 <- filter(data2, `Cause of death Code` %in% c("X66", "X67", "X68", "X69", "X70", "X71", "X72", "X73", "X74",
                                                    "X75", "X76", "X77", "X78", "X79", "X80", "X81", "X82", "X83",
                                                    "X84", "Y16", "Y17", "Y18", "Y19", "Y20", "Y21", "Y22", "Y23",
                                                    "Y24", "Y25", "Y26", "Y27", "Y28", "Y29", "Y30", "Y31", "Y32",
                                                    "Y33", "Y34"))

data2$cause <- "Scd"

data1$cause <- case_when(
  data1$`Cause of death Code` %in% c("F10.0", "F10.1", "F10.2", "F10.3", "F10.4", "F10.7", "F10.9", "G31.2", "I42.6",
                                     "K70.0", "K70.1", "K70.3", "K70.4", "K70.9", "K85.2", "K86.0", "X45") ~ "Alc",
  data1$`Cause of death Code` %in% c("F11.1", "F11.2", "F11.9", "F14.1", "F14.2", "F14.9", "F15.1", "F15.2", "F15.9",
                                     "F17.9", "F19.1", "F19.2", "F19.9", "X40", "X41", "X42", "X43", "X44","X85") ~ "Drg",
  data1$`Cause of death Code` %in% c("X60", "X61", "X62", "X63", "X64", "X64", "Y11", "Y12", "Y13", "Y14", "Y15") ~ "Scd",
  TRUE ~ "ERROR")

data <- rbind(data1, data2)

datanew <- data %>%
  group_by(State, Year, cause) %>%
  summarise(deaths=sum(Deaths))

StatePop <- fread("Data/USPopState.csv", header=T)
StatePop_long <- gather(StatePop, Year, Pop, c(2:21))
StatePop_long$Year <- as.numeric(StatePop_long$Year)

datanew <- merge(datanew, StatePop_long, by=c("State", "Year"))

datanew$mortrate <- datanew$deaths*100000/datanew$Pop

facetlabs <- c("Alcohol", "Drugs", "Suicide")
names(facetlabs) <- c("Alc", "Drg", "Scd")

tiff("Outputs/DoDUSStates.tiff", units="in", width=14, height=10, res=300)
ggplot(datanew, aes(x=Year, y=fct_rev(State), fill=mortrate, z=mortrate))+
  geom_tile(colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Deaths per 100,000")+
  scale_y_discrete(name="")+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank())+
  labs(title="Deaths from alcohol, drugs and suicide among white adults in the US", 
       caption="Data from CDC Wonder database | Plot by @VictimOfMaths")+
  facet_wrap(~cause, labeller=labeller(cause=facetlabs))+
  theme(strip.background=element_blank(), strip.text=element_text(size=rel(1), face="bold"))
dev.off() 
