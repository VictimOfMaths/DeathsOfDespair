rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(readxl)
library(ragg)
library(extrafont)
library(ggtext)
library(ggrepel)
library(cowplot)
library(paletteer)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
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

#############################################################################################
#Alcohol-specific deaths in Scotland
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2022/alcohol-specific-deaths-22-all-tabs.xlsx"
rawdata <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Overall plots
sexdata <- read_excel(rawdata, sheet="Table_1", range="A5:M49") %>% 
  set_names("Year", "Deaths_Total", "Deaths_Female", "Deaths_Male", "ASMR_Total", "ASMR_Total.Lower",
            "ASMR_Total.Upper", "ASMR_Female", "ASMR_Female.Lower", "ASMR_Female.Upper",
            "ASMR_Male", "ASMR_Male.Lower", "ASMR_Male.Upper") %>% 
  pivot_longer(cols=c(2:13), names_to=c("Metric", "Sex"), names_sep="_", values_to="Value")

png("Outputs/ASDScotland2022xSex.png", units="in", width=8, height=6, res=800)
sexdata %>% 
  filter(Metric=="Deaths") %>% 
  ggplot(aes(x=Year, y=Value, colour=Sex, linetype=Sex))+
  geom_line()+
  geom_text_repel(data=. %>% filter(Year==max(Year)),
                  aes(x=Year, y=Value, color = Sex, label = paste0(Sex, ": ", Value)),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.2, hjust=0,
                  xlim = c(2022.3, NA), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(1978, 2027))+
  scale_y_continuous(name="Annual deaths", limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc", "grey30"))+
  scale_linetype_manual(values=c(1,1,2))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"), legend.position="none",
        plot.title=element_markdown())+
  labs(title="Alcohol-specific deaths rose in 2022, driven by an increase among <span style='color:#00cc99;'>women",
       subtitle="Annual numbers of deaths from causes that are 100% attributable to alcohol in Scotland",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

png("Outputs/ASDScotland2022xSexASMR.png", units="in", width=8, height=6, res=800)
sexdata %>% 
  filter(Metric=="ASMR" & !is.na(Value)) %>% 
  mutate(Type=case_when(
    endsWith(Sex, "Upper") ~ "Upper",
    endsWith(Sex, "Lower") ~ "Lower",
    TRUE ~ "Central"),
    Sex=case_when(
      startsWith(Sex, "Male") ~ "Male",
      startsWith(Sex, "Female") ~ "Female",
      TRUE ~ "Total")) %>% 
  spread(Type, Value) %>% 
  ggplot(aes(x=Year, colour=Sex))+
  geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=Sex), alpha=0.3, colour=NA)+
  geom_line(aes(y=Central, linetype=Sex))+
  geom_text_repel(data=. %>% filter(Year==max(Year)),
                  aes(x=Year, y=Central, color = Sex, label = paste0(Sex, ": ", Central)),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.2, hjust=0,
                  xlim = c(2022.3, NA), show.legend=FALSE, segment.color = NA)+
  scale_x_continuous(name="", limits=c(1994, 2026))+
  scale_y_continuous(name="Annual deaths per 100,000", limits=c(0,NA))+
  scale_colour_manual(values=c("#00cc99", "#6600cc", "grey30"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc", "grey30"))+
  scale_linetype_manual(values=c(1,1,2))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"), legend.position="none",
        plot.title=element_markdown())+
  labs(title="Alcohol-specific deaths rose in 2022, driven by an increase among <span style='color:#00cc99;'>women",
       subtitle="Annual age-standardised mortality rates from causes that are 100% attributable to alcohol in Scotland.\nShaded areas represent 95% confidence intervals.",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Age group plot
agedata <- read_excel(rawdata, sheet="Table_2B", range="A5:W92") %>% 
  gather("Age", "ASDrate", c(4:23)) %>% 
  mutate(Age=str_replace(Age, "Age ", ""),
         Age=if_else(Age=="90 or more", "90+", Age),
         Age=factor(Age, levels=c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                                  "80-84", "85-89", "90+")))

ASDplot <- agedata %>% 
  filter(Sex=="Persons" & !Age %in% c("0", "1-4", "5-9", "10-14", "15-19") & Year>=2001) %>% 
  ggplot(aes(x=Year, y=ASDrate))+
  geom_area(fill="skyblue")+
  geom_line(data=. %>% filter(Year<2022), colour="grey30")+
  geom_line(data=. %>% filter(Year>=2021), colour="tomato",
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  geom_point(data=. %>% filter(Year==2021), colour="grey30")+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000")+
  facet_grid(~Age, switch="x")+
  theme_custom()+
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  labs(title="The rise in alcohol-specific deaths in Scotland in 2022 is driven by older age groups",
       subtitle="Deaths from causes that are wholly-attributable to alcohol in Scotland 2001-2022",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

ASDinset <- ggplot()+
  geom_polygon(aes(x=c(1, 1:21, 21), 
                   y=c(0,21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10,14,0)), 
               fill="SkyBlue")+
  geom_line(aes(x=c(1:20), 
                y=c(21,20,18,15,17,21,18,20,16,17,14,12,15,10,13,9,3,5,4,10)), colour="Grey40")+
  geom_line(aes(x=c(20,21), y=c(10,14)), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), 
            colour="Tomato")+
  geom_point(aes(x=20, y=10), colour="grey30")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDfull <- ggdraw()+
  draw_plot(ASDplot)+
  draw_plot(ASDinset, x=0.15, y=0.65, width=0.13, height=0.2)+
  draw_label("2001", x=0.17, y=0.66, size=10, colour="Grey40")+
  draw_label("2021", x=0.26, y=0.66, size=10, colour="Grey40")+
  draw_label("2022", x=0.27, y=0.8, size=10, colour="Tomato")+
  draw_label("Key", x=0.17, y=0.85, size=11, fontface="bold")

png("Outputs/ASDScotland2022xAge.png", units="in", width=12, height=6, res=600)
ggdraw(ASDfull)
dev.off()

#ASD by SIMD quintile
IMDdata <- read_excel(rawdata, sheet="Table_5", range="A5:G335") %>% 
  rename("ASMR"="Age-Standardised Rate of Mortality (ASMR)",
         "Lower"="Lower Confidence Interval Limit",
         "Upper"="Upper Confidence Interval Limit") 

IMDdata %>% 
  filter(Sex!="Persons") %>% 
  ggplot(aes(x=Year, y=ASMR))+
  geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=as.factor(`SIMD quintile`)), alpha=0.4)+
  geom_line(aes(colour=as.factor(`SIMD quintile`)))+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual deaths per 100,000", limits=c(0,NA))+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"),
                      name="SIMD quintile", labels=c("Most deprived", "", "", "", "Least deprived"))+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"),
                      name="SIMD quintile", labels=c("Most deprived", "", "", "", "Least deprived"))+
  #scale_colour_paletteer_d("NineteenEightyR::miami2", direction=-1,
  #                         name="SIMD quintile", labels=c("Most deprived", "", "", "", "Least deprived"))+
  #scale_fill_paletteer_d("NineteenEightyR::miami2", direction=-1,
  #                       name="SIMD quintile", labels=c("Most deprived", "", "", "", "Least deprived"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"))+
  facet_wrap(~Sex)
