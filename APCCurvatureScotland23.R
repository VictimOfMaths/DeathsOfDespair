rm(list=ls())

#Mortality smooth has an older versio currently on CRAN, so take mirrored version from Tim Riffe's GitHub
#remotes::install_github("timriffe/MortalitySmooth")

library(curl)
library(readxl)
library(tidyverse)
library(HMDHFDplus)
library(MortalitySmooth)
library(paletteer)
library(ragg)
library(extrafont)
library(RcppRoll)
library(gt)
library(patchwork)
library(keyring)
library(scales)
library(MASS)
library(Epi)
library(modelr) 
library(splines2)

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

#Download data from NRS website on drug-related deaths by age and sex
#https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/drug-related-deaths-in-scotland/2023
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/23/drug-related-deaths-23-data.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

drgdata.raw <- read_excel(temp, sheet="Table_4", range="A5:W77") %>% 
  gather(Age, Deaths, c(4:23)) %>% 
  mutate(Cause="Drugs") %>% 
  dplyr::select(c(Year, Sex, Age, Deaths, Cause))

#And then suicide deaths #https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/suicides
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/probable-suicides/2023/suicides-23-data.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

scddata.raw <- read_excel(temp, sheet="Table_2A", range="A5:U155") %>% 
  gather(Age, Deaths, c(4:21))%>% 
  mutate(Cause="Suicide") %>% 
  dplyr::select(c(Year, Sex, Age, Deaths, Cause))

#And alcohol deaths #https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/alcohol-deaths
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2022/alcohol-specific-deaths-22-all-tabs.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

alcdata.raw <- read_excel(temp, sheet="Table_2A", range="A5:W137") %>% 
  gather(Age, Deaths, c(4:23))%>% 
  mutate(Cause="Alcohol") %>% 
  dplyr::select(c(Year, Sex, Age, Deaths, Cause))

#Harmonise data - aligning age groups that are unhelpfully slightly different
alldata <- drgdata.raw %>% 
  mutate(Age=case_when(
    Age %in% c("0", "1-4") ~ "0-4",
    Age %in% c("85-89", "90+") ~ "85+",
    TRUE ~ Age)) %>% 
  bind_rows(scddata.raw %>% 
              mutate(Age=gsub("Age ", "", Age)),
            alcdata.raw %>% 
              mutate(Age=gsub("Age ", "", Age),
                     Age=case_when(
                       Age %in% c("0", "1-4") ~ "0-4",
                       Age %in% c("85-89", "90 or more") ~ "85+",
                       TRUE ~ Age))) %>% 
  group_by(Age, Sex, Year, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop")
           
#Bring in populations
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-year-pop-est-time-series.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

pop.1 <- read_excel(temp, sheet="Table 1", range="B6:CR4164") %>% 
  filter(`Area name`=="Scotland") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Age=if_else(Age=="90 and over", "90+", Age)) %>% 
  dplyr::select(-c(`All Ages`, `Area name`))

#Recycle 2022 populations for 2023 as NRS haven't produced 2023 estimated yet
#(A clever person would do more than this, but I'm not busting out fertility rates
#to do some proper modelling when it won't make any difference)

pop.1 <- pop.1 %>%
  bind_rows(pop.1 %>% filter(Year==2022) %>% 
              mutate(Year=2023))

#Plot populations in a lexis surface
agg_png("Outputs/ScotlandPopLexis.png", units="in", width=6, height=6, res=500)
ggplot(pop.1 %>% filter(Sex!="Persons" & Year!=2023) %>% mutate(Age=as.numeric(Age)),
       aes(x=Year, y=Age, fill=Pop))+
  geom_tile()+
  geom_hline(yintercept=0, colour="grey30")+
  scale_fill_paletteer_c("viridis::inferno", limits=c(0,NA))+
  facet_wrap(~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.line.x=element_blank())+
  labs(title="Scotland's evolving population over time",
       subtitle="Age structure of the Scottish population 1980-2022\n",
       caption="Data from National Records of Scotland\nPlot by @VictimOfMaths")

dev.off()

#Collapse to 5-year bands
pop.5 <- pop.1 %>% 
  mutate(Age=as.numeric(if_else(Age=="90+", "90", Age)),    
         Age=case_when(
           Age<5 ~ "0-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19",
           Age<25 ~ "20-24", Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39",
           Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54", Age<60 ~ "55-59",
           Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79",
           Age<85 ~ "80-84", Age>=85 ~ "85+"),
         Age=factor(Age, levels=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                  "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))) %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

finaldata <- merge(alldata, pop.5) %>% 
  mutate(mx=Deaths*100000/Pop,
         Age=factor(Age, levels=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                  "60-64", "65-69", "70-74", "75-79", "80-84", "85+")),
         agestart=gsub("-.*", "", Age),
         agestart=as.numeric(gsub("\\+", "", agestart)))

#Visualise Lexis surfaces in 5-year bands
finaldata %>% filter(Sex=="Persons") %>% 
  ggplot(aes(x=Year, y=Age, fill=mx))+
  geom_tile()+
  geom_hline(yintercept=0, colour="grey30")+
  scale_fill_paletteer_c("viridis::inferno", limits=c(0,NA))+
  facet_wrap(Cause~.)+
  coord_equal()+
  theme_custom()+
  theme(axis.line.x=element_blank())

finaldata %>% filter(Sex!="Persons") %>% 
  ggplot(aes(x=Year, y=Age, fill=mx))+
  geom_tile()+
  geom_hline(yintercept=0, colour="grey30")+
  scale_fill_paletteer_c("viridis::inferno", limits=c(0,NA))+
  facet_grid(Cause~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.line.x=element_blank())

###############################################################################
#Smooth 5-year age bands out to single year of age data

#Apply smoothing based approach suggested by Tim Riffe
#Prediction models fall over if you include <10 year olds, so exclude them as not relevant to analysis

#Have to do each cause separately due to different time periods
#Drugs
x <- seq(10,80, by=5)
finaldata.1 <- finaldata %>% filter(agestart>=10 & agestart<85 & Cause=="Drugs")
y <- 2000:2023
z <- finaldata.1 %>% dplyr::select(-c(Pop, mx, Cause, Age)) %>% 
  spread(Year, Deaths) %>% 
  arrange(agestart)

offset <- finaldata.1 %>% dplyr::select(Pop, agestart, Year, Sex) %>% 
  spread(Year, Pop) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx.smoothed.drugs <- data.frame(Sex=character(), Age=integer(), Year=integer(), 
                            mx_smt1D=double())

for(i in c("Persons", "Males", "Females")){
      for(j in 2000:2023){
        y <- z %>% filter(Sex==i) %>% 
          dplyr::select(-c(agestart, Sex)) %>% 
          dplyr::select(c(j-1999)) %>% 
          unlist() %>% 
          as.vector()
        
        offset_i <- offset %>% filter(Sex==i & agestart>=10) %>% 
          dplyr::select(-c(agestart, Sex)) %>% 
          dplyr::select(c(j-1999)) %>% 
          log() %>% 
          unlist() %>% 
          as.vector()
        
        mod <- Mort1Dsmooth(x, y, offset=offset_i)
        
        mx.smoothed.drugs <- predict(mod, newdata=c(10:80)) %>% 
          exp() %>% 
          as.data.frame() %>% 
          rename(mx_smt1D=1) %>% 
          mutate(Age=c(10:80), Sex=i, Year=j) %>% 
          bind_rows(mx.smoothed.drugs)
  }
}

#Suicide
finaldata.2 <- finaldata %>% filter(agestart>=10 & agestart<85 & Cause=="Suicide")
y <- 1981:2023
z <- finaldata.2 %>% dplyr::select(-c(Pop, mx, Cause, Age)) %>% 
  spread(Year, Deaths) %>% 
  arrange(agestart)

offset <- finaldata.2 %>% dplyr::select(Pop, agestart, Year, Sex) %>% 
  spread(Year, Pop) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx.smoothed.suicide <- data.frame(Sex=character(), Age=integer(), Year=integer(), 
                                mx_smt1D=double())

for(i in c("Persons", "Males", "Females")){
  for(j in 1981:2023){
    y <- z %>% filter(Sex==i) %>% 
      dplyr::select(-c(agestart, Sex)) %>% 
      dplyr::select(c(j-1980)) %>% 
      unlist() %>% 
      as.vector()
    
    offset_i <- offset %>% filter(Sex==i & agestart>=10) %>% 
      dplyr::select(-c(agestart, Sex)) %>% 
      dplyr::select(c(j-1980)) %>% 
      log() %>% 
      unlist() %>% 
      as.vector()
    
    mod <- Mort1Dsmooth(x, y, offset=offset_i)
    
    mx.smoothed.suicide <- predict(mod, newdata=c(10:80)) %>% 
      exp() %>% 
      as.data.frame() %>% 
      rename(mx_smt1D=1) %>% 
      mutate(Age=c(10:80), Sex=i, Year=j) %>% 
      bind_rows(mx.smoothed.suicide)
  }
}

#Alcohol
finaldata.3 <- finaldata %>% filter(agestart>=10 & agestart<85 & Cause=="Alcohol")
y <- 1981:2022
z <- finaldata.3 %>% dplyr::select(-c(Pop, mx, Cause, Age)) %>% 
  spread(Year, Deaths) %>% 
  arrange(agestart)

offset <- finaldata.3 %>% dplyr::select(Pop, agestart, Year, Sex) %>% 
  spread(Year, Pop) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx.smoothed.alcohol <- data.frame(Sex=character(), Age=integer(), Year=integer(), 
                                mx_smt1D=double())

for(i in c("Persons", "Males", "Females")){
  for(j in 1981:2022){
    y <- z %>% filter(Sex==i) %>% 
      dplyr::select(-c(agestart, Sex)) %>% 
      dplyr::select(c(j-1980)) %>% 
      unlist() %>% 
      as.vector()
    
    offset_i <- offset %>% filter(Sex==i & agestart>=10) %>% 
      dplyr::select(-c(agestart, Sex)) %>% 
      dplyr::select(c(j-1980)) %>% 
      log() %>% 
      unlist() %>% 
      as.vector()
    
    mod <- Mort1Dsmooth(x, y, offset=offset_i)
    
    mx.smoothed.alcohol <- predict(mod, newdata=c(10:80)) %>% 
      exp() %>% 
      as.data.frame() %>% 
      rename(mx_smt1D=1) %>% 
      mutate(Age=c(10:80), Sex=i, Year=j) %>% 
      bind_rows(mx.smoothed.alcohol)
  }
}

smoothdata <- mx.smoothed.drugs %>% 
  mutate(Cause="Drugs") %>% 
  bind_rows(mx.smoothed.suicide %>% mutate(Cause="Suicide"),
            mx.smoothed.alcohol %>% mutate(Cause="Alcohol")) %>% 
  merge(pop.1) %>% 
  mutate(Dx_smt1D=mx_smt1D*Pop)

#Plot lexis surfaces of smoothed data
smoothdata %>% filter(Sex=="Persons") %>% 
  ggplot(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  geom_tile()+
  geom_hline(yintercept=0, colour="grey30")+
  scale_fill_paletteer_c("viridis::inferno", limits=c(0,NA))+
  facet_wrap(Cause~.)+
  coord_equal()+
  theme_custom()+
  theme(axis.line.x=element_blank())

smoothdata %>% filter(Sex!="Persons") %>% 
  ggplot(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  geom_tile()+
  geom_hline(yintercept=0, colour="grey30")+
  scale_fill_paletteer_c("viridis::inferno", limits=c(0,NA))+
  facet_grid(Cause~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.line.x=element_blank())

###############################################
#Fit APC model using code from Acosta & van Raalte paper
#https://www.demographic-research.org/volumes/vol41/42/default.htm

### Most of this function is an adaptation of the apc.fit function from the Epi package (Carstensen 2019, https://CRAN.R-project.org/package=Epi)
curvature_dAPC <- function(amin=10, amax=80, pmin=1981, pmax=2023, gr=1, 
                           C = "Alcohol", S="Males"){
  amax <- amin - 1 + (floor((amax-amin+1)/gr))*gr
  pmax <- min(pmax, 2023)
  pmax <- pmin - 1 + (floor((pmax-pmin+1)/gr))*gr
  
  data0 <- smoothdata %>% 
    filter(Year>=pmin, Year<=pmax, Age>=amin, Age<=amax, Cause==C, Sex==S) %>% 
    dplyr::select(Age, Year, Pop, Dx_smt1D)
  
  data1 <- data0 %>% 
    mutate(a_gr=as.integer(amin+floor((Age-amin)/gr)*gr), 
           p_gr=as.integer(pmin+floor((Year-pmin)/gr)*gr))
  
  data2 <- data1 %>% 
    group_by(a_gr, p_gr) %>% 
    summarise(D=sum(Dx_smt1D), 
              Y=sum(Pop)) %>% 
    rename(A=a_gr, P=p_gr) 
  
  data <- data2 %>% 
    mutate(C = as.integer(P - A),
           D = round(D),
           D = ifelse(D < 1, 1, D),
           D = as.integer(D),
           Y = as.integer(Y))
  
  # rm(list=setdiff(ls(), "data"))
  # select <- dplyr::select
  
  # ref.c <- NA
  # ref.p <- NA
  dist <- "poisson" 
  model<- "bSpline" 
  dr.extr <- "weighted"
  parm = "APC"
  npar=c(10,7,15)
  scale = 1
  alpha = 0.05 
  print.AOV = TRUE
  
  
  # dist <- match.arg(dist)
  # model <- match.arg(model)
  drtyp <- deparse(substitute(dr.extr))
  # parm <- toupper(match.arg(parm))
  if (!missing(data)) {
    if (length(match(c("A", "P", "D", "Y"), names(data))) != 
        4) 
      stop("Data frame ", deparse(substitute(data)), " has columns:\n", 
           names(data), "\nmust have variables:\n", "A (age), P (period), D (cases) and Y (person-time)")
    data <- data[, c("A", "P", "D", "Y")]
    data <- data[complete.cases(data), ]
    A <- data$A
    P <- data$P
    D <- data$D
    Y <- data$Y
  } else {
    nm <- logical(4)
    nm[1] <- missing(A)
    nm[2] <- missing(P)
    nm[3] <- missing(D)
    nm[4] <- missing(Y)
    if (any(nm)) 
      stop("Variable", if (sum(nm) > 1) 
        "s", paste(c(" A", " P", " D", " Y")[nm], collapse = ","), 
        " missing from input")
    if (diff(range(lv <- sapply(list(A = A, P = P, D = D, 
                                     Y = Y), length))) != 0) 
      stop("\nLengths of variables (", paste(paste(names(lv), 
                                                   lv, sep = ":"), collapse = ", "), ") are not the same.")
  }
  med <- function(x, y) {
    o <- order(x)
    a <- y[o]
    names(a) <- x[o]
    return(as.numeric(names(a[cumsum(a)/sum(a) > 0.5][1])))
  }
  p0 <- med(P, D)
  c0 <- med(P - A, D)
  ref.p <- F
  ref.c <- F
  if (is.list(npar) & length(npar) < 3) 
    stop("npar as a list should have length 3! \n")
  if (!is.list(npar) & length(npar) != 3) {
    npar <- rep(npar, 3)[1:3]
    names(npar) = c("A", "P", "C")
    cat("NOTE: npar is specified as:")
    print(npar)
  }
  if (is.null(names(npar))) 
    names(npar) <- c("A", "P", "C")
  lu <- paste(formatC(c(alpha/2, 1 - alpha/2) * 100, format = "f", 
                      digits = 1), "%", sep = "")
  proj.ip <- function(X, M, orth = FALSE, weight = rep(1, nrow(X))) {
    if (nrow(X) != length(weight)) 
      stop("Dimension of space and length of i.p. weights differ!")
    if (nrow(X) != nrow(M)) 
      stop("Dimension of space and rownumber of model matrix differ!")
    Pp <- solve(crossprod(X * sqrt(weight)), t(X * weight)) %*% 
      M
    PM <- X %*% Pp
    if (orth) 
      PM <- M - PM
    else PM
  }
  Thin.col <- function(X, tol = 0.000001) {
    QR <- qr(X, tol = tol, LAPACK = FALSE)
    X[, QR$pivot[seq(length = QR$rank)], drop = FALSE]
  }
  detrend <- function(M, t, weight = rep(1, nrow(M))) {
    Thin.col(proj.ip(cbind(1, t), M, orth = TRUE, weight = weight))
  }
  if (is.list(model)) {
    if (!all(sapply(model, is.function))) 
      stop("'model' is a list, but not all elements are functions as they should be.")
    if ((lmod <- length(model)) < 3) 
      stop("'model' is a list, with", lmod, "elements, it should have three.")
    if (is.null(names(model))) 
      names(model) <- c("A", "P", "C")
    MA <- model[["A"]](A)
    MP <- model[["P"]](P)
    MC <- model[["C"]](P - A)
    Rp <- model[["P"]](p0)
    Rc <- model[["C"]](c0)
  } else {
    if (model == "factor") {
      MA <- model.matrix(~factor(A) - 1)
      MP <- model.matrix(~factor(P) - 1)
      MC <- model.matrix(~factor(P - A) - 1)
      Rp <- MP[abs(P - p0) == min(abs(P - p0)), , drop = FALSE][1, 
      ]
      Rc <- MC[abs(P - A - c0) == min(abs(P - A - c0)), 
               , drop = FALSE][1, ]
    }
    if (model == "ns") {
      knl <- is.list(npar)
      if (!knl & length(npar) == 1) 
        npar <- rep(npar, 3)
      if (is.null(names(npar))) 
        names(npar) <- c("A", "P", "C")
      names(npar) <- toupper(substr(names(npar), 1, 1))
      MA <- if (knl) 
        Ns(A, knots = npar[["A"]])
      else Ns(A, knots = quantile(rep(A, D), probs = (1:npar["A"] - 
                                                        0.5)/npar["A"]))
      MP <- if (knl) 
        Ns(P, knots = npar[["P"]])
      else Ns(P, knots = quantile(rep(P, D), probs = (1:npar["P"] - 
                                                        0.5)/npar["P"]))
      MC <- if (knl) 
        Ns(P - A, knots = npar[["C"]])
      else Ns(P - A, knots = quantile(rep(P - A, D), probs = (1:npar["C"] - 
                                                                0.5)/npar["C"]))
      Rp <- ns(p0, knots = attr(MP, "knots"), Boundary.knots = attr(MP, 
                                                                    "Boundary.knots"))
      Rc <- ns(c0, knots = attr(MC, "knots"), Boundary.knots = attr(MC, 
                                                                    "Boundary.knots"))
      Knots <- list(Age = sort(c(attr(MA, "knots"), attr(MA, 
                                                         "Boundary.knots"))), Per = sort(c(attr(MP, "knots"), 
                                                                                           attr(MP, "Boundary.knots"))), Coh = sort(c(attr(MC, 
                                                                                                                                           "knots"), attr(MC, "Boundary.knots"))))
    }
    if (model %in% c("bSpline", "ls")) {
      deg <- switch(model, ls = 1, bSpline = 3)
      knl <- is.list(npar)
      if (knl) 
        nk <- sapply(npar, length)
      MA <- if (knl) 
        bSpline(A, knots = npar[["A"]][-c(1, nk[1])], Boundary.knots = npar[["A"]][c(1, 
                                                                                nk[1])], degree = deg)
      else bSpline(A, df = npar[["A"]], degree = deg)
      MP <- if (knl) 
        bSpline(P, knots = npar[["P"]][-c(1, nk[2])], Boundary.knots = npar[["P"]][c(1, 
                                                                                nk[2])], degree = deg)
      else bSpline(P, df = npar[["P"]], degree = deg)
      MC <- if (knl) 
        bSpline(P - A, knots = npar[["C"]][-c(1, nk[3])], 
           Boundary.knots = npar[["C"]][c(1, nk[3])], 
           degree = deg)
      else bSpline(P - A, df = npar[["C"]], degree = deg)
      Rp <- bSpline(p0, knots = attr(MP, "knots"), Boundary.knots = attr(MP, 
                                                                    "Boundary.knots"), degree = attr(MP, "degree"))
      Rc <- bSpline(c0, knots = attr(MC, "knots"), Boundary.knots = attr(MC, 
                                                                    "Boundary.knots"), degree = attr(MC, "degree"))
      Knots <- list(Age = sort(c(attr(MA, "knots"), attr(MA, 
                                                         "Boundary.knots"))), Per = sort(c(attr(MP, "knots"), 
                                                                                           attr(MP, "Boundary.knots"))), Coh = sort(c(attr(MC, 
                                                                                                                                           "knots"), attr(MC, "Boundary.knots"))))
    }
  }
  if (tolower(substr(dist, 1, 2)) == "po") {
    m.APC <- glm(D ~ MA + I(P - p0) + MP + MC, offset = log(Y), 
                 family = poisson)
    Dist <- "Poisson with log(Y) offset"
  }
  if (tolower(substr(dist, 1, 3)) %in% c("bin")) {
    m.APC <- glm(cbind(D, Y - D) ~ MA + I(P - p0) + MP + 
                   MC, family = binomial)
    Dist <- "Binomial regression (logistic) of D/Y"
  }
  m.AP <- update(m.APC, . ~ . - MC)
  m.AC <- update(m.APC, . ~ . - MP)
  m.Ad <- update(m.AP, . ~ . - MP)
  m.A <- update(m.Ad, . ~ . - I(P - p0))
  m.0 <- update(m.A, . ~ . - MA)
  AOV <- anova(m.A, m.Ad, m.AC, m.APC, m.AP, m.Ad, test = "Chisq")
  attr(AOV, "heading") <- "\nAnalysis of deviance for Age-Period-Cohort model\n"
  attr(AOV, "row.names") <- c("Age", "Age-drift", "Age-Cohort", 
                              "Age-Period-Cohort", "Age-Period", "Age-drift")
  A.pt <- unique(A)
  A.pos <- match(A.pt, A)
  P.pt <- unique(P)
  P.pos <- match(P.pt, P)
  C.pt <- unique(P - A)
  C.pos <- match(C.pt, P - A)
  MA <- cbind(1, MA)
  if (!mode(dr.extr) %in% c("character", "numeric")) 
    stop("\"dr.extr\" must be of mode \"character\" or \"numeric\".\n")
  if (is.character(dr.extr)) {
    wt <- rep(1, length(D))
    drtyp <- "1-weights"
    if (toupper(substr(dr.extr, 1, 1)) %in% c("T", "D")) {
      wt <- D
      drtyp <- "D-weights"
    }
    else if (toupper(substr(dr.extr, 1, 1)) %in% c("L", "R")) {
      wt <- (Y^2)/D
      drtyp <- "Y^2/D-weights"
    }
    else if (toupper(substr(dr.extr, 1, 1)) %in% c("Y")) {
      wt <- Y
      drtyp <- "Y-weights"
    }
    else if (dr.extr %in% names(data)) {
      wt <- data[, dr.extr]
      drtyp <- paste(dr.extr, "weights")
    }
  }
  if (is.numeric(dr.extr)) 
    wt <- dr.extr
  Rp <- matrix(Rp, nrow = 1)
  Rc <- matrix(Rc, nrow = 1)
  xP <- detrend(rbind(Rp, MP), c(p0, P), weight = c(0, wt))
  xC <- detrend(rbind(Rc, MC), c(c0, P - A), weight = c(0, 
                                                        wt))
  MPr <- xP[-1, , drop = FALSE] - ref.p * xP[rep(1, nrow(MP)), 
                                             , drop = FALSE]
  MCr <- xC[-1, , drop = FALSE] - ref.c * xC[rep(1, nrow(MC)), 
                                             , drop = FALSE]
  if (length(grep("-", parm)) == 0) {
    if (parm %in% c("ADPC", "ADCP", "APC", "ACP")) 
      m.APC <- update(m.0, . ~ . - 1 + MA + I(P - p0) + 
                        MPr + MCr)
    drift <- rbind(ci.exp(m.APC, subset = "I\\(", alpha = alpha), 
                   ci.exp(m.Ad, subset = "I\\(", alpha = alpha))
    rownames(drift) <- c(paste("APC (", drtyp, ")", sep = ""), 
                         "A-d")
    if (parm == "ADCP") 
      m.APC <- update(m.0, . ~ . - 1 + MA + I(P - A - c0) + 
                        MPr + MCr)
    if (parm == "APC") {
      MPr <- cbind(P - p0, MPr)
      m.APC <- update(m.0, . ~ . - 1 + MA + MPr + MCr)
    }
    if (parm == "ACP") {
      MCr <- cbind(P - A - c0, MCr)
      m.APC <- update(m.0, . ~ . - 1 + MA + MPr + MCr)
    }
    Age <- cbind(Age = A.pt, ci.exp(m.APC, subset = "MA", 
                                    ctr.mat = MA[A.pos, , drop = FALSE], alpha = alpha))[order(A.pt), 
                                    ]
    Per <- cbind(Per = P.pt, ci.exp(m.APC, subset = "MPr", 
                                    ctr.mat = MPr[P.pos, , drop = FALSE], alpha = alpha))[order(P.pt), 
                                    ]
    Coh <- cbind(Coh = C.pt, ci.exp(m.APC, subset = "MCr", 
                                    ctr.mat = MCr[C.pos, , drop = FALSE], alpha = alpha))[order(C.pt), 
                                    ]
    colnames(Age)[-1] <- c("Rate", lu)
    colnames(Per)[-1] <- c("P-RR", lu)
    colnames(Coh)[-1] <- c("C-RR", lu)
    Type <- paste("ML of APC-model", Dist, ": (", parm, "):\n")
    Model <- m.APC
  } else {
    adc <- update(m.0, . ~ . - 1 + MA + I(P - A - c0))
    adp <- update(m.0, . ~ . - 1 + MA + I(P - p0))
    drift <- ci.exp(adc, subset = "I\\(")
    rownames(drift) <- "A-d"
    xP <- cbind(1, P - p0, MPr)
    xC <- cbind(1, P - A - c0, MCr)
    lP <- cbind(P - p0, MPr)
    lC <- cbind(P - A - c0, MCr)
    if (parm == "AD-C-P") {
      rc <- update(m.0, . ~ . - 1 + xC, offset = predict(adc, 
                                                         type = "link"))
      rp <- update(m.0, . ~ . - 1 + xP, offset = predict(adc, 
                                                         type = "link"))
      A.eff <- ci.exp(adc, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(rc, subset = "xC", ctr.mat = xC[C.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(rp, subset = "xP", ctr.mat = xP[P.pos, 
      ], alpha = alpha)
      Model <- list(adc, rc, rp)
    } else if (parm == "AD-P-C") {
      rp <- update(m.0, . ~ . - 1 + xP, offset = predict(adp, 
                                                         type = "link"))
      rc <- update(m.0, . ~ . - 1 + xC, offset = predict(rp, 
                                                         type = "link"))
      A.eff <- ci.exp(adp, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(rp, subset = "xP", ctr.mat = xP[P.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(rc, subset = "xC", ctr.mat = xC[C.pos, 
      ], alpha = alpha)
      Model <- list(adp, rp, rc)
    } else if (parm == "AC-P") {
      ac <- update(m.0, . ~ . - 1 + MA + lC)
      rp <- update(m.0, . ~ . - 1 + xP, offset = predict(ac, 
                                                         type = "link"))
      A.eff <- ci.exp(ac, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(ac, subset = "lC", ctr.mat = lC[C.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(rp, subset = "xP", ctr.mat = xP[P.pos, 
      ], alpha = alpha)
      Model <- list(ac, rp)
    } else if (parm == "AP-C") {
      ap <- update(m.0, . ~ . - 1 + MA + lP)
      rc <- update(m.0, . ~ . - 1 + xC, offset = predict(ap, 
                                                         type = "link"))
      A.eff <- ci.exp(ap, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(ap, subset = "lP", ctr.mat = lP[P.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(rc, subset = "xC", ctr.mat = xC[C.pos, 
      ], alpha = alpha)
      Model <- list(ap, rc)
    }
    Age <- cbind(Age = A.pt, A.eff)[order(A.pt), ]
    Per <- cbind(Per = P.pt, P.eff)[order(P.pt), ]
    Coh <- cbind(Cph = C.pt, C.eff)[order(C.pt), ]
    colnames(Age)[-1] <- c("A.eff", lu)
    colnames(Per)[-1] <- c("P.eff", lu)
    colnames(Coh)[-1] <- c("C.eff", lu)
    Type <- paste("Sequential modelling", Dist, ": (", parm, 
                  "):\n")
  }
  res <- list(Type = Type, Model = Model, Age = Age, Per = Per, 
              Coh = Coh, Drift = drift, Ref = c(Per = if (parm %in% 
                                                          c("APC", "ADPC", "Ad-P-C", "AP-C")) p0 else NA, Coh = if (parm %in% 
                                                                                                                    c("ACP", "ADCP", "Ad-C-P", "AC-P")) c0 else NA), 
              Anova = AOV)
  if (model %in% c("ns", "bSpline")) 
    res <- c(res, list(Knots = Knots))
  res$Age[, -1] <- res$Age[, -1] * scale
  if (print.AOV) {
    print(res$Type)
    print(res$Anova)
  }
  if (!ref.p & parm %in% c("APC", "ADPC")) 
    cat("No reference period given:\n", "Reference period for age-effects is chosen as\n", 
        "the median date of event: ", p0, ".\n")
  if (!ref.c & parm %in% c("ACP", "ADCP")) 
    cat("No reference period given:\n", "Reference period for age-effects is chosen as\n", 
        "the median date of birth for persons  with event: ", 
        c0, ".\n")
  class(res) <- "apc"
  invisible(res)
  
  # MCr backup
  MCr2 <- MCr
  MCr <- MCr2
  data2 <- data %>%
    add_predictions(m.APC, var = "log_d_spl") %>% 
    mutate(d_spl = exp(log_d_spl))
  
  # Fiting with cohort effects set to zero
  MCr <- MCr * 0
  data3 <- data2 %>% 
    add_predictions(m.APC, var = "log_d_ap_spl") %>% 
    mutate(d_ap_spl = exp(log_d_ap_spl))
  
  ####
  #### (smoothed observed mortality) - (AP fitting from a full APC model)
  ####
  
  mtx <- data %>% 
    arrange(P, A)
  ylist <- unique(mtx$P) %>% sort()
  alist <- unique(mtx$A) %>% sort()
  deaths <- matrix(mtx$D, nrow=length(alist), ncol=length(ylist), byrow=F)
  colnames(deaths) <- ylist
  rownames(deaths) <- alist
  # population at risk (population/exposure)
  exposure <- matrix(mtx$Y, nrow=length(alist), ncol=length(ylist), byrow=F)
  colnames(exposure) <- ylist
  rownames(exposure) <- alist
  # smoothing mortality
  fit <- Mort2Dsmooth(x = alist, y = ylist, Z = deaths, offset = log(exposure),
                      overdispersion = TRUE, method = 2)
  summary(fit)
  mx.smooth <- (exp(fit[26]$logmortality))*100000
  d_smth <- (exp(fit[26]$logmortality))*exposure
  
  
  smt <- d_smth %>%
    as_tibble() %>% 
    mutate(A = alist) %>% 
    gather(-A, key = P, value = d_sth) %>% 
    mutate(A = as.integer(A),
           P = as.integer(P))
  
  data4 <- data3 %>% 
    left_join(smt) %>% 
    dplyr::select(-log_d_spl, -log_d_ap_spl) %>% 
    rename(Age = A,
           Year = P,
           Dx_smt = D,
           Pop = Y) %>% 
    mutate(Cause = C, Sex=S)
  
}

#################
#Fit models
curv_drg_m <- curvature_dAPC(amin=10, amax=85, pmin=2000, pmax=2023, gr=1, 
                             C = "Drugs", S="Males")
curv_drg_f <- curvature_dAPC(amin=10, amax=85, pmin=2000, pmax=2023, gr=1, 
                             C = "Drugs", S="Females")
curv_scd_m <- curvature_dAPC(amin=10, amax=85, pmin=1981, pmax=2023, gr=1, 
                             C = "Suicide", S="Males")
curv_scd_f <- curvature_dAPC(amin=10, amax=85, pmin=1981, pmax=2023, gr=1, 
                             C = "Suicide", S="Females")
curv_alc_m <- curvature_dAPC(amin=10, amax=85, pmin=1981, pmax=2022, gr=1, 
                             C = "Alcohol", S="Males")
curv_alc_f <- curvature_dAPC(amin=10, amax=85, pmin=1981, pmax=2022, gr=1, 
                             C = "Alcohol", S="Females")

curvs <- bind_rows(curv_drg_m, curv_drg_f, curv_scd_m,
                   curv_scd_f, curv_alc_m, curv_alc_f)

db <- curvs %>% 
  mutate(mx = 100000 * Dx_smt / Pop,
         mx_sth = 100000 * d_sth / Pop,
         mx_spl = 100000 * d_spl / Pop,
         mx_ap_spl = 100000 * d_ap_spl / Pop,
         cohort = Year - Age,
         mx_exc = mx_sth - mx_ap_spl,
         rr = mx_sth / mx_ap_spl) 

agg_png("Outputs/APCCurvatureExcess.png", units="in", width=6, height=6, res=800)
ggplot(db, aes(x=Year, y=Age, fill=mx_exc))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_paletteer_c("scico::roma", direction=-1,
                         limits=c(-max(abs(db$mx_exc)), max(abs(db$mx_exc))),
                         name="Excess mortality\nPer 100,000")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  coord_equal()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

dev.off()


APCcurvs <- db %>% 
  filter(Age<=75) %>% 
  group_by(Year, Cause, Sex) %>% 
  summarise(Age=Age[mx_exc==max(mx_exc)], mx_exc=max(mx_exc), 
            rr=max(rr), .groups="drop")

ann_text <- data.frame(Age=seq(20, 77, by=5), Year=rep(2026.5, times=12), 
                       label=as.character(seq(2005, 1950, by=-5)))

agg_png("Outputs/APCCurvaturePlotFull.png", units="in", width=9, height=7, res=700)
ggplot(APCcurvs, aes(x=Year, y=Age))+
  geom_point(aes(fill=Cause, size=rr), shape=21, colour="black", alpha=0.7)+
  scale_x_continuous(name="")+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause")+
  scale_size_continuous(name="Relative Risk")+
  geom_vline(xintercept = seq(1980, 2025, by=5), linetype="dashed", 
             color="grey30", linewidth=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(15, 75, by=5), linetype="dashed", color="grey30", 
             linewidth=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-2010, -1860, by=5), slope = 1, linetype="dashed", 
              color="grey30", linewidth=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2025, y = 13.5, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  facet_grid(~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="APC Curvature plot of alcohol, suicide and drug deaths",
       subtitle="Modal age of greatest excess mortality vs. a counterfactual assuming no cohort trends\n",
       caption="Data from National Records of Scotland\nPlot concept by Kike Acosta & Alyson van Raalte\nCreated by @VictimOfMaths")

dev.off()

agg_png("Outputs/APCCurvaturePlot2000.png", units="in", width=8, height=7, res=700)
ggplot(APCcurvs %>% filter(Year>=2000), aes(x=Year, y=Age))+
  geom_point(aes(fill=Cause, size=rr), shape=21, colour="black", alpha=0.7)+
  scale_x_continuous(name="", limits=c(2000, 2028), breaks=c(2000, 2010, 2020))+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_manual(values=c("#00A1FF", "#E69F00", "#CC5395"), name="Cause")+
  scale_size_continuous(name="Relative Risk")+
  geom_vline(xintercept = seq(2000, 2025, by=5), linetype="dashed", 
             color="grey30", linewidth=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(15, 75, by=5), linetype="dashed", color="grey30", 
             linewidth=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-2010, -1860, by=5), slope = 1, linetype="dashed", 
              color="grey30", linewidth=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2025, y = 13.5, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  facet_grid(~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="APC Curvature plot of alcohol, suicide and drug deaths",
       subtitle="Modal age of greatest excess mortality vs. a counterfactual assuming no cohort trends\n",
       caption="Data from National Records of Scotland\nPlot concept by Kike Acosta & Alyson van Raalte\nCreated by @VictimOfMaths")

dev.off()

