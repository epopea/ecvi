# CEVI - Climate Extreme Vulnerability Index #

# Date: Mar 1, 2021
# Programmers: Jeferson's email: jeferson@epopea.com.br
#              Gilvan's   email: gilvan@epopea.com.br

#------Setup--------
# Initial settings #
#------------------#

# Cleaning the environment
rm(list=ls())

# Installing the required packages
myPackages <- c("convey", "dplyr", "magrittr", "survey", "ggplot2", "fmsb", 
                "gtools", "broom", "maptools", "rgdal", "sp", "rgeos")
new.packages <- myPackages[!(myPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(myPackages, require, character.only = TRUE)

# Defining the path and reading the data frame

setwd("C:\\Users\\Jeferson\\Desktop\\CEVI")
data <- readxl::read_excel(path="Datasets/dados_novos.xlsx")

# Renaming variable names

names(data)

data <- rename(data, old = `_P_Idosa`,
               kid = `P5anos`,
               literate = `P20alfab`,
               poor = `Perc_pobres`,
               pcincome = `Renda_média_per_capita`,
               urb = `P_UB`,
               water = `AbH20`,
               sewage = `Esgsanitario`,
               sewage_alt = `Perc_esgoto`,
               garbage = `Coletadelixo`,
               fhs = `txPSF`,
               beds = `txleitos`,
               zone = `Zona`,
               dzone = `Regiao`,
               region = `Regiao2`,
               mesoregion = `Mesorregião`,
               urban = `Urbana`,
               total = `Total`,
               rural = `Rural`,
               state = `UF`)

names(data)

#-----Functions--------
# Functions developed #
#---------------------#

source("Final Scripts/ECVI Functions.R")

#--------------------Cutoffs---------------------
# Establishing the uncensored headcount cutoffs #
#-----------------------------------------------#

source("Final Scripts/Cutoffs.R", encoding = "utf8")


#-------Mapping Regions-------
# Exporting data for mapping #
#----------------------------#
#----------------------------#
data %>% select(zone, CD_GEOCME, region) %>% 
  mutate(map_zone = case_when(
    zone == 1 ~ 1,
    zone == 11 ~ 2,
    zone == 12 ~ 3,
    zone == 21 ~ 4,
    zone == 22 ~ 5,
    TRUE ~ 6
    )) %>% write.csv(., file="Maps/zones.csv", row.names = F)


#-------Subetting by region----------
# Subsetting the datasets by region #
#-----------------------------------#

data %<>%
  select(mesoregion, TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>%
  mutate_if(.predicate = is.numeric, .funs = function(x){x <= quantile(x, probs = .75)}) %>%
  mutate_if(.predicate = is.logical, .funs = as.numeric) %>%
  inner_join(data,suffix = c("d", ""), by = "mesoregion")

data %<>%
  filter(dzone == 1) %>%
  select(mesoregion, TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>%
  mutate_if(.predicate = is.numeric, .funs = function(x){x <= quantile(x, probs = .75)}) %>%
  mutate_if(.predicate = is.logical, .funs = as.numeric) %>%
  right_join(data,suffix = c("dwet", ""), by = "mesoregion")

data %<>%
  filter(dzone == 2) %>%
  select(mesoregion, TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>%
  mutate_if(.predicate = is.numeric, .funs = function(x){x <= quantile(x, probs = .75)}) %>%
  mutate_if(.predicate = is.logical, .funs = as.numeric) %>%
  right_join(data,suffix = c("ddry", ""), by = "mesoregion")

wet_data <- data %>% filter(dzone==1)
dry_data <- data %>% filter(dzone==2)

#------------------Subsetting by disease-----------------------
# Subsetting the datasets by incidence of infectious diseases #
#-------------------------------------------------------------#

low_data <- data %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.20)))
high_data <- data %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.60)))

#data %>% select(mesoregion,dzone,TIP_Morb) %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.25))) %>% View()
#data %>% select(mesoregion,dzone,TIP_Morb) %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.75))) %>% View()

low_wet_data <- wet_data %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.20)))
high_wet_data <- wet_data %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.60)))

low_dry_data <- dry_data %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.20)))
high_dry_data <- dry_data %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.60)))

#--------------Svydesign--------------------
# Declaring the data as a svydesign object #
#------------------------------------------#

cdata <- svydesign( ids = ~1 , weights=~total, data = data )
cwet_data <- svydesign( ids = ~1 , weights=~total, data = wet_data )
cdry_data <- svydesign( ids = ~1 , weights=~total, data = dry_data )

low_cdata <- svydesign( ids = ~1 , weights=~total, data = low_data )
low_cwet_data <- svydesign( ids = ~1 , weights=~total, data = low_wet_data )
low_cdry_data <- svydesign( ids = ~1 , weights=~total, data = low_dry_data )

high_cdata <- svydesign( ids = ~1 , weights=~total, data = high_data )
high_cwet_data <- svydesign( ids = ~1 , weights=~total, data = high_wet_data )
high_cdry_data <- svydesign( ids = ~1 , weights=~total, data = high_dry_data )

#------------------------------------Convey------------------------------------
# Converting the data into data appropriate for the use in the convey package #
#-----------------------------------------------------------------------------#

cdata <- convey_prep(cdata)
cwet_data <- convey_prep(cwet_data)
cdry_data <- convey_prep(cdry_data)

low_cdata <- convey_prep(low_cdata)
low_cwet_data <- convey_prep(low_cwet_data)
low_cdry_data <- convey_prep(low_cdry_data)

high_cdata <- convey_prep(high_cdata)
high_cwet_data <- convey_prep(high_cwet_data)
high_cdry_data <- convey_prep(high_cdry_data)


#----Estimation----
# CEVI ESTIMATION #
#-----------------#

ecvi(data = high_cwet_data,
     formula = ~ TXxd + TNxd + TX90pd + TN90pd + DTRd + Cddd + R99pd +
       elderly + children + pcincome + poor + literate +
       sewage + water + garbage + urb + fhs + beds,
     cutoffs = list(expcut[1],expcut[2],expcut[3],expcut[4],expcut[5],expcut[6],expcut[7],
                    suscut[1],suscut[2],suscut[3],suscut[4],suscut[5],
                    adapcut[1],adapcut[2],adapcut[3],adapcut[4],adapcut[5],adapcut[6]),
     dimw = c(rep(1/3/7,7),rep(1/3/5,5),rep(1/3/6,6)),
     subgroup = ~factor(dzone),
     k= .25, g= 0, decomp.complete=TRUE, 
     decompose = TRUE, index = TRUE,
     length = c(7, 5, 6))

#-----------Sensitivity---------------
# SENSITIVITY AND DOMINANCE ANALYSIS #
#------------------------------------#

data$dzone <- factor(data$dzone,
                     levels=c(1,2),
                     labels=c("ER","ED-HT"))

sensitivity(data = data, grouping.var = "dzone", weights = "total",ids = 1, 
            formula = ~ TXxd + TNxd + TX90pd + TN90pd + DTRd + Cddd + R99pd +
              elderly + children + pcincome + poor + literate +
              sewage + water + garbage + urb + fhs + beds,
            cutoffs = c(expcut, suscut, adapcut),
            dimw = c(rep(1/3/7,7),rep(1/3/5,5),rep(1/3/6,6)),
            g = 0, k=3,
            na.rm = T, sensitivity = TRUE, sensitivity.plot = TRUE,
            dominance = TRUE, summary = F, type = "text")


#--------------Radar charts--------------
# GRAPHICAL REPRESENTATION (RADARCHART) #
#---------------------------------------#

source("Final Scripts/Radar Charts.R")

#---------Decomposition Graphs---------
# GRAPHICAL REPRESENTATION (BARPLOTS) #
#-------------------------------------#

source("Final Scripts/Barplots and Boxplots.R")

#------------------------#
# DESCRIPTIVE STATISTICS #
#------------------------#

# Difference-in-means and difference-in-proportions tests

# Exposure
t.test(x=data$TXx[data$dzone=="ER"],
       y=data$TXx[data$Regiao=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$TNx[data$dzone=="ER"],
       y=data$TNx[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$TX90p[data$dzone=="ER"],
       y=data$TX90p[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$TN90p[data$dzone=="ER"],
       y=data$TN90p[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$DTR[data$dzone=="ER"],
       y=data$DTR[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$Cdd[data$dzone=="ER"],
       y=data$Cdd[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$R99p[data$dzone=="ER"],
       y=data$R99p[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Susceptibility
t.test(x=data$old[data$dzone=="ER"],
       y=data$old[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$kid[data$dzone=="ER"],
       y=data$kid[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$pcincome[data$dzone=="ER"],
       y=data$pcincome[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$literate[data$dzone=="ER"],
       y=data$literate[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Adaptive Capacity
t.test(x=data$water[data$dzone=="ER"],
       y=data$water[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$sewage[data$dzone=="ER"],
       y=data$sewage[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$garbage[data$dzone=="ER"],
       y=data$garbage[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$urb[data$dzone=="ER"],
       y=data$urb[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Health
t.test(x=data$TIP_Morb[data$dzone=="ER"],
       y=data$TIP_Morb[data$dzone=="ED-HT"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Cutoff values for variables used

quantile(data$TIP_Morb,probs=c(0.2,0.6))

data %>% select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p,
                old, kid) %>%
  mutate_all(.funs= function(x) {quantile(x, probs=c(0.75))}) %>% head(1) %>%
  rbind %>% t %>% round(1)

data %>% select(literate,sewage, water, garbage, urb) %>%
  mutate_all(.funs= function(x) {quantile(x, probs=c(0.25))}) %>% head(1) %>%
  rbind %>% t %>% round(1)

data %>% filter(dzone == "ER") %>%
  select(old, kid) %>%
  mutate_all(.funs= function(x) {quantile(x,probs=c(0.25))}) %>% summarise_all(.funs=mean)

data %>% filter(dzone == "ED-HT") %>%
  select(old, kid) %>%
  mutate_all(.funs= function(x) {quantile(x,probs=c(0.25))}) %>% summarise_all(.funs=mean)


data %>% 
  ungroup %>%
  filter(dzone == "ER") %>%
  select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p,
         old, kid, pcincome, literate, sewage, water, garbage, urb,
         TIP_Morb) %>% 
  as.data.frame() %>%
  stargazer(type="text",
            summary=T, omit.summary.stat=c("n", "sd", "p25","p75"),
            digits=1,
            notes="Source: Xavier et al. (2015); Brazilian Census (IBGE, 2010); SIH/MS (Brazil, 2009/2011)",
            title="Descriptive Statistics for Indicators in Amazonia, Brazil - 2010")

data %>% 
  filter(dzone == "ED-HT") %>%
  select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p,
         old, kid, pcincome, literate, sewage, water, garbage, urb,
         TIP_Morb) %>% 
  as.data.frame() %>%
  stargazer(type="text",
            summary=T, omit.summary.stat=c("n", "sd", "p25","p75"),
            digits=1,
            notes="Source: Xavier et al. (2015); Brazilian Census (IBGE, 2010); SIH/MS (Brazil, 2009/2011)",
            title="Descriptive Statistics for Indicators in the Northeast Region, Brazil - 2010")

