# CEVI - Climate Extreme Vulnerability Index #

# Date: Nov 10, 2020
# Programmers' email: gilvan@epopea.com.br

#------Setup--------
# Initial settings #
#------------------#

# Cleaning the environment
rm(list=ls())

# Installing the required packages
myPackages <- c("convey", "dplyr", "magrittr", "survey", "ggplot2", "fmsb", "gtools")
new.packages <- myPackages[!(myPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(myPackages, require, character.only = TRUE)

# Defining the path and reading the data frame

setwd("/Users/grguedes/Dropbox/My Mac (Air-de-Gilvan)/Downloads/CEVI/")
data <- readxl::read_excel(path="Datasets/dados.xlsx")

# Renaming variable names

names(data)

data <- rename(old = `%_P_Idosa`,
               kid = `%P.<5 anos`,
               data, urb = `%P_UB`,
               water = `Ab.H20(%)`,
               sewage = `Esg.sanitario(%)`,
               garbage = `Coleta de lixo(%)`,
               literate = `P.20-alfab(%)`,
               pcincome = `Renda_média_per_capita`)

names(data)

#-----Functions--------
# Functions developed #
#---------------------#

source("Final Scripts/CEVI Functions.R")

#--------------------Cutoffs---------------------
# Establishing the uncensored headcount cutoffs #
#-----------------------------------------------#

source("Final Scripts/Cutoffs.R", encoding = "utf8")

#-------Subetting by region----------
# Subsetting the datasets by region #
#-----------------------------------#

data %<>%
  select(Mesorregião, TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>%
  mutate_if(.predicate = is.numeric, .funs = function(x){x <= quantile(x, probs = .75)}) %>%
  mutate_if(.predicate = is.logical, .funs = as.numeric) %>%
  inner_join(data,suffix = c("d", ""), by = "Mesorregião")

data %<>%
  filter(Regiao == 1) %>%
  select(Mesorregião, TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>%
  mutate_if(.predicate = is.numeric, .funs = function(x){x <= quantile(x, probs = .75)}) %>%
  mutate_if(.predicate = is.logical, .funs = as.numeric) %>%
  right_join(data,suffix = c("dno", ""), by = "Mesorregião")

data %<>%
  filter(Regiao == 2) %>%
  select(Mesorregião, TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p) %>%
  mutate_if(.predicate = is.numeric, .funs = function(x){x <= quantile(x, probs = .75)}) %>%
  mutate_if(.predicate = is.logical, .funs = as.numeric) %>%
  right_join(data,suffix = c("dne", ""), by = "Mesorregião")

north_data <- data %>% filter(Regiao==1)
northeast_data <- data %>% filter(Regiao==2)

#------------------Subsetting by disease-----------------------
# Subsetting the datasets by incidence of infectious diseases #
#-------------------------------------------------------------#

low_data <- data %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.20)))
high_data <- data %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.60)))

#data %>% select(Mesorregião,Regiao,TIP_Morb) %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.25))) %>% View()
#data %>% select(Mesorregião,Regiao,TIP_Morb) %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.75))) %>% View()

low_north_data <- north_data %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.20)))
high_north_data <- north_data %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.60)))

low_northeast_data <- northeast_data %>% filter(TIP_Morb <= quantile(TIP_Morb, probs=c(0.20)))
high_northeast_data <- northeast_data %>% filter(TIP_Morb > quantile(TIP_Morb, probs=c(0.60)))

#--------------Svydesign--------------------
# Declaring the data as a svydesign object #
#------------------------------------------#

cdata <- svydesign( ids = ~1 , weights=~Total, data = data )
cnorth_data <- svydesign( ids = ~1 , weights=~Total, data = north_data )
cnortheast_data <- svydesign( ids = ~1 , weights=~Total, data = northeast_data )

low_cdata <- svydesign( ids = ~1 , weights=~Total, data = low_data )
low_cnorth_data <- svydesign( ids = ~1 , weights=~Total, data = low_north_data )
low_cnortheast_data <- svydesign( ids = ~1 , weights=~Total, data = low_northeast_data )

high_cdata <- svydesign( ids = ~1 , weights=~Total, data = high_data )
high_cnorth_data <- svydesign( ids = ~1 , weights=~Total, data = high_north_data )
high_cnortheast_data <- svydesign( ids = ~1 , weights=~Total, data = high_northeast_data )

#------------------------------------Convey------------------------------------
# Converting the data into data appropriate for the use in the convey package #
#-----------------------------------------------------------------------------#

cdata <- convey_prep(cdata)
cnorth_data <- convey_prep(cnorth_data)
cnortheast_data <- convey_prep(cnortheast_data)

low_cdata <- convey_prep(low_cdata)
low_cnorth_data <- convey_prep(low_cnorth_data)
low_cnortheast_data <- convey_prep(low_cnortheast_data)

high_cdata <- convey_prep(high_cdata)
high_cnorth_data <- convey_prep(high_cnorth_data)
high_cnortheast_data <- convey_prep(high_cnortheast_data)


#----Estimation----
# CEVI ESTIMATION #
#-----------------#

cevi(data = high_cnortheast_data,
     formula = ~ TXxd + TNxd + TX90pd + TN90pd + DTRd + Cddd + R99pd +
       elderly + children + pcincome + literate +
       sewage + water + garbage + urb,
     cutoffs = list(expcut[1],expcut[2],expcut[3],expcut[4],expcut[5],expcut[6],expcut[7],
                    suscut[1],suscut[2],suscut[3],suscut[4],
                    adapcut[1],adapcut[2],adapcut[3],adapcut[4]),
     dimw = c(rep(0.04761905,7),rep(0.08333333,8)),
     subgroup = ~factor(Regiao),
     k= .25, g= 0, decomp.complete=TRUE, 
     decompose = TRUE, index = TRUE,
     length = c(7, 4, 4))

#-----------Sensitivity---------------
# SENSITIVITY AND DOMINANCE ANALYSIS #
#------------------------------------#

table(data$Regiao)
data$Regiao <- factor(data$Regiao,
                      levels=c(1,2),
                      labels=c("Amazonia","Northeast"))
sensitivity(data = data, grouping.var = "Regiao", weights = "Total",ids = 1, 
            formula = ~ TXxd + TNxd + TX90pd + TN90pd + DTRd + Cddd + R99pd +
            elderly + children + pcincome + literate +
            sewage + water + garbage + urb,
            cutoffs = c(expcut, suscut, adapcut),
            dimw = c(rep(0.04761905,7),rep(0.08333333,8)),
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

source("Final Scripts/Barplot with Multi Legend.R")

#------------------------#
# DESCRIPTIVE STATISTICS #
#------------------------#

# Difference-in-means and difference-in-proportions tests

# Exposure
t.test(x=data$TXx[data$Regiao=="Amazonia"],
       y=data$TXx[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$TNx[data$Regiao=="Amazonia"],
       y=data$TNx[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$TX90p[data$Regiao=="Amazonia"],
       y=data$TX90p[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$TN90p[data$Regiao=="Amazonia"],
       y=data$TN90p[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$DTR[data$Regiao=="Amazonia"],
       y=data$DTR[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$Cdd[data$Regiao=="Amazonia"],
       y=data$Cdd[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$R99p[data$Regiao=="Amazonia"],
       y=data$R99p[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Susceptibility
t.test(x=data$old[data$Regiao=="Amazonia"],
       y=data$old[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$kid[data$Regiao=="Amazonia"],
       y=data$kid[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$pcincome[data$Regiao=="Amazonia"],
       y=data$pcincome[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$literate[data$Regiao=="Amazonia"],
       y=data$literate[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Adaptive Capacity
t.test(x=data$water[data$Regiao=="Amazonia"],
       y=data$water[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$sewage[data$Regiao=="Amazonia"],
       y=data$sewage[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$garbage[data$Regiao=="Amazonia"],
       y=data$garbage[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

t.test(x=data$urb[data$Regiao=="Amazonia"],
       y=data$urb[data$Regiao=="Northeast"],
       alternative = "two.sided",
       conf.level=0.95,
       mu=0,
       var.equal=F)

# Health
t.test(x=data$TIP_Morb[data$Regiao=="Amazonia"],
       y=data$TIP_Morb[data$Regiao=="Northeast"],
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

data %>% filter(Regiao=="Amazonia") %>%
  select(old, kid) %>%
  mutate_all(.funs= function(x) {quantile(x,probs=c(0.25))}) %>% summarise_all(.funs=mean)

data %>% filter(Regiao=="Northeast") %>%
  select(old, kid) %>%
  mutate_all(.funs= function(x) {quantile(x,probs=c(0.25))}) %>% summarise_all(.funs=mean)


data %>% filter(Regiao=="Amazonia") %>%
  select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p,
         old, kid, pcincome, literate, sewage, water, garbage, urb,
         TIP_Morb) %>%  as.data.frame() %>% stargazer(type="text",
                                                      summary=T, omit.summary.stat=c("n",
                                                                                     "sd", "p25","p75"),
                                                      digits=1,
                                                      notes="Source: Xavier et al. (2015); Brazilian Census (IBGE, 2010); SIH/MS (Brazil, 2009/2011)",
                                                      title="Descriptive Statistics for Indicators in Amazonia, Brazil - 2010")

data %>% filter(Regiao=="Northeast") %>%
  select(TXx, TNx, TX90p, TN90p, DTR, Cdd, R99p,
         old, kid, pcincome, literate, sewage, water, garbage, urb,
         TIP_Morb) %>%  as.data.frame() %>% stargazer(type="text",
                                                      summary=T, omit.summary.stat=c("n",
                                                                                     "sd", "p25","p75"),
                                                      digits=1,
                                                      notes="Source: Xavier et al. (2015); Brazilian Census (IBGE, 2010); SIH/MS (Brazil, 2009/2011)",
                                                      title="Descriptive Statistics for Indicators in the Northeast Region, Brazil - 2010")

