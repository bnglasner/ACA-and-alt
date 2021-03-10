# Ben Glasner
# Medicaid and nonemployers analysis
# Full Analysis
# Set up

#################################################
library(dplyr)
library(broom)
library(ggplot2)
library(stargazer)
library(MASS)
library(plm)
library(scales)
library(readxl)
library(gsynth)
library(panelView)
library(tidyr)
library(lubridate)
library(dummies)
library(readstata13)
library(readr)
library(statar)
library(reshape2)

options(scipen=10000)

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/PhD Requirements"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/PhD Requirements"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"//ACS Data//Data")
path_CPS <- paste0(path_project,"//CPS Data//Data//")
# Path to NES data 
path_NES <- paste0(path_project,"//Nonemployer data//Data")
# Path where matched samples should be saved 
path_output <- paste0(path_project,"//ACA and alt//output")

if(Sys.info()[["nodename"]]=="SIM1"){
  # Root folder
  path_project <- "H:/Phd Requirements"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"//ACS Data//Data")
  path_CPS <- paste0(path_project,"//CPS Data//Data//")
  # Path to NES data 
  path_NES <- paste0(path_project,"//Nonemployer data//Data")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"//ACA and alt//output")
}
if(Sys.info()[["nodename"]]=="SIM2"){
  # Root folder
  path_project <- "H:/Phd Requirements"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"//ACS Data//Data")
  path_CPS <- paste0(path_project,"//CPS Data//Data//")
  # Path to NES data 
  path_NES <- paste0(path_project,"//Nonemployer data//Data")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"//ACA and alt//output")
}
if(Sys.info()[["nodename"]]=="SIM3"){
  # Root folder
  path_project <- "H:/Phd Requirements"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"//ACS Data//Data")
  path_CPS <- paste0(path_project,"//CPS Data//Data//")
  # Path to NES data 
  path_NES <- paste0(path_project,"//Nonemployer data//Data")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"//ACA and alt//output")
}
set.seed(99199)



#################################################################
#                              Set Up                           #
#################################################################
setwd(paste0(path_CPS))
load("CPS.RData") # load the CPS data
CPS <- data
rm(data)

setwd(paste(path_data))
load("ACS.RData") # load the ACS data
load("ACS_desc.RData") # load the ACS data

setwd(paste0(path_NES))
load("Nonemployer_statistics_state_1997_2017.RData") # load the NES state data
NES<- subset(NES, naics == "00" & as.numeric(YEAR.id)>1999)

medicaid <- read_excel("medicaid.xlsx")
colnames(medicaid)[2] <- "st"

load("state_population_2000_2018.RData") 

sapply(data, function(x) sum(is.na(x)))
data <- subset(data, LABFORCE==2)
CPS <- subset(CPS, LABFORCE==2)

state_aggs_ACS  <- data %>% 
  group_by(STATEFIP, YEAR) %>%
  summarize(Estab_pct_ACS = weighted.mean(CLASSWKRD == 13, PERWT))
colnames(state_aggs_ACS)[1] <- "st"
colnames(state_aggs_ACS)[2] <- "YEAR.id"
state_aggs_ACS <- subset(state_aggs_ACS, YEAR.id <2019 & YEAR.id>1999)


state_aggs_CPS  <- CPS %>% 
  group_by(STATEFIP, YEAR) %>%
  summarize(Estab_pct_CPS = weighted.mean(CLASSWKR == 13, ASECWTH))
colnames(state_aggs_CPS)[1] <- "st"
colnames(state_aggs_CPS)[2] <- "YEAR.id"
state_aggs_CPS <- subset(state_aggs_CPS, YEAR.id <2019 & YEAR.id>1999)

state_aggs <- merge(state_aggs_ACS, state_aggs_CPS)
state_aggs <- merge(state_aggs, medicaid)
state_aggs <- merge(state_aggs, st_population)

NES <- merge(NES, medicaid)
NES <- merge(NES, st_population)
NES$estab_pop <- NES$estab/NES$population

ever_medicaid <- subset(medicaid, YEAR.id=="2017")
ever_medicaid$Ever_medicaid <- ever_medicaid$Medicaid
ever_medicaid <- ever_medicaid %>% dplyr::select("st", "Ever_medicaid")

state_aggs <- merge(state_aggs, ever_medicaid)
NES <- merge(NES, ever_medicaid)

state_aggs <- merge(state_aggs,NES)

save(state_aggs, file = "state_NES_ACS_CPS.RData")
