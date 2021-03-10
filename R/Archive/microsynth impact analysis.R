# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Impact Analysis from Synthetic results
# Set up

#################################################
library(dplyr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(stargazer)
library(MASS)
library(plm)
library(margins)
library(scales)
library(microsynth)
library(readxl)
library(gsynth)
library(panelView)
library(tidyr)

library(readxl)
library(dplyr)
library(lubridate)
library(dummies)
library(readstata13)
library(readr)
library(statar)

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
path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
# Path where matched samples should be saved 
path_output <- paste0(path_project,"\\ACA and alt\\output")

if(Sys.info()[["nodename"]]=="SIM1"){
  # Root folder
  path_project <- "H:/Phd Requirements min wage"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"\\ACA and alt\\output")
}
if(Sys.info()[["nodename"]]=="SIM2"){
  # Root folder
  path_project <- "H:/Phd Requirements min wage"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"\\ACA and alt\\output")
}
if(Sys.info()[["nodename"]]=="SIM3"){
  # Root folder
  path_project <- "H:/Phd Requirements min wage"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"\\ACA and alt\\output")
}
setwd(paste0(path_data))
set.seed(99199)

#################################################################
#                              Set Up                           #
#################################################################
synth_list <- list()
synth_Uber_list <- list()
synth_Uber_effect_list <- list()
estab_tidy_list <- list()

load("Nonemployer_statistics_1997_2017.RData") # load the data

medicaid <- read_excel("medicaid.xlsx")

load("county_population_2000_2018.RData")

# load("GDP_2001_2018.RData")
# load("fips_1997_2018.RData")

load("Uber_2000_2017.RData")

# GDP <- merge(GDP, fips)

using_all <- merge(NES, medicaid)
using_all <- merge(using_all, population)
using_all <- merge(using_all, Uber_timing)
# using_all <- merge(using_all, GDP)

industry_list <- sort(unique(using_all$naics))
#################################################################
#                              Industry                         #
#################################################################
# i<-1 # for total
# i <- 9 # for 48-49
# i <- 10 # for 4853
# for (i in seq_along(industry_list)) {
setwd(paste0(path_data))

print(paste0(industry_list[1],"start"))

using <- subset(using_all, naics==industry_list[1])

using$estab_pop <- (using$estab/using$population) # increase the size of the estab_pop term for easier reading of results
using$id <- as.factor(paste(using$st, using$cty, using$naics, sep = "-")) # create unique id for each county-state
using$cty_st <- as.factor(paste(using$st, using$cty, sep = "-")) # create unique id for each county-state
# using <- subset(using, st!="2" & st!="18" & st!="22" & st!="30" & st!="42") # Drop states that have medicaid expansions after 2014 and before 2018

#######################################################
###     Ever medicaid expansion identification      ###
#######################################################
ever_medicaid <- subset(medicaid, YEAR.id=="2017")
ever_medicaid$Ever_medicaid <- ever_medicaid$Medicaid
ever_medicaid <- ever_medicaid %>% dplyr::select("st", "Ever_medicaid")

using <- merge(using, ever_medicaid)
#######################################################
###     Microsynthetic for local Min Wage increases ###
#######################################################
using$YEAR.id <- as.factor(using$YEAR.id)
using$years <- as.numeric(as.character(using$YEAR.id))
using$st <- as.factor(using$st)
using$cty_st_numeric <- as.numeric(using$cty_st)

using_model <- using %>% dplyr::select("estab_pop", 
                                       "estab",
                                       "population",
                                       "Medicaid", 
                                       "Ever_medicaid",
                                       "cty_st",
                                       "YEAR.id",
                                       "cty_st_numeric",
                                       "years",
                                       "id",
                                       "st")
using_model$estab_log <- log(using_model$estab)
using_model <- using_model[complete.cases(using_model),]



# No population qualifier -0.0044 to establishments per person

population_trends <- aggregate(x = using_model$population, by = list(using_model$years, using_model$Medicaid), FUN = sum)
estab_pop_trends <- aggregate(x = using_model$estab_pop, by = list(using_model$years, using_model$Medicaid), FUN = mean)
estab_trends <- aggregate(x = using_model$estab, by = list(using_model$years, using_model$Medicaid), FUN = sum)

# how many more nonemployer establishments would we have had in 2014 if we did not expand medicaid?
(population_trends[19,3]*(estab_pop_trends[19,3]+0.0044)) - (population_trends[19,3]*estab_pop_trends[19,3]) 


# percent reduction from total stock of nonemployer establishments in medicaid states
((population_trends[19,3]*(estab_pop_trends[19,3]+0.0044)) - (population_trends[19,3]*estab_pop_trends[19,3]))/estab_trends[19,3]



