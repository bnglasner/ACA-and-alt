# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Synthetic Control Design
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

estab_tidy_list <- list()

inter_fe_estab_list <- list()

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
# i <- 10 # for 4853
# i <- 9 # for 48-49

  setwd(paste0(path_data))
  
  print(paste0(industry_list[i],"start"))
  
  using <- subset(using_all, naics==industry_list[1])
  
  using$estab_pop <- (using$estab/using$population) # increase the size of the estab_pop term for easier reading of results
  using$id <- as.factor(paste(using$st, using$cty, using$naics, sep = "-")) # create unique id for each county-state
  using$cty_st <- as.factor(paste(using$st, using$cty, sep = "-")) # create unique id for each county-state
  # using <- subset(using, st!="2" & st!="18" & st!="22" & st!="30" & st!="42") # Drop states that have medicaid expansions after 2014 and before 2018
  
  #######################################################
  ###     Uber as low barrier indicator ###
  #######################################################
  
  # Create column for active Uber at the time of observation
  using$uber_year <- year(ymd(using$min_uber)) # pull the year number from the date object "min"
  
  using$Uber_active <-0
  using$Uber_active [using$YEAR.id >= using$uber_year] <- 1 # replace the value with 1 if the year of the observation is passed the year UBer entered and it is a location where uber entered
  
  using$uber_yday <- yday(ymd(using$min)) # pull the year number from the date object "min"
  using$Uber_year_share <- (366-using$uber_yday)/365
  using$Uber_year_share[is.na(using$Uber_year_share)] <- 0
  using$Uber_year_share <- as.numeric(using$Uber_year_share)
  using$Uber_year_share[using$Uber_active==0] <- 0
  
  using$years_active <- as.numeric(using$YEAR.id - using$uber_year)
  using$years_active[is.na(using$years_active)] <- 0
  using$years_active[using$years_active < 0] <- 0
  
  using$dosage_uber <- using$years_active + using$Uber_year_share
  
  #######################################################
  ###     Microsynthetic for local Min Wage increases ###
  #######################################################
  
  
  using$YEAR.id <- as.factor(using$YEAR.id)
  using$years <- as.numeric(as.character(using$YEAR.id))
  using$st <- as.factor(using$st)
  using$cty_st_numeric <- as.numeric(using$cty_st)
  
  using$Medicaid_Uber <- using$Medicaid*using$Uber_active
  
  using$Urban <- 0
  using$Urban[using$population>50000] <- 1
  using$Medicaid_Urban <- using$Medicaid*using$Urban
  
  using$Medicaid_Urban_Uber <- using$Medicaid*using$Uber_active*using$Urban
  using$Urban_Uber <- using$Uber_active*using$Urban
  
  using_model <- using %>% dplyr::select("estab_pop", 
                                         "estab",
                                         "population",
                                         "Medicaid", 
                                         "Medicaid_Uber",
                                         "Medicaid_Urban",
                                         "Medicaid_Urban_Uber",
                                         "Uber_active",
                                         "Urban",
                                         "Urban_Uber",
                                         "cty_st",
                                         "YEAR.id",
                                         "cty_st_numeric",
                                         "years",
                                         "id",
                                         "st",)
  
  using_model <- using_model[complete.cases(using_model),]
  
  county_counts <- as.data.frame(table(using_model$id)) # count how many times a county-state appears in the sample
  county_counts <- subset(county_counts, Freq==18) # keep only the counties that are in the full sample for balance
  colnames(county_counts)[1]<- "id" # relabel the id for the merge
  using_model<- merge(using_model, county_counts) # merge as a way to drop the unbalanced counties
  rm(county_counts) # drop county_counts from the env.
  
  using_model_urban <- subset(using_model,Urban==1)
  county_counts <- as.data.frame(table(using_model_urban$id)) # count how many times a county-state appears in the sample
  county_counts <- subset(county_counts, Freq==18) # keep only the counties that are in the full sample for balance
  colnames(county_counts)[1]<- "id" # relabel the id for the merge
  using_model_urban<- merge(using_model_urban, county_counts) # merge as a way to drop the unbalanced counties
  rm(county_counts) # drop county_counts from the env.
  
  using_model_rural <- subset(using_model,Urban==0)
  county_counts <- as.data.frame(table(using_model_rural$id)) # count how many times a county-state appears in the sample
  county_counts <- subset(county_counts, Freq==18) # keep only the counties that are in the full sample for balance
  colnames(county_counts)[1]<- "id" # relabel the id for the merge
  using_model_rural<- merge(using_model_rural, county_counts) # merge as a way to drop the unbalanced counties
  rm(county_counts) # drop county_counts from the env.
  
  using_model <- rbind(using_model_rural, using_model_urban)
  
  inter_fe_all <- interFE(estab_pop ~ 
                               Medicaid,
                             data = using_model, index=c("cty_st_numeric","years"),
                             force = "two-way", nboots = 100, se = TRUE)
  
  inter_fe_urban <- interFE(estab_pop ~ 
                              Medicaid,
                            data = using_model_urban, index=c("cty_st_numeric","years"),
                            force = "two-way", nboots = 100, se = TRUE)

  inter_fe_rural <- interFE(estab_pop ~ 
                               Medicaid,
                             data = using_model_rural, index=c("cty_st_numeric","years"),
                             force = "two-way", nboots = 100, se = TRUE)
  
  inter_fe_all_Uber <- interFE(estab_pop ~ 
                                   Medicaid + Medicaid_Uber + Uber_active,
                                 data = using_model_urban, index=c("cty_st_numeric","years"),
                                 force = "two-way", nboots = 100, se = TRUE)
  
  inter_fe_urban_Uber <- interFE(estab_pop ~ 
                              Medicaid + Medicaid_Uber + Uber_active,
                            data = using_model_urban, index=c("cty_st_numeric","years"),
                            force = "two-way", nboots = 100, se = TRUE)
  
  inter_fe_rural_Uber <- interFE(estab_pop ~ 
                              Medicaid + Medicaid_Uber + Uber_active,
                            data = using_model_rural, index=c("cty_st_numeric","years"),
                            force = "two-way", nboots = 100, se = TRUE)
  
  inter_fe_all_Uber_urban <- interFE(estab_pop ~ 
                                       Medicaid + Medicaid_Uber + Medicaid_Urban + Medicaid_Urban_Uber +
                                       Uber_active + Urban_Uber,
                                     data = using_model, index=c("cty_st_numeric","years"),
                                     force = "two-way", nboots = 100, se = TRUE)

  
  
  ##################################################################################################
  
  
  gsynth_all <- gsynth(Y = "estab_pop",
                       D = "Medicaid",
                       X = c("estab",
                             "population"),
                       data = using_model,
                       index = c("cty_st_numeric","years"),
                       weight = "population",
                       force = "two-way", 
                       CV = TRUE, 
                       se = TRUE,
                       # EM = TRUE,
                       estimator = "mc",
                       nlambda = 10,
                       inference = "nonparametric", 
                       nboots = 300, 
                       parallel = TRUE, 
                       cores = 4)                      

  gsynth_urban <- gsynth(Y = "estab_pop",
                       D = "Medicaid",
                       X = c("estab",
                             "population"),
                       data = using_model_urban,
                       index = c("cty_st_numeric","years"),
                       weight = "population",
                       force = "two-way", 
                       CV = TRUE, 
                       se = TRUE,
                       # EM = TRUE,
                       estimator = "mc",
                       nlambda = 10,
                       inference = "nonparametric", 
                       nboots = 300, 
                       parallel = TRUE, 
                       cores = 4)   
  
  gsynth_rural <- gsynth(Y = "estab_pop",
                         D = "Medicaid",
                         X = c("estab",
                               "population"),
                         data = using_model_rural,
                         index = c("cty_st_numeric","years"),
                         weight = "population",
                         force = "two-way", 
                         CV = TRUE, 
                         se = TRUE,
                         # EM = TRUE,
                         estimator = "mc",
                         nlambda = 10,
                         inference = "nonparametric", 
                         nboots = 300, 
                         parallel = TRUE, 
                         cores = 4)      
  
  