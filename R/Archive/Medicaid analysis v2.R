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
gsynth_list <- list()
EM_list <- list()
MC_list <- list()

gsynth_uber_list <- list()
EM_uber_list <- list()
MC_uber_list <- list()

gsynth_uber_urban_list <- list()
EM_uber_urban_list <- list()
MC_uber_urban_list <- list()

gsynth_uber_rural_list <- list()
EM_uber_rural_list <- list()
MC_uber_rural_list <- list()

inter_fe_list <- list()
lm_list <- list()

load("Nonemployer_statistics_1997_2017.RData") # load the data
medicaid <- read_excel("medicaid.xlsx")
exchanges <- read.csv("State_health_insurance_market_type.csv")
colnames(exchanges)[1] <- "State Name"
load("county_population_2000_2018.RData")
load("GDP_2001_2018.RData")
load("fips_1997_2018.RData")
load("Uber_2000_2017.RData")
LAU <- read.csv("LAU.csv")


LAU <- LAU %>% dplyr::select("CBSA.Code",
                             "YEAR.id",
                             "unemployment_rate",
                             "unemployment",
                             "employment",
                             "labor_force",
                             "chg_labor_force")

colnames(LAU)[1] <- "CBSA Code" 

GDP <- merge(GDP, fips)

using_all <- NES
#################################################################
#                              Industry                         #
#################################################################
industry_list <- sort(unique(using_all$naics))

# i<-1 # for total
# i <- 9 # for 48-49
# i <- 10 # for 4853
# for (i in seq_along(industry_list)) {
setwd(paste0(path_data))

print(paste0(industry_list[1],"start"))

using <- subset(using_all, naics==industry_list[1])
# using <- subset(using_all, naics==industry_list[9]) # transportation and warehousing


#################################################################
#                              Merge                            #
#################################################################


using <- merge(using, medicaid, all.x = TRUE)
using <- merge(using, population, all.x = TRUE)
using <- merge(using, Uber_timing, all.x = TRUE)
using <- merge(using, GDP, all.x = TRUE)
using <- merge(using, LAU, all.x = TRUE)
using <- merge(using, exchanges, all.x = TRUE)

#################################################################
#                              Clean                            #
#################################################################
# using$estab[is.na(using$estab)] <- 0
using$estab_pop <- (using$estab/using$population) # increase the size of the estab_pop term for easier reading of results
using$id <- as.factor(paste(using$st, using$cty, using$naics, sep = "-")) # create unique id for each county-state
using$cty_st <- as.factor(paste(using$st, using$cty, sep = "-")) # create unique id for each county-state

using$Marketplace.Type[as.numeric(using$YEAR.id)<2014] <- NA

using$Federally_facilitated_Marketplace <- 0
using$Federally_facilitated_Marketplace[using$Marketplace.Type == "Federally-facilitated Marketplace"] <- 1

using$State_based_Marketplace <- 0
using$State_based_Marketplace[using$Marketplace.Type == "State-based Marketplace"] <- 1

using$State_based_Marketplace_Federal_Platform <- 0
using$State_based_Marketplace_Federal_Platform[using$Marketplace.Type == "State-based Marketplace-Federal Platform"] <- 1

using$State_Partnership_Marketplace <- 0
using$State_Partnership_Marketplace[using$Marketplace.Type == "State-Partnership Marketplace"] <- 1



#######################################################
###     Ever medicaid expansion identification      ###
#######################################################
ever_medicaid <- subset(medicaid, YEAR.id=="2017")
ever_medicaid$Ever_medicaid <- ever_medicaid$Medicaid
ever_medicaid <- ever_medicaid %>% dplyr::select("st", "Ever_medicaid")

using <- merge(using, ever_medicaid)

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
using$Ever_Uber <- 0 
using$Ever_Uber[using$uber_year>2000] <- 1

##################################
###     Turn it into a panel   ###
##################################

using_panel <- pdata.frame(using, index=c("id","YEAR.id"), drop.index=FALSE, row.names=TRUE) # create a panel df for lags and leads
using_panel$Medicaid_lag1 <- lag(using_panel$Medicaid)
using_panel$Medicaid_lag2 <- lag(using_panel$Medicaid, 2)
using_panel$Medicaid_lag3 <- lag(using_panel$Medicaid, 3)

using <- as.data.frame(using_panel, row.names = NULL) # create a nonpanel df again


#######################################################
###     Microsynthetic for local Min Wage increases ###
#######################################################
using$YEAR.id <- as.factor(using$YEAR.id)
using$years <- as.numeric(as.character(using$YEAR.id))
using$st <- as.factor(using$st)
using$cty_st_numeric <- as.numeric(using$cty_st)

using$urban <- 0
using$urban[using$population>50000] <- 1

using$Medicaid_Uber <- using$Medicaid*using$Uber_active

using$Medicaid_Uber_urban <- 0
using$Medicaid_Uber_urban[using$Medicaid_Uber==1 & using$urban ==1] <- 1

using$Medicaid_Uber_rural <- 0
using$Medicaid_Uber_rural[using$Medicaid_Uber==1 & using$urban ==0] <- 1

using$Uber_urban <- using$Uber_active*using$urban

using$Medicaid_urban <- using$Medicaid*using$urban

using$uber_year[is.na(using$uber_year)] <- 0 

using$GDP <- as.numeric(as.character(using$GDP))
using$GDP_pop <- using$GDP/using$population
# using_model <- subset(using, estab_pop<.15 & population < 5000000)
using_model <- using
using_model <- using_model %>% dplyr::select("estab_pop", 
                                             "estab",
                                             "population",
                                             "Medicaid",
                                             "Medicaid_lag1",
                                             "Medicaid_lag2",
                                             "Medicaid_lag3",
                                             "Medicaid_Uber",
                                             "Medicaid_Uber_urban",
                                             "Medicaid_Uber_rural",
                                             "Medicaid_urban",
                                             "Uber_urban",
                                             "urban",
                                             "Ever_medicaid",
                                             "Federally_facilitated_Marketplace",
                                             "State_based_Marketplace",
                                             "State_based_Marketplace_Federal_Platform",
                                             "State_Partnership_Marketplace",
                                             "Uber_active",
                                             "GDP",
                                             "GDP_pop",
                                             "unemployment_rate",
                                             "labor_force",
                                             "uber_year",
                                             "cty_st",
                                             "YEAR.id",
                                             "cty_st_numeric",
                                             "years",
                                             "id")
using_model_full <- using_model[complete.cases(using_model[c(1:13,19:21,25:29)]),]
using_model_labor <- using_model[complete.cases(using_model),]

# using_model_full <- subset(using_model_full, years<2015)
# restrict the sample to counties that never receive medicaid expansion, and those which receive it in 2014, and have Uber in 2014.
# using_model_a <- subset(using_model_full, uber_year!=2014 & uber_year!=2013 & uber_year!=2012)
# using_model_b <- subset(using_model_full, uber_year==2014 | uber_year == 0)
# using_model_c <- subset(using_model_full, uber_year==2015 | uber_year == 0)
# using_model_d <- subset(using_model_full, uber_year==2016 | uber_year == 0)
# using_model_e <- subset(using_model_full, uber_year==2017 | uber_year == 0)

using_model_list <- list(using_model_full,using_model_labor)

for (K in seq_along(using_model_list)) {
  county_counts <- as.data.frame(table(using_model_list[[K]]$id)) # count how many times a county-state appears in the sample
  county_counts <- subset(county_counts, Freq==17) # keep only the counties that are in the full sample for balance
  # county_counts <- subset(county_counts, Freq==14) # keep only the counties that are in the full sample for balance
  colnames(county_counts)[1]<- "id" # relabel the id for the merge
  using_model_list[[K]]<- merge(using_model_list[[K]], county_counts) # merge as a way to drop the unbalanced counties
  rm(county_counts) # drop county_counts from the env.
  
  # using_model_list[[K]] <- subset(using_model_list[[K]], years>2004)
}

rm(ever_medicaid,
   medicaid,
   NES,
   population,
   Uber_timing,
   using_all, 
   using_model,
   using_model_full,
   using_model_labor)



#######################################################
###                   synthetic Control             ###
#######################################################

setwd(paste0(path_output))


for (j in seq_along(using_model_list)) {
    gsynth_list[[j]] <- gsynth(Y = "estab_pop",
                           D = "Medicaid",
                           X = c("estab",
                                 "population",
                                 "Uber_active",
                                 "GDP",
                                 "GDP_pop"),
                           data = using_model_list[[j]],
                           index = c("cty_st_numeric","years"),
                           weight = "population",
                           force = "two-way",
                           CV = TRUE,
                           se = TRUE,
                           r = c(0,10),
                           inference = "parametric",
                           nboots = 500,
                           parallel = TRUE,
                           cores =20)
    
    EM_list[[j]] <- gsynth(Y = "estab_pop",
                           D = "Medicaid",
                           X = c("estab",
                                 "population",
                                 "GDP",
                                 "GDP_pop",
                                 "Uber_active"),
                           data = using_model_list[[j]],
                           index = c("cty_st_numeric","years"),
                           weight = "population",
                           force = "two-way", 
                           CV = TRUE, 
                           se = TRUE,
                           EM = TRUE,
                           r = c(0,10),
                           inference = "parametric", 
                           nboots = 500, 
                           parallel = TRUE, 
                           cores =20)
                                    
    
    MC_list[[j]] <- gsynth(Y = "estab_pop",
                           D = "Medicaid",
                           X = c("estab",
                                 "population",
                                 "GDP",
                                 "GDP_pop",
                                 "Uber_active"),
                           data = using_model_list[[j]],
                           index = c("cty_st_numeric","years"),
                           weight = "population",
                           force = "two-way", 
                           CV = TRUE, 
                           se = TRUE,
                           estimator = "mc",
                           nlambda = 10,
                           inference = "nonparametric", 
                           nboots = 500, 
                           parallel = TRUE, 
                           cores =20)
                            
}


for (j in 1) {
  gsynth_uber_list[[j]] <- gsynth(Y = "estab_pop",
                             D = "Medicaid_Uber",
                             X = c("Medicaid",
                                   "estab",
                                   "population",
                                   "Uber_active",
                                   "GDP",
                                   "GDP_pop",
                                   "unemployment_rate",
                                   "labor_force"),
                             data = using_model_list[[2]],
                             index = c("cty_st_numeric","years"),
                             weight = "population",
                             force = "two-way",
                             CV = TRUE,
                             se = TRUE,
                             r = c(0,5),
                             inference = "parametric",
                             nboots = 500,
                             parallel = TRUE,
                             cores =4)
  
  EM_uber_list[[j]] <- gsynth(Y = "estab_pop",
                              D = "Medicaid_Uber",
                              X = c("Medicaid",
                                    "estab",
                                    "population",
                                    "Uber_active",
                                    "GDP",
                                    "GDP_pop",
                                    "unemployment_rate",
                                    "labor_force"),
                         data = using_model_list[[2]],
                         index = c("cty_st_numeric","years"),
                         weight = "population",
                         force = "two-way", 
                         CV = TRUE, 
                         se = TRUE,
                         EM = TRUE,
                         r = c(0,5),
                         inference = "parametric", 
                         nboots = 500, 
                         parallel = TRUE, 
                         cores =4)
  
  
  MC_uber_list[[j]] <- gsynth(Y = "estab_pop",
                              D = "Medicaid_Uber",
                              X = c("Medicaid",
                                    "estab",
                                    "population",
                                    "Uber_active",
                                    "GDP",
                                    "GDP_pop",
                                    "unemployment_rate",
                                    "labor_force"),
                         data = using_model_list[[2]],
                         index = c("cty_st_numeric","years"),
                         weight = "population",
                         force = "two-way", 
                         CV = TRUE, 
                         se = TRUE,
                         estimator = "mc",
                         nlambda = 5,
                         inference = "nonparametric", 
                         nboots = 500, 
                         parallel = TRUE, 
                         cores =4)
  
}

for (j in 1) {
  gsynth_uber_urban_list[[j]] <- gsynth(Y = "estab_pop",
                                  D = "Medicaid_Uber_urban",
                                  X = c("Medicaid",
                                        "Medicaid_urban",
                                        "Uber_urban",
                                        "estab",
                                        "population",
                                        "GDP",
                                        "GDP_pop",
                                        "Uber_active",
                                        "unemployment_rate",
                                        "labor_force"),
                                  data = using_model_list[[2]],
                                  index = c("cty_st_numeric","years"),
                                  weight = "population",
                                  force = "two-way",
                                  CV = TRUE,
                                  se = TRUE,
                                  r = c(0,5),
                                  inference = "parametric",
                                  nboots = 500,
                                  parallel = TRUE,
                                  cores =4)
  
  EM_uber_urban_list[[j]] <- gsynth(Y = "estab_pop",
                              D = "Medicaid_Uber_urban",
                              X = c("Medicaid",
                                    "Medicaid_urban",
                                    "Uber_urban",
                                    "estab",
                                    "population",
                                    "GDP",
                                    "GDP_pop",
                                    "Uber_active",
                                    "unemployment_rate",
                                    "labor_force"),
                              data = using_model_list[[2]],
                              index = c("cty_st_numeric","years"),
                              weight = "population",
                              force = "two-way", 
                              CV = TRUE, 
                              se = TRUE,
                              EM = TRUE,
                              r = c(0,5),
                              inference = "parametric", 
                              nboots = 500, 
                              parallel = TRUE, 
                              cores =4)
  
  
  MC_uber_urban_list[[j]] <- gsynth(Y = "estab_pop",
                              D = "Medicaid_Uber_urban",
                              X = c("Medicaid",
                                    "Medicaid_urban",
                                    "Uber_urban",
                                    "estab",
                                    "population",
                                    "GDP",
                                    "GDP_pop",
                                    "Uber_active"),
                              data = using_model_list[[2]],
                              index = c("cty_st_numeric","years"),
                              weight = "population",
                              force = "two-way", 
                              CV = TRUE, 
                              se = TRUE,
                              estimator = "mc",
                              nlambda = 5,
                              inference = "nonparametric", 
                              nboots = 500, 
                              parallel = TRUE, 
                              cores =4)
  
}


for (j in 1) {
  gsynth_uber_rural_list[[j]] <- gsynth(Y = "estab_pop",
                                        D = "Medicaid_Uber_rural",
                                        X = c("Medicaid",
                                              "Medicaid_urban",
                                              "Uber_urban",
                                              "estab",
                                              "population",
                                              "GDP",
                                              "GDP_pop",
                                              "Uber_active",
                                              "unemployment_rate",
                                              "labor_force"),
                                        data = using_model_list[[2]],
                                        index = c("cty_st_numeric","years"),
                                        weight = "population",
                                        force = "two-way",
                                        CV = TRUE,
                                        se = TRUE,
                                        r = c(0,5),
                                        inference = "parametric",
                                        nboots = 500,
                                        parallel = TRUE,
                                        cores =4)
  
  EM_uber_rural_list[[j]] <- gsynth(Y = "estab_pop",
                                    D = "Medicaid_Uber_rural",
                                    X = c("Medicaid",
                                          "Medicaid_urban",
                                          "Uber_urban",
                                          "estab",
                                          "population",
                                          "GDP",
                                          "GDP_pop",
                                          "Uber_active",
                                          "unemployment_rate",
                                          "labor_force"),
                                    data = using_model_list[[2]],
                                    index = c("cty_st_numeric","years"),
                                    weight = "population",
                                    force = "two-way", 
                                    CV = TRUE, 
                                    se = TRUE,
                                    EM = TRUE,
                                    r = c(0,5),
                                    inference = "parametric", 
                                    nboots = 500, 
                                    parallel = TRUE, 
                                    cores =4)
  
  
  MC_uber_rural_list[[j]] <- gsynth(Y = "estab_pop",
                                    D = "Medicaid_Uber_rural",
                                    X = c("Medicaid",
                                          "Medicaid_urban",
                                          "Uber_urban",
                                          "estab",
                                          "population",
                                          "GDP",
                                          "GDP_pop",
                                          "Uber_active",
                                          "unemployment_rate",
                                          "labor_force"),
                                    data = using_model_list[[2]],
                                    index = c("cty_st_numeric","years"),
                                    weight = "population",
                                    force = "two-way", 
                                    CV = TRUE, 
                                    se = TRUE,
                                    estimator = "mc",
                                    nlambda = 5,
                                    inference = "nonparametric", 
                                    nboots = 500, 
                                    parallel = TRUE, 
                                    cores =4)
  
}


#######################################################
###                   Interactive FX                ###
#######################################################
using_model_list[[1]]$Medicaid_lag1_Uber <- using_model_list[[1]]$Medicaid_lag1*using_model_list[[1]]$Uber_active
using_model_list[[1]]$Medicaid_lag2_Uber <- using_model_list[[1]]$Medicaid_lag2*using_model_list[[1]]$Uber_active
using_model_list[[1]]$Medicaid_lag3_Uber <- using_model_list[[1]]$Medicaid_lag3*using_model_list[[1]]$Uber_active


inter_fe_list[[1]] <- interFE(estab_pop ~  Medicaid + Medicaid_lag1 + Medicaid_lag2 + Medicaid_lag3 + Uber_active + GDP + GDP_pop,
                              data = using_model_list[[1]], index=c("cty_st_numeric","years"),
                              force = "two-way", nboots = 500, se = TRUE, r = 2)


inter_fe_list[[2]] <- interFE(estab_pop ~
                                Medicaid + Medicaid_lag1 + Medicaid_lag2 + Medicaid_lag3  +
                                Medicaid_Uber  + Medicaid_lag1_Uber + Medicaid_lag2_Uber + Medicaid_lag3_Uber  +
                                Uber_active + GDP + GDP_pop,
                              data = using_model_list[[1]], index=c("cty_st_numeric","years"),
                              force = "two-way", nboots = 500, se = TRUE, r =  2)

#######################################################
###                   Two Way                       ###
#######################################################

lm_list[[1]] <- lm(estab_pop ~  Medicaid + Medicaid_lag1 + Medicaid_lag2 + Medicaid_lag3 + 
                     Uber_active + GDP + GDP_pop + as.factor(years) + as.factor(cty_st_numeric), data = using_model_list[[1]])
lm_list_1_tidy <- tidy(lm_list[[1]])
lm_list[[2]] <- lm(estab_pop ~  Medicaid + Medicaid_lag1 + Medicaid_lag2 + Medicaid_lag3  + 
                     Medicaid*Uber_active  + Medicaid_lag1*Uber_active + Medicaid_lag2*Uber_active + Medicaid_lag3*Uber_active  +
                     Uber_active + GDP + GDP_pop + as.factor(years) + as.factor(cty_st_numeric), data = using_model_list[[1]])
lm_list_2_tidy <- tidy(lm_list[[2]])

#######################################################
###                   Results                       ###
#######################################################

Sample_list <- c("Full Sample", "Labor Sample")

summary(lm_list[[1]])
summary(lm_list[[2]])

inter_fe_list[[1]]
inter_fe_list[[2]]

impacted_pop <- sum(using_model_list[[1]]$population[using_model_list[[1]]$years==2014 & using_model_list[[1]]$Medicaid==1])

# MEdicaid enrollment increased by 14,098,890 people by December 2017 in expansion states  Monthly Medicaid & CHIP Application, Eligibility Determination, and Enrollment Reports & Data Preliminary December 2017 Report

# $22,108 is 138% of FPL in 2016
# $30,510 is the median earnings for the unincorporated sel-employed (Primary job) "Measuring Entrepreneurship in the American Community Survey: A Demographic and Occupational Profile of Self-Employed Workers"

#  In 2014, 87 percent of all filers earned income solely from wages, while 7 percent earned income solely from self-employment and 6 percent earned a mix from both "U.S. Treasury Department Office of Tax Analysis (Working Paper 114)"

# 14098890*.07 # 7% of people are self-employed as the primary source of income
# 14098890*.07 + 14098890*.06 # 7% of people are self-employed as primary and supplemental 

# estimated effect from 2014-2017
lm_1_imp <- impacted_pop*lm_list[[1]]$coefficients[2] + impacted_pop*lm_list[[1]]$coefficients[3] + impacted_pop*lm_list[[1]]$coefficients[4] + impacted_pop*lm_list[[1]]$coefficients[5]

lm_2_imp <- impacted_pop*lm_list[[2]]$coefficients[2] + impacted_pop*lm_list[[2]]$coefficients[3] + impacted_pop*lm_list[[2]]$coefficients[4] + impacted_pop*lm_list[[2]]$coefficients[5]  
lm_2_imp_uber <- impacted_pop*lm_list[[2]]$coefficients[2] + impacted_pop*lm_list[[2]]$coefficients[3] + impacted_pop*lm_list[[2]]$coefficients[4] + impacted_pop*lm_list[[2]]$coefficients[5]  + impacted_pop*lm_list[[2]]$coefficients[3087] + impacted_pop*lm_list[[2]]$coefficients[3088]  + impacted_pop*lm_list[[2]]$coefficients[3089] + impacted_pop*lm_list[[2]]$coefficients[3090]

int_1_imp <- impacted_pop*inter_fe_list[[1]]$est.table[1] + impacted_pop*inter_fe_list[[1]]$est.table[2] +impacted_pop*inter_fe_list[[1]]$est.table[3] +impacted_pop*inter_fe_list[[1]]$est.table[4]

int_2_imp <- impacted_pop*inter_fe_list[[2]]$est.table[1] + impacted_pop*inter_fe_list[[2]]$est.table[2] +impacted_pop*inter_fe_list[[2]]$est.table[3] +impacted_pop*inter_fe_list[[2]]$est.table[4] 
int_2_imp_uber <- impacted_pop*inter_fe_list[[2]]$est.table[1] + impacted_pop*inter_fe_list[[2]]$est.table[2] +impacted_pop*inter_fe_list[[2]]$est.table[3] +impacted_pop*inter_fe_list[[2]]$est.table[4] + impacted_pop*inter_fe_list[[2]]$est.table[5] + impacted_pop*inter_fe_list[[2]]$est.table[6]

gsynth_1_imp <- impacted_pop*gsynth_list[[1]]$att[17]
EM_1_imp <- impacted_pop*EM_list[[1]]$att[17]
MC_1_imp <- impacted_pop*MC_list[[1]]$att[17]


lm_1_imp/(14098890*.07 + 14098890*.06)*100 # two-way fixed effect estimate reduction in nonemployers as share of full and partial self-employed
lm_2_imp/(14098890*.07 + 14098890*.06)*100
lm_2_imp_uber/(14098890*.07 + 14098890*.06)*100

int_1_imp/(14098890*.07 + 14098890*.06)*100

int_2_imp/(14098890*.07 + 14098890*.06)*100
int_2_imp_uber/(14098890*.07 + 14098890*.06)*100

EM_1_imp/(14098890*.07 + 14098890*.06)*100
MC_1_imp/(14098890*.07 + 14098890*.06)*100

#######################################
###         Create Plots           ####
#######################################
setwd(paste0(path_output))

cty_st_key <- using %>% dplyr::select("id",
                                      "cty_st",
                                      "cty_st_numeric",
                                      "CBSA.Code",
                                      "CBSA.Title")
cty_st_key <- unique(cty_st_key)                                                     
                                                      
gsynth_results <- list()
EM_results <- list()
MC_results <- list()

gsynth_results[[1]] <- as.data.frame(gsynth_list[[1]]$est.att)
gsynth_results[[1]]$Period <- seq.int(nrow(gsynth_results[[1]])) - 13

EM_results[[1]] <- as.data.frame(EM_list[[1]]$est.att)
# EM_results[[2]] <- as.data.frame(EM_list[[2]]$est.att)

EM_results[[1]]$Period <- seq.int(nrow(EM_results[[1]])) - 13
# EM_results[[2]]$Period <- seq.int(nrow(EM_results[[2]])) - 13

MC_results[[1]] <- as.data.frame(MC_list[[1]]$est.att)
# MC_results[[2]] <- as.data.frame(MC_list[[2]]$est.att)

MC_results[[1]]$Period <- seq.int(nrow(MC_results[[1]])) - 13
# MC_results[[2]]$Period <- seq.int(nrow(MC_results[[2]])) - 13

gsynth_results[[1]]$Method <- "Gsynth"
EM_results[[1]]$Method <- "EM Algorithm"
MC_results[[1]]$Method <- "Matrix Completion"

pdf(paste0("Synth_EM_v_MC_",industry_list[i],".pdf"), width = 8, height = 6)
p <- ggplot(data = EM_results[[1]], aes(x = Period, y = ATT, color = Method, linetype = Method, shape = Method)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=CI.lower, ymax=CI.upper), alpha=0.2) +
  
  geom_point(data = MC_results[[1]], size = 3) +
  geom_line(data = MC_results[[1]], size = 1) +
  geom_ribbon(data = MC_results[[1]], aes(ymin=CI.lower, ymax=CI.upper), alpha=0.2) +
  
  # geom_point(data = gsynth_results[[1]], size = 3) +
  # geom_line(data = gsynth_results[[1]], size = 1) +
  # geom_ribbon(data = gsynth_results[[1]], aes(ymin=CI.lower, ymax=CI.upper), alpha=0.2) +
  
  geom_hline(yintercept = 0, color ="black") +
  geom_vline(xintercept = 0.5, color = "black", linetype = "longdash") + 
  xlab("Period") + ylab("ATT") +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 25, angle = 90),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=NA),
        legend.text = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10), limits = c(-5,4)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

plot(p)
dev.off()

pdf(paste0("Synth_EM_v_MC_estabs_",industry_list[i],".pdf"), width = 8, height = 6)
p <- ggplot(data = EM_results[[1]], aes(x = Period, y = ATT*impacted_pop, color = Method, linetype = Method, shape = Method)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=CI.lower*impacted_pop, ymax=CI.upper*impacted_pop), alpha=0.2) +
  geom_point(data = MC_results[[1]], size = 3) +
  geom_line(data = MC_results[[1]], size = 1) +
  geom_ribbon(data = MC_results[[1]], aes(ymin=CI.lower*impacted_pop, ymax=CI.upper*impacted_pop), alpha=0.2) +
  geom_hline(yintercept = 0, color ="black") +
  geom_vline(xintercept = 0.5, color = "black", linetype = "longdash") + 
  xlab("Period") + ylab("ATT (Establishments)") +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 25, angle = 90),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=NA),
        legend.text = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7), labels = comma, limits = c(-1000000,300000))
plot(p)
dev.off()
 
for(i in seq_along(using_model_list)){
  p1 <- plot(gsynth_list[[i]],
       type = "gap",
       xlab = "Period",
       main = paste0("ATE, Gsynth ",Sample_list[i]),
       theme.bw = TRUE)
  p2 <- plot(EM_list[[i]],
       type = "gap",
       xlab = "Period",
       main = paste0("ATE, EM ",Sample_list[i]),
       theme.bw = TRUE)
  p3 <- plot(MC_list[[i]],
       type = "gap",
       xlab = "Period",
       main = paste0("ATE, MC ",Sample_list[i]),
       theme.bw = TRUE)
  
  p4 <- plot(gsynth_list[[i]],
       type = "counterfactual",
       xlab = "Period",
       main = paste0("ATE, Gsynth ",Sample_list[i]),
       theme.bw = TRUE)
  p5 <- plot(EM_list[[i]],
       type = "counterfactual",
       xlab = "Period",
       main = paste0("ATE, EM ",Sample_list[i]),
       theme.bw = TRUE)
  p6 <- plot(MC_list[[i]],
       type = "counterfactual",
       xlab = "Period",
       main = paste0("ATE, MC ",Sample_list[i]),
       theme.bw = TRUE)
  
  pdf(paste0("gsynth_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p1)
  dev.off()  
  pdf(paste0("EM_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p2)
  dev.off()  
  pdf(paste0("MC_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p3)
  dev.off()
  pdf(paste0("gsynth_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p4)
  dev.off()  
  pdf(paste0("EM_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p5)
  dev.off()  
  pdf(paste0("MC_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p6)
  dev.off()
}

for(i in seq_along(using_model_list)){
  p1 <- plot(gsynth_uber_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber, Gsynth ",Sample_list[i]),
             theme.bw = TRUE)
  p2 <- plot(EM_uber_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber, EM ",Sample_list[i]),
             theme.bw = TRUE)
  p3 <- plot(MC_uber_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber, MC ",Sample_list[i]),
             theme.bw = TRUE)
  
  p4 <- plot(gsynth_uber_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber, Gsynth ",Sample_list[i]),
             theme.bw = TRUE)
  p5 <- plot(EM_uber_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber, EM ",Sample_list[i]),
             theme.bw = TRUE)
  p6 <- plot(MC_uber_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber, MC ",Sample_list[i]),
             theme.bw = TRUE)
  
  pdf(paste0("gsynth_uber_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p1)
  dev.off()  
  pdf(paste0("EM_uber_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p2)
  dev.off()  
  pdf(paste0("MC_uber_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p3)
  dev.off()
  pdf(paste0("gsynth_uber_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p4)
  dev.off()  
  pdf(paste0("EM_uber_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p5)
  dev.off()  
  pdf(paste0("MC_uber_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p6)
  dev.off()
}

for(i in seq_along(using_model_list)){
  p1 <- plot(gsynth_uber_urban_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber & Urban, Gsynth ",Sample_list[i]),
             theme.bw = TRUE)
  p2 <- plot(EM_uber_urban_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber & Urban, EM ",Sample_list[i]),
             theme.bw = TRUE)
  p3 <- plot(MC_uber_urban_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber & Urban, MC ",Sample_list[i]),
             theme.bw = TRUE)
  
  p4 <- plot(gsynth_uber_urban_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber & Urban, Gsynth ",Sample_list[i]),
             theme.bw = TRUE)
  p5 <- plot(EM_uber_urban_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber & Urban, EM ",Sample_list[i]),
             theme.bw = TRUE)
  p6 <- plot(MC_uber_urban_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber & Urban, MC ",Sample_list[i]),
             theme.bw = TRUE)
  
  pdf(paste0("gsynth_uber_urban_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p1)
  dev.off()  
  pdf(paste0("EM_uber_urban_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p2)
  dev.off()  
  pdf(paste0("MC_uber_urban_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p3)
  dev.off()
  pdf(paste0("gsynth_uber_urban_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p4)
  dev.off()  
  pdf(paste0("EM_uber_urban_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p5)
  dev.off()  
  pdf(paste0("MC_uber_urban_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p6)
  dev.off()
}

for(i in seq_along(using_model_list)){
  p1 <- plot(gsynth_uber_rural_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber & Rural, Gsynth ",Sample_list[i]),
             theme.bw = TRUE)
  p2 <- plot(EM_uber_rural_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber & Rural, EM ",Sample_list[i]),
             theme.bw = TRUE)
  p3 <- plot(MC_uber_rural_list[[i]],
             type = "gap",
             xlab = "Period",
             main = paste0("ATE, Uber & Rural, MC ",Sample_list[i]),
             theme.bw = TRUE)
  
  p4 <- plot(gsynth_uber_rural_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber & Rural, Gsynth ",Sample_list[i]),
             theme.bw = TRUE)
  p5 <- plot(EM_uber_rural_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber & Rural, EM ",Sample_list[i]),
             theme.bw = TRUE)
  p6 <- plot(MC_uber_rural_list[[i]],
             type = "counterfactual",
             xlab = "Period",
             main = paste0("ATE, Uber & Rural, MC ",Sample_list[i]),
             theme.bw = TRUE)
  
  pdf(paste0("gsynth_uber_rural_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p1)
  dev.off()  
  pdf(paste0("EM_uber_rural_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p2)
  dev.off()  
  pdf(paste0("MC_uber_rural_att_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p3)
  dev.off()
  pdf(paste0("gsynth_uber_rural_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p4)
  dev.off()  
  pdf(paste0("EM_uber_rural_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p5)
  dev.off()  
  pdf(paste0("MC_uber_rural_count_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(p6)
  dev.off()
}

plot(EM_list[[1]],
     type = "counterfactual",
     xlab = "Period",
     main = paste0("ATE, EM ",Sample_list[1]),
     theme.bw = TRUE)

plot(EM_list[[2]],
     type = "counterfactual",
     xlab = "Period",
     main = paste0("ATE, EM ",Sample_list[2]),
     theme.bw = TRUE)

plot(EM_uber_list[[1]],
     type = "counterfactual",
     xlab = "Period",
     main = paste0("ATE, EM ",Sample_list[1]),
     theme.bw = TRUE)

plot(EM_uber_urban_list[[1]],
     type = "counterfactual",
     xlab = "Period",
     main = paste0("ATE, EM ",Sample_list[2]),
     theme.bw = TRUE)

plot(EM_uber_rural_list[[1]],
     type = "counterfactual",
     xlab = "Period",
     main = paste0("ATE, EM ",Sample_list[2]),
     theme.bw = TRUE)

plot(MC_list[[1]], type = "counterfactual", raw = "band", id = 3043)
plot(MC_uber_list[[1]], type = "counterfactual", raw = "band", id = 3043)
plot(MC_uber_urban_list[[1]], type = "counterfactual", raw = "band", id = 3043)

#####    Effect Estimates     ######

effect_estimates_EM <-as.data.frame(EM_list[[1]]$eff)
effect_estimates_MC <-as.data.frame(MC_list[[1]]$eff)

effect_estimates_EM_long <- gather(effect_estimates_EM,cty_st_numeric, effect_estimate_EM, "68":"3141", factor_key=TRUE)
effect_estimates_EM_long$years <- rep(seq(2001,2017,1), times = 1480)

effect_estimates_MC_long <- gather(effect_estimates_MC,cty_st_numeric, effect_estimate_MC, "68":"3141", factor_key=TRUE)
effect_estimates_MC_long$years <- rep(seq(2001,2017,1), times = 1480)

effect_estimates <- merge(effect_estimates_EM_long, effect_estimates_MC_long)

yearly_pop <- using_model_list[[1]] %>% dplyr::select("population",
                                                      "Medicaid", 
                                                      "Uber_active",
                                                      "cty_st_numeric",
                                                      "years",
                                                      "id")
effect_estimates <- merge(effect_estimates, yearly_pop)
effect_estimates$change_EM <- effect_estimates$effect_estimate_EM*effect_estimates$population 
effect_estimates$change_MC <- effect_estimates$effect_estimate_MC*effect_estimates$population 

aggregate(x = effect_estimates$change_EM, by = list(effect_estimates$years), FUN = "sum")
aggregate(x = effect_estimates$change_MC, by = list(effect_estimates$years), FUN = "sum")

ggplot(effect_estimates, aes(x = years, 
                             y = effect_estimate_EM, 
                             weight = population, 
                             color = as.factor(Medicaid),
                             shape = as.factor(Uber_active),
                             size = population)) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw()

ggplot(effect_estimates, aes(x = years, 
                             y = effect_estimate_MC, 
                             weight = population, 
                             color = as.factor(Medicaid),
                             shape = as.factor(Uber_active),
                             size = population)) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw()

ggplot(effect_estimates, aes(x = years, 
                             y = change_EM, 
                             weight = population, 
                             color = as.factor(Medicaid),
                             shape = as.factor(Uber_active),
                             size = population)) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw()

ggplot(effect_estimates, aes(x = years, 
                             y = change_MC, 
                             weight = population, 
                             color = as.factor(Medicaid),
                             shape = as.factor(Uber_active),
                             size = population)) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw()



#####    Predictions with INteraction Effects     ######

Medicaid_Uber <- subset(using_model_list[[1]], id=="53-33-00")
Medicaid_Uber$Medicaid[Medicaid_Uber$years<2014] <- 0
Medicaid_Uber$Medicaid[Medicaid_Uber$years>2013] <- 1
Medicaid_Uber$Uber_active[Medicaid_Uber$years<2014] <- 0
Medicaid_Uber$Uber_active[Medicaid_Uber$years>2013] <- 1

Medicaid_Uber$Medicaid_lag1[Medicaid_Uber$years<2015] <-0 
Medicaid_Uber$Medicaid_lag1[Medicaid_Uber$years>2014] <-1 
  
Medicaid_Uber$Medicaid_lag2[Medicaid_Uber$years<2016] <-0 
Medicaid_Uber$Medicaid_lag2[Medicaid_Uber$years>2015] <-1 

Medicaid_Uber$Medicaid_lag3[Medicaid_Uber$years<2017] <-0 
Medicaid_Uber$Medicaid_lag3[Medicaid_Uber$years>2016] <-1 

Medicaid_Uber$Medicaid_Uber <- Medicaid_Uber$Medicaid*Medicaid_Uber$Uber_active
Medicaid_Uber$Medicaid_lag1_Uber <- Medicaid_Uber$Medicaid_lag1*Medicaid_Uber$Uber_active
Medicaid_Uber$Medicaid_lag2_Uber <- Medicaid_Uber$Medicaid_lag2*Medicaid_Uber$Uber_active
Medicaid_Uber$Medicaid_lag3_Uber <- Medicaid_Uber$Medicaid_lag3*Medicaid_Uber$Uber_active

Medicaid_Uber$Group <- "Expansion and Uber"

########
Medicaid_noUber <- subset(using_model_list[[1]], id=="53-33-00")
Medicaid_noUber$Medicaid[Medicaid_noUber$years<2014] <- 0
Medicaid_noUber$Medicaid[Medicaid_noUber$years>2013] <- 1
Medicaid_noUber$Uber_active <- 0

Medicaid_noUber$Medicaid_lag1[Medicaid_noUber$years<2015] <-0 
Medicaid_noUber$Medicaid_lag1[Medicaid_noUber$years>2014] <-1 

Medicaid_noUber$Medicaid_lag2[Medicaid_noUber$years<2016] <-0 
Medicaid_noUber$Medicaid_lag2[Medicaid_noUber$years>2015] <-1 

Medicaid_noUber$Medicaid_lag3[Medicaid_noUber$years<2017] <-0 
Medicaid_noUber$Medicaid_lag3[Medicaid_noUber$years>2016] <-1 

Medicaid_noUber$Medicaid_Uber <- Medicaid_noUber$Medicaid*Medicaid_noUber$Uber_active
Medicaid_noUber$Medicaid_lag1_Uber <- Medicaid_noUber$Medicaid_lag1*Medicaid_noUber$Uber_active
Medicaid_noUber$Medicaid_lag2_Uber <- Medicaid_noUber$Medicaid_lag2*Medicaid_noUber$Uber_active
Medicaid_noUber$Medicaid_lag3_Uber <- Medicaid_noUber$Medicaid_lag3*Medicaid_noUber$Uber_active

Medicaid_noUber$Group <- "Expansion Without Uber"

########
noMedicaid_Uber <- subset(using_model_list[[1]], id=="53-33-00")
noMedicaid_Uber$Medicaid <- 0
noMedicaid_Uber$Uber_active[noMedicaid_Uber$years<2014] <- 0
noMedicaid_Uber$Uber_active[noMedicaid_Uber$years>2013] <- 1

noMedicaid_Uber$Medicaid_lag1 <-0  

noMedicaid_Uber$Medicaid_lag2 <-0 

noMedicaid_Uber$Medicaid_lag3 <-0 

noMedicaid_Uber$Medicaid_Uber <- noMedicaid_Uber$Medicaid*noMedicaid_Uber$Uber_active
noMedicaid_Uber$Medicaid_lag1_Uber <- noMedicaid_Uber$Medicaid_lag1*noMedicaid_Uber$Uber_active
noMedicaid_Uber$Medicaid_lag2_Uber <- noMedicaid_Uber$Medicaid_lag2*noMedicaid_Uber$Uber_active
noMedicaid_Uber$Medicaid_lag3_Uber <- noMedicaid_Uber$Medicaid_lag3*noMedicaid_Uber$Uber_active

noMedicaid_Uber$Group <- "No Expansion and Uber"

########
noMedicaid_noUber <- subset(using_model_list[[1]], id=="53-33-00")
noMedicaid_noUber$Medicaid <- 0
noMedicaid_noUber$Uber_active <- 0

noMedicaid_noUber$Medicaid_lag1 <-0  

noMedicaid_noUber$Medicaid_lag2 <-0 

noMedicaid_noUber$Medicaid_lag3 <-0 

noMedicaid_noUber$Medicaid_Uber <- noMedicaid_noUber$Medicaid*noMedicaid_noUber$Uber_active
noMedicaid_noUber$Medicaid_lag1_Uber <- noMedicaid_noUber$Medicaid_lag1*noMedicaid_noUber$Uber_active
noMedicaid_noUber$Medicaid_lag2_Uber <- noMedicaid_noUber$Medicaid_lag2*noMedicaid_noUber$Uber_active
noMedicaid_noUber$Medicaid_lag3_Uber <- noMedicaid_noUber$Medicaid_lag3*noMedicaid_noUber$Uber_active

noMedicaid_noUber$Group <- "No Expansion and No Uber"

######################


lm_interaction_pred <- rbind(Medicaid_Uber,Medicaid_noUber,noMedicaid_Uber,noMedicaid_noUber)
predictions <- predict(lm_list[[2]], lm_interaction_pred, interval="predict")
lm_interaction_pred <- cbind(lm_interaction_pred,predictions)

lm_interaction_pred$fit_pop <- lm_interaction_pred$fit*lm_interaction_pred$population

######################
ex_county <- subset(using_model_list[[1]], id=="53-33-00")
ex_county$Group <- "Observed"

ggplot(data = lm_interaction_pred, aes(x = years, y = fit_pop,
                                       Group = Group,
                                       color = Group, linetype = Group, shape = Group)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_point(data = ex_county, aes(x = years, y = estab)) +
  geom_line(data = ex_county, aes(x = years, y = estab)) +
  ylab("Pred. # of Estab. in Example County") +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 25, angle = 90),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=NA),
        legend.text = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.title = element_blank()) +
  scale_y_continuous(labels = comma) +
  guides(col = guide_legend(nrow=3,override.aes = list(size=2)))


lm_interaction_pred$prediction_inter <- 0.06 + lm_interaction_pred$Medicaid*-0.000923420122 + lm_interaction_pred$Medicaid_lag1*-0.000402574573 + lm_interaction_pred$Medicaid_lag2*-0.000150776352  + lm_interaction_pred$Medicaid_lag3*-0.000671708202 + lm_interaction_pred$Medicaid_Uber* 0.000128876515 + lm_interaction_pred$Medicaid_lag1_Uber*0.000309432804 + lm_interaction_pred$Medicaid_lag2_Uber* -0.000052458374 + lm_interaction_pred$Medicaid_lag3_Uber*0.000040691527 + lm_interaction_pred$Uber_active*0.000310519669 
  
ggplot(data = lm_interaction_pred, aes(x = years, y = prediction_inter,
                                       Group = Group,
                                       color = Group, linetype = Group, shape = Group)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ylab("Pred. # of Estab. in King County") +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 25, angle = 90),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=NA),
        legend.text = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.title = element_blank()) +
  scale_y_continuous(labels = comma) +
  guides(col = guide_legend(nrow=2,override.aes = list(size=2)))





ggplot(data = lm_interaction_pred, aes(x = years, 
                                       Group = Group, 
                                       color = Group, 
                                       linetype = Group,
                                       shape = Group)) +
  geom_point(aes(y = prediction_inter),size = 3) +
  geom_line(aes(y = prediction_inter),size = 1) +
  geom_ribbon()


