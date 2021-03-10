# Ben Glasner
# Minimum wage and nonemployers analysis
# Industry analysis of nonemployers
# Clean data for primary results, Establishments and Receipts
# Perform actual regressions in Stata

##################
###  Library   ###
##################
library(plm)
library(lubridate)
library(dplyr)
library(naniar)
library(readxl)
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

setwd(paste0(path_data))


#################################################################
#                         Load the Data                         #
#################################################################
# industry_list <- c("48-49","00")
# industry_list <- c("31-33", "44-45","72","81")

load("Nonemployer_statistics_fillin_1997_2018.RData") # load the NES data, filled in counties with 0 observations in an industry year, and assign censored counties as 2

medicaid <- read_excel("medicaid.xlsx")
exchanges <- read.csv("State_health_insurance_market_type.csv")
load("county_population_2000_2018.RData")

load("Uber_2000_2018.RData")

load("CBP_2000_2018.RData")
CBP$cty <- CBP$fipscty
CBP$st <- CBP$fipstate

load("LAU.RData")

#################################################################
#                              Industry                         #
#################################################################
industry_list <- sort(unique(NES$naics))

using <- NES


#################################################################
#                              Merge                            #
#################################################################


using <- merge(using, medicaid, all.x = TRUE)
using <- merge(using, population, all.x = TRUE)
using <- merge(using, Uber_timing, all.x = TRUE)
using <- merge(using, LAU, all.x = TRUE)
using <- merge(using, exchanges, all.x = TRUE)
using <- merge(using, CBP, all.x = TRUE)

###################################
### Define dependent variables  ###
###################################  
using$estab_pop <- using$estab/using$`Labor Force` # Extensive marginal effect - number of establishments per member of the labor force
using$receipt_estab <- (using$rcptot/using$estab)*1000 # intesive marginal effect - average receipts per establishment, convert to dollars (initially in thousands)


#################################################################
#                              Clean                            #
#################################################################
using$id <- as.factor(paste(using$st, using$cty, using$naics, sep = "-")) # create unique id for each county-state
using$cty_st <- as.factor(paste(using$st, using$cty, sep = "-")) # create unique id for each county-state

using$Marketplace.Type[as.numeric(using$YEAR.id)<2014] <- NA

using$Marketplace.Type_na <- addNA(using$Marketplace.Type)
levels(using$Marketplace.Type_na) <- c(levels(using$Marketplace.Type), "None")

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
ever_medicaid <- subset(medicaid, YEAR.id=="2018")
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
using_panel$Medicaid_lead1 <- lead(using_panel$Medicaid)
using_panel$Medicaid_lead2 <- lead(using_panel$Medicaid, 2)
using <- as.data.frame(using_panel, row.names = NULL) # create a nonpanel df again


#######################################################
###     Microsynthetic for local Min Wage increases ###
#######################################################
using$YEAR.id <- as.factor(using$YEAR.id)
using$years <- as.numeric(as.character(using$YEAR.id))
using$st <- as.factor(using$st)
using$cty_st_numeric <- as.numeric(using$cty_st)
sapply(using, function(x) sum(is.na(x)))

using_model <- using

using_model <- using_model %>% dplyr::select("estab_pop", 
                                             "estab",
                                             "receipt_estab",
                                             "rcptot",
                                             "naics",
                                             "population",
                                             "Medicaid",
                                             "Ever_medicaid",
                                             "concentration_lower",
                                             "HHI_lower",
                                             "Federally_facilitated_Marketplace",
                                             "State_based_Marketplace",
                                             "State_based_Marketplace_Federal_Platform",
                                             "State_Partnership_Marketplace",
                                             "Uber_active",
                                             "dosage_uber",
                                             "Unemployment.Rate",
                                             "Labor.Force",
                                             "cty_st",
                                             "YEAR.id",
                                             "cty_st_numeric",
                                             "years",
                                             "id",
                                             "st",
                                             "Medicaid_lead1",
                                             "Medicaid_lead2",
                                             "Medicaid_lag1",
                                             "Medicaid_lag2",
                                             "Marketplace.Type_na")
using_model_full <- using_model[complete.cases(using_model[,c(1:23)]),]

  county_counts <- as.data.frame(table(using_model_full$id)) # count how many times a county-state appears in the sample
  county_counts <- subset(county_counts, Freq==19) # keep only the counties that are in the full sample for balance
  # county_counts <- subset(county_counts, Freq==14) # keep only the counties that are in the full sample for balance
  colnames(county_counts)[1]<- "id" # relabel the id for the merge
  using_model_full<- merge(using_model_full, county_counts) # merge as a way to drop the unbalanced counties
  rm(county_counts) # drop county_counts from the env.

rm(ever_medicaid,
   medicaid,
   NES,
   population,
   Uber_timing, 
   using_model,
   CBP,
   exchanges,
   LAU,
   using_panel,
   using)


using <- using_model_full

pop_means <- aggregate(x = using$Labor.Force, by = list(using$id), FUN = mean)
colnames(pop_means)[1] <- "id"
colnames(pop_means)[2] <- "lf_avg"
using <- merge(using, pop_means)

using$id_numeric <- as.numeric(using$id)

using <- within(using, quantile <- as.integer(cut(HHI_lower, quantile(HHI_lower, probs=seq(0,1,.01), na.rm = TRUE), include.lowest=TRUE)))
using$quantile <- as.numeric(as.character(using$quantile))

save(using, file = "medicaid_analysis_data_allnonemployers.RData")
write.csv(using, file = "medicaid_analysis_data_allnonemployers.csv") # Save this data as a .csv to load into stata for analysis

