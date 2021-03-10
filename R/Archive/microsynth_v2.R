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
library(scales)
library(microsynth)
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
synth_list <- list()
synth_Uber_list <- list()
synth_Uber_effect_list <- list()
estab_tidy_list <- list()

load("Nonemployer_statistics_1997_2017.RData") # load the data
medicaid <- read_excel("medicaid.xlsx")
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
# taxi <- subset(using_all, naics==industry_list[9])

#################################################################
#                              Merge                            #
#################################################################


using <- merge(using, medicaid, all.x = TRUE)
using <- merge(using, population, all.x = TRUE)
using <- merge(using, Uber_timing, all.x = TRUE)
using <- merge(using, GDP, all.x = TRUE)
using <- merge(using, LAU, all.x = TRUE)


#################################################################
#                              Clean                            #
#################################################################

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
using$Ever_Uber <- 0 
using$Ever_Uber[using$uber_year>2000] <- 1
##################################
###     Turn it into a panel   ###
##################################

using_panel <- pdata.frame(using, index=c("id","YEAR.id"), drop.index=FALSE, row.names=TRUE) # create a panel df for lags and leads
using_panel$Uber_active_lead1 <- lead(using_panel$Uber_active,1)
using_panel$Uber_active_lead2 <- lead(using_panel$Uber_active,2)
using_panel$Uber_active_lead3 <- lead(using_panel$Uber_active,3)
using <- as.data.frame(using_panel, row.names = NULL) # create a nonpanel df again
using$Uber_active_lead1[is.na(using$Uber_active_lead1) & using$Ever_Uber==1] <- 1
using$Uber_active_lead1[is.na(using$Uber_active_lead1) & using$Ever_Uber==0] <- 0

using$Uber_active_lead2[is.na(using$Uber_active_lead2) & using$Ever_Uber==1] <- 1
using$Uber_active_lead2[is.na(using$Uber_active_lead2) & using$Ever_Uber==0] <- 0

using$Uber_active_lead3[is.na(using$Uber_active_lead3) & using$Ever_Uber==1] <- 1
using$Uber_active_lead3[is.na(using$Uber_active_lead3) & using$Ever_Uber==0] <- 0
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

using$Medicaid_Uber <- using$Medicaid*using$Uber_active

using$Urban <- 0
using$Urban[using$population>50000] <- 1
using$Medicaid_Urban <- using$Medicaid*using$Urban

using$Medicaid_Urban_Uber <- using$Medicaid*using$Uber_active*using$Urban
using$Medicaid_Rural_Uber <- 0
using$Medicaid_Rural_Uber[using$Medicaid==1 & using$Urban==0 & using$Uber_active==1] <- 1

using$Urban_Uber <- using$Uber_active*using$Urban
using$uber_year[is.na(using$uber_year)] <- 0 

using$GDP <- as.numeric(as.character(using$GDP))
using$log_GDP <- log(using$GDP)

using$log_estab <- log(using$estab)
using$log_labor_force <- log(using$labor_force)

using$log_population <- log(using$population)

# using_model <- subset(using, estab_pop<.15 & population<5000000)
# using_model <- subset(using, estab_pop<.15)
using_model <- subset(using, population<5000000)

using_model <- using_model %>% dplyr::select("estab_pop", 
                                       "estab",
                                       "log_estab",
                                       "population",
                                       "Medicaid", 
                                       "Medicaid_Uber",
                                       "Ever_medicaid",
                                       "uber_year",
                                       "Uber_active",
                                       "Urban",
                                       "log_GDP",
                                       "GDP",
                                       "unemployment_rate",
                                       "log_labor_force",
                                       "cty_st",
                                       "YEAR.id",
                                       "CBSA.Title",
                                       "cty_st_numeric",
                                       "years",
                                       "id",
                                       "st")
using_model <- using_model[complete.cases(using_model[c(1,2,3,4,5,12)]),]
using_model_a <- subset(using_model, Urban ==0)
using_model_b <- subset(using_model, Urban ==1)

# restrict the sample to just counties that receive Medicaid Expansion, and compare the ones that got medicaid expansion 
# and Uber in 2014 to the ones that only got the medicaid expansion 
using_model_c <- subset(using_model, Ever_medicaid ==1)
using_model_c <- subset(using_model_c, uber_year==2014 | uber_year == 0)
using_model_d <- subset(using_model_c, Urban ==0)
using_model_e <- subset(using_model_c, Urban ==1)

no_tiny_county <- subset(using_model, population >25000)

using_model_list <- list(using_model,
                         using_model_a,
                         using_model_b,
                         using_model_c,
                         using_model_d,
                         using_model_e,
                         no_tiny_county)


for (K in seq_along(using_model_list)) {
  county_counts <- as.data.frame(table(using_model_list[[K]]$id)) # count how many times a county-state appears in the sample
  county_counts <- subset(county_counts, Freq==17) # keep only the counties that are in the full sample for balance
  # county_counts <- subset(county_counts, Freq==12) # keep only the counties that are in the full sample for balance
  colnames(county_counts)[1]<- "id" # relabel the id for the merge
  using_model_list[[K]]<- merge(using_model_list[[K]], county_counts) # merge as a way to drop the unbalanced counties
  rm(county_counts) # drop county_counts from the env.
}

rm(ever_medicaid,
   medicaid,
   NES,
   population,
   Uber_timing,
   using_all, 
   using_panel,
   using_model,
   using_model_a, 
   using_model_b,
   using_model_c,
   using_model_d,
   using_model_e,
   no_tiny_county)


for (j in c(1)) {
  synth_list[[j]] <- gsynth1 <- gsynth(Y = "estab_pop",
                                       D = "Medicaid",
                                       X = c("estab",
                                             "population",
                                             "Uber_active",
                                             "Urban",
                                             "GDP"),
                                       data = using_model_list[[j]],
                                       index = c("cty_st_numeric","years"),
                                       weight = "population",
                                       force = "two-way", 
                                       CV = TRUE, 
                                       se = TRUE,
                                       # EM = TRUE,
                                       estimator = "mc",
                                       # lambda = 0.07499,
                                       # k =5,
                                       nlambda = 10,
                                       inference = "nonparametric", 
                                       nboots = 1000, 
                                       parallel = TRUE, 
                                       cores =20)
  # print(gsynth1)
}
for (j in 2:3) {
  synth_list[[j]] <- gsynth1 <- gsynth(Y = "estab_pop",
                                       D = "Medicaid",
                                       X = c("estab",
                                             "population",
                                             "Uber_active",
                                             "GDP"),
                                       data = using_model_list[[j]],
                                       index = c("cty_st_numeric","years"),
                                       weight = "population",
                                       force = "two-way", 
                                       CV = TRUE, 
                                       se = TRUE,
                                       # EM = TRUE,
                                       estimator = "mc",
                                       # lambda = 0.07499,
                                       # k =5,
                                       nlambda = 10,
                                       inference = "nonparametric", 
                                       nboots = 1000, 
                                       parallel = TRUE, 
                                       cores =20)
  # print(gsynth1)
}
for (j in c(4)) {
  synth_list[[j]] <- gsynth1 <- gsynth(Y = "estab_pop",
                                       D = "Medicaid_Uber",
                                       X = c("estab",
                                             "population",
                                             "GDP"),
                                       data = using_model_list[[j]],
                                       index = c("cty_st_numeric","years"),
                                       weight = "population",
                                       force = "two-way", 
                                       CV = TRUE, 
                                       se = TRUE,
                                       # EM = TRUE,
                                       estimator = "mc",
                                       # lambda = 0.07499,
                                       # k =5,
                                       nlambda = 10,
                                       inference = "nonparametric", 
                                       nboots = 1000, 
                                       parallel = TRUE, 
                                       cores =20)
}
for (j in c(5,6)) {
  synth_list[[j]] <- gsynth1 <- gsynth(Y = "estab_pop",
                                       D = "Medicaid_Uber",
                                       X = c( "estab",
                                             "population",
                                             "GDP"),
                                       data = using_model_list[[j]],
                                       index = c("cty_st_numeric","years"),
                                       weight = "population",
                                       force = "two-way", 
                                       CV = TRUE, 
                                       se = TRUE,
                                       # EM = TRUE,
                                       estimator = "mc",
                                       # lambda = 0.07499,
                                       # k =5,
                                       nlambda = 10,
                                       inference = "nonparametric", 
                                       nboots = 1000, 
                                       parallel = TRUE, 
                                       cores =20)
}
for (j in c(7)) {
  synth_list[[j]] <- gsynth1 <- gsynth(Y = "estab_pop",
                                       D = "Medicaid",
                                       X = c("estab",
                                             "population",
                                             "Uber_active",
                                             "GDP"),
                                       data = using_model_list[[j]],
                                       index = c("cty_st_numeric","years"),
                                       weight = "population",
                                       force = "two-way", 
                                       CV = TRUE, 
                                       se = TRUE,
                                       # EM = TRUE,
                                       estimator = "mc",
                                       # lambda = 0.07499,
                                       # k =5,
                                       nlambda = 10,
                                       inference = "nonparametric", 
                                       nboots = 1000, 
                                       parallel = TRUE, 
                                       cores =20)
  # print(gsynth1)
}
Sample_list <- c("Full Sample",
                 "Rural",
                 "Urban",
                 "Expansion cty, Uber*Exp",
                 "Expansion cty, Uber*Exp, Rural",
                 "Expansion cty, Uber*Exp, Urban",
                 "No Small Counties")

setwd(paste0(path_output))


for(i in seq_along(using_model_list)){
  gsynth1 <- synth_list[[i]]
  # pdf(paste0("Medicaid_weighted_gap_",industry_list[i],".pdf"), width = 8, height = 6)
  plot(gsynth1,
       type = "gap",
       xlab = "Period",
       main = paste0("ATE, ",Sample_list[i]),
  theme.bw = TRUE)
  # dev.off()
  
  # pdf(paste0("Medicaid_weighted_gap_",industry_list[i],".pdf"), width = 8, height = 6)
  # plot(gsynth1,
  #      type = "counterfactual",
  #      xlab = "Period",
  #      main = paste0("Counter factual, ",Sample_list[i]),
  #      theme.bw = TRUE)
  # dev.off()
  
}

inter_fe_all <- interFE(estab_pop ~ 
                          Medicaid,
                        data = using_model_list[[1]], index=c("cty_st_numeric","years"),
                        force = "two-way", nboots = 500, se = TRUE)

inter_fe_rural <- interFE(estab_pop ~ 
                          Medicaid,
                        data = using_model_list[[2]], index=c("cty_st_numeric","years"),
                        force = "two-way", nboots = 500, se = TRUE)

inter_fe_urban <- interFE(estab_pop ~ 
                          Medicaid,
                        data = using_model_list[[3]], index=c("cty_st_numeric","years"),
                        force = "two-way", nboots = 500, se = TRUE)



inter_fe_all_uber <- interFE(estab_pop ~ 
                          Medicaid + Medicaid_Uber + Uber_active,
                        data = using_model_list[[1]], index=c("cty_st_numeric","years"),
                        force = "two-way", nboots = 500, se = TRUE)

inter_fe_rural_uber <- interFE(estab_pop ~ 
                            Medicaid + Medicaid_Uber + Uber_active,
                          data = using_model_list[[2]], index=c("cty_st_numeric","years"),
                          force = "two-way", nboots = 500, se = TRUE)

inter_fe_urban_uber <- interFE(estab_pop ~ 
                            Medicaid + Medicaid_Uber + Uber_active,
                          data = using_model_list[[3]], index=c("cty_st_numeric","years"),
                          force = "two-way", nboots = 500, se = TRUE)


# ggplot(data = using_model_list[[1]], 
#        aes(x = population, 
#            y = estab_pop, 
#            color = as.factor(Ever_medicaid)))+
# geom_point()



# weights <- as.data.frame(gsynth1$wgt.implied)
# 
# LA_weights <- as.data.frame(sort(as.numeric(gsynth1$wgt.implied[,which(names(weights)%in%c("3043"))])))
# Queens_weights <- as.data.frame(sort(as.numeric(gsynth1$wgt.implied[,which(names(weights)%in%c("1664"))])))
# Chicago_weights <- as.data.frame(sort(as.numeric(gsynth1$wgt.implied[,which(names(weights)%in%c("411"))])))
# PHX_weights <- as.data.frame(sort(as.numeric(gsynth1$wgt.implied[,which(names(weights)%in%c("1919"))])))
# 
# View(subset(using_model_list[[1]], cty_st_numeric==1584|cty_st_numeric==1583|cty_st_numeric==1582))

# # LA county numeric code: 3043
# gsynth1 <- synth_list[[1]]
# plot(gsynth1, type = "counterfactual", id = 3043, raw = "band")
# 
# # Queens in NYC county numeric code: 1664
# gsynth1 <- synth_list[[1]]
# plot(gsynth1, type = "counterfactual", id = 1664, raw = "band")
# 
# # 	Cook County, IL : 411
# gsynth1 <- synth_list[[1]]
# plot(gsynth1, type = "counterfactual", id = 411, raw = "band")
# 
# #  Maricopa County, AZ	: 1919
# gsynth1 <- synth_list[[1]]
# plot(gsynth1, type = "counterfactual", id = 1919, raw = "band")


# print(paste0(Sample_list[1]," ",synth_list[[1]]$att.avg))
# print(paste0(Sample_list[2]," ",synth_list[[2]]$att.avg))
# print(paste0(Sample_list[3]," ",synth_list[[3]]$att.avg))
# print(paste0(Sample_list[4]," ",synth_list[[4]]$att.avg))
# print(paste0(Sample_list[5]," ",synth_list[[5]]$att.avg))
# print(paste0(Sample_list[6]," ",synth_list[[6]]$att.avg))
# 
# print(synth_list[[1]])
# print(synth_list[[2]])
# print(synth_list[[3]])
# print(synth_list[[4]])
# print(synth_list[[5]])
# print(synth_list[[6]])
# print(synth_list[[7]])