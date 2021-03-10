# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex


##################
###  Library   ###
##################
library(stargazer)
library(ggplot2)
library(scales)
library(gsynth)

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

options(scipen=10000)
set.seed(42)

load("medicaid_analysis_data_allnonemployers.RData")

using$market_type[using$concentration_lower==1] <- "Oligopsony"
using$market_type[using$concentration_lower==2] <- "Moderate"
using$market_type[using$concentration_lower==3] <- "Competitive"

using$Uber_active_num <- using$Uber_active
using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

using$market_type <- factor(using$market_type, levels = c("Oligopsony","Moderate","Competitive"))

data_list <- list()
estab_list <- list()
rcp_list <- list()
industry_list <- sort(unique(using$naics))

pb <- txtProgressBar(min = 0, max = length(industry_list), style = 3)

#################################################################
#                         Synthetic Control                     #
#################################################################

for (i in seq_along(industry_list)) {
  data_list[[i]] <- subset(using, naics==industry_list[[i]])
}
rm(using)

# for (i in seq_along(industry_list)) {
for (i in 14:19) {
    
  estab_list[[i]] <- gsynth(Y = "estab_pop",
                            D = "Medicaid",
                            X = c("Labor.Force",
                                  "Uber_active_num",
                                  "rcptot",
                                  "HHI_lower"),
                            data = data_list[[i]],
                            index = c("id_numeric","years"),
                            weight = "lf_avg",
                            force = "two-way", 
                            r = c(0, 5),
                            min.T0 = 8,
                            CV = TRUE, 
                            se = TRUE,
                            EM = TRUE,
                            # estimator = "mc",
                            # nlambda = 10,
                            # inference = "parametric",
                            inference = "nonparametric",
                            nboots = 500, 
                            parallel = TRUE,
                            cl =  "st",
                            cores = 4)
  
  
  rcp_list[[i]] <- gsynth(Y = "receipt_estab",
                          D = "Medicaid",
                          X = c("Labor.Force",
                                "Uber_active_num",
                                "estab",
                                "HHI_lower"),
                          data = data_list[[i]],
                          index = c("id_numeric","years"),
                          weight = "lf_avg",
                          force = "two-way", 
                          r = c(0, 5),
                          min.T0 = 8,
                          CV = TRUE, 
                          se = TRUE,
                          EM = TRUE,
                          # estimator = "mc",
                          # nlambda = 10,
                          # inference = "parametric",
                          inference = "nonparametric",
                          nboots = 500, 
                          parallel = TRUE,
                          cl =  "st",
                          cores = 4)
  
  setTxtProgressBar(pb, i)
  

}
save(estab_list, file = "synthetic_estab_med.RData")
save(rcp_list, file = "synthetic_rcp_med.RData")
