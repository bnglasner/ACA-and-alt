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

load("Nonemployer_statistics_state_1997_2017.RData") # load the data

medicaid <- read_excel("medicaid.xlsx")

load("state_population_2000_2018.RData")


using_all <- merge(NES, medicaid)
using_all <- merge(using_all, st_population)

industry_list <- sort(unique(using_all$naics))
#################################################################
#                              Industry                         #
#################################################################
# i<-1 # for total
# i <- 9 # for 48-49
# i <- 10 # for 4853
for (i in seq_along(industry_list)) {
  setwd(paste0(path_data))
  
      print(paste0(industry_list[i],"start"))
  
      using <- subset(using_all, naics==industry_list[i])
      
      using$estab_pop <- (using$estab/using$population) # increase the size of the estab_pop term for easier reading of results

      #######################################################
      ###     Microsynthetic for local Min Wage increases ###
      #######################################################

      
      using$st_numeric <- as.numeric(using$st)
      using$years <- as.numeric(as.character(using$YEAR.id))
      using$YEAR.id <- as.factor(using$YEAR.id)

      using_model <- using %>% dplyr::select("estab_pop", 
                                             "estab",
                                             "population",
                                             "Medicaid", 
                                             "st_numeric",
                                             "st",
                                             "years",
                                             "YEAR.id")
      
      using_model <- using_model[complete.cases(using_model),]
      
      estab <- lm(estab_pop ~
                    Medicaid +
                    YEAR.id + 
                    st,
                  data = using_model,
                  weights = population)
      
      estab_tidy_list[[i]] <- tidy(estab)

      inter_fe_estab <- interFE(estab_pop ~ 
                                  Medicaid,
                                data = using_model, index=c("st_numeric","years"),
                                force = "two-way", nboots = 100)
      
      

      
        synth_list[[i]] <- gsynth1 <- gsynth(Y = "estab_pop",
                                                D = "Medicaid",
                                                X = c("estab",
                                                      "population"),
                                                data = using_model,
                                                index = c("st_numeric","years"),
                                                weight = "population",
                                                force = "two-way", 
                                                CV = TRUE, 
                                                se = TRUE,
                                                EM = TRUE,
                                                # estimator = "mc",
                                                # nlambda = 10,
                                                inference = "parametric", 
                                                nboots = 1000, 
                                                parallel = TRUE, 
                                                cores = 20)
        
        print(gsynth1)
        
        plot(gsynth1,
             type = "gap", 
             xlab = "Period",
             main = paste0("Weighted ATE ",industry_list[i], " Medicaid"),
             theme.bw = TRUE)
        
        plot(gsynth1,
             type = "counterfactual",
             raw = "all",
             main = paste0("Unweighted Averages ",industry_list[i], " Medicaid"),
             theme.bw = TRUE,
             shade.post = FALSE)
        
        plot(gsynth1,
             type = "counterfactual",
             # raw = "all",
             main = paste0("Unweighted Averages ",industry_list[i], " Medicaid"),
             theme.bw = TRUE,
             shade.post = FALSE)
}

setwd(paste0(path_output))

for(i in seq_along(industry_list)){
  gsynth1 <- synth_list[[i]]
  # print(gsynth1)
  pdf(paste0("Medicaid_weighted_gap_",industry_list[i],"_state.pdf"), width = 8, height = 6)
  plot(gsynth1,
       type = "gap", 
       xlab = "Period",
       main = paste0("Weighted ATE ",industry_list[i], " Medicaid"),
       theme.bw = TRUE)
  dev.off()
}



for(i in seq_along(industry_list)){
  gsynth1 <- synth_list[[i]]
  # print(gsynth1)
  
  pdf(paste0("Medicaid_counterfactual_",industry_list[i],"_state.pdf"), width = 8, height = 6)
  plot(gsynth1,
       type = "counterfactual",
       # raw = "band",
       main = paste0("Unweighted Averages ",industry_list[i], " Medicaid"),
       theme.bw = TRUE,
       shade.post = FALSE)
  dev.off()
  
  
}
