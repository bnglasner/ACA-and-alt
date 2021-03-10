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

load("Nonemployer_statistics_1997_2017.RData") # load the data

medicaid <- read_excel("medicaid.xlsx")

load("county_population_2000_2018.RData")

load("Uber_2000_2017.RData")
using_all <- merge(NES, medicaid)
using_all <- merge(using_all, population)
using_all <- merge(using_all, Uber_timing)

#################################################################
#                              Industry                         #
#################################################################

  setwd(paste0(path_data))
  
  using <- subset(using_all, naics!="00")
  
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
  
  
  using$cty_st_numeric <- as.numeric(using$cty_st)
  using$years <- as.numeric(as.character(using$YEAR.id))
  using$YEAR.id <- as.factor(using$YEAR.id)
  
  using$Medicaid_Uber_active <- using$Medicaid*using$Uber_active
  
  using_model <- using %>% dplyr::select("estab_pop", 
                                         "estab",
                                         "population",
                                         "Medicaid", 
                                         "Medicaid_Uber_active",
                                         "Uber_active",
                                         "cty_st",
                                         "years",
                                         "cty_st",
                                         "YEAR.id",
                                         "id",
                                         "naics")
  using_model <- using_model[complete.cases(using_model),]
  
  estab <- lm(estab_pop ~
                Medicaid*Uber_active*naics +
                YEAR.id + 
                cty_st,
              data = using_model,
              weights = population)
  
  estab_tidy <- tidy(estab)
  
  setwd(paste0(path_output))
  gsynth1 <- gsynth(Y = "estab_pop",
                    D = "Medicaid",
                    X = c("estab",
                          "population",
                          "Uber_active"),
                    data = using_model,
                    index = c("cty_st_numeric","years"),
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
       main = paste0("Weighted ATE Medicaid"),
       theme.bw = TRUE)
  
  plot(gsynth1,
       type = "counterfactual",
       # raw = "band",
       main = paste0("Unweighted Averages Medicaid"),
       theme.bw = TRUE,
       shade.post = FALSE) 


  fit_list_uber_medicaid <- list()
  fit_list_uber_nomedicaid <- list()
  fit_list_nouber_medicaid <- list()
  fit_list_nouber_nomedicaid <- list()
  
  industry_list <- sort(unique(using_model$naics))
  for(j in seq_along(industry_list)){
    coef_pred <- using_model[1,]
    coef_pred$YEAR.id <- "2017"
    coef_pred$Medicaid <- 1
    coef_pred$Uber_active <- 1
    coef_pred$naics <- industry_list[j]
    
    fit_list_uber_medicaid[[j]] <- predict(estab, coef_pred, interval = "confidence")
  }
  for(j in seq_along(industry_list)){
    coef_pred <- using_model[1,]
    coef_pred$YEAR.id <- "2017"
    coef_pred$Medicaid <- 0
    coef_pred$Uber_active <- 1
    coef_pred$naics <- industry_list[j]
    
    fit_list_uber_nomedicaid[[j]] <- predict(estab, coef_pred, interval = "confidence")
  }
  for(j in seq_along(industry_list)){
    coef_pred <- using_model[1,]
    coef_pred$YEAR.id <- "2017"
    coef_pred$Medicaid <- 1
    coef_pred$Uber_active <- 0
    coef_pred$naics <- industry_list[j]
    
    fit_list_nouber_medicaid[[j]] <- predict(estab, coef_pred, interval = "confidence")
  }
  for(j in seq_along(industry_list)){
    coef_pred <- using_model[1,]
    coef_pred$YEAR.id <- "2017"
    coef_pred$Medicaid <- 0
    coef_pred$Uber_active <- 0
    coef_pred$naics <- industry_list[j]
    
    fit_list_nouber_nomedicaid[[j]] <- predict(estab, coef_pred, interval = "confidence")
  }
  
  uber_medicaid <- as.data.frame(cbind(do.call(rbind, fit_list_uber_medicaid),industry_list))  
  uber_nomedicaid <- as.data.frame(cbind(do.call(rbind, fit_list_uber_nomedicaid),industry_list))  
  nouber_medicaid <- as.data.frame(cbind(do.call(rbind, fit_list_nouber_medicaid),industry_list))  
  nouber_nomedicaid <- as.data.frame(cbind(do.call(rbind, fit_list_nouber_nomedicaid),industry_list))  
  
  
  uber_medicaid$Uber <- 1
  uber_nomedicaid$Uber <- 1
  nouber_medicaid$Uber <- 0
  nouber_nomedicaid$Uber <- 0
  
  uber_medicaid$Medicaid <- 1
  uber_nomedicaid$Medicaid <- 0
  nouber_medicaid$Medicaid <- 1
  nouber_nomedicaid$Medicaid <- 0
  
  uber_medicaid$Group <- "Uber and Medicaid"
  uber_nomedicaid$Group <- "Uber and No Medicaid"
  nouber_medicaid$Group <- "No Uber and Medicaid"
  nouber_nomedicaid$Group <- "No Uber and No Medicaid"
  
  coef_results <- rbind(uber_medicaid,uber_nomedicaid,nouber_medicaid,nouber_nomedicaid)
  coef_results$naics <- as.factor(as.character(coef_results$industry_list))
  
  coef_results$type <- as.numeric(coef_results$naics)
  coef_results$fit <- as.numeric(as.character(coef_results$fit))
  coef_results$lwr <- as.numeric(as.character(coef_results$lwr))
  coef_results$upr <- as.numeric(as.character(coef_results$upr))
  
  pdf(paste0("Medicaid_Uber_noUber_by_industry.pdf"), width=12, height = 9)
  ggplot(data = subset(coef_results,Medicaid==1 & naics!=4853 & naics!=4859), aes(y=fit, x = naics, color=Group, group = Group)) +
    # geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_point(size = 1) +
    geom_line() +
    geom_errorbar(aes(x = naics, ymin = lwr, ymax = upr)) +
    theme(plot.title = element_text(hjust = 0.5,size = 20),
          # axis.text.y = element_text(size = 20),
          # axis.text.x = element_blank(),
          # axis.title.y = element_text(size = 20),
          # axis.title.x = element_text(size = 10),
          panel.background = element_rect(fill = "white"),
          # panel.grid = element_line(colour = "grey"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)) +
    ylab("Predicted Establishments Per Person") 
  dev.off()
  
  pdf(paste0("Medicaid_noMedicaid_by_industry.pdf"), width=12, height = 9)
  ggplot(data = subset(coef_results,Uber==0 & naics!=4853 & naics!=4859), aes(y=fit, x = naics, color=Group, group = Group)) +
    # geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_point(size = 1) +
    geom_line() +
    geom_errorbar(aes(x = naics, ymin = lwr, ymax = upr)) +
    theme(plot.title = element_text(hjust = 0.5,size = 20),
          # axis.text.y = element_text(size = 20),
          # axis.text.x = element_blank(),
          # axis.title.y = element_text(size = 20),
          # axis.title.x = element_text(size = 10),
          panel.background = element_rect(fill = "white"),
          # panel.grid = element_line(colour = "grey"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)) +
    ylab("Predicted Establishments Per Person") 
  dev.off()
  
  pdf(paste0("Medicaid_full_results_by_industry.pdf"), width=12, height = 9)
  ggplot(data = subset(coef_results, naics!=4853 & naics!=4859), aes(y=fit, x = naics, color=Group, group = Group)) +
    # geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_point(size = 1) +
    # geom_line() +
    geom_errorbar(aes(x = naics, ymin = lwr, ymax = upr)) +
    theme(plot.title = element_text(hjust = 0.5,size = 20),
          # axis.text.y = element_text(size = 20),
          # axis.text.x = element_blank(),
          # axis.title.y = element_text(size = 20),
          # axis.title.x = element_text(size = 10),
          panel.background = element_rect(fill = "white"),
          # panel.grid = element_line(colour = "grey"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)) +
    ylab("Predicted Establishments Per Person") 
  dev.off()
  