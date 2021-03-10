# Ben Glasner
# Medicaid data Mapping

#Set up
#################################################
# install_github("UrbanInstitute/urbnmapr")

library(devtools)
library(tidyverse)
library(usmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(readxl)
library(lubridate)


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
using <- subset(using, st!="2" & st!="18" & st!="22" & st!="30" & st!="42") # Drop states that have medicaid expansions after 2014 and before 2018

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


########################################
using <- subset(using, YEAR.id>1999)

using$Medicaid[using$Medicaid==0] <- "No Expansion"
using$Medicaid[using$Medicaid==1] <- "Medicaid Expansion"

using$GDP <- as.numeric(as.character(using$GDP))

using$Has_GDP <- "No"
using$Has_GDP[using$GDP>0] <- "Yes"

using$Has_LAU <- "No"
using$Has_LAU[using$unemployment_rate>-1] <- "Yes"

using$Has_pop <- "No"
using$Has_pop[using$population>-1] <- "Yes"

using$Has_uber_data <- "No"
using$Has_uber_data[using$Uber_active>-1] <- "Yes"

using$Has_all_four <- "No"
using$Has_all_four[using$Has_GDP=="Yes" & 
                     using$Has_LAU=="Yes" &
                     using$Has_pop=="Yes" &
                     using$Has_uber_data=="Yes"] <- "Yes"

using_plot <-using %>% dplyr::select("FIPS combo","YEAR.id",
                                     "cty", "st", 
                                     "Has_GDP", "Has_LAU",
                                     "Has_pop", "Has_uber_data",
                                     "Has_all_four") 

colnames(using_plot)[1] <- "fips"




year_list <- c(2017)

for (i in seq_along(year_list)) {
  
  plot_data <- subset(using_plot, YEAR.id==year_list[i])
  
  p <- plot_usmap(regions = c("states"), data = plot_data, 
                  values = "Has_GDP", lines = rgb(0, 0, 0, 0.05)) +
    labs(title = paste0(year_list[i])) +
    scale_fill_manual(name = "Has GDP",
                      labels = c("No", "Yes", "Missing"),
                      values = c("No"="grey", "Yes"="green3"),
                      na.value = "white") +
    theme(legend.position = "top",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_blank(),
          legend.title = element_blank(),
          title = element_text(size = 20))
  
  plot(p)
  
}

for (i in seq_along(year_list)) {
  
  plot_data <- subset(using_plot, YEAR.id==year_list[i])
  
  p <- plot_usmap(regions = c("states"), data = plot_data, 
                  values = "Has_LAU", lines = rgb(0, 0, 0, 0.05)) +
    labs(title = paste0(year_list[i])) +
    scale_fill_manual(name = "Has LAU",
                      labels = c("No", "Yes", "Missing"),
                      values = c("No"="grey", "Yes"="green3"),
                      na.value = "white") +
    theme(legend.position = "top",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_blank(),
          legend.title = element_blank(),
          title = element_text(size = 20))
  
  plot(p)
  
}
for (i in seq_along(year_list)) {
  
  plot_data <- subset(using_plot, YEAR.id==year_list[i])
  
  p <- plot_usmap(regions = c("states"), data = plot_data, 
                  values = "Has_pop", lines = rgb(0, 0, 0, 0.05)) +
    labs(title = paste0(year_list[i])) +
    scale_fill_manual(name = "Has pop",
                      labels = c("No", "Yes", "Missing"),
                      values = c("No"="grey", "Yes"="green3"),
                      na.value = "white") +
    theme(legend.position = "top",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_blank(),
          legend.title = element_blank(),
          title = element_text(size = 20))
  
  plot(p)
  
}

for (i in seq_along(year_list)) {
  
  plot_data <- subset(using_plot, YEAR.id==year_list[i])
  
  p <- plot_usmap(regions = c("states"), data = plot_data, 
                  values = "Has_uber_data", lines = rgb(0, 0, 0, 0.05)) +
    labs(title = paste0(year_list[i])) +
    scale_fill_manual(name = "Has uber",
                      labels = c("No", "Yes", "Missing"),
                      values = c("No"="grey", "Yes"="green3"),
                      na.value = "white") +
    theme(legend.position = "top",
          legend.background = element_rect(fill=alpha("white", 0.4)),
          legend.text = element_blank(),
          legend.title = element_blank(),
          title = element_text(size = 20))
  
  plot(p)
  
}
