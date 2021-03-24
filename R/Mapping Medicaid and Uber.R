# Ben Glasner
# Medicaid Mapping

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
load("fips_1997_2018.RData")

load("Uber_2000_2017.RData")

# GDP <- merge(GDP, fips)

using_all <- merge(NES, medicaid)
using_all <- merge(using_all, population)
using_all <- merge(using_all, Uber_timing)
using_all <- merge(using_all, fips)
# using_all <- merge(using_all, GDP)

industry_list <- sort(unique(using_all$naics))
########################################

setwd(paste0(path_output))
using <- subset(using_all, naics==industry_list[1])

using$estab_pop <- (using$estab/using$population) # increase the size of the estab_pop term for easier reading of results
using$id <- as.factor(paste(using$st, using$cty, using$naics, sep = "-")) # create unique id for each county-state
using$cty_st <- as.factor(paste(using$st, using$cty, sep = "-")) # create unique id for each county-state



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

########################################

using$Medicad_uber <- "Neither"
using$Medicad_uber[using$Medicaid ==1] <- "Medicaid"
using$Medicad_uber[using$Uber_active ==1]  <- "Uber"
using$Medicad_uber[using$Uber_active ==1 & using$Medicaid ==1] <- "Both"

using$Medicaid[using$Medicaid==0] <- "No Expansion"
using$Medicaid[using$Medicaid==1] <- "Medicaid Expansion"

using_plot <-using %>% dplyr::select("FIPS combo","YEAR.id",
                                      "cty", "st", 
                                      "estab",
                                      "Uber_active",
                                       "Medicaid",
                                        "Medicad_uber") 

colnames(using_plot)[1] <- "fips"

year_list <- c(2012,2013,2014,2015,2016,2017)

using_plot <- subset(using_plot, as.numeric(YEAR.id) == 2012 |as.numeric(YEAR.id) == 2013 |as.numeric(YEAR.id) == 2014 |as.numeric(YEAR.id) == 2015 |as.numeric(YEAR.id) == 2016 |as.numeric(YEAR.id) == 2017)



p <- plot_usmap(regions = c("counties"), 
                data = na.omit(using_plot),
                values = "Medicad_uber",
                color = rgb(0, 0, 0, 0.05),
                size = .02) +
  # labs(title = paste0(year_list[i])) +
  scale_fill_manual(name = "Medicaid and Uber",
                    labels = c("Both", "Medicaid", "Neither", "Uber", "Missing"),
                    values = c("Neither"="grey", "Medicaid"="green3", "Uber" = "red3", "Both" = "purple3"),
                    na.value = "black") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        # legend.text = element_blank(),
        # legend.title = element_blank(),
        strip.background = element_rect(colour=NA, fill=NA),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.border = element_rect(fill = NA, color = NA),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        title = element_blank())+ 
  # guides(fill = FALSE)  + 
  facet_wrap( ~ YEAR.id, drop = TRUE, ncol= 3)

p

county_list <- as.data.frame(unique(p[["data"]][["fips"]]))
names(county_list) <- "fips"
county_list$fips <- as.numeric(county_list$fips)
county_merge <- county_list
county_merge$YEAR.id <- 2012
test <- county_list
test$YEAR.id <- 2013
county_merge <- rbind(county_merge,test)

test <- county_list
test$YEAR.id <- 2014
county_merge <- rbind(county_merge,test)

test <- county_list
test$YEAR.id <- 2015
county_merge <- rbind(county_merge,test)

test <- county_list
test$YEAR.id <- 2016
county_merge <- rbind(county_merge,test)

test <- county_list
test$YEAR.id <- 2017
county_merge <- rbind(county_merge,test)

test <- merge(using_plot, county_merge, all = TRUE)
test$Medicad_uber[is.na(test$Medicad_uber)] <- "Neither"


p <- plot_usmap(regions = c("counties"), 
                data = test,
                values = "Medicad_uber",
                color = rgb(0, 0, 0, 0.05),
                size = .0002) +
  # labs(title = paste0(year_list[i])) +
  scale_fill_manual(name = "Medicaid and Uber",
                    labels = c("Both", "Medicaid", "Neither", "Uber", "Missing"),
                    values = c("Neither"="grey", "Medicaid"="green3", "Uber" = "red3", "Both" = "purple3"),
                    na.value = "black") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.text = element_text(20),
        # legend.title = element_blank(),
        strip.background = element_rect(colour=NA, fill=NA),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.border = element_rect(fill = NA, color = NA),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        title = element_blank())+ 
  # guides(fill = FALSE)  + 
  facet_wrap( ~ YEAR.id, drop = TRUE, ncol= 2)

p

  png(paste0("Map_Medicaid_uber.png"), width = 500, height = 750)
  plot(p)
  dev.off()


# for (i in seq_along(year_list)) {
# 
#   plot_data <- subset(using_plot, YEAR.id==year_list[i])
#   
#   p <- plot_usmap(regions = c("states"), data = plot_data, 
#              values = "Medicad_uber",
#              color = rgb(0, 0, 0, 0.05),
#              size = .02) +
#     labs(title = paste0(year_list[i])) +
#     scale_fill_manual(name = "Medicaid and Uber",
#                       labels = c("Neither", "Medicaid", "Uber", "Both", "Missing"),
#                       values = c("Neither"="grey", "Medicaid"="green3", "Uber" = "red3", "Both" = "purple3"),
#                       na.value = "white") +
#     theme(legend.position = "top",
#           legend.background = element_rect(fill=alpha("white", 0.4)),
#           legend.text = element_blank(),
#           legend.title = element_blank(),
#           title = element_text(size = 20)) + 
#     guides(shape = FALSE,
#            color = FALSE,
#            size = FALSE,
#            fill = FALSE) 
#   
#   png(paste0("Map_Medicaid_uber_",year_list[i],".png"), width = 640, height = 420)
#   plot(p)
#   dev.off()
#     
# }
# 
# for (i in seq_along(year_list)) {
#   
#   plot_data <- subset(using_plot, YEAR.id==year_list[i])
#   
#   p <- plot_usmap(regions = c("states"), data = plot_data, 
#                   values = "Medicaid", lines = rgb(0, 0, 0, 0.05)) +
#     labs(title = paste0(year_list[i])) +
#     scale_fill_manual(name = "Medicaid",
#                       labels = c("No Expansion", "Medicaid Expansion", "Missing"),
#                       values = c("No Expansion"="grey", "Medicaid Expansion"="green3"),
#                       na.value = "white") +
#     theme(legend.position = "top",
#           legend.background = element_rect(fill=alpha("white", 0.4)),
#           legend.text = element_blank(),
#           legend.title = element_blank(),
#           title = element_text(size = 20))
#   
#   png(paste0("Map_Medicaid_",year_list[i],".png"), width = 640, height = 420)
#   plot(p)
#   dev.off()
#   
# }