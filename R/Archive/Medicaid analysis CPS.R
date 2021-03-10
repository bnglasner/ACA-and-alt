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
library(reshape2)

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
path_data <- paste0(path_project,"\\CPS Data\\Data\\")
# Path to NES data 
path_NES <- paste0(path_project,"\\Nonemployer data\\Data\\")
# Path where matched samples should be saved 
path_output <- paste0(path_project,"\\ACA and alt\\output")

if(Sys.info()[["nodename"]]=="SIM1"){
  # Root folder
  path_project <- "H:/Phd Requirements min wage"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"\\CPS Data\\Data\\")
  # Path to NES data 
  path_NES <- paste0(path_project,"\\Nonemployer data\\Data\\")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"\\ACA and alt\\output")
}
if(Sys.info()[["nodename"]]=="SIM2"){
  # Root folder
  path_project <- "H:/Phd Requirements min wage"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"\\CPS Data\\Data\\")
  # Path to NES data 
  path_NES <- paste0(path_project,"\\Nonemployer data\\Data\\")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"\\ACA and alt\\output")
}
if(Sys.info()[["nodename"]]=="SIM3"){
  # Root folder
  path_project <- "H:/Phd Requirements min wage"
  # Path to saved cohort data 
  path_data <- paste0(path_project,"\\CPS Data\\Data\\")
  # Path to NES data 
  path_NES <- paste0(path_project,"\\Nonemployer data\\Data\\")
  # Path where matched samples should be saved 
  path_output <- paste0(path_project,"\\ACA and alt\\output")
}
set.seed(99199)


#################################################################
#                              Set Up                           #
#################################################################
setwd(paste0(path_data))
load("CPS.RData") # load the CPS data

setwd(paste0(path_NES))
load("Nonemployer_statistics_state_1997_2017.RData") # load the NES state data
NES<- subset(NES, naics == "00" & as.numeric(YEAR.id)>1999)

medicaid <- read_excel("medicaid.xlsx")
colnames(medicaid)[2] <- "st"

load("state_population_2000_2018.RData") 
#########################
##   State aggregates  ##
#########################

state_aggs  <- data %>% 
  group_by(STATEFIP, YEAR) %>%
  summarize(Estab_pct = weighted.mean(CLASSWKR == 13, ASECWTH))
colnames(state_aggs)[1] <- "st"
colnames(state_aggs)[2] <- "YEAR.id"
state_aggs <- subset(state_aggs, YEAR.id <2018)

state_counts <- as.data.frame(table(state_aggs$st)) # count how many times a county-state appears in the sample
state_counts <- subset(state_counts, Freq==18) # keep only the states that are in the full sample for balance
colnames(state_counts)[1]<- "st" # relabel the id for the merge
state_aggs<- merge(state_aggs, state_counts) # merge as a way to drop the unbalanced counties
rm(data) # drop CPS raw data from the env.

state_aggs <- merge(state_aggs, medicaid)
state_aggs <- merge(state_aggs, st_population)

NES <- merge(NES, medicaid)
NES <- merge(NES, st_population)
NES$estab_pop <- NES$estab/NES$population

ever_medicaid <- subset(medicaid, YEAR.id=="2017")
ever_medicaid$Ever_medicaid <- ever_medicaid$Medicaid
ever_medicaid <- ever_medicaid %>% dplyr::select("st", "Ever_medicaid")

state_aggs <- merge(state_aggs, ever_medicaid)
NES <- merge(NES, ever_medicaid)


  #################################################################
  #                              Descriptive Plot                 #
  #################################################################
setwd(paste0(path_output))

  annual_aggs  <- state_aggs %>% 
    group_by(YEAR.id, Ever_medicaid) %>%
    summarize(Estab_pct = weighted.mean(Estab_pct, population))
  NES_aggs  <- NES %>% 
    group_by(YEAR.id, Ever_medicaid) %>%
    summarize(estab_pop = weighted.mean(estab_pop, population))

  annual_aggs <- merge(NES_aggs, annual_aggs)
  colnames(annual_aggs)[3] <- "NES"
  colnames(annual_aggs)[4] <- "CPS"
  annual_aggs$Ever_medicaid[annual_aggs$Ever_medicaid==0] <- "No Medicaid Expansion"
  annual_aggs$Ever_medicaid[annual_aggs$Ever_medicaid==1] <- "Medicaid Expansion"
  
  annual_long <- melt(annual_aggs, id=c("YEAR.id", "Ever_medicaid"))  # convert to long format
  
  annual_long$Group[annual_long$Ever_medicaid=="No Medicaid Expansion" & annual_long$variable == "NES"] <- "NES, No Medicaid Expansion" 
  annual_long$Group[annual_long$Ever_medicaid=="No Medicaid Expansion" & annual_long$variable == "CPS"] <- "CPS, No Medicaid Expansion" 
  annual_long$Group[annual_long$Ever_medicaid=="Medicaid Expansion" & annual_long$variable == "NES"] <- "NES, Medicaid Expansion" 
  annual_long$Group[annual_long$Ever_medicaid=="Medicaid Expansion" & annual_long$variable == "CPS"] <- "CPS, Medicaid Expansion" 
  
  cols <- c("NES, No Medicaid Expansion" = "blue1",
            "NES, Medicaid Expansion" = "blue4",
            "CPS, No Medicaid Expansion" = "red1",
            "CPS, Medicaid Expansion" = "red4")
  group.colors <- c("blue1", "blue4", "red1", "red4")
  
  pdf(paste0("CPS v NES.pdf"), width = 10, height = 7.5)
  ggplot(data = annual_long, aes(x = YEAR.id,
                                 y = value, 
                                 group = Group,
                                 # color = Group,
                                 linetype = Group,
                                 shape = Group)) +
    geom_line()+
    geom_point(size = 4)+
    xlab("Year") +
    ylab("Establishments Per Person") +
    theme(plot.title = element_blank(),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(colour = "grey"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.background = element_rect(fill=NA),
          legend.text = element_text(size = 10),
          legend.key = element_rect(fill="white"),
          legend.title = element_blank()) +
    scale_color_manual(values=cols,
                       breaks = c("NES, No Medicaid Expansion",
                                  "NES, Medicaid Expansion",
                                  "CPS, No Medicaid Expansion",
                                  "CPS, Medicaid Expansion")) +
    guides(shape=guide_legend(nrow=2),
           color = FALSE,
           linetype = FALSE)
  dev.off()


  #################################################################
  #                              LM                               #
  #################################################################
  
  lm_1 <- lm(Estab_pct ~ Medicaid + as.factor(st) + as.factor(YEAR.id), data = state_aggs)
  lm_2 <- lm(estab_pop ~ Medicaid + as.factor(st) + as.factor(YEAR.id), data = NES)
  
  summary(lm_1)
  summary(lm_2)

#################################################################
#                              Logit                            #
#################################################################
setwd(paste0(path_data))
load("CPS.RData") # load the CPS data

colnames(medicaid)[2] <- "STATEFIP"
colnames(medicaid)[3] <- "YEAR"

data <- merge(data, medicaid)

data$estab <-0 
data$estab[data$CLASSWKR == 13] <- 1 
glm_1 <- glm(estab ~ Medicaid + as.factor(STATEFIP) + as.factor(YEAR), family = "binomial", data = data)

glm_2 <- glm(estab ~ Medicaid + as.factor(METRO) + as.factor(EDUC) + as.factor(POVERTY) + as.factor(STATEFIP) + as.factor(YEAR), family = "binomial", data = data)








