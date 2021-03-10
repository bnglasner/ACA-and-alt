# Ben Glasner
# Medicaid and nonemployers analysis
# Desccriptive paths
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
i <-1

setwd(paste0(path_data))

print(paste0(industry_list[i],"start"))

using <- subset(using_all, naics==industry_list[i])

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


#######################################################
###     Microsynthetic for local Min Wage increases ###
#######################################################


using$cty_st_numeric <- as.numeric(using$cty_st)
using$years <- as.numeric(as.character(using$YEAR.id))
using$YEAR.id <- as.factor(using$YEAR.id)

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
                                       "Ever_Uber",
                                       "Uber_active",
                                       "Urban",
                                       "cty_st_numeric",
                                       "years",
                                       "cty_st",
                                       "YEAR.id",
                                       "id",
                                       "st")
using_model <- using_model[complete.cases(using_model),]
county_counts <- as.data.frame(table(using_model$id)) # count how many times a county-state appears in the sample
county_counts <- subset(county_counts, Freq==18) # keep only the counties that are in the full sample for balance
colnames(county_counts)[1]<- "id" # relabel the id for the merge
using_model<- merge(using_model, county_counts) # merge as a way to drop the unbalanced counties
rm(county_counts) # drop county_counts from the env.



##########################################################################
### Identify which counties will eventually get the medicaid expansion ###
##########################################################################
setwd(paste0(path_output))

medicaid_ever <- subset(medicaid, YEAR.id==2017)
medicaid_ever$Medicaid_ever <- medicaid_ever$Medicaid
medicaid_ever <- medicaid_ever %>% dplyr::select("st", "Medicaid_ever")
 
using_model <- merge(using_model, medicaid_ever)

using_model$Medicaid_ever[using_model$Medicaid_ever ==0] <- "No Medicaid Expansion" 
using_model$Medicaid_ever[using_model$Medicaid_ever ==1] <- "Medicaid Expansion" 

# using_model$estab_log <- log(using_model$estab)

trends <- aggregate(x=using_model$estab_pop, by = list(using_model$YEAR.id, using_model$Medicaid_ever, using_model$Ever_Uber, using_model$Urban), FUN = mean)

trends$Group.5 <- "No Medicaid Expansion, Urban, Uber"
trends$Group.5[trends$Group.2=="Medicaid Expansion" & trends$Group.3 == 1 & trends$Group.4==1] <- "Medicaid Expansion, Urban, Uber"
trends$Group.5[trends$Group.2=="No Medicaid Expansion" & trends$Group.3 == 1 & trends$Group.4==0] <- "No Medicaid Expansion, Rural, Uber"
trends$Group.5[trends$Group.2=="Medicaid Expansion" & trends$Group.3 == 1 & trends$Group.4==0] <- "Medicaid Expansion, Rural, Uber"
trends$Group.5[trends$Group.2=="No Medicaid Expansion" & trends$Group.3 == 0 & trends$Group.4==1] <- "No Medicaid Expansion, Urban, No Uber"
trends$Group.5[trends$Group.2=="Medicaid Expansion" & trends$Group.3 == 0 & trends$Group.4==1] <- "Medicaid Expansion, Urban, No Uber"
trends$Group.5[trends$Group.2=="Medicaid Expansion" & trends$Group.3 == 0 & trends$Group.4==0] <- "Medicaid Expansion, Rural, No Uber"
trends$Group.5[trends$Group.2=="No Medicaid Expansion" & trends$Group.3 == 0 & trends$Group.4==0] <- "No Medicaid Expansion, Rural, No Uber"

trends2 <- aggregate(x=using_model$estab_pop, by = list(using_model$YEAR.id, using_model$Medicaid_ever, using_model$Ever_Uber), FUN = mean)
trends2$Group.5 <- "Medicaid Expansion, Uber"
trends2$Group.5[trends2$Group.2=="No Medicaid Expansion" & trends2$Group.3 == 1] <- "No Medicaid Expansion, Uber"
trends2$Group.5[trends2$Group.2=="Medicaid Expansion" & trends2$Group.3 == 0] <- "Medicaid Expansion, No Uber"
trends2$Group.5[trends2$Group.2=="No Medicaid Expansion" & trends2$Group.3 == 0] <- "No Medicaid Expansion, No Uber"

trends3 <- aggregate(x=using_model$estab_pop, by = list(using_model$YEAR.id, using_model$Medicaid_ever, using_model$Urban), FUN = mean)
trends3$Group.5 <- "Medicaid Expansion, Urban"
trends3$Group.5[trends3$Group.2=="No Medicaid Expansion" & trends3$Group.3 == 1] <- "No Medicaid Expansion, Urban"
trends3$Group.5[trends3$Group.2=="Medicaid Expansion" & trends3$Group.3 == 0] <- "Medicaid Expansion, Rural"
trends3$Group.5[trends3$Group.2=="No Medicaid Expansion" & trends3$Group.3 == 0] <- "No Medicaid Expansion, Rural"

trends4 <- aggregate(x=using_model$estab_pop, by = list(using_model$YEAR.id, using_model$Medicaid_ever), FUN = mean)
trends4$Group.5 <- "Medicaid Expansion"
trends4$Group.5[trends4$Group.2=="No Medicaid Expansion"] <- "No Medicaid Expansion"

trends5 <- aggregate(x=using_model$Uber_active, by = list(using_model$YEAR.id, using_model$Medicaid_ever, using_model$Urban), FUN = mean)
trends5$Group.4 <- "Medicaid Expansion, Urban"
trends5$Group.4[trends5$Group.2=="No Medicaid Expansion" & trends5$Group.3 == 1] <- "No Medicaid Expansion, Urban"
trends5$Group.4[trends5$Group.2=="Medicaid Expansion" & trends5$Group.3 == 0] <- "Medicaid Expansion, Rural"
trends5$Group.4[trends5$Group.2=="No Medicaid Expansion" & trends5$Group.3 == 0] <- "No Medicaid Expansion, Rural"


pdf(paste0("Estab Pop Trend by Medicaid, Urban, and Uber.pdf"), width = 14, height = 9)
ggplot(data = trends,
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           color = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=4,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=4,order = 2)) 
dev.off()


pdf(paste0("Estab Pop Trend by Medicaid, Urban only, uber no uber.pdf"), width = 14, height = 9)
ggplot(data = subset(trends, Group.4==1),
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()

pdf(paste0("Estab Pop Trend by Medicaid, Rural only, uber no uber.pdf"), width = 14, height = 9)
ggplot(data = subset(trends, Group.4==0),
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()

pdf(paste0("Estab Pop Trend by Medicaid, Urban, uber only.pdf"), width = 14, height = 9)
ggplot(data = subset(trends, Group.3==1),
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()

pdf(paste0("Estab Pop Trend by Medicaid, Urban, no uber only.pdf"), width = 14, height = 9)
ggplot(data = subset(trends, Group.3==0),
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()

pdf(paste0("Estab Pop Trend by Medicaid and Uber.pdf"), width = 11, height = 8)
ggplot(data = trends2,
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()

pdf(paste0("Estab Pop Trend by Medicaid and Urban.pdf"), width = 11, height = 8)
ggplot(data = trends3,
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()



pdf(paste0("Estab Pop Trend by Medicaid.pdf"), width = 11, height = 8)
ggplot(data = trends4,
       aes(group = Group.5, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.5)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Establishments Per Person") +
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
  guides(col = guide_legend("Lines",nrow=1,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=1,order = 2)) 
dev.off()


pdf(paste0("Uber Expansion Trend by Medicaid and Urban.pdf"), width = 11, height = 8)
ggplot(data = trends5,
       aes(group = Group.4, 
           x = Group.1, 
           y = x,
           # color = Group.5,
           shape = Group.4)) +
  geom_point(size = 2.5) +
  geom_line() +
  geom_vline(xintercept = 14.5, color = "red")+
  xlab("Year") +
  ylab("Uber is Active") +
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
  guides(col = guide_legend("Lines",nrow=2,override.aes = list(size=2), order = 1),
         shape = guide_legend(nrow=2,order = 2)) 
dev.off()