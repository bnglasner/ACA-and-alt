# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex


##################
###  Library   ###
##################

library(devtools)
# install_github("xuyiqing/gsynth")

library(stargazer)
library(lfe)
library(estimatr)
library(ggplot2)
library(ggeffects)
library(broom)
library(scales)
library(gsynth)
library(panelView)

library(dplyr)
library(see)

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
using <- subset(using, naics=="00") # All Nonemployer Establishments

# using <- subset(using, naics=="48-49")  # Transportation and Warehousing

using$Uber_active_num <- using$Uber_active
using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

estab_list <- list()
rcp_list <- list()

using$id <- factor(using$id)
#################################################################
#                         Synthetic Control                     #
#################################################################

###### Create subsets of the data for isolating effect of Uber in the premedciaid and post medicaid periods
ids_medi <- using %>%
  group_by(id) %>%
  summarize(number = n(),
            years_of_medi = sum(Medicaid),
            years_of_uber = sum(Uber_active_num))

# Treatment Group - Uber After Medicaid, Does the county receive Uber after Medicaid already expanded
treat_1 <- subset(ids_medi, years_of_medi > years_of_uber & years_of_uber>0)

# Treatment Group - Medicaid After Uber, Does the county receive Uber before Medicaid expanded
treat_2 <- subset(ids_medi, years_of_medi < years_of_uber & years_of_medi>0)

# Treatment Group - Medicaid After Uber, Does the county receive Uber before Medicaid expanded
treat_3 <- subset(ids_medi, years_of_medi == years_of_uber & years_of_medi>0)

# Treatment Group - Uber After Medicaid, Does the county receive Uber, after Medicaid already expanded
treat_4 <- subset(ids_medi, years_of_medi > 0 & years_of_uber==0)

# Control Group - Uber but no Medicaid, Does the county receive Uber but no Medicaid expansion
control_1 <- subset(ids_medi, years_of_medi ==0 & years_of_uber>0)
# Control Group - No Uber and no Medicaid, Does the county receive neither Uber or Medicaid expansion
control_2 <- subset(ids_medi, years_of_medi ==0 & years_of_uber==0)

treatment_list <- list()
treatment_list[[1]] <- treat_1$id
treatment_list[[2]] <- treat_2$id
treatment_list[[3]] <- treat_3$id
treatment_list[[4]] <- treat_4$id

control_list <- list()
control_list[[1]] <- control_1$id
control_list[[2]] <- control_2$id

rm(treat_1,treat_2,treat_3,treat_4,control_1,control_2, ids_medi)

treat_1 <- using[using$id %in% treatment_list[[1]], ]
treat_2 <- using[using$id %in% treatment_list[[2]], ]
treat_3 <- using[using$id %in% treatment_list[[3]], ]
treat_4 <- using[using$id %in% treatment_list[[4]], ]

control_1 <- using[using$id %in% control_list[[1]], ]
control_2 <- using[using$id %in% control_list[[2]], ]


#################################################################
#                         Synthetic Control                     #
#################################################################
data_list <- list()
data_list[[1]] <- using
data_list[[2]] <- rbind(treat_1,treat_3,treat_2,treat_4, control_1)
data_list[[3]] <- rbind(treat_1,treat_3,treat_2,treat_4, control_2)
data_list[[4]] <- rbind(treat_1, control_1,control_2)
data_list[[5]] <- rbind(treat_1, control_1)
data_list[[6]] <- rbind(treat_1, control_2)
data_list[[7]] <- rbind(treat_3, control_1,control_2)
data_list[[8]] <- rbind(treat_3, control_1)
data_list[[9]] <- rbind(treat_3, control_2)
data_list[[10]] <- rbind(treat_2, control_1, control_2)
data_list[[11]] <- rbind(treat_2, control_1)
data_list[[12]] <- rbind(treat_2, control_2)
data_list[[13]] <- rbind(treat_4, control_1, control_2)
data_list[[14]] <- rbind(treat_4, control_1)
data_list[[15]] <- rbind(treat_4, control_2)

title_list <- c("Full Sample",
                "Full Treatment vs Uber and No Medicaid",
                "Full Treatment vs No Uber and No Medicaid",
                "Uber after Medicaid vs Full Control",
                "Uber after Medicaid vs Uber and No Medicaid",
                "Uber after Medicaid vs No Uber and No Medicaid",
                "Uber and Medicaid vs Full Control",
                "Uber and Medicaid vs Uber and No Medicaid",
                "Uber and Medicaid vs No Uber and No Medicaid",
                "Uber before Medicaid vs Full Control",
                "Uber before Medicaid vs Uber and No Medicaid",
                "Uber before Medicaid vs No Uber and No Medicaid",
                "No Uber and Medicaid vs Full Control",
                "No Uber and Medicaid vs Uber and No Medicaid",
                "No Uber and Medicaid vs No Uber and No Medicaid")

for(i in c(1,2,4,5,7,8,10,11,13,14)){
  # Full Sample
  estab_list[[i]] <- gsynth(Y = "estab_pop",
                            D = "Medicaid",
                            X = c("Labor.Force",
                                  "rcptot",
                                  "HHI_lower"),
                            data = data_list[[i]],
                            index = c("cty_st_numeric","years"),
                            weight = "lf_avg",
                            force = "two-way", 
                            # r = c(0, 1),
                            min.T0 = 8,
                            CV = TRUE, 
                            se = TRUE,
                            EM = TRUE,
                            # estimator = "mc",
                            # nlambda = 10,
                            # inference = "parametric",
                            inference = "nonparametric",
                            nboots = 1000, 
                            parallel = TRUE,
                            cl =  "st",
                            cores = 3)
  
  # Full Sample
  rcp_list[[i]] <- gsynth(Y = "receipt_estab",
                          D = "Medicaid",
                          X = c("Labor.Force",
                                "estab",
                                "HHI_lower",
                                "Uber_active_num"),
                          data = data_list[[i]],
                          index = c("cty_st_numeric","years"),
                          weight = "lf_avg",
                          force = "two-way", 
                          # r = c(0, 1),
                          min.T0 = 8,
                          CV = TRUE, 
                          se = TRUE,
                          EM = TRUE,
                          # estimator = "mc",
                          # nlambda = 10,
                          # inference = "parametric",
                          inference = "nonparametric",
                          nboots = 1000, 
                          parallel = TRUE,
                          cl =  "st",
                          cores = 3)
  
}

for(i in c(3,6,9,12,15)){
  # Full Sample
  estab_list[[i]] <- gsynth(Y = "estab_pop",
                            D = "Medicaid",
                            X = c("Labor.Force",
                                  "rcptot",
                                  "HHI_lower"),
                            data = data_list[[i]],
                            index = c("cty_st_numeric","years"),
                            weight = "lf_avg",
                            force = "two-way", 
                            # r = c(0, 1),
                            min.T0 = 8,
                            CV = TRUE, 
                            se = TRUE,
                            EM = TRUE,
                            # estimator = "mc",
                            # nlambda = 10,
                            # inference = "parametric",
                            inference = "nonparametric",
                            nboots = 1000, 
                            parallel = TRUE,
                            cl =  "st",
                            cores = 3)

  # Full Sample
  rcp_list[[i]] <- gsynth(Y = "receipt_estab",
                            D = "Medicaid",
                            X = c("Labor.Force",
                                  "estab",
                                  "HHI_lower"),
                            data = data_list[[i]],
                            index = c("cty_st_numeric","years"),
                            weight = "lf_avg",
                            force = "two-way", 
                            # r = c(0, 1),
                            min.T0 = 8,
                            CV = TRUE, 
                            se = TRUE,
                            EM = TRUE,
                            # estimator = "mc",
                            # nlambda = 10,
                            # inference = "parametric",
                            inference = "nonparametric",
                            nboots = 1000, 
                            parallel = TRUE,
                            cl =  "st",
                            cores = 3)
  
}



for(i in seq_along(data_list)){
    print(paste0("estab"," ",title_list[[i]]))
    print(round(estab_list[[i]]$att.avg,4))
    print(round(sd(estab_list[[i]]$att.avg.boot),4))
    print(round(2*pt(-abs(estab_list[[i]]$att.avg/sd(estab_list[[i]]$att.avg.boot)),df=estab_list[[i]]$N-1),3))
    
    # plot(estab_list[[i]], 
    #      type = "counterfactual",
    #      xlab = "Years to Medicaid Expansion",
    #      main = paste0("Counterfactual"," ",title_list[[i]]),
    #      theme.bw = TRUE)
    # plot(estab_list[[i]],
    #      main = paste0("ATT"," ",title_list[[i]]))
    # cumuEff(estab_list[[i]], cumu = TRUE, id = NULL, period = NULL)
    # plot(estab_list[[i]], type = "loadings")
}

for(i in seq_along(data_list)){
  
    print(paste0("rcp"," ",title_list[[i]]))
    print(round(rcp_list[[i]]$att.avg,1))
    print(round(sd(rcp_list[[i]]$att.avg.boot),1))
    print(round(2*pt(-abs(rcp_list[[i]]$att.avg/sd(rcp_list[[i]]$att.avg.boot)),df=rcp_list[[i]]$N-1),2))
    # plot(rcp_list[[i]], 
    #      type = "counterfactual",
    #      xlab = "Years to Medicaid Expansion",
    #      main = paste0("Counterfactual"," ",title_list[[i]]),
    #      theme.bw = TRUE)
    # plot(rcp_list[[i]],
    #      main = paste0("ATT"," ",title_list[[i]]))
    # cumuEff(rcp_list[[i]], cumu = TRUE, id = NULL, period = NULL)
    # plot(rcp_list[[i]], type = "loadings")
}


##########################
library(panelView)

state_view <- using_data_list[[1]] %>% 
  group_by(st,years) %>%
  summarize(Medcaid = mean(Medicaid),
            estab_pop = weighted.mean(estab_pop,w = lf_avg))
state_view <- as.data.frame(state_view)

panelView(estab_pop ~ Medcaid,
          data = state_view,
          index = c("st","years"),
          pre.post = TRUE) 

panelView(estab_pop ~ Medcaid,
          data = state_view,
          index = c("st","years"),
          pre.post = TRUE,
          type = "outcome") + 
  ylab("Nonemployer Estab./Labor Force") + 
  theme(plot.title = element_text(size=30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 30, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(3, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))


