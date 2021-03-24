# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex


##################
###  Library   ###
##################

# library(devtools)
# install_github("xuyiqing/gsynth")
# devtools::install_github("bcallaway11/did")
library(did)
library(ggplot2)
library(dplyr)

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

using_data_list <- list()
summary_list <- list()

load("medicaid_analysis_data_allnonemployers.RData")
industry_list <- unique(using$naics)

for(l in seq_along(industry_list)){
  using_data_list[[l]] <- subset(using, naics==industry_list[[l]])
}


for(t in seq_along(using_data_list)){
  using <- using_data_list[[t]]
  
    using$Uber_active_num <- using$Uber_active
    using$Uber_active[using$Uber_active==0] <- "No Uber"
    using$Uber_active[using$Uber_active==1] <- "Uber Active"
    
    estab_list <- list()
    estab_ag_list <- list()
    rcp_list <- list()
    rcp_ag_list <- list()
    using$id <- factor(using$id)
    
    Medicaid_subset <- subset(using, Medicaid==1)
    Medicaid_subset <- Medicaid_subset[order(Medicaid_subset$id, abs(Medicaid_subset$years) ), ]
    Medicaid_subset <- Medicaid_subset[ !duplicated(Medicaid_subset$id), ]  
    Medicaid_subset$first.treat <- Medicaid_subset$years
    Medicaid_subset <- Medicaid_subset %>% select("id","first.treat")
    
    using <- merge(using, Medicaid_subset, all = TRUE)
    using$first.treat[is.na(using$first.treat)] <- 0
    using$id_numeric <- as.numeric(using$id)
    
    
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
    
    # Treatment Group - Medicaid and Uber, Does the county receive Uber before Medicaid expanded
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
    
    title_list <- c("M = 1",
                    "M = 1",
                    "M = 1",
                    "U post M",
                    "U post M",
                    "U post M",
                    "U and M",
                    "U and M",
                    "U and M",
                    "U pre M",
                    "U pre M",
                    "U pre M",
                    "U = 0 & M = 1",
                    "U = 0 & M = 1",
                    "U = 0 & M = 1")
    
    for(i in seq_along(data_list)){
      
      estab_list[[i]] <- att_gt(yname="estab_pop",
                                gname="first.treat",
                                idname="cty_st_numeric",
                                tname="years",
                                xformla=~ quantile + rcptot,
                                data=data_list[[i]],
                                weightsname = "lf_avg",
                                # control_group = "notyettreated",
                                control_group = "nevertreated",
                                # control_group = c("nevertreated","notyettreated"),
                                clustervars = "cty_st_numeric",
                                bstrap = TRUE,
                                panel = TRUE,
                                est_method = "reg",
                                print_details=FALSE,
                                cores = 4,
                                alp = .05)
      
      
      estab_ag_list[[i]] <-aggte(estab_list[[i]],
                                 # type = "group", # This will create a group level average ATT and SE and can be plotted
                                 # type = "simple", # This will create an overall simple ATT and teh SE
                                 # type = "calendar",
                                 type = "dynamic",
                                 # max_e = 4,
                                 clustervars = "cty_st_numeric",
                                 na.rm = TRUE,
                                 alp = .05)
      
      rcp_list[[i]] <- att_gt(yname="receipt_estab",
                              gname="first.treat",
                              idname="cty_st_numeric",
                              tname="years",
                              xformla=~ quantile + estab,
                              data=data_list[[i]],
                              weightsname = "lf_avg",
                              # control_group = "notyettreated",
                              control_group = "nevertreated",
                              # control_group = c("nevertreated","notyettreated"),
                              clustervars = "cty_st_numeric",
                              bstrap = TRUE,
                              panel = TRUE,
                              est_method = "reg",
                              print_details=FALSE,
                              cores = 4,
                              alp = .05)
      
      
      rcp_ag_list[[i]] <-aggte(rcp_list[[i]],
                               # type = "group", # This will create a group level average ATT and SE and can be plotted
                               # type = "simple", # This will create an overall simple ATT and teh SE
                               # type = "calendar",
                               type = "dynamic",
                               # max_e = 4,
                               clustervars = "cty_st_numeric",
                               na.rm = TRUE,
                               alp = .05)
    }
    
    
    coef_estab_list <- list()
    se_estab_list <- list()
    upper_estab_list <- list()
    lower_estab_list <- list()
    
    for(i in seq_along(data_list)){
      print(paste0("estab"," ",title_list[[i]]))
      
      print(round(estab_ag_list[[i]]$overall.att,4))
      print(round(estab_ag_list[[i]]$overall.se,4))
      tscore <- as.numeric(estab_ag_list[[i]]$overall.att/estab_ag_list[[i]]$overall.se)
      print(tscore)
      if(abs(tscore) >1.645){
        print(paste0("*"))
      }
      if(abs(tscore) >1.960){
        print(paste0("**"))
      }
      if(abs(tscore) >2.576){
        print(paste0("***"))
      }
      coef_estab_list[[i]] <- estab_ag_list[[i]]$overall.att
      se_estab_list[[i]] <- estab_ag_list[[i]]$overall.se
      upper_estab_list[[i]] <- estab_ag_list[[i]]$overall.att + estab_ag_list[[i]]$overall.se*1.96
      lower_estab_list[[i]] <- estab_ag_list[[i]]$overall.att - estab_ag_list[[i]]$overall.se*1.96
    }
    
    for(i in seq_along(data_list)){
      print(paste0("rcp"," ",title_list[[i]]))
      print(round(rcp_ag_list[[i]]$overall.att,1))
      print(round(rcp_ag_list[[i]]$overall.se,1))
      tscore <- as.numeric(rcp_ag_list[[i]]$overall.att/rcp_ag_list[[i]]$overall.se)
      print(tscore)
      if(abs(tscore) >1.645){
        print(paste0("*"))
      }
      if(abs(tscore) >1.960){
        print(paste0("**"))
      }
      if(abs(tscore) >2.576){
        print(paste0("***"))
      }
    }
    
    # summary(estab_ag_list[[5]])
    # summary(rcp_ag_list[[1]])
    
    # ggdid(estab_ag_list[[5]]) + geom_hline(yintercept = 0, color = "black")
    # ggdid(rcp_ag_list[[1]]) + geom_hline(yintercept = 0, color = "black")
    
    
    summary <- as.data.frame(c(coef_estab_list,se_estab_list, upper_estab_list,lower_estab_list))
    
    summary <- cbind(title_list,
                     do.call(rbind.data.frame, coef_estab_list),
                  do.call(rbind.data.frame, se_estab_list),
                  do.call(rbind.data.frame, upper_estab_list),
                  do.call(rbind.data.frame, lower_estab_list))
    names(summary) <- c("subset","Coef","se", "upper","lower")
    
    summary$subset_uber  <- c("Full Treatment",
                                  "Full Treatment",
                                  "Full Treatment",
                                  "Uber after Medicaid",
                                  "Uber after Medicaid",
                                  "Uber after Medicaid",
                                  "Uber and Medicaid Simultaneously",
                                  "Uber and Medicaid Simultaneously",
                                  "Uber and Medicaid Simultaneously",
                                  "Uber before Medicaid",
                                  "Uber before Medicaid",
                                  "Uber before Medicaid",
                                  "Never Receives Uber",
                                  "Never Receives Uber",
                                  "Never Receives Uber")
    summary$subset_uber  <- factor(summary$subset_uber, 
                                   levels = c("Full Treatment",
                                              "Uber before Medicaid",
                                              "Uber and Medicaid Simultaneously",
                                              "Uber after Medicaid",
                                              "Never Receives Uber"))
    
    summary$control_uber  <- c("Full Control Group",
                               "Controls that get Uber",
                               "Controls that do not get Uber",
                               "Full Control Group",
                               "Controls that get Uber",
                               "Controls that do not get Uber",
                               "Full Control Group",
                               "Controls that get Uber",
                               "Controls that do not get Uber",
                               "Full Control Group",
                               "Controls that get Uber",
                               "Controls that do not get Uber",
                               "Full Control Group",
                               "Controls that get Uber",
                               "Controls that do not get Uber")
    summary$control_uber <- factor(summary$control_uber, 
                             levels = c("Full Control Group",
                                        "Controls that get Uber",
                                        "Controls that do not get Uber"))
    summary$subset <- factor(summary$subset, 
                             levels = c("M = 1",
                                        "U pre M",
                                        "U and M",
                                        "U post M",
                                        "U = 0 & M = 1"))
    n_1 <- NROW(unique(treat_1$id))
    n_2 <- NROW(unique(treat_2$id))
    n_3 <- NROW(unique(treat_3$id))
    n_4 <- NROW(unique(treat_4$id))
    n_5 <- NROW(unique(control_1$id))
    n_6 <- NROW(unique(control_2$id))
    # Treatment Group 1- Uber After Medicaid, Does the county receive Uber after Medicaid already expanded
    # Treatment Group 2- Medicaid After Uber, Does the county receive Uber before Medicaid expanded
    # Treatment Group 3- Medicaid and Uber, Does the county receive Uber before Medicaid expanded
    # Treatment Group 4- No Uber and Medicaid, Does the county receive Uber, after Medicaid already expanded
    # Control Group 5- Uber but no Medicaid, Does the county receive Uber but no Medicaid expansion
    # Control Group 6- No Uber and no Medicaid, Does the county receive neither Uber or Medicaid expansion
    
    summary$n <- c(n_1 + n_2 + n_3 + n_4 + n_5 + n_6,
                   n_1 + n_2 + n_3 + n_4 + n_5,
                   n_1 + n_2 + n_3 + n_4 + n_6,
                   n_1 + n_5 + n_6,
                   n_1 + n_5,
                   n_1 + n_6,
                   n_3 + n_5 + n_6,
                   n_3 + n_5,
                   n_3 + n_6,
                   n_2 + n_5 + n_6,
                   n_2 + n_5,
                   n_2 + n_6,
                   n_4 + n_5 + n_6,
                   n_4 + n_5,
                   n_4 + n_6)
    
    summary_list[[t]] <- summary
}

summary_list[[1]]$industry <- "All Nonemployers"
summary_list[[7]]$industry <- "Trans. and Warehousing"

summary_list[[1]]$observed <- mean(using_data_list[[1]]$estab_pop[using_data_list[[1]]$years==2013])
summary_list[[7]]$observed <- mean(using_data_list[[7]]$estab_pop[using_data_list[[7]]$years==2013])

summary<- rbind(summary_list[[1]],summary_list[[7]])
summary$Coef_per <- summary$Coef/summary$

dodge <- position_dodge(width=0.75) 
p <- ggplot(data = summary,
             aes(y = subset_uber,
                 shape = subset_uber)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(aes(x = Coef, size = 3),position = dodge) +
  geom_errorbarh(aes(xmax = upper, xmin = lower, height = .4), size = 1 ,position = dodge) +
  ylab("Treatment Group") + 
  xlab("Effect of Medicaid Expansion") +
  scale_shape_manual(values=c(15,16,17,18,19))+
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
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA)) + 
  guides(shape = FALSE,
         color = FALSE,
         size = FALSE) + facet_grid(industry ~ control_uber)
p
setwd(path_output)
pdf(file = "Data_subset_callaway.pdf", width = 23,height = 15)
plot(p)
dev.off()




##########################
library(panelView)

state_view <- using_data_list[[1]] %>% 
                  group_by(st,years) %>%
                  summarize(Medcaid = mean(Medicaid),
                            estab_pop = weighted.mean(estab_pop,w = lf_avg))
state_view <- as.data.frame(state_view)

p <- panelView(estab_pop ~ Medcaid,
               data = state_view,
               index = c("st","years"),
               pre.post = TRUE)  + 
  ylab("State FIPS") + 
  theme(plot.title = element_text(size=30),
        axis.text.x = element_text(size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 30, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30),
        plot.background = element_rect(fill = "white"),
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

pdf(file = "state_panel_view.pdf", width = 12,height = 13)
p
dev.off()


p <- panelView(estab_pop ~ Medcaid,
               data = state_view,
               index = c("st","years"),
               pre.post = TRUE,
               type = "outcome") +
  ylab("Nonemployer Estab./Labor Force") + 
  xlab("Year") +
  labs(title = "State Variation in Nonemployer Estab.") +
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
p
pdf(file = "state_outcome_view.pdf", width = 10,height = 8)
p
dev.off()



p <- ggdid(estab_ag_list[[1]]) + 
  geom_hline(yintercept = 0, alpha = .2) + 
  labs(title = "") +
  theme(plot.title = element_text(size=30),
        axis.text.x = element_text(size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 30, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey90"),
        panel.spacing = unit(3, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "right",
        legend.title = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))

pdf(file = "event_study_estab.pdf", width = 10,height = 8)
p
dev.off()

p <- ggdid(rcp_ag_list[[1]]) + 
  geom_hline(yintercept = 0, alpha = .2) + 
  labs(title = "") +
  theme(plot.title = element_text(size=30),
        axis.text.x = element_text(size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 30, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey90"),
        panel.spacing = unit(3, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "right",
        legend.title = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))

pdf(file = "event_study_rcp.pdf", width = 12,height = 13)
p
dev.off()
