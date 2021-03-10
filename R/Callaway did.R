# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(devtools)
# devtools::install_github("bcallaway11/did")
library(did)
library(dplyr)
library(ggplot2)
library(scales)

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
if(Sys.info()[["nodename"]]=="ES-BGLASNER3"){
  # Root folder
  path_project <- "E:/Dropbox/PhD Requirements"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
# Path where matched samples should be saved 
path_output <- paste0(path_project,"\\ACA and alt\\output")

setwd(paste0(path_data))

options(scipen=10000)
set.seed(42)

load("medicaid_analysis_data_total.RData")

# load("medicaid_analysis_data_allnonemployers.RData")
# using <- subset(using, naics=="48-49")

using <- within(using, quantile <- as.integer(cut(HHI_lower, quantile(HHI_lower, probs=seq(0,1,.01), na.rm = TRUE), include.lowest=TRUE)))
using$quantile <- as.numeric(as.character(using$quantile))
#################################################################
#                         Callaway DID set up                   #
#################################################################

###
Medicaid_subset <- subset(using, Medicaid==1)
Medicaid_subset <- Medicaid_subset[order(Medicaid_subset$id, abs(Medicaid_subset$years) ), ]
Medicaid_subset <- Medicaid_subset[ !duplicated(Medicaid_subset$id), ]  
Medicaid_subset$first.treat <- Medicaid_subset$years
Medicaid_subset <- Medicaid_subset %>% select("id","first.treat")

using <- merge(using, Medicaid_subset, all = TRUE)
using$first.treat[is.na(using$first.treat)] <- 0
using$id_numeric <- as.numeric(using$id)
# ###
# Medicaid_subset <- subset(using, Uber_active_num==1)
# Medicaid_subset <- Medicaid_subset[order(Medicaid_subset$id, abs(Medicaid_subset$years) ), ]
# Medicaid_subset <- Medicaid_subset[ !duplicated(Medicaid_subset$id), ]  
# Medicaid_subset$first.treat.uber <- Medicaid_subset$years
# Medicaid_subset <- Medicaid_subset %>% select("id","first.treat.uber")
# 
# using <- merge(using, Medicaid_subset, all = TRUE)
# using$first.treat.uber[is.na(using$first.treat.uber)] <- 0
# ###
# Medicaid_subset <- subset(using, Medicaid==1 & Uber_active_num==1)
# Medicaid_subset <- Medicaid_subset[order(Medicaid_subset$id, abs(Medicaid_subset$years) ), ]
# Medicaid_subset <- Medicaid_subset[ !duplicated(Medicaid_subset$id), ]  
# Medicaid_subset$first.treat.uber.medicaid <- Medicaid_subset$years
# Medicaid_subset <- Medicaid_subset %>% select("id","first.treat.uber.medicaid")
# 
# using <- merge(using, Medicaid_subset, all = TRUE)
# using$first.treat.uber.medicaid[is.na(using$first.treat.uber.medicaid)] <- 0
# ###

Med_diff_estabpop <-  att_gt(yname="estab_pop",
                             gname="first.treat",
                             idname="id_numeric",
                             tname="years",
                             xformla=~quantile + Labor.Force,
                             data=using,
                             weightsname = "lf_avg",
                             control_group = "notyettreated",
                             clustervars = "st",
                             bstrap = TRUE,
                             panel = TRUE,
                             # est_Method = "reg",
                             print_details=FALSE,
                             cores = 4)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
es_estabpop_df <- data.frame(es_estabpop[["egt"]],es_estabpop[["att.egt"]],es_estabpop[["se.egt"]])
names(es_estabpop_df) <- c("Period", "ATT", "se")
es_estabpop_df$Var <- "Estab./Labor Force"
ggdid(es_estabpop)
summary(es_estabpop)


###
Med_diff_rcp <-  att_gt(yname="receipt_estab",
                        gname="first.treat",
                        idname="id_numeric",
                        tname="years",
                        xformla=~quantile + Labor.Force,
                        data=using,
                        weightsname = "lf_avg",
                        control_group = "notyettreated",
                        clustervars = "st",
                        bstrap = TRUE,
                        panel = TRUE,
                        # est_Method = "reg",
                        print_details=FALSE,
                        cores = 4)

es_rcp <- aggte(Med_diff_rcp, type="dynamic")
es_rcp_df <- data.frame(es_rcp[["egt"]],es_rcp[["att.egt"]],es_rcp[["se.egt"]])
names(es_rcp_df) <- c("Period", "ATT", "se")
es_rcp_df$Var <- "Avg. Rec."
summary(es_rcp)
ggdid(es_rcp)

es_df <- rbind(es_estabpop_df,es_rcp_df)
es_df$Treatment <- "Pre-Expansion"
es_df$Treatment[es_df$Period>-1] <- "Post-Expansion"


myColors <- c("green3","black")
names(myColors) <- levels(es_df$Treatment)
colScale <- scale_colour_manual(name = "grp",values = myColors)

p1 <-ggplot(data = es_df, 
       aes(x = Period,
           color = Treatment)) +
  geom_point(aes(y = ATT), size =3) +
  geom_errorbar(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), size = 1)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "HHI Quantile") +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y.right = element_text(margin = margin(r=15)),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.title = element_blank()) + 
  colScale +
  scale_y_continuous(label=comma) +
  facet_grid( Var ~ ., 
              scales = "free_y") 

setwd(path_output)
pdf(file = paste0("Event_study_allnonemployer.pdf"), width = 12, height = 10)
plot(p1)
dev.off()


###################################################
###       Industry Analysis                     ###
###################################################
setwd(path_data)
load("medicaid_analysis_data_allnonemployers.RData")
industry_list <- unique(using$naics)
att_estabpop_list <- list()
att_rcpestab_list <- list()

#############
Medicaid_subset <- subset(using, Medicaid==1)
Medicaid_subset <- Medicaid_subset[order(Medicaid_subset$id, abs(Medicaid_subset$years) ), ]
Medicaid_subset <- Medicaid_subset[ !duplicated(Medicaid_subset$id), ]  
Medicaid_subset$first.treat <- Medicaid_subset$years
Medicaid_subset <- Medicaid_subset %>% select("id","first.treat")

using <- merge(using, Medicaid_subset, all = TRUE)
using$first.treat[is.na(using$first.treat)] <- 0
using$id_numeric <- as.numeric(using$id)
#############

allnonemployers <- using

for (k in seq_along(industry_list)) {
  using <- subset(allnonemployers, naics==industry_list[[k]])

  Med_diff_estabpop <-  att_gt(yname="estab_pop",
                               gname="first.treat",
                               idname="id_numeric",
                               tname="years",
                               xformla=~quantile + Labor.Force,
                               data=using,
                               weightsname = "lf_avg",
                               control_group = "notyettreated",
                               clustervars = "st",
                               bstrap = TRUE,
                               panel = TRUE,
                               # est_Method = "reg",
                               print_details=FALSE,
                               cores = 4)
  
  att_estabpop_list[[k]] <- aggte(Med_diff_estabpop, type="dynamic")
  
  
  ###
  Med_diff_rcp <-  att_gt(yname="receipt_estab",
                          gname="first.treat",
                          idname="id_numeric",
                          tname="years",
                          xformla=~quantile + Labor.Force,
                          data=using,
                          weightsname = "lf_avg",
                          control_group = "notyettreated",
                          clustervars = "st",
                          bstrap = TRUE,
                          panel = TRUE,
                          # est_Method = "reg",
                          print_details=FALSE,
                          cores = 4)
  
  att_rcpestab_list[[k]] <- aggte(Med_diff_rcp, type="dynamic")

}

att <- c(att_estabpop_list[[1]]$overall.att,
         att_estabpop_list[[2]]$overall.att,
         att_estabpop_list[[3]]$overall.att,
         att_estabpop_list[[4]]$overall.att,
         att_estabpop_list[[5]]$overall.att,
         att_estabpop_list[[6]]$overall.att,
         att_estabpop_list[[7]]$overall.att,
         att_estabpop_list[[8]]$overall.att,
         att_estabpop_list[[9]]$overall.att,
         att_estabpop_list[[10]]$overall.att,
         att_estabpop_list[[11]]$overall.att,
         att_estabpop_list[[12]]$overall.att,
         att_estabpop_list[[13]]$overall.att,
         att_estabpop_list[[14]]$overall.att,
         att_estabpop_list[[15]]$overall.att,
         att_estabpop_list[[16]]$overall.att,
         att_estabpop_list[[17]]$overall.att,
         att_estabpop_list[[18]]$overall.att,
         att_estabpop_list[[19]]$overall.att,
         att_rcpestab_list[[1]]$overall.att,
         att_rcpestab_list[[2]]$overall.att,
         att_rcpestab_list[[3]]$overall.att,
         att_rcpestab_list[[4]]$overall.att,
         att_rcpestab_list[[5]]$overall.att,
         att_rcpestab_list[[6]]$overall.att,
         att_rcpestab_list[[7]]$overall.att,
         att_rcpestab_list[[8]]$overall.att,
         att_rcpestab_list[[9]]$overall.att,
         att_rcpestab_list[[10]]$overall.att,
         att_rcpestab_list[[11]]$overall.att,
         att_rcpestab_list[[12]]$overall.att,
         att_rcpestab_list[[13]]$overall.att,
         att_rcpestab_list[[14]]$overall.att,
         att_rcpestab_list[[15]]$overall.att,
         att_rcpestab_list[[16]]$overall.att,
         att_rcpestab_list[[17]]$overall.att,
         att_rcpestab_list[[18]]$overall.att,
         att_rcpestab_list[[19]]$overall.att)

se <- c(att_estabpop_list[[1]]$overall.se,
        att_estabpop_list[[2]]$overall.se,
        att_estabpop_list[[3]]$overall.se,
        att_estabpop_list[[4]]$overall.se,
        att_estabpop_list[[5]]$overall.se,
        att_estabpop_list[[6]]$overall.se,
        att_estabpop_list[[7]]$overall.se,
        att_estabpop_list[[8]]$overall.se,
        att_estabpop_list[[9]]$overall.se,
        att_estabpop_list[[10]]$overall.se,
        att_estabpop_list[[11]]$overall.se,
        att_estabpop_list[[12]]$overall.se,
        att_estabpop_list[[13]]$overall.se,
        att_estabpop_list[[14]]$overall.se,
        att_estabpop_list[[15]]$overall.se,
        att_estabpop_list[[16]]$overall.se,
        att_estabpop_list[[17]]$overall.se,
        att_estabpop_list[[18]]$overall.se,
        att_estabpop_list[[19]]$overall.se,
        att_rcpestab_list[[1]]$overall.se,
        att_rcpestab_list[[2]]$overall.se,
        att_rcpestab_list[[3]]$overall.se,
        att_rcpestab_list[[4]]$overall.se,
        att_rcpestab_list[[5]]$overall.se,
        att_rcpestab_list[[6]]$overall.se,
        att_rcpestab_list[[7]]$overall.se,
        att_rcpestab_list[[8]]$overall.se,
        att_rcpestab_list[[9]]$overall.se,
        att_rcpestab_list[[10]]$overall.se,
        att_rcpestab_list[[11]]$overall.se,
        att_rcpestab_list[[12]]$overall.se,
        att_rcpestab_list[[13]]$overall.se,
        att_rcpestab_list[[14]]$overall.se,
        att_rcpestab_list[[15]]$overall.se,
        att_rcpestab_list[[16]]$overall.se,
        att_rcpestab_list[[17]]$overall.se,
        att_rcpestab_list[[18]]$overall.se,
        att_rcpestab_list[[19]]$overall.se)

dependent <- c("estab/pop","estab/pop","estab/pop","estab/pop","estab/pop",
               "estab/pop","estab/pop","estab/pop","estab/pop","estab/pop",
               "estab/pop","estab/pop","estab/pop","estab/pop","estab/pop",
               "estab/pop","estab/pop","estab/pop","estab/pop",
               "avg receipts","avg receipts","avg receipts","avg receipts","avg receipts",
               "avg receipts","avg receipts","avg receipts","avg receipts","avg receipts",
               "avg receipts","avg receipts","avg receipts","avg receipts","avg receipts",
               "avg receipts","avg receipts","avg receipts","avg receipts")

callaway_est <- data.frame(industry_list, dependent,att,se)
names(callaway_est) <- c("naics", "dependent","att","se")


###################################
using <- subset(allnonemployers,years==2018)

estab_counts <- aggregate(x=using$estab, by = list(using$naics, using$Medicaid), FUN = sum)
colnames(estab_counts)[1] <- "naics"
colnames(estab_counts)[2] <- "Medicaid"
colnames(estab_counts)[3] <- "estab"
estab_counts <- subset(estab_counts, Medicaid==1)

rcptot_counts <- aggregate(x=using$rcptot, by = list(using$naics, using$Medicaid), FUN = sum)
colnames(rcptot_counts)[1] <- "naics"
colnames(rcptot_counts)[2] <- "Medicaid"
colnames(rcptot_counts)[3] <- "rcptot"
rcptot_counts <- subset(rcptot_counts, Medicaid==1)

industry_name <- data.frame(cbind(c("00","11","21","22","23","31-33","42","44-45","48-49","51","52",
                                    "53","54","56","61","62","71","72","81"),
                                  c("All Nonemployers","Agr./Forestry/Fish/Hunting","Mining/Quarrying/Oil/Gas","Utilities","Construction",
                                    "Manufacturing","Wholesale Trade","Retail Trade","Trans./Warehousing",
                                    "Information","Finance/Insurance","Real Estate/Rental/Leasing","Prof./Sci./Technical Services",
                                    "Admin./Support/Waste/Remediation","Educational Services","Health Care/Social Assist.",
                                    "Arts/Entertainment/Recreation","Accommodation/Food Services","Other Services")))
colnames(industry_name)[1]<- "naics"
colnames(industry_name)[2]<- "industry_names"

industry_name$industry_names <- factor(industry_name$industry_names,
                                       levels = c("All Nonemployers","Agr./Forestry/Fish/Hunting","Mining/Quarrying/Oil/Gas","Utilities","Construction",
                                                  "Manufacturing","Wholesale Trade","Retail Trade","Trans./Warehousing",
                                                  "Information","Finance/Insurance","Real Estate/Rental/Leasing","Prof./Sci./Technical Services",
                                                  "Admin./Support/Waste/Remediation","Educational Services","Health Care/Social Assist.",
                                                  "Arts/Entertainment/Recreation","Accommodation/Food Services","Other Services"))
###################################

callaway_est$labor_force <- 98219447

callaway_est <- merge(callaway_est, estab_counts)
callaway_est <- merge(callaway_est, rcptot_counts)
callaway_est <- merge(callaway_est, industry_name)

callaway_est$Upper <- callaway_est$att + 1.96*callaway_est$se
callaway_est$Lower <- callaway_est$att - 1.96*callaway_est$se

callaway_est$estab_pop <- callaway_est$estab/callaway_est$labor_force
callaway_est$rcptot_estab <- callaway_est$rcptot/callaway_est$estab*1000

callaway_est$ATT_perc[callaway_est$dependent=="estab/pop"] <- callaway_est$att[callaway_est$dependent=="estab/pop"]/callaway_est$estab_pop[callaway_est$dependent=="estab/pop"]
callaway_est$ATT_perc[callaway_est$dependent=="avg receipts"] <- callaway_est$att[callaway_est$dependent=="avg receipts"]/callaway_est$rcptot_estab[callaway_est$dependent=="avg receipts"]

callaway_est$ci.higher_perc[callaway_est$dependent=="estab/pop"] <- callaway_est$Upper[callaway_est$dependent=="estab/pop"]/callaway_est$estab_pop[callaway_est$dependent=="estab/pop"]
callaway_est$ci.higher_perc[callaway_est$dependent=="avg receipts"] <- callaway_est$Upper[callaway_est$dependent=="avg receipts"]/callaway_est$rcptot_estab[callaway_est$dependent=="avg receipts"]

callaway_est$ci.lower_perc[callaway_est$dependent=="estab/pop"] <- callaway_est$Lower[callaway_est$dependent=="estab/pop"]/callaway_est$estab_pop[callaway_est$dependent=="estab/pop"]
callaway_est$ci.lower_perc[callaway_est$dependent=="avg receipts"] <- callaway_est$Lower[callaway_est$dependent=="avg receipts"]/callaway_est$rcptot_estab[callaway_est$dependent=="avg receipts"]

###################################

setwd(path_output)


p1 <- ggplot(data = subset(callaway_est,naics!=21 & naics!=22),
            aes(y = industry_names)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(aes(x = att), size = 2) +
  geom_errorbarh(aes(xmax = Upper, xmin = Lower, height = .4)) +
  ylab("NAICS Industry") + xlab("Effect of Medicaid Expansion") +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
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
        legend.background = element_rect(fill=NA)) +
  scale_x_continuous(label=comma) +
  facet_wrap( ~ dependent, scales = "free_x")

pdf(file = paste0("callaway_results_ATT_avg.pdf"), width = 18, height = 15)
plot(p1)
dev.off()

p2 <- ggplot(data = subset(callaway_est,naics!=21 & naics!=22),
             aes(y = industry_names)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(aes(x = ATT_perc), size = 2) +
  geom_errorbarh(aes(xmax = ci.higher_perc, xmin = ci.lower_perc, height = .4)) +
  ylab("NAICS Industry") + xlab("Effect of Medicaid Expansion") +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
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
        legend.background = element_rect(fill=NA)) +
  scale_x_continuous(label=percent) +
  facet_wrap( ~ dependent, scales = "free_x")

setwd(path_output)
pdf(file = paste0("callaway_results_ATT_avg_perc.pdf"), width = 18, height = 15)
plot(p2)
dev.off()
