# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(stargazer)
library(sjPlot)
library(estimatr)
library(ggplot2)
library(broom)
library(scales)
library(devtools)
library(dplyr)
# install_github("sgaure/lfe")
# install_github("xuyiqing/gsynth")
# remotes::install_version("lfe", version="2.8-5.1")
library(lfe)
library(gsynth)
library(did)
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

industry_analysis <- 1
load("state_NES_ACS_CPS.RData")

load("LAU.RData")
LAU_st <- aggregate(x = LAU$`Labor Force`, by = list(LAU$st, LAU$YEAR.id), FUN = sum, na.rm = TRUE)
colnames(LAU_st)[1] <- "st"
colnames(LAU_st)[2] <- "YEAR.id"
colnames(LAU_st)[3] <- "Labor.Force"

state_aggs <- merge(state_aggs, LAU_st)
state_aggs$estab_pop <- state_aggs$estab/state_aggs$Labor.Force
estab_list <- list()

#################################################################
#                         Estab All                             #
#################################################################

estab_list[[1]] <- felm(estab_pop ~ Medicaid | st + YEAR.id | 0 | st,
                        data = state_aggs,
                        weights = state_aggs$Labor.Force)
estab_list[[2]] <- felm(Estab_pct_ACS ~ Medicaid | st + YEAR.id | 0 | st,
                        data = state_aggs,
                        weights = state_aggs$Labor.Force)
estab_list[[3]] <- felm(Estab_pct_CPS ~ Medicaid | st + YEAR.id | 0 | st,
                        data = state_aggs,
                        weights = state_aggs$Labor.Force)

#################################################################
#                         Stargazer                             #
#################################################################

stargazer(estab_list[[1]], estab_list[[2]], estab_list[[3]],
          keep = "Medicaid",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 4)

#################################################################
#                         Synthetic Control                     #
#################################################################

gsynth_1 <- gsynth(Y = "estab_pop",
                   D = "Medicaid",
                   X = c("estab",
                         "population",
                         "Labor.Force"),
                   data = state_aggs,
                   index = c("st","YEAR.id"),
                   weight = "Labor.Force",
                   force = "two-way", 
                   # r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   # EM = TRUE,
                   estimator = "mc",
                   nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 1000, 
                   parallel = TRUE,
                   # cl =  "st",
                   cores = 20)


gsynth_2 <- gsynth(Y = "Estab_pct_ACS",
                   D = "Medicaid",
                   X = c("estab",
                         "population",
                         "Labor.Force"),
                   data = state_aggs,
                   index = c("st","YEAR.id"),
                   weight = "Labor.Force",
                   force = "two-way", 
                   # r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   # EM = TRUE,
                   estimator = "mc",
                   nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 1000, 
                   parallel = TRUE,
                   # cl =  "st",
                   cores = 20)

gsynth_3 <- gsynth(Y = "Estab_pct_CPS",
                   D = "Medicaid",
                   X = c("estab",
                         "population",
                         "Labor.Force"),
                   data = state_aggs,
                   index = c("st","YEAR.id"),
                   weight = "Labor.Force",
                   force = "two-way", 
                   # r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   # EM = TRUE,
                   estimator = "mc",
                   nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 1000, 
                   parallel = TRUE,
                   # cl =  "st",
                   cores = 20)


print(gsynth_1)

counter_plot <- cbind(rownames(gsynth_1[["Y.bar"]]), data.frame(gsynth_1[["Y.bar"]], row.names=NULL))
colnames(counter_plot)[1] <- "Year"
counter_plot$Year <- as.numeric(as.character(counter_plot$Year))


plot_1 <- ggplot(data = counter_plot,
                 aes(x = Year)) + 
  geom_point(aes(y = Y.tr.bar, colour = "Treated"), size = 3) +
  geom_line(aes(y = Y.tr.bar, colour = "Treated"), size = 1.5) +
  geom_point(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 3) +
  geom_line(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 1.5) +
  geom_vline(xintercept = 2014) +
  ylab("Establishments/Labor Force, NES") + xlab("Year") +
  scale_colour_manual("",
                      breaks = c("Treated", "Synthetic Control"),
                      values = c("Treated"="red1", "Synthetic Control"="black")) +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA)) 

gap_plot <- cbind(rownames(gsynth_1[["est.att"]]), data.frame(gsynth_1[["est.att"]], row.names=NULL))
colnames(gap_plot)[1] <- "Period"
gap_plot$Period <- as.numeric(as.character(gap_plot$Period))

plot_2 <- ggplot(data = gap_plot,
                 aes(x = Period,
                     y = ATT)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin=CI.lower, ymax=CI.upper),alpha=0.2) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = .5, size = 1.25) +
  ylab("Treatment - Control") + xlab("Period") +
  # ylim(-0.002,0.009) +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        # axis.title.y = element_blank(),
        # axis.text.x = element_text(size = 20),
        # axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA)) 

###########################################

print(gsynth_2)

counter_plot <- cbind(rownames(gsynth_2[["Y.bar"]]), data.frame(gsynth_2[["Y.bar"]], row.names=NULL))
colnames(counter_plot)[1] <- "Year"
counter_plot$Year <- as.numeric(as.character(counter_plot$Year))


plot_3 <- ggplot(data = counter_plot,
                 aes(x = Year)) + 
  geom_point(aes(y = Y.tr.bar, colour = "Treated"), size = 3) +
  geom_line(aes(y = Y.tr.bar, colour = "Treated"), size = 1.5) +
  geom_point(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 3) +
  geom_line(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 1.5) +
  geom_vline(xintercept = 2014) +
  ylab("Uninc. Self-Emp./Labor Force, ACS") + xlab("Year") +
  scale_colour_manual("",
                      breaks = c("Treated", "Synthetic Control"),
                      values = c("Treated"="red1", "Synthetic Control"="black")) +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))   +
  scale_y_continuous(label=comma)  

gap_plot <- cbind(rownames(gsynth_2[["est.att"]]), data.frame(gsynth_2[["est.att"]], row.names=NULL))
colnames(gap_plot)[1] <- "Period"
gap_plot$Period <- as.numeric(as.character(gap_plot$Period))

plot_4 <- ggplot(data = gap_plot,
                 aes(x = Period,
                     y = ATT)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin=CI.lower, ymax=CI.upper),alpha=0.2) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = .5, size = 1.25) +
  ylab("Treatment - Control") + xlab("Period") +
  # ylim(-0.002,0.009) +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        # axis.title.y = element_blank(),
        # axis.text.x = element_text(size = 20),
        # axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))  +
  scale_y_continuous(label=comma)  

###########################################

print(gsynth_3)

counter_plot <- cbind(rownames(gsynth_3[["Y.bar"]]), data.frame(gsynth_3[["Y.bar"]], row.names=NULL))
colnames(counter_plot)[1] <- "Year"
counter_plot$Year <- as.numeric(as.character(counter_plot$Year))


plot_5 <- ggplot(data = counter_plot,
                 aes(x = Year)) + 
  geom_point(aes(y = Y.tr.bar, colour = "Treated"), size = 3) +
  geom_line(aes(y = Y.tr.bar, colour = "Treated"), size = 1.5) +
  geom_point(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 3) +
  geom_line(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 1.5) +
  geom_vline(xintercept = 2014) +
  ylab("Uninc. Self-Emp./Labor Force, CPS") + xlab("Year") +
  scale_colour_manual("",
                      breaks = c("Treated", "Synthetic Control"),
                      values = c("Treated"="red1", "Synthetic Control"="black")) +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))   +
  scale_y_continuous(label=comma)  

gap_plot <- cbind(rownames(gsynth_3[["est.att"]]), data.frame(gsynth_3[["est.att"]], row.names=NULL))
colnames(gap_plot)[1] <- "Period"
gap_plot$Period <- as.numeric(as.character(gap_plot$Period))

plot_6 <- ggplot(data = gap_plot,
                 aes(x = Period,
                     y = ATT)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin=CI.lower, ymax=CI.upper),alpha=0.2) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = .5, size = 1.25) +
  ylab("Treatment - Control") + xlab("Period") +
  # ylim(-0.002,0.009) +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        # axis.title.y = element_blank(),
        # axis.text.x = element_text(size = 20),
        # axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.background = element_rect(fill=NA))  +
  scale_y_continuous(label=comma)  


setwd(path_output)
pdf(file = paste0("synthetic_total_NES_counterfactual.pdf"), width = 10, height = 8)
plot(plot_1)
dev.off()

pdf(file = paste0("synthetic_total_NES_att.pdf"), width = 10, height = 8)
plot(plot_2)
dev.off()

pdf(file = paste0("synthetic_total_ACS_counterfactual.pdf"), width = 10, height = 8)
plot(plot_3)
dev.off()

pdf(file = paste0("synthetic_total_ACS_att.pdf"), width = 10, height = 8)
plot(plot_4)
dev.off()

pdf(file = paste0("synthetic_total_CPS_counterfactual.pdf"), width = 10, height = 8)
plot(plot_5)
dev.off()

pdf(file = paste0("synthetic_total_CPS_att.pdf"), width = 10, height = 8)
plot(plot_6)
dev.off()



#################################################################
#                         Callaway DID                          #
#################################################################
###
Medicaid_subset <- subset(state_aggs, Medicaid==1)
Medicaid_subset <- Medicaid_subset[order(Medicaid_subset$st, abs(Medicaid_subset$YEAR.id) ), ]
Medicaid_subset <- Medicaid_subset[ !duplicated(Medicaid_subset$st), ]  
Medicaid_subset$first.treat <- Medicaid_subset$YEAR.id
Medicaid_subset <- Medicaid_subset %>% select("st","first.treat")

state_aggs <- merge(state_aggs, Medicaid_subset, all = TRUE)
state_aggs$first.treat[is.na(state_aggs$first.treat)] <- 0

######
Med_diff_estabpop <-  att_gt(yname = "estab_pop",
                             tname = "YEAR.id",
                             idname = "st",
                             gname = "first.treat",
                             xformla=~Labor.Force,
                             data = state_aggs,
                             panel = TRUE,
                             allow_unbalanced_panel = FALSE,
                             control_group = c("nevertreated"),
                             anticipation = 0,
                             weightsname = "Labor.Force",
                             alp = 0.05,
                             bstrap = TRUE,
                             cband = TRUE,
                             biters = 1000,
                             clustervars = "st",
                             # est_method = "ipw",
                             # est_method = "reg",
                             est_method = "dr",
                             print_details = FALSE,
                             pl = FALSE,
                             cores = 1)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
ggdid(es_estabpop) + geom_hline(yintercept = 0)
summary(es_estabpop)

######
Med_diff_estabpop <-  att_gt(yname="Estab_pct_CPS",
                             tname = "YEAR.id",
                             idname = "st",
                             gname = "first.treat",
                             xformla=~Labor.Force,
                             data = state_aggs,
                             panel = TRUE,
                             allow_unbalanced_panel = FALSE,
                             control_group = c("nevertreated"),
                             anticipation = 0,
                             weightsname = "Labor.Force",
                             alp = 0.05,
                             bstrap = TRUE,
                             cband = TRUE,
                             biters = 1000,
                             clustervars = "st",
                             # estMethod = "ipw",
                             # estMethod = "reg",
                             est_method = "dr",
                             print_details = FALSE,
                             pl = FALSE,
                             cores = 1)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
ggdid(es_estabpop) + geom_hline(yintercept = 0)
summary(es_estabpop)

######
Med_diff_estabpop <-  att_gt(yname="Estab_pct_ACS",
                             tname = "YEAR.id",
                             idname = "st",
                             gname = "first.treat",
                             xformla=~Labor.Force,
                             data = state_aggs,
                             panel = TRUE,
                             allow_unbalanced_panel = FALSE,
                             control_group = c("nevertreated"),
                             anticipation = 0,
                             weightsname = "Labor.Force",
                             alp = 0.05,
                             bstrap = TRUE,
                             cband = TRUE,
                             biters = 1000,
                             clustervars = "st",
                             # estMethod = "ipw",
                             # estMethod = "reg",
                             est_method = "dr",
                             print_details = FALSE,
                             pl = FALSE,
                             cores = 1)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
ggdid(es_estabpop) + geom_hline(yintercept = 0)
summary(es_estabpop)

