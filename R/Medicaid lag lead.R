# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(stargazer)
library(lfe)
library(sjPlot)
library(estimatr)
library(ggplot2)
library(broom)
library(scales)
library(gsynth)

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
load("medicaid_analysis_data_total.RData")

using$market_type[using$concentration_lower==1] <- "Oligopsony"
using$market_type[using$concentration_lower==2] <- "Moderate"
using$market_type[using$concentration_lower==3] <- "Competitive"

using$Uber_active_num <- using$Uber_active
using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

using$market_type <- factor(using$market_type, levels = c("Oligopsony","Moderate","Competitive"))

estab_list <- list()
rcp_list <- list()

using <- using[complete.cases(using),]
#################################################################
#                         Estab All                             #
#################################################################

estab_list[[1]] <- felm(estab_pop ~ Medicaid_lead2 + Medicaid_lead1 + Medicaid + Medicaid_lag1 + Medicaid_lag2 + factor(market_type) + factor(Uber_active)| id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

estab_list[[2]] <- felm(estab_pop ~ Medicaid_lead1 + Medicaid + Medicaid_lag1 + factor(market_type) + factor(Uber_active) | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

estab_list[[3]] <- felm(estab_pop ~ Medicaid + factor(market_type) + factor(Uber_active) | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

#################################################################
#                         Average Receipts                      #
#################################################################


rcp_list[[1]] <- felm(receipt_estab ~ Medicaid_lead2 + Medicaid_lead1 + Medicaid + Medicaid_lag1 + Medicaid_lag2 + factor(market_type) + factor(Uber_active) | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

rcp_list[[2]] <- felm(receipt_estab ~ Medicaid_lead1 + Medicaid + Medicaid_lag1 + factor(market_type) + factor(Uber_active) | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

rcp_list[[3]] <- felm(receipt_estab ~ Medicaid + factor(market_type) + factor(Uber_active) | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

#################################################################
#                         Stargazer                             #
#################################################################

stargazer(estab_list[[1]], estab_list[[2]], estab_list[[3]],rcp_list[[1]], rcp_list[[2]], rcp_list[[3]],
          keep = "Medicaid", 
          "Medicaid:Uber_active",
          "Medicaid:market_type", 
          "Medicaid:Uber_active:market_type",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 4)

stargazer(rcp_list[[1]], rcp_list[[2]], rcp_list[[3]],
          keep = "Medicaid", 
          "Medicaid:Uber_active",
          "Medicaid:market_type", 
          "Medicaid:Uber_active:market_type",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 1)


#################################################################
#                         lag-lead plot                         #
#################################################################

setwd(paste(path_output))
estab_results <- summary(estab_list[[1]])
rcp_results <- summary(rcp_list[[1]])

estab_results <- data.frame(estab_results$coefficients[c(1:5),])
estab_results$terms <- c("min_change_lead2", "min_change_lead1", "min_change", "min_change_lag1", 
                         "min_change_lag2")
estab_results$Period <- c(-2,-1,0,1,2)
estab_results$group <- "coefficient"


rcp_results <- data.frame(rcp_results$coefficients[c(1:5),])
rcp_results$terms <- c("min_change_lead2", "min_change_lead1", "min_change", "min_change_lag1", 
                       "min_change_lag2")
rcp_results$Period <- c(-2,-1,0,1,2)
rcp_results$group <- "coefficient"

estab_match_unmatch <-estab_results
rcp_match_unmatch <-rcp_results

estab_match_unmatch$dependent <- "Establishments/Labor Force"
rcp_match_unmatch$dependent <- "Average Receipts"

match_unmatch <- rbind(estab_match_unmatch,rcp_match_unmatch)

estab_match_unmatch$conf.low <- estab_match_unmatch$Estimate - 1.96*estab_match_unmatch$Cluster.s.e.
estab_match_unmatch$conf.high <- estab_match_unmatch$Estimate + 1.96*estab_match_unmatch$Cluster.s.e.
rcp_match_unmatch$conf.low <- rcp_match_unmatch$Estimate - 1.96*rcp_match_unmatch$Cluster.s.e.
rcp_match_unmatch$conf.high <- rcp_match_unmatch$Estimate + 1.96*rcp_match_unmatch$Cluster.s.e.
match_unmatch$conf.low <- match_unmatch$Estimate - 1.96*match_unmatch$Cluster.s.e.
match_unmatch$conf.high <- match_unmatch$Estimate + 1.96*match_unmatch$Cluster.s.e.

p1 <- ggplot(data = estab_match_unmatch,
             aes(x = Period,
                 group = group,
                 color = group,
                 shape = group)) +
  geom_point(aes(y = Estimate), size = 8) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.75,
                position=position_dodge(0.25))+
  geom_hline(yintercept = 0) +
  labs(y = "Coefficient",
       title = "Establishments/Labor Force") +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30, margin = margin(r=15)),
        axis.title.x = element_text(size = 30),
        axis.text.y.right = element_text(margin = margin(r=15)),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) +
  scale_x_continuous("Period", labels = as.character(estab_match_unmatch$Period), breaks = estab_match_unmatch$Period)



p2 <- ggplot(data = rcp_match_unmatch,
             aes(x = Period,
                 group = group,
                 color = group,
                 shape = group)) +
  geom_point(aes(y = Estimate), size = 8) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.75,
                position=position_dodge(0.25))+
  geom_hline(yintercept = 0) +
  labs(y = "Coefficient",
       title = "Average Receipts") +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30, margin = margin(r=15)),
        axis.title.x = element_text(size = 30),
        axis.text.y.right = element_text(margin = margin(r=15)),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) +
  scale_y_continuous(label=comma) +
  scale_x_continuous("Period", labels = as.character(rcp_match_unmatch$Period), breaks = rcp_match_unmatch$Period)

p3 <- ggplot(data = match_unmatch,
             aes(x = Period,
                 group = group,
                 color = group,
                 shape = group)) +
  geom_point(aes(y = Estimate), size = 8) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.75,
                position=position_dodge(0.25))+
  geom_hline(yintercept = 0) +
  labs(y = "Coefficient") +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30, margin = margin(r=15)),
        axis.title.x = element_text(size = 30),
        axis.text.y.right = element_text(margin = margin(r=15)),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.margin.y = unit(1,"lines"),
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(fill = "white"),
        strip.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) +
  scale_y_continuous(label=comma) +
  scale_x_continuous("Period", labels = as.character(match_unmatch$Period), breaks = match_unmatch$Period) +
  facet_grid(dependent~., scales = "free_y") 


pdf(file = "lead_lag_plots_estab.pdf", width = 8, height = 6)
plot(p1)
dev.off()

pdf(file = "lead_lag_plots_rcp.pdf", width = 8, height = 6)
plot(p2)
dev.off()

pdf(file = "lead_lag_plots.pdf", width = 7, height = 9)
plot(p3)
dev.off()