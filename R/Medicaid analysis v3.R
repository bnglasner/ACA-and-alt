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

industry_analysis <- 0
transit <- 1
# load("medicaid_analysis_data_total.RData")

load("medicaid_analysis_data_allnonemployers.RData")
using <- subset(using, naics=="48-49")
transit <- 1

using$market_type[using$concentration_lower==1] <- "Oligopsony"
using$market_type[using$concentration_lower==2] <- "Moderate"
using$market_type[using$concentration_lower==3] <- "Competitive"

using$Uber_active_num <- using$Uber_active
using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

using$market_type <- factor(using$market_type, levels = c("Oligopsony","Moderate","Competitive"))

using$Medicaid_Uber <- using$Medicaid*using$Uber_active_num
using$Work_require <- 0
using$Work_require[using$st==5 & using$years==2018] <- 1



estab_list <- list()
rcp_list <- list()

#################################################################
#                         Estab All                             #
#################################################################

estab_list[[1]] <- felm(estab_pop ~ Medicaid +
                          Uber_active_num | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

# estab_list[[2]] <- felm(estab_pop ~ Medicaid*quantile +
#                           Uber_active_num | id + YEAR.id | 0 | st,
#                         data = using,
#                         weights = using$lf_avg)

estab_list[[3]] <- felm(estab_pop ~ Medicaid*Uber_active_num
                           | id + YEAR.id | 0 | st,
                        data = using,
                        weights = using$lf_avg)

#################################################################
#                         Average Receipts                      #
#################################################################

rcp_list[[1]] <- felm(receipt_estab ~ Medicaid +
                        Uber_active_num | id + YEAR.id | 0 | st,
                      data = using,
                      weights = using$lf_avg)

# rcp_list[[2]] <- felm(receipt_estab ~ Medicaid*quantile +
#                         Uber_active_num | id + YEAR.id | 0 | st,
#                       data = using,
#                       weights = using$lf_avg)

rcp_list[[3]] <- felm(receipt_estab ~  Medicaid*Uber_active_num
                         | id + YEAR.id | 0 | st,
                      data = using,
                      weights = using$lf_avg)

#################################################################
#                         Stargazer                             #
#################################################################

stargazer(estab_list[[1]], estab_list[[3]],
          # keep = "Medicaid", 
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 4)

stargazer(rcp_list[[1]], rcp_list[[3]], 
          # keep = "Medicaid", 
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 3)

#################################################################
#                         Synthetic Control                     #
#################################################################

with_uber <- using
with_uber$keep <- 1
with_uber$keep[with_uber$Medicaid==1 & with_uber$Uber_active_num==0] <-0
with_uber <- subset(with_uber, keep==1)

without_uber <- using
without_uber$keep <- 1
without_uber$keep[without_uber$Medicaid==1 & without_uber$Uber_active_num==1] <-0
without_uber <- subset(without_uber, keep==1)

county_counts <- as.data.frame(table(with_uber$id)) # count how many times a county-state appears in the sample
county_counts <- subset(county_counts, Freq==19) # keep only the counties that are in the full sample for balance
colnames(county_counts)[1]<- "id" # relabel the id for the merge
with_uber<- merge(with_uber, county_counts) # merge as a way to drop the unbalanced counties
rm(county_counts) # drop county_counts from the env.

county_counts <- as.data.frame(table(without_uber$id)) # count how many times a county-state appears in the sample
county_counts <- subset(county_counts, Freq==19) # keep only the counties that are in the full sample for balance
colnames(county_counts)[1]<- "id" # relabel the id for the merge
without_uber<- merge(without_uber, county_counts) # merge as a way to drop the unbalanced counties
rm(county_counts) # drop county_counts from the env.


#################################################################
#                         Synthetic Control                     #
#################################################################
# panelView(estab_pop ~ Medicaid,
#           data = using,
#           index = c("cty_st_numeric","years"),
#           type = "outcome",
#           main = "Medicaid Expansion and The Number of Nonemployer Establishments per Member of the Labor Force",
#           by.group = TRUE)


gsynth_1 <- gsynth(Y = "estab_pop",
                           D = "Medicaid",
                           X = c("Labor.Force",
                                 "Uber_active_num",
                                 "rcptot",
                                 "HHI_lower"),
                           data = using,
                           index = c("cty_st_numeric","years"),
                           weight = "lf_avg",
                           force = "two-way", 
                           r = c(0, 5),
                           min.T0 = 8,
                           CV = TRUE, 
                           se = TRUE,
                           EM = TRUE,
                           # estimator = "mc",
                           # nlambda = 10,
                           # inference = "parametric",
                           inference = "nonparametric",
                           nboots = 500, 
                           parallel = TRUE,
                           cl =  "st",
                           cores = 3)
print(gsynth_1)
plot(gsynth_1, type = "counterfactual", xlab = "Years to Medicaid Expansion", theme.bw = TRUE)
plot(gsynth_1)
cumuEff(gsynth_1, cumu = TRUE, id = NULL, period = c(0,5))
plot(gsynth_1, type = "loadings")

gsynth_2 <- gsynth(Y = "receipt_estab",
                   D = "Medicaid",
                   X = c("Labor.Force",
                         "Uber_active_num",
                         "estab",
                         "HHI_lower"),
                   data = using,
                   index = c("cty_st_numeric","years"),
                   weight = "lf_avg",
                   force = "two-way", 
                   r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   EM = TRUE,
                   # estimator = "mc",
                   # nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 500, 
                   parallel = TRUE,
                   cl =  "st",
                   cores = 3)
plot(gsynth_2, type = "counterfactual")
plot(gsynth_2)
print(gsynth_2)
cumuEff(gsynth_2, cumu = TRUE, id = NULL, period = c(0,5))
plot(gsynth_2, type = "loadings")

gsynth_3 <- gsynth(Y = "estab_pop",
                   D = "Medicaid",
                   X = c("Labor.Force",
                         "Uber_active_num",
                         "rcptot",
                         "HHI_lower"),
                   data = with_uber,
                   index = c("cty_st_numeric","years"),
                   weight = "lf_avg",
                   force = "two-way", 
                   r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   EM = TRUE,
                   # estimator = "mc",
                   # nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 500, 
                   parallel = TRUE,
                   cl =  "st",
                   cores = 3)
plot(gsynth_3)
gsynth_4 <- gsynth(Y = "receipt_estab",
                   D = "Medicaid",
                   X = c("Labor.Force",
                         "Uber_active_num",
                         "estab",
                         "HHI_lower"),
                   data = with_uber,
                   index = c("cty_st_numeric","years"),
                   weight = "lf_avg",
                   force = "two-way", 
                   r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   EM = TRUE,
                   # estimator = "mc",
                   # nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 500, 
                   parallel = TRUE,
                   cl =  "st",
                   cores = 3)

plot(gsynth_4)

gsynth_5 <- gsynth(Y = "estab_pop",
                   D = "Medicaid",
                   X = c("Labor.Force",
                         "Uber_active_num",
                         "rcptot",
                         "HHI_lower"),
                   data = without_uber,
                   index = c("cty_st_numeric","years"),
                   weight = "lf_avg",
                   force = "two-way", 
                   r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   EM = TRUE,
                   # estimator = "mc",
                   # nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 500, 
                   parallel = TRUE,
                   cl =  "st",
                   cores = 3)
plot(gsynth_5)
gsynth_6 <- gsynth(Y = "receipt_estab",
                   D = "Medicaid",
                   X = c("Labor.Force",
                         "Uber_active_num",
                         "estab",
                         "HHI_lower"),
                   data = without_uber,
                   index = c("cty_st_numeric","years"),
                   weight = "lf_avg",
                   force = "two-way", 
                   r = c(0, 5),
                   min.T0 = 8,
                   CV = TRUE, 
                   se = TRUE,
                   EM = TRUE,
                   # estimator = "mc",
                   # nlambda = 10,
                   # inference = "parametric",
                   inference = "nonparametric",
                   nboots = 500, 
                   parallel = TRUE,
                   cl =  "st",
                   cores = 3)

plot(gsynth_6)

##############################

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
  ylab("Establishments Per Person") + xlab("Year") +
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
  ylab("Average Receipts") + xlab("Year") +
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


#########################################3

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
  ylab("Establishments Per Person") + xlab("Year") +
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
        legend.background = element_rect(fill=NA)) 

###########################################

counter_plot <- cbind(rownames(gsynth_4[["Y.bar"]]), data.frame(gsynth_4[["Y.bar"]], row.names=NULL))
colnames(counter_plot)[1] <- "Year"
counter_plot$Year <- as.numeric(as.character(counter_plot$Year))


plot_7 <- ggplot(data = counter_plot,
                 aes(x = Year)) + 
  geom_point(aes(y = Y.tr.bar, colour = "Treated"), size = 3) +
  geom_line(aes(y = Y.tr.bar, colour = "Treated"), size = 1.5) +
  geom_point(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 3) +
  geom_line(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 1.5) +
  geom_vline(xintercept = 2014) +
  ylab("Average Receipts") + xlab("Year") +
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

gap_plot <- cbind(rownames(gsynth_4[["est.att"]]), data.frame(gsynth_4[["est.att"]], row.names=NULL))
colnames(gap_plot)[1] <- "Period"
gap_plot$Period <- as.numeric(as.character(gap_plot$Period))

plot_8 <- ggplot(data = gap_plot,
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


#########################################3

counter_plot <- cbind(rownames(gsynth_5[["Y.bar"]]), data.frame(gsynth_5[["Y.bar"]], row.names=NULL))
colnames(counter_plot)[1] <- "Year"
counter_plot$Year <- as.numeric(as.character(counter_plot$Year))


plot_9 <- ggplot(data = counter_plot,
                 aes(x = Year)) + 
  geom_point(aes(y = Y.tr.bar, colour = "Treated"), size = 3) +
  geom_line(aes(y = Y.tr.bar, colour = "Treated"), size = 1.5) +
  geom_point(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 3) +
  geom_line(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 1.5) +
  geom_vline(xintercept = 2014) +
  ylab("Establishments Per Person") + xlab("Year") +
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

gap_plot <- cbind(rownames(gsynth_5[["est.att"]]), data.frame(gsynth_5[["est.att"]], row.names=NULL))
colnames(gap_plot)[1] <- "Period"
gap_plot$Period <- as.numeric(as.character(gap_plot$Period))

plot_10 <- ggplot(data = gap_plot,
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

counter_plot <- cbind(rownames(gsynth_6[["Y.bar"]]), data.frame(gsynth_6[["Y.bar"]], row.names=NULL))
colnames(counter_plot)[1] <- "Year"
counter_plot$Year <- as.numeric(as.character(counter_plot$Year))


plot_11 <- ggplot(data = counter_plot,
                 aes(x = Year)) + 
  geom_point(aes(y = Y.tr.bar, colour = "Treated"), size = 3) +
  geom_line(aes(y = Y.tr.bar, colour = "Treated"), size = 1.5) +
  geom_point(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 3) +
  geom_line(aes(y = Y.ct.bar, colour = "Synthetic Control"), size = 1.5) +
  geom_vline(xintercept = 2014) +
  ylab("Average Receipts") + xlab("Year") +
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

gap_plot <- cbind(rownames(gsynth_6[["est.att"]]), data.frame(gsynth_6[["est.att"]], row.names=NULL))
colnames(gap_plot)[1] <- "Period"
gap_plot$Period <- as.numeric(as.character(gap_plot$Period))

plot_12 <- ggplot(data = gap_plot,
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

#################
setwd(path_output)

if(transit ==0){
  pdf(file = paste0("synthetic_total_estab_counterfactual.pdf"), width = 10, height = 8)
  plot(plot_1)
  dev.off()
  
  pdf(file = paste0("synthetic_total_estab_att.pdf"), width = 10, height = 8)
  plot(plot_2)
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_counterfactual.pdf"), width = 10, height = 8)
  plot(plot_3)
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_att.pdf"), width = 10, height = 8)
  plot(plot_4)
  dev.off()
  
  pdf(file = paste0("synthetic_total_estab_counterfactual_uber.pdf"), width = 10, height = 8)
  plot(plot_5)
  dev.off()
  
  pdf(file = paste0("synthetic_total_estab_att_uber.pdf"), width = 10, height = 8)
  plot(plot_6)
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_counterfactual_uber.pdf"), width = 10, height = 8)
  plot(plot_7)
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_att_uber.pdf"), width = 10, height = 8)
  plot(plot_8)
  dev.off()
  
  pdf(file = paste0("synthetic_total_estab_counterfactual_nouber.pdf"), width = 10, height = 8)
  plot(plot_9)
  dev.off()
  
  pdf(file = paste0("synthetic_total_estab_att_nouber.pdf"), width = 10, height = 8)
  plot(plot_10)
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_counterfactual_nouber.pdf"), width = 10, height = 8)
  plot(plot_11)
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_att_nouber.pdf"), width = 10, height = 8)
  plot(plot_12)
  dev.off()
}

if(transit ==1){
  pdf(file = paste0("synthetic_taxi_estab_counterfactual.pdf"), width = 10, height = 8)
  plot(plot_1)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_estab_att.pdf"), width = 10, height = 8)
  plot(plot_2)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_rcp_counterfactual.pdf"), width = 10, height = 8)
  plot(plot_3)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_rcp_att.pdf"), width = 10, height = 8)
  plot(plot_4)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_estab_counterfactual_uber.pdf"), width = 10, height = 8)
  plot(plot_5)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_estab_att_uber.pdf"), width = 10, height = 8)
  plot(plot_6)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_rcp_counterfactual_uber.pdf"), width = 10, height = 8)
  plot(plot_7)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_rcp_att_uber.pdf"), width = 10, height = 8)
  plot(plot_8)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_estab_counterfactual_nouber.pdf"), width = 10, height = 8)
  plot(plot_9)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_estab_att_nouber.pdf"), width = 10, height = 8)
  plot(plot_10)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_rcp_counterfactual_nouber.pdf"), width = 10, height = 8)
  plot(plot_11)
  dev.off()
  
  pdf(file = paste0("synthetic_taxi_rcp_att_nouber.pdf"), width = 10, height = 8)
  plot(plot_12)
  dev.off()
}
