# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex


##################
###  Library   ###
##################
library(stargazer)
library(ggplot2)
library(scales)
library(gsynth)
library("readxl")
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
load("synthetic_estab_med.RData")
load("synthetic_rcp_med.RData")
load("medicaid_analysis_data_allnonemployers.RData")
using <- subset(using,years==2018)

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

industry_list <- industry_name$naics
#########################
###         Plots     ###
#########################


plot1_list_estab <- list()
plot2_list_estab <- list()
plot1_list_rcp <- list()
plot2_list_rcp <- list()

for (i in seq_along(industry_list)) {
  
  counter_plot <- cbind(rownames(estab_list[[i]][["Y.bar"]]), data.frame(estab_list[[i]][["Y.bar"]], row.names=NULL))
  colnames(counter_plot)[1] <- "Year"
  counter_plot$Year <- as.numeric(as.character(counter_plot$Year))
  
  
  plot1_list_estab[[i]] <- ggplot(data = counter_plot,
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
  
  gap_plot <- cbind(rownames(estab_list[[i]][["est.att"]]), data.frame(estab_list[[i]][["est.att"]], row.names=NULL))
  colnames(gap_plot)[1] <- "Period"
  gap_plot$Period <- as.numeric(as.character(gap_plot$Period))
  
  plot2_list_estab[[i]] <- ggplot(data = gap_plot,
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
  

  counter_plot <- cbind(rownames(rcp_list[[i]][["Y.bar"]]), data.frame(rcp_list[[i]][["Y.bar"]], row.names=NULL))
  colnames(counter_plot)[1] <- "Year"
  counter_plot$Year <- as.numeric(as.character(counter_plot$Year))
  
  
  plot1_list_rcp[[i]] <- ggplot(data = counter_plot,
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
  
  gap_plot <- cbind(rownames(rcp_list[[i]][["est.att"]]), data.frame(rcp_list[[i]][["est.att"]], row.names=NULL))
  colnames(gap_plot)[1] <- "Period"
  gap_plot$Period <- as.numeric(as.character(gap_plot$Period))
  
  plot2_list_rcp[[i]] <- ggplot(data = gap_plot,
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
  
}

setwd(path_output)
for (i in seq_along(industry_list)) {
  pdf(file = paste0("synthetic_total_estab_counterfactual",industry_list[[i]],".pdf"), width = 10, height = 8)
  plot(plot1_list_estab[[i]])
  dev.off()
  
  pdf(file = paste0("synthetic_total_estab_att",industry_list[[i]],".pdf"), width = 10, height = 8)
  plot(plot2_list_estab[[i]])
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_counterfactual",industry_list[[i]],".pdf"), width = 10, height = 8)
  plot(plot1_list_rcp[[i]])
  dev.off()
  
  pdf(file = paste0("synthetic_total_rcp_att",industry_list[[i]],".pdf"), width = 10, height = 8)
  plot(plot2_list_rcp[[i]])
  dev.off()
}
ATT <- c(as.numeric(estab_list[[1]]$att.avg),as.numeric(estab_list[[2]]$att.avg),as.numeric(estab_list[[3]]$att.avg),
         as.numeric(estab_list[[4]]$att.avg),as.numeric(estab_list[[5]]$att.avg),as.numeric(estab_list[[6]]$att.avg),
         as.numeric(estab_list[[7]]$att.avg),as.numeric(estab_list[[8]]$att.avg),as.numeric(estab_list[[9]]$att.avg),
         as.numeric(estab_list[[10]]$att.avg),as.numeric(estab_list[[11]]$att.avg),as.numeric(estab_list[[12]]$att.avg),
         as.numeric(estab_list[[13]]$att.avg),as.numeric(estab_list[[14]]$att.avg),as.numeric(estab_list[[15]]$att.avg),
         as.numeric(estab_list[[16]]$att.avg),as.numeric(estab_list[[17]]$att.avg),as.numeric(estab_list[[18]]$att.avg),
         as.numeric(estab_list[[19]]$att.avg))

SE <- c(as.numeric(sd(estab_list[[1]]$att.avg.boot)),as.numeric(sd(estab_list[[2]]$att.avg.boot)),as.numeric(sd(estab_list[[3]]$att.avg.boot)),
        as.numeric(sd(estab_list[[4]]$att.avg.boot)),as.numeric(sd(estab_list[[5]]$att.avg.boot)),as.numeric(sd(estab_list[[6]]$att.avg.boot)),
        as.numeric(sd(estab_list[[7]]$att.avg.boot)),as.numeric(sd(estab_list[[8]]$att.avg.boot)),as.numeric(sd(estab_list[[9]]$att.avg.boot)),
        as.numeric(sd(estab_list[[10]]$att.avg.boot)),as.numeric(sd(estab_list[[11]]$att.avg.boot)),as.numeric(sd(estab_list[[12]]$att.avg.boot)),
        as.numeric(sd(estab_list[[13]]$att.avg.boot)),as.numeric(sd(estab_list[[14]]$att.avg.boot)),as.numeric(sd(estab_list[[15]]$att.avg.boot)),
        as.numeric(sd(estab_list[[16]]$att.avg.boot)),as.numeric(sd(estab_list[[17]]$att.avg.boot)),as.numeric(sd(estab_list[[18]]$att.avg.boot)),
        as.numeric(sd(estab_list[[19]]$att.avg.boot)))

labor.force_med2018 <- c(98219447, 98219447, 98219447,
                         98219447, 98219447, 98219447,
                         98219447, 98219447, 98219447,
                         98219447, 98219447, 98219447,
                         98219447, 98219447, 98219447,
                         98219447, 98219447, 98219447,
                         98219447)

Bar_plot_estab <- data.frame(cbind(industry_name,ATT,SE,labor.force_med2018))
Bar_plot_estab$ci.higher <- Bar_plot_estab$ATT + 1.96*Bar_plot_estab$SE
Bar_plot_estab$ci.lower <- Bar_plot_estab$ATT - 1.96*Bar_plot_estab$SE


#######

ATT <- c(as.numeric(rcp_list[[1]]$att.avg),as.numeric(rcp_list[[2]]$att.avg),as.numeric(rcp_list[[3]]$att.avg),
         as.numeric(rcp_list[[4]]$att.avg),as.numeric(rcp_list[[5]]$att.avg),as.numeric(rcp_list[[6]]$att.avg),
         as.numeric(rcp_list[[7]]$att.avg),as.numeric(rcp_list[[8]]$att.avg),as.numeric(rcp_list[[9]]$att.avg),
         as.numeric(rcp_list[[10]]$att.avg),as.numeric(rcp_list[[11]]$att.avg),as.numeric(rcp_list[[12]]$att.avg),
         as.numeric(rcp_list[[13]]$att.avg),as.numeric(rcp_list[[14]]$att.avg),as.numeric(rcp_list[[15]]$att.avg),
         as.numeric(rcp_list[[16]]$att.avg),as.numeric(rcp_list[[17]]$att.avg),as.numeric(rcp_list[[18]]$att.avg),
         as.numeric(rcp_list[[19]]$att.avg))

SE <- c(as.numeric(sd(rcp_list[[1]]$att.avg.boot)),as.numeric(sd(rcp_list[[2]]$att.avg.boot)),as.numeric(sd(rcp_list[[3]]$att.avg.boot)),
        as.numeric(sd(rcp_list[[4]]$att.avg.boot)),as.numeric(sd(rcp_list[[5]]$att.avg.boot)),as.numeric(sd(rcp_list[[6]]$att.avg.boot)),
        as.numeric(sd(rcp_list[[7]]$att.avg.boot)),as.numeric(sd(rcp_list[[8]]$att.avg.boot)),as.numeric(sd(rcp_list[[9]]$att.avg.boot)),
        as.numeric(sd(rcp_list[[10]]$att.avg.boot)),as.numeric(sd(rcp_list[[11]]$att.avg.boot)),as.numeric(sd(rcp_list[[12]]$att.avg.boot)),
        as.numeric(sd(rcp_list[[13]]$att.avg.boot)),as.numeric(sd(rcp_list[[14]]$att.avg.boot)),as.numeric(sd(rcp_list[[15]]$att.avg.boot)),
        as.numeric(sd(rcp_list[[16]]$att.avg.boot)),as.numeric(sd(rcp_list[[17]]$att.avg.boot)),as.numeric(sd(rcp_list[[18]]$att.avg.boot)),
        as.numeric(sd(rcp_list[[19]]$att.avg.boot)))

labor.force_med2018 <- c(98219447, 98219447, 98219447,
               98219447, 98219447, 98219447,
               98219447, 98219447, 98219447,
               98219447, 98219447, 98219447,
               98219447, 98219447, 98219447,
               98219447, 98219447, 98219447,
               98219447)


Bar_plot_rcp <- data.frame(cbind(industry_name,ATT,SE,labor.force_med2018))
Bar_plot_rcp$ci.higher <- Bar_plot_rcp$ATT + 1.96*Bar_plot_rcp$SE
Bar_plot_rcp$ci.lower <- Bar_plot_rcp$ATT - 1.96*Bar_plot_rcp$SE

#######
Bar_plot_estab$x <- "Estab. (Thousands)"
Bar_plot_estab$ATT <- Bar_plot_estab$ATT*Bar_plot_estab$labor.force_med2018/1000
Bar_plot_estab$ci.lower <- Bar_plot_estab$ci.lower*Bar_plot_estab$labor.force_med2018/1000
Bar_plot_estab$ci.higher <- Bar_plot_estab$ci.higher*Bar_plot_estab$labor.force_med2018/1000

Bar_plot_rcp$x <- "Average Receipts"

Bar_plot <- rbind(Bar_plot_estab,Bar_plot_rcp)

Bar_plot <- merge(Bar_plot, estab_counts)
Bar_plot <- merge(Bar_plot, rcptot_counts)

Bar_plot$avgrcp <- (Bar_plot$rcptot/Bar_plot$estab)*1000
Bar_plot$estab <- Bar_plot$estab/1000

Bar_plot$ATT_perc[Bar_plot$x=="Estab. (Thousands)"] <- Bar_plot$ATT[Bar_plot$x=="Estab. (Thousands)"]/Bar_plot$estab[Bar_plot$x=="Estab. (Thousands)"]
Bar_plot$ci.lower_perc[Bar_plot$x=="Estab. (Thousands)"] <- Bar_plot$ci.lower[Bar_plot$x=="Estab. (Thousands)"]/Bar_plot$estab[Bar_plot$x=="Estab. (Thousands)"]
Bar_plot$ci.higher_perc[Bar_plot$x=="Estab. (Thousands)"] <- Bar_plot$ci.higher[Bar_plot$x=="Estab. (Thousands)"]/Bar_plot$estab[Bar_plot$x=="Estab. (Thousands)"]

Bar_plot$ATT_perc[Bar_plot$x=="Average Receipts"] <- Bar_plot$ATT[Bar_plot$x=="Average Receipts"]/Bar_plot$avgrcp[Bar_plot$x=="Average Receipts"]
Bar_plot$ci.lower_perc[Bar_plot$x=="Average Receipts"] <- Bar_plot$ci.lower[Bar_plot$x=="Average Receipts"]/Bar_plot$avgrcp[Bar_plot$x=="Average Receipts"]
Bar_plot$ci.higher_perc[Bar_plot$x=="Average Receipts"] <- Bar_plot$ci.higher[Bar_plot$x=="Average Receipts"]/Bar_plot$avgrcp[Bar_plot$x=="Average Receipts"]

Bar_plot <- Bar_plot[-c(5,6,7,8),]
  
p1 <- ggplot(data = Bar_plot,
                     aes(y = industry_names)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(aes(x = ATT), size = 2) +
              geom_errorbarh(aes(xmax = ci.higher, xmin = ci.lower, height = .4)) +
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
              facet_wrap( ~ x, scales = "free_x")

setwd(path_output)
pdf(file = paste0("synthetic_results_ATT_avg.pdf"), width = 18, height = 15)
plot(p1)
dev.off()


p2 <- ggplot(data = Bar_plot,
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
  facet_wrap( ~ x, scales = "free_x")

setwd(path_output)
pdf(file = paste0("synthetic_results_ATT_avg_perc.pdf"), width = 18, height = 15)
plot(p2)
dev.off()

###################################################

DIDM <- read_excel("DIDM results industry.xlsx")
DIDM <- merge(DIDM, industry_name)
DIDM$labor_force <- 98219447

DIDM <- merge(DIDM, estab_counts)
DIDM <- merge(DIDM, rcptot_counts)
DIDM$estab_pop <- DIDM$estab/DIDM$labor_force
DIDM$rcptot_estab <- DIDM$rcptot/DIDM$estab

DIDM$ATT_perc[DIDM$dependent=="estab/pop"] <- DIDM$ATE[DIDM$dependent=="estab/pop"]/DIDM$estab_pop[DIDM$dependent=="estab/pop"]
DIDM$ATT_perc[DIDM$dependent=="avg receipts"] <- DIDM$ATE[DIDM$dependent=="avg receipts"]/DIDM$rcptot_estab[DIDM$dependent=="avg receipts"]

DIDM$ci.higher_perc[DIDM$dependent=="estab/pop"] <- DIDM$Upper[DIDM$dependent=="estab/pop"]/DIDM$estab_pop[DIDM$dependent=="estab/pop"]
DIDM$ci.higher_perc[DIDM$dependent=="avg receipts"] <- DIDM$Upper[DIDM$dependent=="avg receipts"]/DIDM$rcptot_estab[DIDM$dependent=="avg receipts"]

DIDM$ci.lower_perc[DIDM$dependent=="estab/pop"] <- DIDM$Lower[DIDM$dependent=="estab/pop"]/DIDM$estab_pop[DIDM$dependent=="estab/pop"]
DIDM$ci.lower_perc[DIDM$dependent=="avg receipts"] <- DIDM$Lower[DIDM$dependent=="avg receipts"]/DIDM$rcptot_estab[DIDM$dependent=="avg receipts"]


p3 <- ggplot(data = subset(DIDM,naics!=21 & naics!=22),
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
