# Ben Glasner
# Affordable Care Act and nonemployers Uber analysis

#Set up
#################################################
library(readxl)
library(dplyr)
library(lubridate)
library(broom)
library(corrplot)
library(raster)
library(dummies)
library(ggplot2)
library(MASS)
library(RColorBrewer)
library(simcf)
library(tile)
library(readstata13)
library(readr)
library(boot)
library(verification)
library(lars)
library(dummies)
library(broom)
library(ggplot2)
library(stargazer)
library(randomForest)

setwd("C:/Users/bglasner/Dropbox/PhD requirements/Nonemployer data/Data")
# setwd("C:/Users/bngla/Dropbox/PhD requirements/Nonemployer data/Data")

#################################################
load("total_v2.RData")

using <- subset(using, YEAR.id >1999)
using$estab_pop <- using$estab/using$population
using$population <- using$population/10000
using$id <- as.factor(paste(using$st, using$cty, sep = "-"))
county_counts <- as.data.frame(table(using$id))
county_counts <- subset(county_counts, Freq>16)
colnames(county_counts)[1]<- "id"
using<- merge(using, county_counts)
rm(county_counts)

#################################################
###                  Analysis                 ###
#################################################


# Estab, ACA_insmark, state (FX), year(FX), state time trend
using_model <- using[c(8,206, 32:81, 86:101,103:152)]

glm_ACA.nb <- glm.nb(estab ~ ., data =  using_model)

using_model <- using[c(222,206, 32:81, 86:101,103:152)]
lm_ACA <- lm(estab_pop~ ., data =  using_model)


#################################################
load("taxi_v2.RData")
using <- subset(using, YEAR.id >1999)
using$estab_pop <- using$estab/using$population
using$population <- using$population/10000
using$id <- as.factor(paste(using$st, using$cty, sep = "-"))
county_counts <- as.data.frame(table(using$id))
county_counts <- subset(county_counts, Freq>16)
colnames(county_counts)[1]<- "id"
using<- merge(using, county_counts)
rm(county_counts)

#################################################
###                  Analysis                 ###
#################################################


# Estab, ACA_insmark, state (FX), year(FX), state time trend
using_model <- using[c(8,206, 32:81, 86:101,103:152)]

glm_ACA_taxi_base.nb <- glm.nb(estab ~ ., data =  using_model)
glm_ACA_taxi_base.nb_tidy <- tidy(glm_ACA_taxi_base.nb)

using_model <- using[c(222,206, 32:81, 86:101,103:152)]
lm_ACA_taxi_base <- lm(estab_pop~ ., data =  using_model)
lm_ACA_taxi_base_tidy <- tidy(lm_ACA_taxi_base)


# Estab, uber active, ACA_insmark, Uber_active_ACA_insmark, state (FX), year(FX), state time trend, Wave (FX)
using_model <- using[c(8,206, 19, 217, 1, 86:101,103:152)]

glm_ACA_taxi.nb <- glm.nb(estab ~ ., data =  using_model)
glm_ACA_taxi.nb_tidy <- tidy(glm_ACA_taxi.nb)


using_model <- using[c(222,206, 19, 217, 1, 86:101,103:152)]
lm_ACA_taxi <- lm(estab_pop~ ., data =  using_model)
lm_ACA_taxi_tidy <- tidy(lm_ACA_taxi)

stargazer(glm_ACA.nb,glm_ACA_taxi_base.nb, glm_ACA_taxi.nb, 
          no.space = TRUE,
          omit.stat = c("LL","ser","f"),
          keep = c("ACA_insmark","Uber_active","Uber_active_ACA_insmark", "Constant"))



stargazer(lm_ACA,lm_ACA_taxi_base, lm_ACA_taxi,
          no.space = TRUE,
          omit.stat = c("LL","ser","f"),
          keep = c("ACA_insmark","Uber_active","Uber_active_ACA_insmark", "Constant"))




############################################

#    Results and graphs for Uber Models    #

############################################

# setwd("C:/Users/bngla/Dropbox/PhD requirements/ACA and alt/Output")
setwd("C:/Users/bglasner/Dropbox/PhD requirements/ACA and alt/Output")


Pred_data <- rbind(using_model[1,], using_model[1,], using_model[1,], using_model[1,], using_model[1,],
                   using_model[1,], using_model[1,], using_model[1,], using_model[1,], using_model[1,],
                   using_model[1,], using_model[1,], using_model[1,], using_model[1,], using_model[1,],
                   using_model[1,], using_model[1,], using_model[1,], using_model[1,], using_model[1,])

                   
Pred_data$rownumber = 1:dim(Pred_data)[1]


for(i in 1:20){
  Pred_data$YEAR.id[Pred_data$rownumber == i] <- 1996 + i
}

           
Pred_data$Uber_active <- 0
Pred_data$Uber_active[Pred_data$YEAR.id>2011] <- 1             
                   
Pred_data$ACA_insmark <- 0
Pred_data$ACA_insmark[Pred_data$YEAR.id>2012] <- 1             

                   
Pred_data$Uber_active_ACA_insmark <-0
Pred_data$Uber_active_ACA_insmark <- Pred_data$ACA_insmark * Pred_data$Uber_active                     
                   
Pred_data$dosage_uber <- 0
Pred_data$dosage_uber[Pred_data$YEAR.id>2011] <- 1             
Pred_data$dosage_uber[Pred_data$YEAR.id>2012] <- 2             
Pred_data$dosage_uber[Pred_data$YEAR.id>2013] <- 3             
Pred_data$dosage_uber[Pred_data$YEAR.id>2014] <- 4             
Pred_data$dosage_uber[Pred_data$YEAR.id>2015] <- 5             

Pred_data$dosage_uber_2 <- Pred_data$dosage_uber*Pred_data$dosage_uber

Pred_data$Ever_uber <- 1                  

pred_uber <- predict(glm_ACA_1, Pred_data, type = "response")                   


# Without Uber and ACA insurance market in 2013
Pred_data$Uber_active <- 0
Pred_data$Uber_active_ACA_insmark <- Pred_data$ACA_insmark * Pred_data$Uber_active                     

Pred_data$dosage_uber <- 0

Pred_data$dosage_uber_2 <- Pred_data$dosage_uber*Pred_data$dosage_uber

Pred_data$Ever_uber <- 0                 

pred_nouber <- predict(glm_ACA_1, Pred_data, type = "response")  


ggplot(data = Pred_data) +
  geom_point(aes(x=YEAR.id, y= pred_uber, colour = "With Uber")) +
  geom_point(data = Pred_data ,aes(x=YEAR.id,y= pred_nouber, colour = "Without Uber")) +
  ylab("Predicted # Nonemployer Establishments") + xlab("Year")+ 
  scale_colour_manual("", 
                      breaks = c("With Uber", "Without Uber"),
                      values = c("With Uber"="blue", "Without Uber"="red"))


using_model$pred <- predict(glm_ACA_1, using_model, type = "response") 

ggplot(data = using_model, aes(x=estab, y= pred )) +
  geom_point(color="red") +
  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  ylab("Predicted # Nonemployers") + xlab("Observed # Nonemployters")

# #plot the observed path of CBSAs
# health$pred <- predict(glm_ACA_1, health, type = "response") 
# 
# plot_list = list()
# city_list <- sort(unique(health$city_name))
# 
# 
# 
# health_city <- subset(health, city_name == "Akron")
# 
# p <- ggplot(health_city,aes(x=YEAR.id, y=ESTAB)) + 
#   geom_point() + 
#   theme(legend.position="bottom") +
#   ggtitle(health_city$city_name , health_city$`State Name`) + labs(y= "Price", x = "Date")
# 
# 
# 
# for(i in seq_along(city_list)){
#   health_city <- subset(health, city_name == i)
#   
#   p <- ggplot(health_city,aes(x=YEAR.id, y=ESTAB)) + 
#     geom_point() + 
#     theme(legend.position="bottom") +
#     ggtitle(health_city$city_name , health_city$`State Name`) + labs(y= "Price", x = "Date")
#   
#   plot_list[[i]] = p
#   
# }
# 
# pdf("plots_pred_prices.pdf",width = 11, height = 8)
# for (i in seq_along(cbsa_list)) {
#   print(plot_list[[i]])
# }
# dev.off()