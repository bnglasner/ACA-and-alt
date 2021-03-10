# interactive Fixed Effects vs Synthetic test
# Medicaid Paper
# Ben Glasner
# 2/14/2020
library(ggplot2)
library(gsynth)
library(dplyr)

set.seed(989898)
###############################################
###         Build the fake Uber data        ###
###############################################

uber_year <- sample(c(0, 1, 2, 3, 4, 5, 6), size = 50, replace = TRUE, prob = c(.3, .05, .05, .2, .2, .1, .1)) + 2011
Urban <- rep(0, each=50)
ids <- seq(1, by=2, len=50)
Uber_year_rural <- as.data.frame(cbind(ids,Urban,uber_year))

uber_year <- sample(c(0, 1, 2, 3, 4, 5, 6), size = 50, replace = TRUE, prob = c(.1, .1, .1, .4, .1, .1, .1)) + 2011
Urban <- rep(1, each=50)
ids <- seq(2,100,2)
Uber_year_urban <- as.data.frame(cbind(ids,Urban,uber_year))

uber_data <- rbind(Uber_year_rural,Uber_year_urban)
uber_data <- uber_data %>% dplyr::select("ids","uber_year")
rm(Uber_year_rural, Uber_year_urban)
uber_data$uber_year[uber_data$uber_year<2012] <- 0

###############################################
###         Build the fake data             ###
###############################################

ids <- rep(rep(1:100,each=1),18)
years <- rep(2000:2017, each=100)
Urban <- rep(seq(0,1,by=10), each=1800) + seq(0,1)
Medicaid_expansion <- rep(rep(0:1,each=2),450)

test_data <- as.data.frame(cbind(ids,years,Urban,Medicaid_expansion))
test_data <- merge(test_data,uber_data)

test_data$expansion <- 0
test_data$expansion[test_data$Medicaid_expansion==1 & test_data$years>2013]<-1

test_data$recession <- 0
test_data$recession[test_data$years>2007 & test_data$years<2011] <- 1

test_data$Uber_active <- 0
test_data$Uber_active[test_data$uber_year==test_data$years] <- 1
test_data$Uber_active[test_data$uber_year<test_data$years] <- 1

test_data$group <-"Rural No Expansion"
test_data$group[test_data$Urban==0 & test_data$Medicaid_expansion==1] <- "Rural Expansion"
test_data$group[test_data$Urban==1 & test_data$Medicaid_expansion==0] <- "Urban No Expansion"
test_data$group[test_data$Urban==1 & test_data$Medicaid_expansion==1] <- "Urban Expansion"

test_data$expansion_urban <- test_data$expansion*test_data$Urban

test_data$estab_pop[test_data$group=="Rural No Expansion"] <- 
  (
    .06 
    + .0015*(test_data$years[test_data$group=="Rural No Expansion"]-1999) 
    - 0.0014*test_data$recession[test_data$group=="Rural No Expansion"]*(test_data$years[test_data$group=="Rural No Expansion"]-1999) 
    + 0.001*test_data$Uber_active[test_data$group=="Rural No Expansion"]
)
test_data$estab_pop[test_data$group=="Urban No Expansion"] <- 
  (
    .054 
    + .0012*(test_data$years[test_data$group=="Urban No Expansion"]-1999) 
    - 0.0012*test_data$recession[test_data$group=="Urban No Expansion"]*(test_data$years[test_data$group=="Urban No Expansion"]-1999) 
    + 0.001*test_data$Uber_active[test_data$group=="Urban No Expansion"]
  )
test_data$estab_pop[test_data$group=="Urban Expansion"] <- 
  (
  .054 
  + .001*(test_data$years[test_data$group=="Urban Expansion"]-1999) 
  - 0.001*test_data$recession[test_data$group=="Urban Expansion"]*(test_data$years[test_data$group=="Urban Expansion"]-1999)  
  - 0.002*test_data$expansion[test_data$group=="Urban Expansion"] 
  + 0.001*test_data$Uber_active[test_data$group=="Urban Expansion"]
  )
test_data$estab_pop[test_data$group=="Rural Expansion"] <- 
  (
    .063 
    + .001*(test_data$years[test_data$group=="Rural Expansion"]-1999) 
    - .001*test_data$recession[test_data$group=="Rural Expansion"]*(test_data$years[test_data$group=="Rural Expansion"]-1999)  
    - 0.001*test_data$expansion[test_data$group=="Rural Expansion"] 
    + 0.001*test_data$Uber_active[test_data$group=="Rural Expansion"]
  )
test_data$estab_pop_err <- jitter(test_data$estab_pop)

ggplot(data = test_data, 
       aes(x = years, y = estab_pop_err, group = group, color = group,)) +
  geom_smooth()+
  xlab("Year") +
  ylab("Estab Per Person") +
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
  
############################
# Two-way Fixed Effects #
############################
estab_0 <- lm(data = test_data,
              estab_pop_err~
                expansion +
                factor(ids)+ 
                factor(years))
summary(estab_0)


estab_urban <- lm(data = subset(test_data,Urban==1),
              estab_pop_err~
                expansion +
                factor(ids)+ 
                factor(years))
summary(estab_urban)

estab_rural <- lm(data = subset(test_data,Urban==0),
              estab_pop_err~
                expansion +
                factor(ids)+ 
                factor(years))
summary(estab_rural)

############################
# Interacted Fixed Effects #
############################

inter_fe_estab0 <- interFE(estab_pop_err ~ 
                             expansion,
                           data = test_data, index=c("ids","years"),
                           force = "two-way", nboots = 100, se = TRUE)

inter_fe_estab1 <- interFE(estab_pop_err ~ 
                             expansion,
                           data = subset(test_data,Urban==1), index=c("ids","years"),
                           force = "two-way", nboots = 100, se = TRUE)

inter_fe_estab2 <- interFE(estab_pop_err ~ 
                             expansion,
                           data = subset(test_data,Urban==0), index=c("ids","years"),
                           force = "two-way", nboots = 100, se = TRUE)

############################
#         gsynth           #
############################

 gsynth1 <- gsynth(Y = "estab_pop_err",
                   D = "expansion", 
                   data = test_data,
                   index = c("ids","years"),
                   force = "time",
                   CV = TRUE, 
                   se = TRUE,
                   estimator = "mc",
                   nlambda = 10,
                   inference = "nonparametric", 
                   nboots = 300, 
                   parallel = TRUE, 
                   cores =4)
print(gsynth1)


gsynth1 <- gsynth(Y = "estab_pop_err",
                  D = "expansion", 
                  data = subset(test_data,Urban==1),
                  index = c("ids","years"),
                  force = "time",
                  CV = TRUE, 
                  se = TRUE,
                  estimator = "mc",
                  nlambda = 10,
                  inference = "nonparametric", 
                  nboots = 300, 
                  parallel = TRUE, 
                  cores =4)
print(gsynth1)


gsynth1 <- gsynth(Y = "estab_pop_err",
                  D = "expansion", 
                  data = subset(test_data,Urban==0),
                  index = c("ids","years"),
                  force = "time",
                  CV = TRUE, 
                  se = TRUE,
                  estimator = "mc",
                  nlambda = 10,
                  inference = "nonparametric", 
                  nboots = 300, 
                  parallel = TRUE, 
                  cores =4)
print(gsynth1)
