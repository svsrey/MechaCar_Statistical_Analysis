library(tidyverse)

##multiple regression on MechaCar dataset

#import & read csv file
mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)

#perform linear regression
mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_data)

#determine the p-value & r-squared
summary(mecha_lm)

#summary analysis on suspension coil dataset
#read in csv file
suspension_data <- read.csv("Suspension_Coil.csv",stringsAsFactors = F, check.names = F)

#total summary
total_summary <- suspension_data %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

#summary by lot
lot_summary <- suspension_data %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')

#suspension Coil T-Tests
#perform t-test for all lots
t.test(suspension_data$PSI,mu = 1500)

#t-test for lot 1
t.test(subset(suspension_data,Manufacturing_Lot=="Lot1")$PSI,mu =1500)

#t-test for lot 2
t.test(subset(suspension_data,Manufacturing_Lot=="Lot2")$PSI,mu =1500)

#t-test for lot 3
t.test(subset(suspension_data,Manufacturing_Lot=="Lot3")$PSI,mu =1500)


