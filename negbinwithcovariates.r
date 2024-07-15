# Initializing libraries and reading the spreadsheet
library(tidyverse)
library(readxl)
library(MASS)  
bikedata <- read_csv("C:/Users/lisaj/Downloads/Sharrows/Products/editedcyclingdata.csv")

bikedata$infrasimple <- as.factor(bikedata$infrasimple)

# converting traffic data into a numeric form and creating tertiles (note, no crashes in the low-tertile data!)
bikedata$carsavg <- as.numeric(bikedata$carsavg) 
bikedata$bikesavg <- as.numeric(bikedata$bikesavg)
bikedata$cars_group <- cut(bikedata$carsavg, breaks = 3, labels = c("low", "medium", "high")) 
bikedata$bikes_group <- cut(bikedata$bikesavg, breaks = 3, labels = c("low", "medium", "high")) 

# This regression can’t handle negative values, so we add a constant based on the most negative value to the whole dataset
bikedata$change_in_crashes <- bikedata$crashesafter - bikedata$crashesbefore + abs(min(bikedata$crashesafter - bikedata$crashesbefore))

# Only including sharrows and controls (the other treatments do not have enough crash events)
bikedata_filtered <- bikedata %>%
  filter(infrasimple %in% c("Sharrows", "None"))

# Fitting and summarizing the negative binomial regression model
model_negbin <- glm.nb(change_in_crashes ~ infrasimple + cars_group + bikes_group, data = bikedata_filtered)
summary(model_negbin)
