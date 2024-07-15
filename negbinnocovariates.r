# Initializing libraries and reading the spreadsheet
library(tidyverse)
library(readxl)
library(MASS)  
bikedata <- read_csv("C:/Users/lisaj/Downloads/Sharrows/Products/editedcyclingdata.csv")

bikedata$infrasimple <- as.factor(bikedata$infrasimple)

# This regression can’t handle negative values, so we add a constant based on the most negative value to the whole dataset
bikedata$change_in_crashes <- bikedata$crashesafter - bikedata$crashesbefore + abs(min(bikedata$crashesafter - bikedata$crashesbefore))

# Only including sharrows and controls (the other treatments do not have enough crash events)
bikedata_filtered <- bikedata %>%
  filter(infrasimple %in% c("Sharrows", "None"))

# Fitting and summarizing the negative binomial regression model
model_negbin <- glm.nb(change_in_crashes ~ infrasimple + cars_group + bikes_group, data = bikedata_filtered)
summary(model_negbin)
