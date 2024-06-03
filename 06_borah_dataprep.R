# Final Model Selection #### 

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created: 02/01/23
# Date adapted: 04/16/2024

# This script uses a GLMM, a GLMM with ARMA, and MLR to understand the relationship
# of canal flows with urban area and climate.

# Import packages

library(brms)
library(bayesplot)
library(dplyr)
library(Matrix)
library(tidyverse)
library(tidybayes)
library(readr)
library(tibble)
library(ggrepel)
library(flexmix)
#install.packages('lattice')
library(modelr)
library(loo)
library(here)
#install.packages('tseries')
library(tseries)
#install.packages('urca')
library(urca) #kpss test
#install.packages('plm')
library(plm)
#install.packages('pracma')
library(pracma)
library(dplyr)

#PREPROSSESING
# ------------------------------------------------------------------------------- #
# This part of the script standardizes variables and creates a new, scaled column  
# in the original .csv file. 
# ------------------------------------------------------------------------------- #

# Without zeros
data <- data.frame(read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full_0531.csv'))
data <- data[-c(1,6,37,38)] # drops Python index output with csv
data <- subset(data, select=-c(Month, DayofYear, Irrigation.Year, Sum, Diversion..cfs.))
data['Mar_et'][is.na(data['Mar_et'])] <- 0 #fill NA et values with 0
data['contagion'][is.na(data['contagion'])] <- 100 # fill NA contagion values with 100
nas <- data[rowSums(is.na(data)) > 0, ] #check for any data with remaining NA values
data <- na.omit(data)

# The explanatory variables will be substracted by the mean 
# and then divided by two standard deviations to place data on similar range. 

scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

col_name <- c('ant_prcp',
              'annual_prcp',
              'irrig_temp',
              'JuneAug_temp',
              'Mar_tmp',
              'Mar_prcp',
              'LP_inflows',
              'Max_Fill',
              'Carryover',
              'AF_used',
              'AF_remaining',
              'AF_available')

for (i in col_name) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- scale2sd(data[,i])
}

## Convert percentages to proportions to get between 0 and 1

col_name <- c('class1_urban',
              'class2_crops',
              'contagion',
              'largest_patch_index')

for (i in col_name) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- (data[,i])/100
}

tt <- table(data$Name)
data <- subset(data, Name %in% names(tt[tt>4]))

write.csv(data, '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full_0531.csv', row.names = FALSE)



# With zeros data
data <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/mixed_model_input_0531.csv')
data <- data[!duplicated(data[c('Name', 'Year')]),] #remove duplicates

# The explanatory variables will be substracted by the mean 
# and then divided by two standard deviations to place data on similar range. 

scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

col_name <- c('ant_prcp',
              'annual_prcp',
              'irrig_temp',
              'JuneAug_temp',
              'Mar_tmp',
              'Mar_prcp',
              'LP_inflows',
              'Max_Fill',
              'Carryover',
              'AF_used',
              'AF_remaining',
              'AF_available')

for (i in col_name) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- scale2sd(data[,i])
}

## Convert percentages to proportions to get between 0 and 1

col_name <- c('class1_urban',
              'class2_crops',
              'contagion',
              'largest_patch_index')

for (i in col_name) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- (data[,i])/100
}

tt <- table(data$Name)
data <- subset(data, Name %in% names(tt[tt>4]))

# Storage data 
data$perc_used <- ifelse(data$AF_available > 0, data$AF_used/data$AF_available, NA)
data$wr_storage <- ifelse(data$AF_available >0, 1, 0)

write.csv(data, '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/mixed_model_input_0531.csv', row.names = FALSE)


# ARMA MODEL ####

# ------------------------------------------------------------------------------- #
# This part of the script uses a Bayesian Generalized Linear Mixed model with an 
# autoregressive moving average component. This goes through removing data with 
# data gaps and making non-stationary variables, stationary.
# ------------------------------------------------------------------------------- #

# Import the dataset to work with
div <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full_0531.csv')
div$lt <- log(div$Acre_feet)

# Remove data that has 0 

div_arma <- subset(div, (Acre_feet > 0.00001))
div_arma$lt <- log(div_arma$Acre_feet)
names <- unique(div_arma$Name)

# Find diversions with less than 34 years worth of data, and remove if there
# are data gaps, or if discharge for a given year in the middle was 0
for (i in names){
  sub <- subset(div_arma, Name == i)
  if (length(sub$Acre_feet) < 34){
    print(i)
    print(length(sub$Acre_feet))
  }
} 

remove <- c("Barber pumps", 
            "Mace-Mace Canal",
            "River Run",
            "Surprise Valley and Micron",
            "Thomas Aiken Canal",
            "Warm Springs Canal", 
            'Ester Simplot',
            "Stutheit",
            "Riverside Village",
            "McCurry Pump",
            "Golden Gate Canal",
            "Capitol View Canal",
            "Shakespeare",
            "Suez")

div_arma <- subset(div_arma, !(Name %in% remove))           

# Check after removal to ensure continuous datasets
names <- unique(div_arma$Name)
for (i in names){
  sub <- subset(div_arma, Name == i)
  if (length(sub$Acre_feet) < 34){
    print(i)
    print(length(sub$Acre_feet))
  }
}

# Check for stationary for both response and predictor variables using Dickey Fuller Test

# Variables include: Annual diversion volume (Acre_feet), evapotranspiration (et), urban
# proportion (scale_class1_urban), annual precip (scale_annual_prcp), avg max 
# irrigation season temp (scale_irrig_temp), storage water use (scale_AF_used)

# Stationarity test for panel data

# Need change in a groups for a test, filter out groups with no change in urban proportion
ll.test <- div_arma %>%
  group_by(Name) %>%
  filter((max(class1_urban) - min(class1_urban)) > 0) %>%
  ungroup()
# Need change in a groups for a test, filter out groups that use no storage over whole time period
# for stationarity test
ll.use <- div_arma %>%
  group_by(Name) %>%
  filter((max(AF_used) - min(AF_used)) > 0) %>%
  ungroup()

# Panel dataframe for test
new_use <- pdata.frame(ll.use, index = c('Name', 'Year'))
new_use$AF_used <- as.numeric(new_use$AF_used) # change from integer to numeric for test
new_urb <- pdata.frame(ll.test, index = c('Name', 'Year'))
new <-pdata.frame(div_arma, index = c('Name', 'Year'))

urb.test <- purtest(new_urb$class1_urban, data = new_urb, lags = 'AIC', test = 'levinlin') #non-stationary
use.test <- purtest(new_use$AF_used, data = new_use, lags = 'AIC', test = 'levinlin') #non-stationary
lt.test <- purtest(new$lt, data = new, lags ='AIC', test = 'levinlin')
AF.test <- purtest(new$Acre_feet, data = new, lags = 'AIC', test = 'levinlin')
temp.test <- purtest(new$irrig_temp, data = new, lags = 'AIC', test = 'levinlin')
prcp.test <- purtest(new$annual_prcp, data = new, lags = 'AIC', test = 'levinlin') #non-stationary
et.test <- purtest(new$et, data = new, lags = 'AIC', test = 'levinlin') # non-stationary

# ET, urban, and storage use will all be differenced. Other variables don't need to be

arma_input <- div_arma %>%
  select(Name, Year, Acre_feet, irrig_temp, annual_prcp, AF_used, class1_urban, et, lt)

arma_input = arma_input %>%
  group_by(Name) %>%
  mutate(d.et = c(NA, diff(et)),
         d.urb = c(NA, diff(class1_urban)),
         d.use = c(NA, diff(AF_used)),
         d.prcp = c(NA, diff(annual_prcp)),
         d.temp = c(NA, diff(irrig_temp)),
         d.Acre_feet = c(NA, diff(Acre_feet))) %>%
  ungroup()
arma_input <- na.omit(arma_input)

# Make sure everything is numeric
arma_input$d.et <- as.numeric(arma_input$d.et)
arma_input$d.temp <- as.numeric(arma_input$d.temp)
arma_input$d.use <- as.numeric(arma_input$d.use)
arma_input$d.Acre_feet <- as.numeric(arma_input$d.Acre_feet)
arma_input$d.urb <- as.numeric(arma_input$d.urb)
arma_input$d.prcp <- as.numeric(arma_input$d.prcp)

# Check for stationarity in groups that were differenced 

# Need change in a groups for a test, filter out groups with no change in urban proportion
ll.test <- arma_input %>%
  group_by(Name) %>%
  filter((max(class1_urban) - min(class1_urban)) > 0) %>%
  ungroup()
# Need change in a groups for a test, filter out groups that use no storage over whole time period
# for stationarity test
ll.use <- arma_input %>%
  group_by(Name) %>%
  filter((max(AF_used) - min(AF_used)) > 0) %>%
  ungroup()

new_use <- pdata.frame(ll.use, index = c('Name', 'Year'))
new_use$d.use <- as.numeric(new_use$d.use)
new_urb <- pdata.frame(ll.test, index = c('Name', 'Year'))
new <-pdata.frame(arma_input, index = c('Name', 'Year'))
et.test <- purtest(new$d.et, data = new, lags = 'AIC', test = 'levinlin') #stationary
urb.test <- purtest(new_urb$d.urb, data = new_urb, lags = 'AIC', test = 'levinlin') #stationary
use.test <- purtest(new_use$d.use, data = new_use, lags = 'AIC', test = 'levinlin') #stationary
AF.test <- purtest(new$d.Acre_feet, data = new, lags = 'AIC', test = 'levinlin')
prcp.test <- purtest(new$d.prcp, data = new, lags = 'AIC', test = 'levinlin')
temp.test <- purtest(new$d.temp, data = new, lags = 'AIC', test = 'levinlin')

# Standardize all variables before exporting
scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

vars <- c('d.use',
          'd.urb',
          'd.prcp',
          'd.temp',
          'd.et')

for (i in vars){
  var <- colnames(arma_input[i])
  new_col_name <- paste('scale_', var, sep='')
  col <- arma_input %>% select(i)
  arma_input[new_col_name] <- scale2sd(unlist(arma_input[,i]))
}

# Export data for model in borah
write.csv(arma_input, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/arma_input_0531.csv')

# MODEL WITH NO ARMA ####

# Import appropriate data 

div <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full_0531.csv')
div$lt <- log(div$Acre_feet)
div <- subset(div, (Acre_feet > 0.00001)) # Remove data that has 0 

str(div2)
# Import file with Quinns Pond and Caldwell Lowline
div2 <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/mixed_model_input_0531.csv')
div2$lt <- log(div2$Acre_feet)
sel.name <- c("Quinns Pond", 'Caldwell Lowline Canal')
div2 <- subset(div2, Name %in% sel.name)
div_new <- rbind(div,div2)

div_new <- div_new %>%
  select(Year, Name, Acre_feet, class1_urban, et, lt, AF_used, annual_prcp, irrig_temp) #changed from irrig_prcp to annual_prcp

# Scale response variables 
vars <- c('class1_urban',
          'et',
          'AF_used',
          'annual_prcp', #changed from irrig_prcp to annual_prcp
          'irrig_temp')

for (i in vars){
  var <- colnames(div_new[i])
  new_col_name <- paste('scale_', var, sep='')
  col <- div_new %>% select(i)
  div_new[new_col_name] <- scale2sd(unlist(div_new[,i]))
}


# Export data for model in borah
write.csv(div_new, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/glmm_input_0531.csv')

