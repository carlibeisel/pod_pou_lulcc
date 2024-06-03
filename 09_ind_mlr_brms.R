## MLR In Bayes ##

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created: 03/16/2023
# Date adapted: 04/16/2024

## Import packages
library(brms)
library(Matrix)
library(tidyverse)
library(dplyr)

## Import data
div <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full_0531.csv')
div <- div[!duplicated(div[c('Name', 'Year')]),] #remove duplicates
div <- subset(div, (Acre_feet > 0.00001)) # Remove data that has 0 
div_new <- subset(div, !(Name == 'Ester Simplot')) #Removes short dataframe


## Mean absolute error function

mae <- function(model, data_compare){
  yhat <- (posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(mean(abs(resid)))
}

## Scaling function for variables
scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

## Build function to run all diverisons through
mlr_brm <- function(data, name) {
  
  #Subset data to an individual canal
  sub_data <- subset(data, Name == name)
  
  # Select only variables going into the model
  sub_data <- sub_data[,c('Acre_feet',
                          'scale_class1_urban', 
                          'annual_prcp',
                          'irrig_temp',
                          'et',
                          'AF_used')]
  
  #Select variables to scale around mean
  vars_scale <- c('annual_prcp',
                  'irrig_temp')
  
  #Scale variables
  for (i in vars_scale){
    var_name <- i 
    sub_data[var_name] <- scale2sd(sub_data[,i])
  }
  
  # If storage water use is not 0, scale the variable
  if (mean(sub_data$AF_used) != 0){
    sub_data$AF_used <- scale2sd(sub_data$AF_used)
  }
  
  
  # Run the linear regression
  and_mod<-brm(Acre_feet ~ scale_class1_urban + et + annual_prcp + irrig_temp + AF_used, 
               data=sub_data,
               family = 'gamma',
               iter = 2000)
  
  # Save the model output in a table
  and_sum <- list(mod = and_mod)
  and_sum$r2 <- bayes_R2(and_mod) # Bayesian R2
  and_sum$MAE <- mae(and_mod, sub_data$Acre_feet) # Mean absolute error of model
  
  # Look at posterior predictive check 
  pp_check(and_mod)
  
  return(and_sum)
}


# Run all diversions through the Bayesian framework ####

mods <- list()
names <- unique(div_new$Name)

for (i in names) {
  data <- subset(div_new, Name == i)
  if (length(data$Name) > 5){
    mod_out <- mlr_brm(div_new, i)
    mods[[i]] <- mod_out
  }
}

# Extract summaries and check for convergence issues ####
sums <- list()
mod_fits <- list()
for (i in names){
  model <- mods[[i]]
  summary <- rbind(summary(model$mod)$fixed, summary(model$mod)$spec_pars)
  summary$Name <- i
  summary$vars <- rownames(summary)
  mod_fits[[i]] <- data.frame(cbind(model$r2, mae = model$MAE, name = i))
  sums[[i]] <- summary
}

sum.df <- bind_rows(sums) # merge all the summaries together
sum.df <- sum.df %>% as.data.frame(row.names = 1:nrow(.)) #fix the index
fit.df <- bind_rows(mod_fits) # merge all model fits together

# Save raw model summaries and model fits
write.csv(sum.df, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_sum_raw.csv')
write.csv(fit.df, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_fit_raw.csv')

# # Read in files if R crashes on model reruns
sum.df <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_sum_raw.csv')
fit.df <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_fit_raw.csv')

# Extract problematic runs based on Rhat
probs <- sum.df %>%
  group_by(Name) %>%
  subset(Rhat > 1.01)

unique(probs$Name) # 8 of the 60 canals had problematic Rhats

# Check for effective sample size

ESS <- sum.df %>%
  group_by(Name) %>%
  subset(Bulk_ESS < 400 | Tail_ESS < 400) 

unique(ESS$Name) # Same 8 canals with convergence issues also had low ESS

# Extract models of each problematic run for visual convergence and potential rerun
prob_names <- unique(ESS$Name)

prob_mods <- list()
for (i in prob_names){
  prob_mods[[i]] <- mods[[i]]
  print(pp_check(prob_mods[[i]]$mod))
}

# Try to rerun models with different specifications ####
# What was changed: 
#         1) Adapt delta
#         2) Variables that had no change through time 
#  This was specifically for places that saw no urban change or did not use storage water

mlr_brm_probs <- function(data, name, remove = c('both', 'urb', 'storage', 'none')) {
  
  #Subset data to an individual canal
  sub_data <- subset(data, Name == name)
  
  # Select only variables going into the model
  sub_data <- sub_data[,c('Acre_feet',
                          'scale_class1_urban',
                          'annual_prcp',
                          'irrig_temp',
                          'et',
                          'AF_used')]
  
  #Select variables to scale around mean
  vars_scale <- c('annual_prcp',
                  'irrig_temp')
  
  #Scale variables
  for (i in vars_scale){
    var_name <- i 
    sub_data[var_name] <- scale2sd(sub_data[,i])
  }
  
  # If storage water use is not 0, scale the variable
  if (mean(sub_data$AF_used) != 0){
    sub_data$AF_used <- scale2sd(sub_data$AF_used)
  }
  
  # Run model conditionally on what needs to be removed from the analysis
  
  if (var == 'storage'){
    #model does not include storage water as predictor variables
    and_mod<-brm(Acre_feet ~ scale_class1_urban + et + annual_prcp + irrig_temp, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000, 
                 control = list(adapt_delta = 0.99))
  }
  
  if (var == 'both'){
    #run model with no urban area or storage water used 
    and_mod<-brm(Acre_feet ~ et + annual_prcp + irrig_temp, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000,
                 control = list(adapt_delta = 0.99))
  }
  if (var == 'urb'){
    # run model with no urban area
    and_mod<-brm(Acre_feet ~ et + annual_prcp + irrig_temp + AF_used, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000,
                 control = list(adapt_delta = 0.99))
  }
  if (var == 'none') {
    # run model with all variables
    and_mod<-brm(Acre_feet ~ scale_class1_urban + et + annual_prcp + irrig_temp + AF_used, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000)
  }
  
  # Save the model output in a table
  and_sum <- list(mod = and_mod)
  and_sum$r2 <- bayes_R2(and_mod) # Bayesian R2
  and_sum$MAE <- mae(and_mod, sub_data$Acre_feet) # Mean absolute error of model
  
  return(and_sum)
}


mod_rerun <- list()
# Need to remove variables with no change for model analysis
for (i in prob_names){
  sub_data <- subset(div_new, Name == i)
  if ((first(sub_data$scale_class1_urban) - last(sub_data$scale_class1_urban)) == 0) {
    var = 'urb'
  }
  if (mean(sub_data$AF_used) == 0){
    var = 'storage'
  }
  if (((first(sub_data$scale_class1_urban) - last(sub_data$scale_class1_urban)) == 0) & 
      (mean(sub_data$AF_used) == 0)){
    var = 'both'
  }
  model <- mlr_brm_probs(data = div_new, i, remove = var)
  mod_rerun[[i]] <- model
}

# Check for convergence issues again on model reruns ####
for (i in prob_names){
  model <- mod_rerun[[i]]$mod
  print(i)
  print(summary(model))
}

sum_probs <- list()
fits_probs <- list()
for (i in prob_names){
  model <- mod_rerun[[i]]
  summary <- rbind(summary(model$mod)$fixed, summary(model$mod)$spec_pars)
  summary$Name <- i
  summary$vars <- rownames(summary)
  fits_probs[[i]] <- data.frame(cbind(model$r2, mae = model$MAE, name = i))
  sum_probs[[i]] <- summary
}

sum.df.prob <- bind_rows(sum_probs) # merge all the summaries together
sum.df.prob <- sum.df.prob %>% as.data.frame(row.names = 1:nrow(.)) #fix the index
fit.df.prob <- bind_rows(fits_probs) # merge all model fits together
fit.df.prob <- fit.df.prob %>% as.data.frame(row.names = 1:nrow(.)) #fix the index

# Save dataframes from reruns 
write.csv(sum.df.prob, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_sum_prob.csv')
write.csv(fit.df.prob, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_fit_prob.csv')


# Extract problematic runs based on Rhat
probs.prob <- sum.df.prob %>%
  group_by(Name) %>%
  subset(Rhat > 1.01)

unique(probs.prob$Name) # 8 of the 60 canals had problematic Rhats

# Check for effective sample size

ESS.prob <- sum.df.prob %>%
  group_by(Name) %>%
  subset(Bulk_ESS < 400 | Tail_ESS < 400) 

unique(ESS.prob$Name)

# If all models are good on convergence, merge coefficient and model fit dataframes ####

# Coefficient dataframe
sum.df.final <- subset(sum.df, !(Name %in% prob_names))
# sum.df.final <- sum.df.final[,-1] # removes index column after reimporting dataframe
sum.df.final <- bind_rows(sum.df.final, sum.df.prob) # will have less rows than original because variables removed

# Model fit dataframe
fit.df.final <- subset(fit.df, !(name %in% prob_names))
# fit.df.final <- fit.df.final[,-1] #removed index column after reimporting dataframe
# fit.df.prob <- fit.df.prob[-1] #removed index column after reimporting dataframe
fit.df.final <- bind_rows(fit.df.final, fit.df.prob)

# Save final model summaries and model fits
write.csv(sum.df.final, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_sum_final.csv')
write.csv(fit.df.final, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_fit_final.csv')

## Rerun brms models with no standardizing of variables ####

mlr_brm_ns <- function(data, name, remove = c('both', 'urb', 'storage', 'none')) {
  
  #Subset data to an individual canal
  sub_data <- subset(data, Name == name)
  
  # No standardization of variables
  sub_data$prcp.m <- sub_data$annual_prcp/1000
  sub_data$KAF_used <- sub_data$AF_used/10000
  
  # Run model conditionally on what needs to be removed from the analysis
  
  if (var == 'storage'){
    #model does not include storage water as predictor variables
    and_mod<-brm(Acre_feet ~ scale_class1_urban + et + prcp.m + irrig_temp, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000, 
                 control = list(adapt_delta = 0.99,
                                max_treedepth = 20))
  }
  
  if (var == 'both'){
    #run model with no urban area or storage water used 
    and_mod<-brm(Acre_feet ~ et + prcp.m + irrig_temp, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000,
                 control = list(adapt_delta = 0.99,
                                max_treedepth = 20))
  }
  if (var == 'urb'){
    # run model with no urban area
    and_mod<-brm(Acre_feet ~ et + prcp.m + irrig_temp + KAF_used, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000,
                 control = list(adapt_delta = 0.99,
                                max_treedepth = 20))
  }
  if (var == 'none') {
    # run model with all variables
    and_mod<-brm(Acre_feet ~ scale_class1_urban + et + prcp.m + irrig_temp + KAF_used, 
                 data=sub_data,
                 family = 'gamma',
                 iter = 2000,
                 control = list(adapt_delta = 0.99,
                                max_treedepth = 20))
  }
  
  # Save the model output in a table
  and_sum <- list(mod = and_mod)
  and_sum$r2 <- bayes_R2(and_mod) # Bayesian R2
  and_sum$MAE <- mae(and_mod, sub_data$Acre_feet) # Mean absolute error of model
  
  return(and_sum)
}

# Run all diversions through the Bayesian framework ####

mods <- list()
names <- unique(div_new$Name)

for (i in names){
  sub_data <- subset(div_new, Name == i)
  if ((first(sub_data$scale_class1_urban) - last(sub_data$scale_class1_urban)) == 0) {
    var = 'urb'
  }
  if (mean(sub_data$AF_used) == 0){
    var = 'storage'
  }
  if (((first(sub_data$scale_class1_urban) - last(sub_data$scale_class1_urban)) == 0) & 
      (mean(sub_data$AF_used) == 0)){
    var = 'both'
  }
  if (((first(sub_data$scale_class1_urban) - last(sub_data$scale_class1_urban)) != 0) & 
      (mean(sub_data$AF_used) != 0)){
    var = 'none'
  }
  print(i)
  print(var)
  model <- mlr_brm_ns(data = div_new, i, remove = var)
  mods[[i]] <- model
}

# Extract summaries and check for convergence issues ####
sums <- list()
mod_fits <- list()
for (i in names){
  model <- mods[[i]]
  summary <- rbind(summary(model$mod)$fixed, summary(model$mod)$spec_pars)
  summary$Name <- i
  summary$vars <- rownames(summary)
  mod_fits[[i]] <- data.frame(cbind(model$r2, mae = model$MAE, name = i))
  sums[[i]] <- summary
}

sum.df <- bind_rows(sums) # merge all the summaries together
sum.df <- sum.df %>% as.data.frame(row.names = 1:nrow(.)) #fix the index
fit.df <- bind_rows(mod_fits) # merge all model fits together

# Extract problematic runs based on Rhat
probs <- sum.df %>%
  group_by(Name) %>%
  subset(Rhat > 1.01)

# No problematic values

# Check effect sizes that are large
urb <- subset(sum.df, vars == 'scale_class1_urban')
kaf <- subset(sum.df, vars == 'KAF_used')
prcp <- subset(sum.df, vars =='prcp.m')
temp <- subset(sum.df, vars == 'irrig_temp')
et <- subset(sum.df, vars == 'et')

# Export dataframes
write.csv(sum.df, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_sum_final.csv')
write.csv(fit.df, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_fit_final.csv')

