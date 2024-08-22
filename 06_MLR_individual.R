# Individual MLR for each diverison #

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created: 01/23/2023
# Date adapted: 04/16/2024


# The purpose of this script was to run an individual multiple linear regression for each 
# of the 63 diversions. Function and script based on Kendra Kaiser Wood River Collaborative Analysis.

# Import packages
library(Matrix)
library(tidyverse)
library(dplyr)
#install.packages('leaps')
library(leaps) #regsubsets
library(mvtnorm) #multivariate distributions
#install.packages('caret')
library(caret) # for train command

# Import the data
div <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/model_input_0822.csv')
div <- div[!duplicated(div[c('Name', 'Year')]),] #remove duplicates
div <- subset(div, (Acre_feet > 0.00001)) # Remove data that has 0 
div_new <- subset(div, !(Name == 'Ester Simplot')) #Removes short dataframe

# Function for scaling data
scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

# Function to find best model for each diversion
mlr_run <- function(data, name) {
  
  #Subset data to an individual canal
  sub_data <- subset(data, Name == name)
  
  # Select only variables going into the model
  sub_data <- sub_data[,c('Acre_feet',
                          'class1_urban',
                          'irrig_prcp',
                          'irrig_temp',
                          'et',
                          'AF_used',
                          'Carryover',
                          'ubrb_prcp',
                          'pivot_prop',
                          'sw_wr',
                          'gw_wr')]
  
  #Select variables to scale around mean
  vars_scale <- c('irrig_prcp', #changed from irrig_prcp
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
  
  # Use regsubsets to find model with lowest BIC 
  tryCatch({regsubsets.out<-regsubsets(sub_data$Acre_feet~., data=sub_data, nbest=1, nvmax=12)}, 
           error= function(e) {print("Fit did not work")}) #error catch
  reg_sum <- summary(regsubsets.out)
  rm(regsubsets.out)
  
  # Select variables from model with lowest BIC
  vars<-reg_sum$which[which.min(reg_sum$bic),]
  and_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])
  
  # Model formula
  form<- paste("Acre_feet~ ", paste(and_sum$vars, collapse=" + "), sep = "")
  
  # Run the linear regression
  and_mod<-lm(form, data=sub_data)
  
  # Save the adjusted R-squared in the summary table
  and_sum$lm<-summary(and_mod)$adj.r.squared
  and_sum$coef_val <- and_mod$coefficients
  and_sum$pval <- summary(and_mod)$coefficients[,'Pr(>|t|)']
  
  # Save summary of LOOCV
  ctrl <- trainControl(method = 'LOOCV')
  model <- train(as.formula(form), data = sub_data, method = "lm", trControl = ctrl)
  and_sum$loocv<- model$results
  mod.red<- resid(model)
  and_sum$name <- name
  
  plt_norm <- plot((model$pred$obs)/1000, (model$pred$pred)/1000, pch=19, xlab="Observed", ylab=name)
  abline(0,1,col="gray50",lty=1)
  
  return(and_sum)
}


# Pull names of all the diversions
names <- unique(div_new$Name)

mods <- list()

# Run a for loop to determine best predictors and r-squared of model for each diversion
for (i in names) {
  data <- subset(div_new, Name == i)
  if (length(data$Name) > 5){
    mod_out <- mlr_run(div_new, i)
    mods[[i]] <- mod_out
  }
}

df <- data.frame(names)
df$prcp <- NA
df$prcp.coef <- NA
df$prcp.p <- NA
df$temp <- NA
df$temp.coef <-NA
df$temp.p <- NA
df$urb <- NA
df$urb.coef <- NA
df$urb.p <- NA
df$stor <- NA
df$stor.coef <- NA
df$stor.p <- NA
df$et <- NA
df$et.coef <- NA
df$et.p <- NA
df$adjr2 <- NA
df$ubrb_prcp <- NA
df$ubrb_prcp.coef <- NA
df$ubrb_prcp.p <- NA
df$sw_wr <- NA
df$sw_wr.coef <- NA
df$sw_wr.p <- NA
df$gw_wr <- NA
df$gw_wr.coef <- NA
df$gw_wr.p <- NA
df$carryover <- NA
df$carryover.coef <- NA
df$carryover.p <- NA


for (i in 1:60) {
  name <- names[i]
  output <- mods[[i]]
  vars <- output$vars
  if ("AF_used" %in% vars) {
    df[i, 'stor'] = 1
    df[i, 'stor.coef'] = output$coef_val['AF_used']
    pval = output$pval['AF_used']
    if (pval < 0.05){
      df[i, 'stor.p'] = 1
    }
    else{
      df[i, 'stor.p'] = 0
    }
  }
  else {
    df[i,'stor'] = 0
  }
  if ("scale_class1_urban" %in% vars) {
    df[i, 'urb'] = 1
    df[i, 'urb.coef'] = output$coef_val['scale_class1_urban']
    pval = output$pval['scale_class1_urban']
    if (pval < 0.05){
      df[i, 'urb.p'] = 1
    }
    else{
      df[i, 'urb.p'] = 0
    }
  }
  else {
    df[i,'urb'] = 0
  }
  if ("irrig_prcp" %in% vars) {
    df[i, 'prcp'] = 1
    df[i, 'prcp.coef'] = output$coef_val['irrig_prcp']
    pval = output$pval['irrig_prcp']
    if (pval < 0.05){
      df[i, 'prcp.p'] = 1
    }
    else{
      df[i, 'prcp.p'] = 0
    }
  }
  else {
    df[i,'prcp'] = 0
  }
  if ("irrig_temp" %in% vars) {
    df[i, 'temp'] = 1
    df[i, 'temp.coef'] = output$coef_val['irrig_temp']
    pval = output$pval['irrig_temp']
    if (pval < 0.05){
      df[i, 'temp.p'] = 1
    }
    else{
      df[i, 'temp.p'] = 0
    }
  }
  else {
    df[i,'temp'] = 0
  }
  if ("et" %in% vars) {
    df[i, 'et'] = 1
    df[i, 'et.coef'] = output$coef_val['et']
    pval = output$pval['et']
    if (pval < 0.05){
      df[i, 'et.p'] = 1
    }
    else{
      df[i, 'et.p'] = 0
    }
  }
  else {
    df[i,'et'] = 0
  }
  adjr2 <- output$adjr2
  df[i, 'adjr2'] <- adjr2
}
df$vars <- rowSums(df[,c('et', 'temp','prcp','urb','stor','sw_wr','gw_wr', 'ubrb_prcp', 'carryover')])

write.csv(df, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/MLR_final_0822.csv')
