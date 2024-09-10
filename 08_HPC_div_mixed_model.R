# Diversions mixed effects models for Borah #

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created:  01/12/23
# Date adapted: 04/16/2024

# Purpose: This runs GLMM with ARMA and without and outputs .RDS files of the 
# models to be later processed. 

# Import packages

library(brms) 
library(tidyverse) # 
library(dplyr)
library(readr)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)

# ARMA Model ####

###### Import the data #

print('Import diversion data for ARMA model')
diversions <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/arma_input_0906.csv')
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
diversions <- na.omit(diversions)

print('Model with ARMA')

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_d.urb'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_d.use'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_d.prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_d.temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_d.et'), 
  #set_prior('normal(0,1)', class = 'b', coef = 'scale_d.ubrb_prcp'), 
  #set_prior('normal(0,1)', class = 'b', coef = 'scale_d.Carryover'), 
  #set_prior('normal(0,1)', class = 'b', coef = 'scale_d.sw_wr'), 
  set_prior('lkj_corr_cholesky(2)', class = 'L')
)

print('Create model with ARMA terms and student-t family')
# brms is used to run mixed effects models
AF.arma.stud <- brms::brm(lt ~ (1 + scale_d.urb | Name) + scale_d.urb + scale_d.prcp + scale_d.temp + scale_d.et + scale_d.use + arma(gr = Name),
                          data = diversions,
                          family = 'student',
                          prior = priors,
                          iter = 4000,
                          control = list(max_treedepth = 20,
                                         adapt_delta = 0.999),
                          cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.arma.stud)
saveRDS(AF.arma.stud, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod1-arma-stud.RDS')
# Calculate LOO for the model & save
loo_glmm_arma <- loo(AF.arma.stud)
saveRDS(loo_glmm_arma, file =  "/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod1-arma-loo.RDS")


# Model without ARMA ####

# Read in the data for model with no ARMA 
diversions <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/glmm_input_0906.csv')
print(c('This is the length before removing duplicates', length(diversions$Acre_feet)))
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
print(length(diversions$Acre_feet))

# Priors
priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_et'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_AF_used')
 # set_prior('normal(0,1)', class = 'b', coef = 'scale_ubrb_prcp'), 
 # set_prior('normal(0,1)', class = 'b', coef = 'scale_Carryover') 
  #set_prior('normal(0,1)', class = 'b', coef = 'scale_sw_wr') 
)

print('Create model without ARMA terms')
glmm <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_et + scale_AF_used,
              data = diversions,
              family = 'lognormal',
              prior = priors,
              iter = 4000,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))

summary(glmm)
saveRDS(glmm, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod1-glmm.RDS')
# Calculate LOO for the model & save
loo_glmm <- loo(glmm)
saveRDS(loo_glmm, file = "/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod1-glmm-loo.RDS")

# ------------------- #
#  Model Comparison   #
# ------------------- #

# Naming convention notes: 
# mod 1 = original
# mod 2 = all variables
# mod 3 = all but sw_wr

## LOO Comparison 
# Load the LOO object from the RDS file
loo_glmm1 <- readRDS("/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod1-arma-loo.RDS")
loo_glmm2 <- readRDS("/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod2-arma-loo.RDS")
loo_glmm3 <- readRDS("/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod3-arma-loo.RDS")
loo_glmm_arma1 <- readRDS("/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod1-glmm-loo.RDS")
loo_glmm_arma2 <- readRDS("/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod2-glmm-loo.RDS")
loo_glmm_arma3 <- readRDS("/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod3-glmm-loo.RDS")

# Compare GLMM models using loo
loo_glmm_comp <- loo_compare(loo_glmm1, loo_glmm2, loo_glmm3)
rownames(loo_glmm_comp) <- c("Model 1: AF.arma.stud", "Model 2: AF.arma.stud", "Model 3: AF.arma.stud")
print(loo_glmm_comp)

# Compare GLMM+ARMA models using loo
loo_glmm_arma_comp <- loo_compare(loo_glmm_arma1, loo_glmm_arma2, loo_glmm_arma3)
print(loo_glmm_arma_comp)
