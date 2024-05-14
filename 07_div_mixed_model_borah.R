# Diversions mixed effects models for Borah #

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created:  01/12/23
# Date adapted: 04/16/2024

# Purpose: This runs GLMM with ARMA and without and outputs .RDS files of the 
# models to be later processed. 

# Import packages

library(brms) # Do bayesian models, use function brm
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
diversions <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/arma_input.csv')
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
saveRDS(AF.arma.stud, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_outputs/mod-arma-stud.RDS')

# Model with no arma ####

# Read in the data for model with no ARMA 
diversions <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/glmm_input.csv')
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
)

print('Create model without ARMA terms')
AF.mix <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + scale_et + scale_AF_used,
              data = diversions,
              family = 'lognormal',
              prior = priors,
              iter = 4000,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))

summary(AF.mix)
saveRDS(AF.mix, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_outputs/mod-mix.RDS')
