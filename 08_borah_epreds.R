# Get predicted draws for each variable from the ARMA model ## 

library(brms) # work with outputs of GLMMs
library(bayesplot) # built in plots with brms
library(tidybayes) # get clean draws from brms object
library(modelr) # model manipulation for visualization
library(dplyr) # dataframe manipulation
library(tidyverse)

cd <- '~/scratch/predict/'

# Load the data and the model 
df.arma <- read.csv(paste(cd, 'data/arma_input_041123.csv', sep = ''))
mod.arma <- readRDS('~/scratch/predict/data/mod-arma-stud-041123.RDS')

# Unscale function
unscale <- function(x, orig){
  unscaled <- (sd(orig)*2*x)+mean(orig)
  return(unscaled)
}  

# Epreds for precipitaiton 
print('Epreds for precipitaiton')

new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = mean(scale_d.et),
            scale_d.prcp = seq_range(scale_d.prcp, n = 200),
            scale_d.temp = mean(scale_d.temp),
            scale_d.use = mean(scale_d.use),
            Year = Year)
new$Name <- NA

# Expected predicted draws
epreddraws <- add_epred_draws(mod.arma, 
                              newdata=new,
                              ndraws=1000,
                              re_formula=NA)
epreddraws$unscale.prcp <- unscale(epreddraws$scale_d.prcp, df.arma$d.prcp)
print('success drawing predictions')
out_file <- paste(cd, 'model_output/epred_prcp_041223.csv', sep = '')
write.csv(epreddraws, file = out_file)

print('Epreds for temp')

new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = mean(scale_d.et),
            scale_d.prcp = mean(scale_d.prcp),
            scale_d.temp = seq_range(scale_d.temp, n = 200),
            scale_d.use = mean(scale_d.use),
            Year = Year)
new$Name <- NA

epreddraws <-  add_epred_draws(mod.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.temp <- unscale(epreddraws$scale_d.temp, df.arma$d.temp)
print('success drawing predictions')
out_file <- paste(cd, 'model_output/epred_temp_041223.csv', sep = '')
write.csv(epreddraws, file = out_file)

print('Epred draws for AF used')
new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = mean(scale_d.et),
            scale_d.prcp = mean(scale_d.prcp),
            scale_d.temp = mean(scale_d.temp),
            scale_d.use = seq_range(scale_d.use, n = 200),
            Year = Year)
new$Name <- NA

epreddraws <-  add_epred_draws(mod.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.use <- unscale(epreddraws$scale_d.use, df.arma$d.use)
print('success drawing predictions')
out_file <- paste(cd, 'model_output/epred_use_041223.csv', sep = '')
write.csv(epreddraws, file = out_file)

print('epred draws for ET')
new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = seq_range(scale_d.et, n = 200),
            scale_d.prcp = mean(scale_d.prcp),
            scale_d.temp = mean(scale_d.temp),
            scale_d.use = mean(scale_d.use),
            Year = Year)
new$Name <- NA

epreddraws <-  add_epred_draws(mod.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.et <- unscale(epreddraws$scale_d.et, df.arma$d.et)
print('success drawing predictions')
out_file <- paste(cd, 'model_output/epred_et_041223.csv', sep = '')
write.csv(epreddraws, file = out_file)