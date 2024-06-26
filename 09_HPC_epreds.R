# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date adapted: 04/24/2024

# Purpose: Get predicted draws for each variable from the ARMA model ## 

library(brms) # work with outputs of GLMMs
library(bayesplot) # built in plots with brms
library(tidybayes) # get clean draws from brms object
library(modelr) # model manipulation for visualization
library(dplyr) # dataframe manipulation
library(tidyverse)

# Load the data and the model 
df.arma <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/arma_input_0531.csv')
mod.arma <- readRDS('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod-arma-stud.RDS')

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
epreddraws_prcp <- add_epred_draws(mod.arma, 
                              newdata=new,
                              ndraws=1000,
                              re_formula=NA)
epreddraws_prcp$unscale.prcp <- unscale(epreddraws_prcp$scale_d.prcp, df.arma$d.prcp)
print('success drawing predictions')
out_file <- paste('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_prcp.csv')
write.csv(epreddraws_prcp, file = out_file)

print('Epreds for temp')

new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = mean(scale_d.et),
            scale_d.prcp = mean(scale_d.prcp),
            scale_d.temp = seq_range(scale_d.temp, n = 200),
            scale_d.use = mean(scale_d.use),
            Year = Year)
new$Name <- NA

epreddraws_temp <-  add_epred_draws(mod.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws_temp$unscale.temp <- unscale(epreddraws_temp$scale_d.temp, df.arma$d.temp)
print('success drawing predictions')
out_file <- paste('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_temp.csv')
write.csv(epreddraws_temp, file = out_file)

print('Epred draws for AF used')
new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = mean(scale_d.et),
            scale_d.prcp = mean(scale_d.prcp),
            scale_d.temp = mean(scale_d.temp),
            scale_d.use = seq_range(scale_d.use, n = 200),
            Year = Year)
new$Name <- NA

epreddraws_af <-  add_epred_draws(mod.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws_af$unscale.use <- unscale(epreddraws_af$scale_d.use, df.arma$d.use)
print('success drawing predictions')
out_file <- paste('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_use.csv')
write.csv(epreddraws_af, file = out_file)

print('epred draws for ET')
new = df.arma %>%
  data_grid(scale_d.urb = mean(scale_d.urb),
            scale_d.et = seq_range(scale_d.et, n = 200),
            scale_d.prcp = mean(scale_d.prcp),
            scale_d.temp = mean(scale_d.temp),
            scale_d.use = mean(scale_d.use),
            Year = Year)
new$Name <- NA

epreddraws_et <-  add_epred_draws(mod.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws_et$unscale.et <- unscale(epreddraws_et$scale_d.et, df.arma$d.et)
print('success drawing predictions')
out_file <- paste('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_et.csv')
write.csv(epreddraws_et, file = out_file)

