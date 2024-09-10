# Figures from model ##

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date adapted: 04/16/2024

# Import packages with explanation #### 
library(ggplot2) # plots
library(ggpubr) # arrange multi-plot figure 
library(dplyr) # dataframe manipulation
library(Matrix)
library(tidyverse) # dataframe manipulation (summaries)
#install.packages('superheat')
library(superheat) #for heat map with na values
#install.packages('gplots')
library(gplots)
library(reshape2)
library(gridExtra)
library(grid) # arrange ggplots in a grid
library(cowplot) # add on for ggplot
library(brms) # work with outputs of GLMMs
library(bayesplot) # built in plots with brms
library(tidybayes) # get clean draws from brms object
library(modelr) # model manipulation for visualization
#install.packages('svglite')
library(svglite) # to save ggplots as svg files to edit in inkscape
#install.packages('wesanderson')
library(wesanderson) # color palette
#install.packages('ggpattern')
library(ggpattern)
library(dplyr)

# Functions ####

# Unscale variables input into the mixed effects models
unscale <- function(x, orig){
  unscaled <- (sd(orig)*2*x)+mean(orig)
  return(unscaled)
}  

# Calculate median absolute error on transformed data
mae_lt <- function(model, data_compare){
  yhat <- exp(posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae <- function(model, data_compare){
  yhat <- (posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

# -------------------------------------------#

#       MLR Figures                      ####

# -------------------------------------------#
# 
# mlr <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/MLR_final_0531.csv')
# div <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full_0531.csv')
# div_full <- subset(div, (Acre_feet > 0.00001))
# div_full <- subset(div_full, Name %in% unique(mlr$names))
# df = div_full %>%
#   group_by(Name) %>%
#   summarise(last(class1_urban)-first(class1_urban))
# mlr$urb_change <- mlr$`last(class1_urban) - first(class1_urban)`
# 
# sub.mlr <- mlr[, c('X', 'prcp.coef', 'temp.coef', 'urb.coef', 'stor.coef', 'et.coef', 'adjr2')]
# sub.mlr$prcp[sub.mlr$prcp.coef > 0] <- 1
# sub.mlr$prcp[sub.mlr$prcp.coef < 0] <- -1
# sub.mlr$urb[sub.mlr$urb.coef > 0] <- 1
# sub.mlr$urb[sub.mlr$urb.coef < 0] <- -1
# sub.mlr$temp[sub.mlr$temp.coef > 0] <- 1
# sub.mlr$temp[sub.mlr$temp.coef < 0] <- -1
# sub.mlr$stor[sub.mlr$stor.coef > 0] <- 1
# sub.mlr$stor[sub.mlr$stor.coef < 0] <- -1
# sub.mlr$et[sub.mlr$et.coef > 0] <- 1
# sub.mlr$et[sub.mlr$et.coef < 0] <- -1
# sub.mlr$r2 <- signif(sub.mlr$adjr2, digits = 3)
# 
# heat.data <- sub.mlr[,c('r2','urb','prcp', 'temp', 'et','stor')]
# df.melt <- melt(heat.data, id.vars = 'r2')
# df.melt$adjr2 <- as.character(df.melt$r2)
# cols <- c('#00798c', 'white', '#edae49')
# 
# #changed X to 'variable' because X doesn't exist as a column name in df.melt
# df.melt$x <- factor(df.melt$variable,
#                     levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
#                                '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
#                                '21', '22', '23', '24', '25', '26', '27', '28', '29', '30',
#                                '31', '32', '33', '34', '35', '36', '37', '38', '39', '40',
#                                '41', '42', '43', '44', '45', '46', '47', '48', '49', '50',
#                                '51', '52', '53', '54', '55', '56', '57', '58', '59', '60'))
# 
# 
# hm <- ggplot(data = df.melt, aes(x = variable, y = adjr2, fill = factor(value))) + 
#   geom_tile() + 
#   scale_fill_manual(values = c('#00798c','#edae49'), na.value = 'white') +
#   scale_x_discrete(breaks = unique(df.melt$variable), labels = c('Urban Area', 'Precipitaiton', 'Temperature', 'ET', 'Storage Usee')) + 
#   theme_bw()  +
#   xlab('Variable') +
#   ylab('R^2 of Individual Models')
# hm
# ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/heatmap.png', plot = hm,
#        width = 6, height = 7)
# rows.df <- data.frame(rowSums(mlr[,c('urb','prcp', 'temp','et','stor')]))
# l <- c('urb','prcp', 'temp','et','stor')
# 
# df <- data.frame(1:5)
# df$val_up <- NA
# df$val_down <- NA
# df$variable <- NA
# 
# 
# urbsub <- subset(mlr, stor.p == 1)
# uin <- subset(urbsub, stor.coef > 0)
# udown <- subset(urbsub, stor.coef < 0)
# df$val_up[5] <- length(uin$X)
# df$val_down[5] <- length(udown$X)
# write.csv(df, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/sig.csv')
# df <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/sig.csv')
# df <- df[,c('val_up', 'val_down', 'variable')]
# colnames(df)[colnames(df) == "variable"] ="name"
# 
# df <- melt(df, id.vars = 'name' )
# df$x <- factor(df$name,
#                levels = c('urb', 'prcp', 'temp', 'et', 'stor'))
# 
# col.df <- data.frame(colSums(mlr[,c('urb.p','prcp.p', 'temp.p','et.p','stor.p')], na.rm = TRUE))
# col.df$Names <- colnames(mlr[,c('urb','prcp', 'temp','et','stor')])
# col.df$vals <- col.df$colSums.mlr...c..urb.p....prcp.p....temp.p....et.p....stor.p.....
# 
# tmp <- ggplot_gtable(ggplot_build(hm))
# leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
# legend <- tmp$grobs[[leg]]
# 
# hm <- hm +
#   theme(legend.position= 'none')
# hm
# 
# col.df$x <- factor(col.df$Names,
#                    levels = c('urb', 'prcp', 'temp', 'et', 'stor'))
# bp.y <- ggplot(col.df, aes(y= vals, x = x)) +
#   geom_bar(stat = 'identity') + 
#   scale_x_discrete(breaks = unique(df.melt$variable), labels = c('Urban Area', 'Precipitaiton', 'Temperature', 'ET', 'Storage Usee')) +
#   ylab('Number of models variable is significant') +
#   theme_bw() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
# bp.y
# rows.df$rowSums.mlr...c..urb....prcp....temp....et....stor....
# rows.df$X <- mlr$X
# 
# cols <- c('#00798c', '#edae49')
# bp.y <- ggplot(df, aes(y= value, x = x, fill = variable)) +
#   geom_bar(stat = 'identity', position = 'stack') + 
#   scale_x_discrete(breaks = unique(df$name), labels = c('Urban Area', 'Precipitaiton', 'Temperature', 'ET', 'Storage Usee')) +
#   scale_fill_manual(values= c('#edae49','#00798c'))+
#   ylab('Number of models variable is significant') +
#   theme_bw() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
#   theme(legend.position = 'none')
# bp.y
# rows.df$x <- factor(rows.df$X,
#                     levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
#                                '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
#                                '21', '22', '23', '24', '25', '26', '27', '28', '29', '30',
#                                '31', '32', '33', '34', '35', '36', '37', '38', '39', '40',
#                                '41', '42', '43', '44', '45', '46', '47', '48', '49', '50',
#                                '51', '52', '53', '54', '55', '56', '57', '58', '59', '60'))
# bp.x <- ggplot(rows.df, aes(y = rowSums.mlr...c..urb....prcp....temp....et....stor...., x = x))+
#   geom_bar(stat = 'identity') +
#   coord_flip() +
#   theme_bw() +
#   xlab('Diversion Number')+
#   ylab('Total variables selected') +
#   theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# bp.x
# 
# 
# grid <- ggarrange(bp.y, hm, nrow = 2, ncol = 1, heights = c(30, 60), widths =  15)
# grid
# ggsave(file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mlr.svg', plot = grid,
#        width = 5, height = 11)
# 
# # Figures for MLR Bayesian ####
# 
# # Read in the dataframes
# sum.df <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_sum_final.csv') # coefficients for variables
# fit.df <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mlr_brm_fit_final.csv') # model fit
# 
# # Clean/Manipulate data 
# 
# for (x in 1:nrow(sum.df)){
#   sum.df$sig[x] <- ifelse(between(0, sum.df$l.95..CI[x], sum.df$u.95..CI[x]), 'no', 'yes')
# } # Determine if 95% CI overlap 0 
# sum.df$more <- ifelse(sum.df$Estimate > 3 | sum.df$Estimate < -3, 'more', 'no')
# urb <- subset(sum.df, vars == 'scale_class1_urban')
# sum.df <- sum.df %>%
#   mutate(class = case_when(sig == 'no' & more == 'no' ~ 'no',
#                            sig == 'no' & more == 'more' ~ 'oob',
#                            sig == 'yes' & more == 'no' ~ 'sig',
#                            sig == 'yes' & more == 'more' ~ 'sig_oob'))
# 
# sum.df <- sum.df %>% 
#   mutate(range_class = case_when(Estimate >= -0.1 & Estimate <= 0.1 ~ '-0.1 to 0.1',
#                                  Estimate > 0.1 & Estimate <= 1 ~ '0.11 to 1',
#                                  Estimate >1 & Estimate <= 10 ~ '1.01 to 10',
#                                  Estimate > 10 & Estimate <=100 ~ '10.01 to 100',
#                                  Estimate > 100 ~ '100.01 to 13,382',
#                                  Estimate < -0.1 & Estimate >= -1 ~ '-1 to - 0.11',
#                                  Estimate < -1 & Estimate >= -10 ~ '-10 to -1.01',
#                                  Estimate < -10 & Estimate > -100 ~ '-57 to -10.01',
#                                  Estimate < -100 & Estimate > -1000 ~ ''))
# 
# sum.df <- sum.df[!(sum.df$vars %in% c('shape', 'Intercept')),] # Drop shape and intercept parameter
# sum.df$r2 <- fit.df$Estimate[match(sum.df$Name, fit.df$name)] # Add R2 value to dataframe
# sum.df$r2.char <- as.character(signif(sum.df$r2, digits = 3)) #Converts to character for ggplot and cuts to 3 sigfigs
# 
# # Make the heatmap 
# cc <- scales::seq_gradient_pal("#00798C", "white", "Lab")(seq(0,1,length.out=4))
# neg <- c("#00798C", "#6FA4B1", "#B8D1D7", "#FFFFFF")
# pos <- c("#EDAE49", "#F6C278", "#FCD6A4", "#FFEAD1", "#FFFFFF")
# 
# hmap <- ggplot(data = sum.df, aes(x = vars, y = r2.char, fill = range_class, pattern = sig,)) +
#   geom_tile() +
#   scale_fill_manual(values = c('-57 to -10.01' = "#00798C", 
#                                '-10 to -1.01' = "#6FA4B1", 
#                                '-1 to - 0.11' = "#B8D1D7", 
#                                '-0.1 to 0.1' = "#FFFFFF", 
#                                '0.11 to 1' = "#FFEAD1" , 
#                                '1.01 to 10' = "#FCD6A4",
#                                '10.01 to 100' = "#F6C278", 
#                                '100.01 to 13,382' = "#EDAE49" )) +
#   theme_bw() +
#   geom_tile_pattern(pattern_color = NA,
#                     pattern_fill = "black",
#                     pattern_density = 0.23,
#                     pattern_spacing = 0.015,
#                     pattern_key_scale_factor = 1) +
#   # scale_pattern_angle_manual(values = c(sig = 45, sig_oob = 45, no = 0, oob = 125)) +
#   scale_pattern_manual(values = c( yes = "stripe", no = 'none')) 
# hmap
# ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_ouput/figures/mlr_bayes_hm.svg', plot = hmap,
#        height = 7,
#        width = 5)
# 
# # Pull out number of values with positive effect
# urb.p <- subset(sum.df, vars == 'scale_class1_urban' & Estimate > 0.1)
# urb.n <- subset(sum.df, vars == 'scale_class1_urban' & Estimate < -0.1)
# urb.sig <- subset(sum.df, vars == 'scale_class1_urban' & sig == 'yes' & Estimate > 0)
# precip <- subset(sum.df, vars == 'prcp.m') #weird variable name for precip. in one of bridgets versions it was irrig_precip
# 
# max(precip$Estimate)
# min(precip$Estimate)
# 
# temp.small <- subset(sum.df, vars == 'irrig_temp' & range_class == '-0.1 to 0.1')
# 
# et.p <- subset(sum.df, vars == 'et' & Estimate > 0.1)
# et.n <- subset(sum.df, vars == 'et' & Estimate < -0.1)
# et.sig <- subset(sum.df, vars == 'et' & sig == 'yes')
# 
# stor.p <- subset(sum.df, vars == 'KAF_used' & Estimate > 0.1)
# stor.sig <- subset(sum.df, vars == 'KAF_used' & sig == 'yes')
# stor.p.large <- subset(sum.df, vars == 'KAF_used' & Estimate > 10)
# # NOTE: Squish pulls extreme values to the max color for that direction.
# #       There are values more extreme, which drowned out the entire color map. 
# #       Should, and if so, how should I denote that?
# 
# box <- ggplot(data = sum.df, aes(x = vars, y = Estimate)) +
#   geom_boxplot()
# 
# box


# -------------------------------------------#

#       Figures for Model with no ARMA    ####

# -------------------------------------------#

#Import data and model 
df.mix <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/glmm_input_0906.csv')
mod.mix <- readRDS('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod2-glmm.RDS')

# Posterior predictive check 

post_pred_check <- pp_check(mod.mix, ndraws = 20) +
  theme_bw() +
  ylab('Density') +
  xlab('Discharge (AF/yr)') +
  coord_cartesian(xlim = c(0,900000)) +
  theme(text = element_text(size=13, family = 'Arial'))

ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/ppcheck-GLMM.tiff',
       plot = post_pred_check,
       width = 5,
       height = 4)

# Create posterior distribution plot
color_scheme_set('blue')
mcmc_plot(mod.mix,
          type = 'areas',
          variable = c('b_scale_et',
                       'b_scale_irrig_prcp',
                       'b_scale_irrig_temp',
                       'b_scale_ubrb_prcp',
                       'b_scale_class1_urban',
                       'b_scale_sw_wr',
                       'b_scale_AF_used',
                       'b_scale_Carryover'),
          prob = 0.95) +
  theme_bw() +
  vline_0() +
  scale_y_discrete(labels = c('Evapotranspiration',
                              'Irrig. Precip.',
                              'Irrig. Temp.',
                              'UBRB Precip',
                              'Urban Percentage',
                              'SW Water Rights',
                              'Reservoir Carryover',
                              'Storage Water Use')) + 
  scale_fill_manual(values = c('Evapotranspiration' = 'grey',
                               'Precipitation' = '#00798c',
                               'Temperature' = 'grey',
                               'Urban Percentage' = '#00798c',
                               'Storage Water Use' = '#edae49'))+
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=18, family = 'Arial'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/postmass_all_mix.svg', 
       width = 7,
       height = 6,
       units = 'in')

## -------------------------------------------------##
##          Marginal Effect Figures                 ##                                          
## -------------------------------------------------##

# URBAN ##

new = df.mix %>%
  data_grid(scale_class1_urban = seq_range(scale_class1_urban, n = 200),
            scale_et = mean(scale_et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.urban <- unscale(epreddraws$scale_class1_urban, df.mix$class1_urban)
urban <- ggplot(data=epreddraws,
                aes(x = unscale.urban, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Percent Urban") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-urb.tiff', 
       plot = urban,
       width = 4,
       height = 4,
       units = 'in')

change_urb <- epreddraws %>%
  select(.epred, unscale.urban) %>%
  group_by(unscale.urban) %>%
  summarise(med = median(.epred)) %>%
  mutate(change = (c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                     NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                     NA, NA, NA, NA, diff(med, lag = 24))))
median(change_urb$change, na.rm = TRUE)
change_urb <- change_urb %>%
  summarise(change = (c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, diff(med, lag = 24))))




# Storage ##

new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = mean(scale_et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = seq_range(scale_AF_used, n = 200),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.use <- unscale(epreddraws$scale_AF_used, df.mix$AF_used)
epreddraws$kaf <- epreddraws$unscale.use/1000
stor <- ggplot(data=epreddraws,
               aes(x = kaf, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill='#edae49', 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Storage Water Use (KAF/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
stor
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-stor.tiff', 
       plot = stor,
       width = 4,
       height = 4,
       units = 'in')

change_stor <- epreddraws %>%
  select(.epred, unscale.use) %>%
  group_by(unscale.use) %>%
  summarise(med = median(.epred),
            avg = mean(.epred))



# Irrig Season Precip ##

new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = mean(scale_et),
            scale_irrig_prcp = seq_range(scale_irrig_prcp, n = 200),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.prcp <- unscale(epreddraws$scale_irrig_prcp, df.mix$irrig_prcp)
prcp <- ggplot(data=epreddraws,
               aes(x = unscale.prcp, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Total Precipitation (mm/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
prcp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-prcp.tiff',
       plot = prcp,
       width = 4,
       height = 4,
       units = 'in')
change_prcp <- epreddraws %>%
  select(.epred, unscale.prcp) %>%
  group_by(unscale.prcp) %>%
  summarise(med = median(.epred),
            avg = mean(.epred)) %>%
  mutate(change = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, diff(med, lag = 73)))



# Irrigation Season Temp ##

new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = mean(scale_et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = seq_range(scale_irrig_temp, n = 200),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.irrig_temp <- unscale(epreddraws$scale_irrig_temp, df.mix$irrig_temp)
temp <- ggplot(data=epreddraws,
               aes(x = unscale.irrig_temp, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Avg. Max. Irrig. Temp. (F)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
temp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-temp.tiff',
       plot = temp,
       width = 4,
       height = 4,
       units = 'in')
change_temp <- epreddraws %>%
  select(.epred, unscale.irrig_temp) %>%
  group_by(unscale.irrig_temp) %>%
  summarise(med = median(.epred),
            avg = mean(.epred)) %>%
  mutate(change = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, diff(med, lag = 73)))

# Irrigation Season ET ##

new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = seq_range(scale_et, n = 200),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.et <- unscale(epreddraws$scale_et, df.mix$et)
et <- ggplot(data=epreddraws,
               aes(x = unscale.et, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Evapotranspiration (in)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
et
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-et.tiff',
       plot = et,
       width = 4,
       height = 4,
       units = 'in')
change_et <- epreddraws %>%
  select(.epred, unscale.et) %>%
  group_by(unscale.et) %>%
  summarise(med = median(.epred),
            avg = mean(.epred)) %>%
  mutate(change = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, diff(med, lag = 73)))

# UBRB Precip ##

new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = mean(scale_et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = seq_range (scale_ubrb_prcp, n = 200))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.ubrb_prcp <- unscale(epreddraws$scale_ubrb_prcp, df.mix$ubrb_prcp)
ubrb_prcp <- ggplot(data=epreddraws,
               aes(x = unscale.ubrb_prcp, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Avg Water Year Precipitation (mm/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
ubrb_prcp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-ubrb_prcp.tiff',
       plot = ubrb_prcp,
       width = 4,
       height = 4,
       units = 'in')
change_ubrb_prcp <- epreddraws %>%
  select(.epred, unscale.ubrb_prcp) %>%
  group_by(unscale.ubrb_prcp) %>%
  summarise(med = median(.epred),
            avg = mean(.epred)) %>%
  mutate(change = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, diff(med, lag = 73)))




# SW Water rights ##
new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = mean(scale_et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = mean(scale_Carryover),
            scale_sw_wr = seq_range (scale_sw_wr, n = 200),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.sw_wr <- unscale(epreddraws$scale_sw_wr, df.mix$sw_wr)
sw_wr <- ggplot(data=epreddraws,
                    aes(x = unscale.sw_wr, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("SW Water Rights (count)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
sw_wr
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-sw_wr.tiff',
       plot = sw_wr,
       width = 4,
       height = 4,
       units = 'in')
change_sw_wr <- epreddraws %>%
  select(.epred, unscale.sw_wr) %>%
  group_by(unscale.sw_wr) %>%
  summarise(med = median(.epred),
            avg = mean(.epred)) %>%
  mutate(change = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, diff(med, lag = 73)))


# Carryover ##
new = df.mix %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            scale_et = mean(scale_et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used),
            scale_Carryover = seq_range (scale_Carryover, n = 200),
            scale_sw_wr = mean (scale_sw_wr),
            scale_ubrb_prcp = mean (scale_ubrb_prcp))

epreddraws <-  add_epred_draws(mod.mix, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA)
epreddraws$unscale.Carryover <- unscale(epreddraws$scale_Carryover, df.mix$Carryover)
Carryover <- ggplot(data=epreddraws,
                aes(x = unscale.Carryover, y = .epred)) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Reservoir Carryover (AF)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(400, 2000))
Carryover
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mix-Carryover.tiff',
       plot = Carryover,
       width = 4,
       height = 4,
       units = 'in')
change_Carryover <- epreddraws %>%
  select(.epred, unscale.Carryover) %>%
  group_by(unscale.Carryover) %>%
  summarise(med = median(.epred),
            avg = mean(.epred)) %>%
  mutate(change = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,
                    NA, NA, NA, diff(med, lag = 73)))



# ALL
comb <- ggarrange(sw_wr, ubrb_prcp, prcp, et, temp, Carryover, stor, urban,  ncol=3, nrow = 4, labels = c('A', 'B', 'C','D','E','F','G','H'))
ggsave(comb, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/marg-GLMM-all.tiff',
       width = 9,
       height = 9)


# -------------------------------------------#

#       Figures for Model with ARMA    ####

# -------------------------------------------#

df.arma <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/arma_input_0906.csv')
mod.arma <- readRDS('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/mod2-arma-stud.RDS')
bayes_R2(mod.arma)
mae_lt(mod.arma, df.arma$Acre_feet)



# Posterior predictive check

post_pred_check <- pp_check(mod.arma, ndraws = 20) +
  theme_bw() +
  ylab('Density') +
  xlab('log(Discharge)') +
  coord_cartesian(xlim = c(0,15)) +
  theme(text = element_text(size=13, family = 'Arial'))

ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/ppcheck-ARMA.tiff',
       plot = post_pred_check,
       width = 5,
       height = 4)


# Posterior mass plot of all
color_scheme_set('darkgray')
mcmc_plot(mod.arma,
          type = 'areas',
          variable = c('b_scale_d.et',
                       'b_scale_d.prcp',
                       'b_scale_d.ubrb_prcp',
                       'b_scale_d.temp',
                       'b_scale_d.urb',
                       'b_scale_d.sw_wr',
                       'b_scale_d.use',
                       'b_scale_d.Carryover'),
          prob = 0.95) +
  theme_bw() +
  vline_0() +
  scale_y_discrete(labels = c('Evapotranspiration',
                              'Irrig. Precip.',
                              'UBRB Precip.',
                              'Irrig. Temp.',
                              'Urban Percentage',
                              'SW Water Rights',
                              'Storage Water Use',
                              'Res. Carryover')) +
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=18, family = 'Arial'))
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/post-arma.svg', 
       width = 8,
       height = 6,
       units = 'in')


# Precip plot 

prcp_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_prcp.csv')

prcp <- ggplot(data=prcp_epred,
               aes(x = unscale.prcp, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Change in Precipitation (mm/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
prcp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-prcp.tiff', 
       plot = prcp,
       width = 4,
       height = 4,
       units = 'in')

change_prcp <- prcp_epred %>%
  select(unscale.prcp, .epred) %>%
  group_by(unscale.prcp) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                         NA, NA, NA,
                         diff(avg, lag = 25)),
         differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                       NA, NA, NA,
                       diff(unscale.prcp, lag = 25)))
mean(change_prcp$differ_pred, na.rm = T)



# ET plot 
et_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_et.csv')

et <- ggplot(data=et_epred,
             aes(x = unscale.et*1000, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill='#edae49', 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Change in ET (mm/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-et.tiff', 
       plot = et,
       width = 4,
       height = 4,
       units = 'in')

et_epred$et_mm <- et_epred$unscale.et*1000
change_et <- et_epred %>%
  select(et_mm, .epred) %>%
  group_by(et_mm) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                         NA, NA, 
                         diff(avg, lag = 12)),
         differ_et = c(NA, NA, NA, NA, NA, NA, NA, NA, NA , NA,
                       NA, NA, 
                       diff(et_mm, lag = 12)))
mean(change_et$differ_pred, na.rm = T)


# Temp plot 

temp_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_temp.csv')
temp <- ggplot(data=temp_epred,
               aes(x = unscale.temp, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Change in Temperature (C/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
temp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-temp.tiff', 
       plot = temp,
       width = 4,
       height = 4,
       units = 'in')

change_temp <- temp_epred %>%
  select(unscale.temp, .epred) %>%
  group_by(unscale.temp) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                         NA, NA, NA, NA, NA, NA, NA, NA, 
                         diff(avg, lag = 16)),
         differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA,
                       diff(unscale.temp, lag = 16)))
mean(change_temp$differ_pred, na.rm = T)


# Storage plot

stor_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_use.csv')
stor_epred$KAF <- stor_epred$unscale.use/1000
stor <- ggplot(data=stor_epred,
               aes(x = KAF, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill='#edae49', 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Change in Storage Use (KAF/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
stor
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-stor.tiff', plot = stor,
       width = 4,
       height = 4,
       units = 'in')

change_stor <- stor_epred %>%
  select(unscale.use, .epred) %>%
  group_by(unscale.use) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                         diff(avg, lag = 8)),
         differ_et = c(NA, NA, NA, NA, NA, NA, NA, NA,  
                       diff(unscale.use, lag = 8)))
mean(change_stor$differ_pred, na.rm = T)


# UBRB Precip

ubrb_prcp_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_ubrb_prcp.csv')

ubrb_prcp <- ggplot(data=ubrb_prcp_epred,
               aes(x = unscale.ubrb_prcp, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill='#edae49', 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Water Year UBRB Precip (in/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
ubrb_prcp
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-ubrb_prcp.tiff', plot = ubrb_prcp,
       width = 4,
       height = 4,
       units = 'in')
change_ubrb_prcp <- ubrb_prcp_epred %>%
  select(unscale.ubrb_prcp, .epred) %>%
  group_by(unscale.ubrb_prcp) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                         diff(avg, lag = 8)),
         differ_et = c(NA, NA, NA, NA, NA, NA, NA, NA,  
                       diff(unscale.ubrb_prcp, lag = 8)))
mean(change_ubrb_prcp$differ_pred, na.rm = T)


# SW Water Rights

sw_wr_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_sw_wr.csv')
sw_wr <- ggplot(data=sw_wr_epred,
                    aes(x = unscale.sw_wr, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill='#edae49', 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("SW Water Rights") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
sw_wr
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-sw_wr.tiff', plot = sw_wr,
       width = 4,
       height = 4,
       units = 'in')
change_sw_wr <- sw_wr_epred %>%
  select(unscale.sw_wr, .epred) %>%
  group_by(unscale.sw_wr) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                         diff(avg, lag = 8)),
         differ_et = c(NA, NA, NA, NA, NA, NA, NA, NA,  
                       diff(unscale.ubrb_prcp, lag = 8)))
mean(change_sw_wr$differ_pred, na.rm = T)

# Reservoir Carryover 
Carryover_epred <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/epred_Carryover.csv')
Carryover_epred$KAF <- Carryover_epred$unscale.use/1000
Carryover <- ggplot(data=Carryover_epred,
               aes(x = KAF, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5), alpha = 0.35, fill='#edae49', 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Reservoir Carryover (KAF/yr)") +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(labels = scales::comma)
Carryover
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/arma-carryover.tiff', plot = Carryover,
       width = 4,
       height = 4,
       units = 'in')

change_Carryover <- Carryover_epred %>%
  select(unscale.Carryover, .epred) %>%
  group_by(unscale.Carryover) %>%
  summarize(avg = median(exp(.epred))) %>%
  mutate(differ_pred = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                         diff(avg, lag = 8)),
         differ_et = c(NA, NA, NA, NA, NA, NA, NA, NA,  
                       diff(unscale.use, lag = 8)))
mean(change_Carryover$differ_pred, na.rm = T)


# ALL

final <- ggarrange(temp, et, prcp, ubrb_prcp, stor, Carryover, urban, sw_wr, Carryover, nrow = 4, ncol = 3, labels = c('A', 'B', 'C', 'D','E','F','G','H'))
ggsave(final, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/grid-arma.tiff',
       width = 9,
       height = 9)

