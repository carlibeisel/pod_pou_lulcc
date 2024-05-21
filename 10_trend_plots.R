# Trend plots # 

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created:  01/12/23
# Date adapted: 04/16/2024

# ------------------------------------------------------------------------------- #
# This script uses Mann Kendall trend analysis to look at change through time for 
# each diversion. Average +/- 95% CI change for each diversion is calculated.
# ------------------------------------------------------------------------------- #

# Import packages
library(dplyr) # data manipulation
library(Matrix) # co-dependency with tidyverse
library(tidyverse) # data manipulation
library(Kendall) # Mann Kendall test
library(ggplot2) # plotting
library(ggpubr) # arrange a grid of plots

# Read in the data  ####
div <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_input/input_full.csv')
div_full <- subset(div, (Acre_feet > 0.00001))

sub <- div_full %>%
  group_by(Year) %>%
  summarise(res = mean(Max_Fill),
            temp = mean(irrig_temp))

# Remove diversions with gaps in the data
remove <- c("Barber pumps", 
            "Mace-Mace Canal",
            "River Run",
            "Surprise Valley and Micron",
            "Thomas Aiken Canal",
            "Warm Springs Canal")

div_full <- subset(div_full, !(Name %in% remove)) 
names <- unique(div_full$Name) #55 canals

# Look for a trend in the discharge data ####

trend <- list()
change_names <- list()
for (i in names) {
  sub_data <- subset(div_full, Name == i)
  if (length(sub_data$Acre_feet) > 3){
    analysis <- MannKendall(sub_data$Acre_feet)
    print(i)
    if (analysis$sl < 0.05) {
      trend[[i]] <- c(i, analysis$tau, analysis$sl)
      change_names[i] <- i
    }
    else {}
  }
}

print(change_names)
change_names <- data.frame(change_names)
div_trend <- subset(div_full, Name %in% change_names)


# Create a function to create plots for trend analysis ####
change_plots <- function(dataframe, name){
  plt <- ggplot(data = dataframe, aes(y= Acre_feet, x = Year)) +
    geom_point() +
    stat_smooth(method = 'lm', color = 'black') + 
    stat_regline_equation(aes(label = after_stat(adj.rr.label)))+
    ggtitle(name) +
    ylab('Discharge (AF)') +
    theme_bw() +
    theme(text = element_text(size = 15))
  return(plt)
}

myplots <- list()
values <- list()
change <- list()
len_name <- 1:29
names <- unique(div_trend$Name)

for (i in len_name){
  print(names[[i]])
  sub <- subset(div_trend, Name == names[[i]])
  mod <- lm(Acre_feet ~ Year, data = sub)
  p <- change_plots(subset(div_trend, Name == names[[i]]), names[[i]])
  vals <- ggplot_build(p)
  vals <- vals$data[[2]]
  values[[i]] <- vals
  df <- data.frame(name = names[[i]],
                   pc = ((vals$y[80] - vals$y[1])/vals$y[1])*100, 
                   ch = vals$y[80] - vals$y[1], 
                   ci = vals$y[80]-vals$ymin[80], 
                   y87 = vals$y[1], 
                   y20 = vals$y[80],
                   adjr2 = summary(mod)$adj.r.squared)
  change[[i]] <- df
  myplots[[i]] <- p
}


change <- ldply(change, data.frame)
sum(change$pc > 0)
sum(change$pc < 0)

new = div_trend %>%
  group_by(Name) %>%
  summarise(last(class1_urban)-first(class1_urban)) %>%
  ungroup()
change$urb_change <- new$`last(class1_urban) - first(class1_urban)`
urb <- subset(change, (pc<0) & (urb_change > 10))
write.csv(change, file = '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/trend.csv')

up <- subset(change, pc > 0)
up_n <- unique(up$name)
u <- subset(div_trend, Name %in% up_n)

down <- subset(change, pc < 0)
down_n <- unique(down$name)
d <- subset(div_trend, Name %in% down_n)

change_mult <- function(dataframe, name){
  plt <- ggplot(data = dataframe, aes(y= Acre_feet, x = Year, group = Name)) +
    geom_point(show.legend = FALSE) +
    stat_smooth(aes(group = Name), method = 'lm', color = 'black', show.legend = FALSE) + 
    ggtitle(name) +
    labs(y = 'Discharge (AF)')+
    theme_bw() +
    theme(text = element_text(size = 15)) +
    scale_y_continuous(labels = scales::comma)+
    coord_cartesian(ylim = c(30000, 170000))
  return(plt)
}

plt_up1 <- change_mult(u, 'Increasing Trends')
plt_down1 <- change_mult(d, 'Decreasing Trends')
plt_up2 <- change_mult(u, 'Increasing Trends')
plt_down2 <- change_mult(d, 'Decreasing Trends')
grid <- ggpubr::ggarrange(plt_up1, plt_down1, plt_up2, plt_down2, ncol = 2, nrow = 2, labels = c('A', 'B', 'C', 'D'))
grid
ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/mk_fig_full.jpg', 
       width = 8,
       height = 8,
       units = 'in')

## Trend through time for timing metrics ####

# Length of irrigation season
trend <- list()
neg_change_names <- list()
pos_change_names <- list()
for (i in names) {
  sub_data <- subset(div_full, Name == i)
  if (length(sub_data$Acre_feet) > 3){
    analysis <- MannKendall(as.numeric(sub_data$Time))
    print(i)
    if (analysis$sl < 0.05) {
      trend[[i]] <- c(i, analysis$tau, analysis$sl)
      if (analysis$tau > 0){
        pos_change_names[i] <- i
      }
      else{
        neg_change_names[i] <- i
      }
    }
    else {}
  }
}

neg_change_names <- data.frame(neg_change_names)
pos_change_names <- data.frame(pos_change_names)
n_time_trend <- subset(div_full, Name %in% neg_change_names)
p_time_trend <- subset(div_full, Name %in% pos_change_names)

# Start day of year 
trend <- list()
neg_change_names <- list()
pos_change_names <- list()
for (i in names) {
  sub_data <- subset(div_full, Name == i)
  if (length(sub_data$Acre_feet) > 3){
    analysis <- MannKendall(as.numeric(sub_data$StartDayofYear))
    print(i)
    if (analysis$sl < 0.05) {
      trend[[i]] <- c(i, analysis$tau, analysis$sl)
      if (analysis$tau > 0){
        pos_change_names[i] <- i
      }
      else{
        neg_change_names[i] <- i
      }
    }
    else {}
  }
}

neg_change_names <- data.frame(neg_change_names)
pos_change_names <- data.frame(pos_change_names)
n_start_trend <- subset(div_full, Name %in% neg_change_names)
p_start_trend <- subset(div_full, Name %in% pos_change_names)

# End day of year 
trend <- list()
neg_change_names <- list()
pos_change_names <- list()
for (i in names) {
  sub_data <- subset(div_full, Name == i)
  if (length(sub_data$Acre_feet) > 3){
    analysis <- MannKendall(as.numeric(sub_data$EndDayofYear))
    print(i)
    if (analysis$sl < 0.05) {
      trend[[i]] <- c(i, analysis$tau, analysis$sl)
      if (analysis$tau > 0){
        pos_change_names[i] <- i
      }
      else{
        neg_change_names[i] <- i
      }
    }
    else {}
  }
}

neg_change_names <- data.frame(neg_change_names)
pos_change_names <- data.frame(pos_change_names)
n_end_trend <- subset(div_full, Name %in% neg_change_names)
p_end_trend <- subset(div_full, Name %in% pos_change_names)

# Plots for each trend

change_mult <- function(dataframe, name, vars, ylabel){
  if (name != 'none'){
    plt <- ggplot(data = dataframe, aes(y= vars, x = Year, group = Name)) +
      geom_point(show.legend = FALSE) +
      stat_smooth(aes(group = Name), method = 'lm', color = 'black', show.legend = FALSE) + 
      ggtitle(name) +
      labs(y = ylabel)+
      theme_bw() +
      theme(text = element_text(size = 15)) 
  }
  else{
    plt <- ggplot(data = dataframe, aes(y= vars, x = Year, group = Name)) +
      geom_point(show.legend = FALSE) +
      stat_smooth(aes(group = Name), method = 'lm', color = 'black', show.legend = FALSE) + 
      labs(y = ylabel)+
      theme_bw() +
      theme(text = element_text(size = 15)) 
  }
  return(plt)
}

# p_time_trend <- subset(p_time_trend, Name != 'Suez')
p_time_trend$Time <- as.numeric(p_time_trend$Time)
p_time <- change_mult(p_time_trend, 'Length of Season', p_time_trend$Time,'Days') +
  coord_cartesian(ylim = c(95, 365))
length(unique(p_time_trend$Name))

n_time_trend$Time <- as.numeric(n_time_trend$Time)
n_time <- change_mult(n_time_trend, 'none', n_time_trend$Time,'Days') 
length(unique(n_time_trend$Name))

# n_start_trend <- subset(n_start_trend, Name != 'Suez')
n_start_trend$StartDayofYear <- as.numeric(n_start_trend$StartDayofYear)
n_start <- change_mult(n_start_trend, 'none', n_start_trend$StartDayofYear,'Days') +
  coord_cartesian(ylim = c(0, 155))
n_start
length(unique(p_start_trend$Name))
length(unique(n_start_trend$Name))

p_start_trend$StartDayofYear <- as.numeric(p_start_trend$StartDayofYear)
p_start <- change_mult(p_start_trend, 'Start Day', p_start_trend$StartDayofYear,'Days') 
p_start
p_end_trend$EndDayofYear <- as.numeric(p_end_trend$EndDayofYear)
p_end <- change_mult(p_end_trend, 'End Day', p_end_trend$EndDayofYear,'Days') +
  coord_cartesian(ylim = c(240, 365))
length(unique(p_end_trend$Name))


n_end_trend$EndDayofYear <- as.numeric(n_end_trend$EndDayofYear)
n_end <- change_mult(n_end_trend, 'none', n_end_trend$EndDayofYear,'Days')
length(unique(n_end_trend$Name))

trends_all <- ggarrange(p_time, p_start, p_end, n_time, n_start, n_end, 
                        ncol = 3,
                        nrow = 2)

ggsave('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/model_output/figures/time_trend.svg',
       plot = trends_all,
       width = 8,
       height = 6)