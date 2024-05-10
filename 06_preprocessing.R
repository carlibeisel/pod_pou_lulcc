## -------------------------------- ##
## Preprocessing for Diversion Data ## 
## -------------------------------- ## 

# By Carli Beisel
# Adapted from Bridget Bittmann (2023, Github: bridgetmarie24)
# Date originally created: 06/09/2022
# Date adapted: 04/16/2024

## --------------------------------------- ## 
## Section 1: Import packages and the data ##
## --------------------------------------- ##

install.packages('corrplot')
library(corrplot)
install.packages('tidyverse')
remove.packages('tidyverse')
install.packages('tidyverse')
library(dplyr)
library(tidyverse)
library(ggplot2)
install.packages('GGally')
library(GGally)
install.packages('ggfortify')
library(ggfortify)
remove.packages('dplyr')
install.packages('dplyr')
install.packages('Kendall')
library(Kendall)

# Without zeros
data <- data.frame(read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/input_full_013023.csv'))
data <- data[-c(1,6,37,38)] # drops Python index output with csv
data <- subset(data, select=-c(Month, DayofYear, Irrigation.Year, Sum, Diversion..cfs.))
data['Mar_et'][is.na(data['Mar_et'])] <- 0 #fill NA et values with 0
data['contagion'][is.na(data['contagion'])] <- 100 # fill NA contagion values with 100
nas <- data[rowSums(is.na(data)) > 0, ] #check for any data with remaining NA values
data <- na.omit(data)

# With zeros data
data <- read.csv('/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/mixed_model_input_013023.csv')
data <- data[!duplicated(data[c('Name', 'Year')]),] #remove duplicates

# OPTIONAL #
# Checks for rows where the length of the irrigation season is greater than 350
#data <- subset(data, Range<350)
#data <- subset(data, StartDayofYear>35)
#data <- subset(data, EndDayofYear<350)

## --------------------------- ####
## Section 2: Standardize data ## 
## --------------------------- ##

# The explanatory variables will be substracted by the mean 
# and then divided by two standard deviations to place data on similar range. 

scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

col_name <- c('ant_prcp',
              'irrig_prcp',
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
# For data with zeros
write.csv(data, '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/input_full_013023.csv', row.names = FALSE)

# Storage data 

data$perc_used <- ifelse(data$AF_available > 0, data$AF_used/data$AF_available, NA)
data$wr_storage <- ifelse(data$AF_available >0, 1, 0)

write.csv(data, '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/mixed_model_input_013023.csv', row.names = FALSE)

## -------------------------------------------------------------- ## 
## Section 3: Plot correlation matrix and check data distribution ## 
## -------------------------------------------------------------- ## 

## Independent variables for data distribution check:
## Aannual acre feet
## Start day of year
## End day of year
## Length of irrigation season (range)

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/acreft_hist_whole.pdf',
    width=4,
    height=4)
hist(data$Acre_feet, 
     breaks=15, 
     prob=TRUE, 
     col='lightblue', 
     xlab='Discharge (Acre-ft/yr)',
     main = 'Distribution of Annual Discharge',
     ylim=c(0, 1.0e-04))
lines(density(data$Acre_feet), 
      col='black',
      lwd=2)
dev.off()

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/acreft_hist_whole.pdf',
    width=4,
    height=3)
ggplot(data=data)+
  aes(x=Acre_feet)+
  geom_histogram(color='black')+
  ylab('Count')+
  xlab('Annual Discharge (AF/yr)')+
  theme_bw() +
  theme(text = element_text(size = 20))
dev.off()



pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/start_hist.pdf',
    width=6,
    height=4)
hist(data$StartDayofYear, 
     breaks=15,
     prob=T,
     col='lightblue',
     xlab='Start Day of Year',
     main='Distribution of the Beginning of the Irrigation Season',
     ylim=c(0,0.05))
lines(density(data$StartDayofYear), 
      col='red',
      lwd=2)
dev.off()

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/end_hist.pdf',
    width=6,
    height=4)
hist(data$EndDayofYear, 
     breaks=15,
     prob=T,
     col='lightblue',
     xlab='End Day of Year',
     main='Distribution of the last day of the irrigation season',
     ylim=c(0,0.09))
lines(density(data$EndDayofYear), 
      col='red',
      lwd=2)
dev.off()

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/length_hist.pdf',
    width=6,
    height=4)
data$Time <- as.numeric(data$Time)
ggplot(data=data)+
  aes(x=Time)+
  geom_histogram(color='black')+
  ylab('Count')+
  xlab('Length of Irrigation Season (days)')+
  theme_bw() +
  theme(text = element_text(size=13, family = 'Arial'))
ggsave('~/Desktop/diversion_models/ManuscriptFigures/length_hist.tiff',
       height = 4,
       width =4)

## Create a dataframe to store basic statistics and normality test statistics
## Use Shapiro Wilkins test for normality

ind_variables <- c(2, 4, 6, 7)
base_stats <- data.frame()

for (i in ind_variables){
  shap_results <- shapiro.test(data[,i])
  base_stats[i,1] <- colnames(data)[i]
  base_stats[i,2] <- mean(data[,i])
  base_stats[i,3] <- median(data[,i])
  base_stats[i,4] <- sd(data[,i])
  base_stats[i,5] <- shap_results$p.value
  base_stats[i,6] <- shap_results$statistic
}
base_stats <- na.omit(base_stats)
colnames(base_stats) <- c('Variable.Name', 'Mean', 'Median', 'Standard.Deviation', 'p.value', 'statistic')


## Create a correlation matrix 
cols <- c('Acre_feet',
          'class1_urban',
          'class2_crops',
          'ant_prcp',
          'irrig_prcp',
          'irrig_temp',
          'JuneAug_temp',
          'et',
          'Mar_et',
          'Mar_tmp',
          'Mar_prcp',
          'Max_Fill',
          'Carryover',
          'AF_used',
          'AF_available',
          'AF_remaining')

M <- cor(data[,28:43])
cols <- c(17, 20:29)
M <- cor(data[,cols])
colnames(M) <- c('AF', 'Urban', 'Crops', ':P[ant]', ':P[ir]', ':T[ir]', ':T[JA]', 'ET', ':ET[Mar]', ':T[Mar]', ':P[Mar]', 'Res AF', 'Carry', ':S[use]', ':S[tot]', ':S[carry]')
rownames(M) <- c('AF', 'Urban', 'Crops', ':P[ant]', ':P[ir]', ':T[ir]', ':T[JA]', 'ET', ':ET[Mar]', ':T[Mar]', ':P[Mar]', 'Res AF', 'Carry', ':S[use]', ':S[tot]', ':S[carry]')
colnames(M) <- c('ET',':Pr[non-irrig]', ':Pr[irrig]', ':T[irrig]', ':T[June-Aug]',':Q[in_LP]', 'MaxFill', ':p[urban]',':p[ag]','C','LPI')
rownames(M) <- c('ET',':Pr[non-irrig]', ':Pr[irrig]', ':T[irrig]', ':T[June-Aug]',':Q[in_LP]', 'MaxFill', ':p[urban]',':p[ag]','C','LPI')

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/corr_matrix_discharge.pdf',
    width=8,
    height=8)
corrplot(M, method='square', col= COL2('RdBu', 10), type='lower')
dev.off()

#addCoef.col='black',

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/mixcorr_discharge.pdf',
    width=8,
    height = 8)
corrplot.mixed(M, lower='number', upper='shade', upper.col= COL2('RdBu', 10), lower.col='black')
dev.off()

land <- cor(data[, c(26:29)])
colnames(land) <- c(':p[urban]',':p[ag]','C','LPI')
rownames(land) <- c(':p[urban]',':p[ag]','C','LPI')

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/corr_matrix_land.pdf',
    width=8,
    height=8)
corrplot(land, method='square', col= COL2('RdBu', 10), addCoef.col='white', type='lower')
dev.off()

climate <- cor(data[, c(17, 20:25)])
colnames(climate) <- c('ET',':Pr[non-irrig]', ':Pr[irrig]', ':T[irrig]', ':T[June-Aug]', ':Q[in_LP]', 'MaxFill')
rownames(climate) <- c('ET',':Pr[non-irrig]', ':Pr[irrig]', ':T[irrig]', ':T[June-Aug]', ':Q[in_LP]', 'MaxFill')

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/corr_matrix_climate.pdf',
    width=8,
    height=8)
corrplot(climate, method='square', col= COL2('RdBu', 10), addCoef.col='white', type='lower')
dev.off()


## ------------------------ ##
## SECTION 4: PLOT THE DATA ## 
## ------------------------ ##

# Create figure for smaller diversions
small <- data %>% 
  group_by(Name) %>%
  filter(Acre_feet<10000)

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/urb_v_discharge_S.pdf',
    width=12,
    height = 6)
ggplot(data=small) + 
  aes(x = irrig_prcp, y = Acre_feet, color=Name) + 
  geom_point() + # Color code observations by DivID
  theme_bw() +
  ylab("Surface Water Volume (acre-ft/yr)") +
  xlab("Urban Proportion")+
  guides(fill=guide_legend(title='Diversion Name'))+
  ggtitle("Annual Diversion Volume Across Urban Landscapes")
dev.off()

# Create figure for larger diversions
large <- data %>%
  group_by(Name) %>%
  filter(Acre_feet>10000 & Name!= 'New York Canal')

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/urb_v_discharge_L.pdf',
    width=12,
    height = 8)
ggplot(data=large) + 
  aes(x = irrig_temp, y = Range, color=Name) + 
  geom_point() + # Color code observations by DivID
  theme_bw() +
  ylab("Surface Water Volume (acre-ft/yr)") +
  xlab("Urban Proportion")+
  guides(fill=guide_legend(title='Diversion Name'))+
  ggtitle("Annual Diversion Volume Across Urban Landscapes")
dev.off()

ny <- data %>%
  group_by(Name) %>%
  filter(Name== 'New York Canal')
pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/NY_urb_v_discharge.pdf',
    width=12,
    height = 8)
ggplot(data=ny) + 
  aes(x = class1_urban, y = Acre_feet, color=Name) + 
  geom_point() + # Color code observations by DivID
  theme_bw() +
  ylab("Surface Water Volume (acre-ft/yr)") +
  xlab("Urban Proportion")+
  guides(fill=guide_legend(title='Diversion Name'))+
  ggtitle("Annual Diversion Volume Across Urban Landscapes")
dev.off()

names <- unique(data$Name)

for (i in names) {
  sub_data <- subset(data, Name == i)
  title = i
  plot(x=sub_data$class1_urban, y=sub_data$class2_crops, main=title)
}




change <- data.frame()
change <- data %>%
  select(Name, class1_urban) %>%
  group_by(Name) %>%
  summarize(change = max(class1_urban)-min(class1_urban))

avgs <- data %>%
  select(Name, Acre_feet) %>%
  group_by(Name) %>%
  summarize(avg = mean(Acre_feet), maxi = max(Acre_feet), mini= min(Acre_feet))

high_urb_change <- data.frame(subset(change, change>10))
write.csv(high_urb_change, '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/high_change.csv', row.names = FALSE)

low_change <- data.frame(subset(change, change<10))
write.csv(low_change, '/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Data.Inputs/low_change.csv', row.names = FALSE)


## LOOK INTO ET DATA ## 

plot(data$irrig_temp, data$et)

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=irrig_temp, y=et, color= irrig_prcp)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('Avg Maximum Irrigation Season Temp (C)')+
          ylab('Avg Total Evapotranspiration (m)') +
          labs(color=guide_legend(title='Precipitation (mm)')))
}

plot(data$JuneAug_temp, data$Acre_feet)

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=JuneAug_temp, y=Acre_feet, color= irrig_prcp)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('Avg Maximum June-Aug Temp (C)')+
          ylab('Total Discharge (AF/yr)') +
          labs(color=guide_legend(title='Precipitation (mm)')))
}

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/JAtemp_v_year.pdf',
    width=6,
    height = 5)
ggplot(data=diversions)+
  aes(x=Year, y=JuneAug_temp)+
  geom_point()+
  theme_bw()+
  ggtitle('Summer Temperatures through Time')+
  xlab('Year') +
  ylab('Average Maximum June-Aug Temp (C)')
dev.off()

pdf(file='/Users/dbeisel/Desktop/DATA/Bridget/pod_pou_lulcc/Figures/JAtemp_v_AF.pdf',
    width=5,
    height = 7)
ggplot(data=diversions)+
  aes(x=JuneAug_temp, y=Acre_feet, color=Name)+
  geom_point()+
  theme_bw()+ 
  scale_y_log10()+
  ggtitle('Summer Temperatures through Time')+
  xlab('Year') +
  ylab('Average Maximum June-Aug Temp (C)')+
  guides(color='none')
dev.off()

ggplot(data=data)+
  aes(x=Year, y=Max_Fill)+
  geom_point()+
  theme_bw()

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=Year, y=Max_Fill)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('Year')+
          ylab('Max Reservoir Fill (AF)'))
}

plot(data$Year, data$StartDayofYear)
plot(data$Year, data$EndDayofYear)

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=Year, y=irrig_temp)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('Year')+
          ylab('Temp'))
}

ggplot(data=data) +
  aes(x=Year, y=irrig_temp)+
  geom_point()+
  theme_bw()+
  ggtitle('Temp through Time') +
  xlab('Year')+
  ylab('Temp')

ggplot(data=data) +
  aes(x=Year, y=irrig_prcp)+
  geom_point()+
  theme_bw()+
  ggtitle('Precip through Time') +
  xlab('Year')+
  ylab('Precip')

ggplot(data=data) +
  aes(x=Year, y=et)+
  geom_point()+
  theme_bw()+
  ggtitle('ET through Time') +
  xlab('Year')+
  ylab('ET')

ggplot(data=data) +
  aes(x=Year, y=Max_Fill)+
  geom_point()+
  theme_bw()+
  ggtitle('Reservoir Max. Fill through Time') +
  xlab('Year')+
  ylab('Res. Fill (AF)')

ggplot(data=data) +
  aes(x=Year, y=Carryover)+
  geom_point()+
  theme_bw()+
  ggtitle('Reservoir Carryover through Time') +
  xlab('Year')+
  ylab('Res. Carryover (AF)')

ggplot(data=data) +
  aes(x=Year, y=class1_urban)+
  geom_point()+
  theme_bw()+
  ggtitle('Urban Cover through Time') +
  xlab('Year')+
  ylab('Percent Urban')


for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=Year, y=StartDayofYear)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('Year')+
          ylab('Start Day of Year'))
}

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=Year, y=EndDayofYear)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('Year')+
          ylab('End Day of Year'))
}

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data = sub_data) +
          aes(x = Year, y = Quantiles) +
          geom_point() +
          theme_bw() +
          ylab('Days') +
          xlab('Year') +
          ggtitle(i))
  
}

trends <- data %>%
  select(Name, Range, Quantiles, StartDayofYear, EndDayofYear, Acre_feet) %>%
  group_by(Name) %>%
  filter(n() > 3) %>%
  summarize(t_range = MannKendall(Range)$sl,
            t_quant = MannKendall(Quantiles)$sl,
            t_start = MannKendall(StartDayofYear)$sl,
            t_end = MannKendall(EndDayofYear)$sl,
            t_vol = MannKendall(Acre_feet)$sl) %>%
  mutate(r= t_range < 0.05,
         q = t_quant < 0.05,
         s = t_start < 0.05,
         e = t_end < 0.05,
         vol = t_vol < 0.05)
vals <- trends %>%
  select(r, q, s, e, vol) %>%
  summarize(ry = sum(r == TRUE),
            qy = sum(q == TRUE),
            sy = sum(s == TRUE),
            ey = sum(e == TRUE),
            voly = sum(vol == TRUE))

## Look for spurious correlation ####

# Plot 1987 urban vs volume

data <- subset(diversions, Year == 1987)

ggplot(data = diversions) + 
  aes(x = class1_urban, y = Acre_feet) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  ylim(0, 5000)+
  theme(legend.position = 'none')+
  theme_bw() +
  ylab('Discharge (AF)') +
  xlab('Urban Percentage (%)') +
  ggtitle('Discharge vs. Urban Percent')
ggsave('~/Desktop/urb v dis none.jpg', 
       width = 5,
       height = 5,
       units = 'in')