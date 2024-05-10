# pod_pou_lulcc
Modified from Bridget Bittmann (2023; github: bridgetmarie24)


Analyze the Impacts of Urbanization and Climate on Diversions in the Lower Boise River Basin

Summary

This analysis analyzes the impacts of urbanization and climate on diverisons in the Lower Boise River Basin from 1987 to 2020. This also analyzes trend through time in flow volumes. This analysis was done because the Lower Boise River Basin is rapidly urbanizing its agricultural lands, and water managers did not possess the long term analysis to understand how the urbanization was impacting the distribution of irrigation water resources.

Steps to running diversion analysis:

Python scripts are basically all the data preprocessing for what goes into the models, and the R scripts are all the modeling.

Python

* 01_diversion_timeseries.ipynb : This script takes daily irrigation season flow values for the diversions and calculates 1) total water diverted, 2) start day of irrigation season, 3) end day of irrigation season, 4) length of irrigation season. It corrects for diversions that would go beyond the bounds of a true irrigation season (e.g., February).
* 02_extract_gridment.ipynb: This script calculates zonal stats for each POU from Daymet and SSEBop data.
* 03_subset_LULCC.ipynb: This script uses LCMAP data to calculate annual percent of each land class from 1987 to 2020 for each POU.
* 04_ hydromet_data.ipynb: This script calculates the reservoir carryover and the maximum fill for the 3 reservoirs (Anderson Ranch, Arrowrock, Lucky Peak) in the Lower Boise River Basin.
* 05_data_compilation.ipynb: This compiles climate, land use, and flow outputs and puts them in a usable, long-format csv file to import into R.

R
* 06_preprocessing.R:This script standardizes compiled data for use in the GLMM and GLMM+ARMA models, plots correlation matrix, and checks data distribution. 
* 07_borah_dataprep.R: This takes full csv outputs from the Python output and puts it in the usable format for both the Generalized Linear Mixed Model with and without the ARMA. This exports 2 different csv files to be put into the respective models.
* 08_div_mixed_model_borah.R : Runs GLMMs with priors and model specifications and outputs .RDS files borah_epreds.R : Uses model outputs to run expected predictions based on posterior outputs. This saves csv files for each of the variables that had a non-zero effect. These predictions are used to create marginal effects figures.
* 09_borah_epreds.R : Get predicted draws for each variable from the ARMA model
* 10_ind_mlr_brms.R: Runs individual GLMs for each diversion and summarized effect sizes. Outputs a csv file with the effect size for each variable and the uncertainty around it.
* 11_trend_plots.R: Runs Mann Kendall test and outputs plots for diversions with trends
* 12_figures.R: This script creates all the figures in my thesis from model outputs besides the Mann Kendall analysis. Long script broken into sections based on the figures being made for each analysis

Contact Information

For more information or questions regarding this analysis, please email Bridget Bittmann at bridgetbittmann@u.boisestate.edu.
