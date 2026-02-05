# Beaverdam Deep Chlorophyll Maximum Drivers

## About This Repository

This repository includes all workflows needed to reproduce the data frames and figures used in the analysis of deep chlorophyll maxima (DCM) in Beaverdam Reservoir for the project called: \> *WILL UPDATE THIS WITH PUBLICATION NAME*

------------------------------------------------------------------------

## Instructions to reproduce figures and analysis

1.  Run `01_DataDownload.R`

2.  Run scripts in numerical order within `02_DataWrangling/` to fully reproduce the data products and figures used in the manuscript.

3.  Run `03_Machine_learning/RF_and_SHAP.R`

All outputs (CSVs and figures) are written to the `CSVs/` and `Figs/` directories.

------------------------------------------------------------------------

## Repository Structure

### `01_DataDownload.R`

Scripts and helper functions used to download the raw datasets required for this project.

### `02_DataWrangling/`

This folder contains a sequence of 11 scripts that build the core analysis datasets and manuscript figures. The scripts within are described below.

#### `01_water_level.R`

Downloads Beaverdam Reservoir water-level data (2014–2024) from EDI, merges pressure-sensor and historical sources, interpolates and aggregates them to weekly water levels.\
Generates a year-colored time-series plot, saves a cleaned CSV, and computes summary statistics on within- and across-year variability (including pre- vs. post-2022 comparisons).

#### `02_Phytos_dataframe.R`

Cleans and filters FluoroProbe phytoplankton profiles, removes problematic casts, and computes deep chlorophyll maximum (DCM) metrics (peak depth and peak concentration) for each cast and week.\
Produces QC plots, annual boxplots and summaries, pairwise year-to-year significance tests, and exports weekly-aggregated DCM depth and magnitude datasets for downstream analyses (e.g., random forest modeling).

#### `03_Phytos_heatmaps_profiles.R`

Generates depth–time heatmaps of phytoplankton concentration for Beaverdam Reservoir (2015–2024), masking values below the water level and standardizing color scales across years.\
Identifies the day of peak phytoplankton concentration each year and produces profile plots showing full FluoroProbe casts (by algal group and total) for those maximum-bloom events.

#### `04_photic_temp_thermo.R`

This script calculates weekly photic zone depth, interpolated temperature profiles, and thermocline depth for Beaverdam Reservoir (Site 50). It cleans and merges CTD and YSI data, derives light availability from Secchi depth, and computes thermocline depth from temperature–depth profiles. The final output combines photic zone, thermocline, and water level into a single dataset for use in downstream analyses.

#### `O5_buoyancy_freq.R`

This script calculates buoyancy frequency (stratification strength) from temperature-depth profiles and extracts the value at the depth of the deep chlorophyll maximum (DCM) each week. The final output is a weekly time series of buoyancy frequency at the DCM for use in analyses of physical controls on phytoplankton structure.

#### `06_nutrients.R`

This script processes nutrient and metal chemistry data for Beaverdam Reservoir, including filtering and flag handling, and interpolating concentrations at the deep chlorophyll maximum (DCM). The final output provides weekly aggregated values of key nutrients (SRP, NH₄) and soluble iron (SFe) for downstream analyses.

#### `07_schmidt_stability.R`

This script calculates Schmidt stability for Beaverdam Reservoir by combining temperature profiles with bathymetry adjusted for water level. It produces a weekly time series of Schmidt stability, which quantifies the energy required to mix the water column, reflecting stratification strength over time.

#### `08_NLDAS_downscaling.R`

This script merges EDI and NLDAS meteorological datasets for Beaverdam Reservoir by first computing weekly averages, performing quality control, and downscaling NLDAS to match EDI values using linear regressions. It then combines the datasets, creates lagged variables, visualizes both 2015 and all years of data, and exports a clean weekly meteorological dataset ready for machine learning analyses.

#### `09_join_all_frames.R`

This code joins all the previously processed weekly datasets-phytoplankton, metals, photic/thermal profiles, buoyancy, chemistry, Schmidt stability, and meteorology—into a single full_weekly_data dataframe. It then cleans and renames key variables, organizes lagged meteorological variables, filters for 2015–2024, and exports the complete weekly dataset. Finally, it calculates summary statistics for high phytoplankton concentrations (max_conc \> 20), including the median, standard deviation, ±1 SD, and sample size, both for concentration and DCM depth.

#### `10_Choosing_variables_correlation_matrix.R`

This code creates correlation heatmaps for different groups of weekly variables from full_weekly_data. It also creates the pretty labels used for the remaining figures

#### `11_Visualize_final_chosen_variables.R`

This code produces a seasonal overview of the chosen environmental and depth-related variables with a custom DOY window (133-285, roughly May-October).

### `03_Machine_learning/`

This folder contains a script for running the random forest model with Shapley Additive Explanations (SHAP)

#### `RF_and_SHAP.R`

This code uses Random Forest models to predict DCM_depth and max_conc from environmental variables, then applies SHAP values to quantify each predictor’s contribution to the model. It runs separate analyses for depth and magnitude, including weather lags and water column metrics, and generates plots of variable importance, model robustness (jackknife), and SHAP interactions. The results help identify which physical, chemical, or meteorological factors most strongly influence phytoplankton dynamics.

### `Functions/`

Reusable functions that support data processing and figure creation. These functions do not need to be run individually- they are called in using the `01_DataDownload.R` file. They are used throughout the scripts. Descriptions of each are provided here.

#### `data_availability_function.R`

This is for QAQC purposes and was developed so anyone can see the data availability of any variable within any dataframe. Not to produce publication-ready figures. It produces a figure that displays which days within each year that we have data available for.

#### `final_data_availability_plot.R`

This was developed to produce the final FluoroProbe Data Availability plot in the supplementary figure.

#### `interpolate_variable.R`

This function interpolates missing values for a list of water quality variables across both depth and weeks in a reservoir. It first summarizes the raw data by date, depth, and week, then linearly interpolates values within observed depth and week ranges, filling gaps while avoiding extrapolation beyond measured data. The result is a complete weekly-depth grid for each variable, ready for further analysis or modeling.

#### `jackknife.R`

This function performs a jackknife-style random forest analysis to quantify the importance of each predictor variable (%IncMSE) for a response variable, separately for each year and pooled across all years. It tunes the RF parameters per year, runs leave-one-out models to estimate robustness, and then summarizes the results as mean ± SD %IncMSE, which it visualizes as a heatmap with variables on the y-axis and years on the x-axis. The output also includes numeric summaries, full jackknife distributions, overall variable rankings, and metadata about the models.

#### `new_var_importance_shap_plots.R`

This function computes and visualizes variables importance (%IncMSE) and SHAP value distributions for a random forest model on a given dataset, optionally saving combined plots.

#### `plot_shap_vs_value_loop.R`

For each variable in vars_to_plot, create a scatter of SHAP values vs predictor values colored by z-scaled predictor values, save individual plots, and optionally create a multi-panel figure.

#### `weekly_sum_variables.R`

Summarizes one or more water-quality variables by week (Year + Week) from a depth profile dataset, returning max/min values, depths of extremes, and values at the DCM (Deep Chlorophyll Maximum).
