# Beaverdam Deep Chlorophyll Maximum Drivers

## About This Repository

This repository includes all workflows needed to reproduce the analyses and figures for the publication "Ten years of monitoring data and explainable AI reveal a persistent deep-water phytoplankton bloom driven by physical and biogeochemical processes in a eutrophic reservoir"

------------------------------------------------------------------------

## Instructions to reproduce figures and analysis

1.  Run `01_DataDownload.R` (loads packages/functions, downloads or reloads input data, and creates `variable_labels` used by downstream scripts).

2.  Run scripts in numerical order within `02_Datawrangling/` to reproduce the data products and figures used in the manuscript.

3.  Run `03_Machine_learning/RF_and_SHAP.R`

All outputs are written to the `CSVs/` (data products) and `Figs/` (figures) directories. These directories are created automatically by `01_DataDownload.R` if they do not already exist.

### Where figures are saved

Figures are organized into subfolders of `Figs/` by purpose:

- **`Figs/Main Manuscript/`** — the final, curated figures used in the manuscript (`Figure2.png`–`Figure7.png`). Figure 1 is a conceptual/site figure made outside this repository.
- **`Figs/Supporting Information/`** — the final, curated figures used in the Supporting Information (`Figure S1.png`–`Figure S10.png`, including `Figure S8a.png` and `Figure S8b.png`).
- **All other `Figs/` subfolders** (e.g. `Phytos_viz/`, `Data_availability/`, `correlations/`, `Thermocline/`, `metdata/`, `MachineLearning/`, `Daily_interp_Casts/`, `raw_flora_casts/`) — working and diagnostic figures generated along the way. Several of the manuscript and Supporting Information figures are also saved to one of these working folders in addition to the curated `Main Manuscript/` or `Supporting Information/` folder.

The per-script descriptions below list which figures each script produces and where they are written.

###  Notes

- The workflow requires scripts to run in order in the same R session.
- `02_Datawrangling/02_Phytos_dataframe.R` creates `frame_weeks.csv` and `final_phytos.csv`, which are required by later scripts.
- `02_Datawrangling/04_photic_temp_thermo.R` creates `temp_depths_interp` in memory, which is used by `05_buoyancy_freq.R` and `07_schmidt_stability.R`.
- If you run scripts independently (instead of sequentially), some objects must be loaded manually first.
- `00_run_all.R` can be used to source the data-wrangling scripts in order after running `01_DataDownload.R`.

------------------------------------------------------------------------

## Repository Structure

### `01_DataDownload.R`

Scripts and helper functions used to download the raw datasets required for this project. Also creates the output directory tree (`CSVs/`, `Figs/`, `Figs/Main Manuscript/`, `Figs/Supporting Information/`).

*Figures:* none (other scripts populate the figure folders).

### `02_Datawrangling/`

This folder contains a numbered sequence of scripts that build the core analysis datasets and manuscript figures, plus a `00_run_all.R` driver and one standalone presentation-only script (`02_alternate_phyto_heatmap.R`). The scripts are described below.

#### `00_run_all.R`

Sources and runs the numbered DataWrangling scripts in order (does not source `02_alternate_phyto_heatmap.R`).

#### `01_water_level.R`

Downloads Beaverdam Reservoir water-level data (2014–2024) from EDI, merges pressure-sensor and historical water level data sources, interpolates the data, and aggregates the time series to weekly water levels.\
Generates a time-series plot colored by year, saves a cleaned CSV, and computes summary statistics on within- and across-year variability (including pre- vs. post-2022 comparisons).

*Figures:* the water-level time-series plot is drawn to the plotting window only (not written to disk).

#### `02_Phytos_dataframe.R`

Cleans and filters FluoroProbe phytoplankton depth profiles (casts), removes questionable casts, and computes deep chlorophyll maximum (DCM) metrics (peak depth and peak concentration) for each cast and week.\
Produces QC plots, annual boxplots and summaries, pairwise year-to-year significance tests, and exports weekly-aggregated DCM depth and magnitude datasets for downstream analyses (e.g., random forest modeling).

*Figures:*
- **Manuscript Figure 4** (paneled DCM depth/magnitude boxplots) → `Figs/Main Manuscript/Figure4.png` (also `Figs/Phytos_viz/boxplots_paneled.png`).
- **Supporting Information Figure S4** (FluoroProbe data availability) → `Figs/Supporting Information/Figure S4.png`.
- **Supporting Information Figure S6** (phytoplankton summary) → `Figs/Supporting Information/Figure S6.png` (also `Figs/Phytos_viz/phytoplankton_summary.png`).
- **Supporting Information Figure S7** (Kruskal–Wallis year comparisons) → `Figs/Supporting Information/Figure S7.png` (also `Figs/Phytos_viz/kruskal-wallis.png`).
- Working/QC figures: group composition plots → `Figs/Phytos_viz/` (`comp.png`, `comp_at_DCM.png`); per-year raw casts → `Figs/raw_flora_casts/`; per-year casts with DCM marked → `Figs/raw_flora_casts_with_DCM/`; total-phytos availability → `Figs/Data_availability/`.

#### `02_alternate_phyto_heatmap.R`

Optional presentation-only script. Produces a depth-capped (0–10 m), water-level-free variant of the phytoplankton heatmap from `03_Phytos_heatmaps_profiles.R`. Not part of the manuscript pipeline and not sourced by `00_run_all.R`.

#### `03_Phytos_heatmaps_profiles.R`

Generates depth–time heatmaps of phytoplankton concentration for Beaverdam Reservoir (2015–2024), masking values below the water level and standardizing color scales across years.\
Identifies the day of peak phytoplankton concentration each year and produces profile plots showing full FluoroProbe casts (by algal group and total) for those maximum-bloom events.

#### `04_photic_temp_thermo.R`

This script calculates weekly photic zone depth, interpolated temperature profiles, and thermocline depth for Beaverdam Reservoir (Site 50). It cleans and merges CTD and YSI data, estimates light availability from Secchi depth data, and computes thermocline depth from temperature–depth profiles. The final output combines photic zone, thermocline, and water level observations into a single dataset for use in downstream analyses.

#### `05_buoyancy_freq.R`

This script calculates buoyancy frequency (stratification strength) from temperature-depth profiles and extracts the value at the depth of the deep chlorophyll maximum (DCM) each week. The final output is a weekly time series of buoyancy frequency at the DCM for use in analyses of physical controls on phytoplankton structure.

#### `06_nutrients.R`

This script processes nutrient and metal chemistry data for Beaverdam Reservoir, including filtering and flag handling, and interpolating concentrations at the deep chlorophyll maximum (DCM). The final output provides weekly aggregated values of key nutrients (SRP, NH₄) and soluble iron (SFe) for downstream analyses.

#### `07_schmidt_stability.R`

This script calculates Schmidt stability for Beaverdam Reservoir by combining temperature profiles with bathymetry adjusted for water level. It produces a weekly time series of Schmidt stability, which quantifies the energy required to mix the water column, reflecting stratification strength over time.

#### `08_NLDAS_downscaling.R`

This script merges EDI and NLDAS meteorological datasets for Beaverdam Reservoir by first computing weekly averages, performing quality control, and downscaling NLDAS to match EDI values using linear regressions. It then combines the datasets, creates lagged variables, visualizes all years of data, and exports a clean weekly meteorological dataset ready for downstream analyses.

#### `09_join_all_frames.R`

This code joins all the previously processed weekly datasets — phytoplankton, photic/thermal profiles, buoyancy, nutrient and metals chemistry, Schmidt stability, and meteorology — into a single full_weekly_data dataframe. It then cleans and renames key variables, organizes lagged meteorological variables, filters the time series to be just  2015–2024, and exports the complete weekly dataset. Finally, it calculates summary statistics for high phytoplankton concentrations (max_conc \> 20), including the median, standard deviation, ±1 SD, and sample size, both for concentration and DCM depth.

#### `10_Choosing_variables_correlation_matrix.R`

This code creates correlation heatmaps for different groups of weekly variables from full_weekly_data. It also creates the pretty labels used for the remaining figures.

#### `11_Visualize_final_chosen_variables.R`

This code produces a seasonal overview of the chosen environmental and depth-related variables with a custom DOY window (133-285, roughly May-October).

### `03_Machine_learning/`

This folder contains a script for running the random forest model with Shapley Additive Explanations (SHAP).

#### `RF_and_SHAP.R`

This code uses random forest models to predict two metrics of phytoplankton vertical structure (DCM_depth and max_conc) from environmental variables, then applies SHAP values to quantify each predictor’s contribution to the model. It runs separate analyses for depth and magnitude, including weather lags and water column metrics, and generates plots of variable importance, model robustness (jackknife), and SHAP interactions. The results help identify which physical, chemical, and meteorological factors most strongly influence phytoplankton dynamics.

### `Functions/`

Reusable functions that support data processing and figure creation. These functions are sourced in `01_DataDownload.R` and then used throughout the pipeline scripts. Descriptions of each are provided here.

#### `data_availability_function.R`

This function is for QAQC purposes and was developed so anyone can see the data availability of any monitoring variable within any dataframe (not to produce publication-ready figures!). It produces a figure that displays which days within each year that we have available observations.

#### `date_sum_variables.R`

Summarizes one or more water-quality variables by Date from a depth profile dataset (typically the daily-interpolated output of `interpolate_variable()`). Returns max/min values, depths of those extremes, and the value at the DCM for each Date. Used by `06_nutrients.R` to build the chemistry and metals summaries that feed the joined RF dataset.

#### `final_data_availability_plot.R`

This function produces the final FluoroProbe Data Availability plot for the Supporting Information.

#### `find_depths.R`

Reads BVR platform sensor data (pressure transducer + thermistor string) together with a depth-offset table and assigns a calibrated depth to each sensor reading. Used by `04_photic_temp_thermo.R` to derive 2021 sensorstring temperature profiles when CTD/YSI data are not available.

#### `interpolate_variable.R`

This function interpolates missing values for a list of water quality variables across both depth and time in a reservoir. It first summarizes the raw data by date and depth, then linearly interpolates values within observed depth and day-of-year ranges, filling gaps while avoiding extrapolation beyond measured data. The result is a complete daily depth grid for each variable, ready for further analysis or modeling.

#### `jackknife.R`

This function performs a jackknife-style random forest analysis to quantify the importance of each predictor variable (%IncMSE) for a response variable, both separately for each year and pooled across all years. It tunes the random forest parameters per year, runs leave-one-out models to estimate robustness, and then summarizes the results as mean ± SD %IncMSE, which it visualizes as a heatmap with variables on the y-axis and years on the x-axis. The output also includes numeric summaries, full jackknife distributions, overall variable rankings, and metadata about the models.

#### `new_var_importance_shap_plots.R`

This function computes and visualizes variables importance (%IncMSE) and SHAP value distributions for a random forest model on a given dataset, optionally saving combined plots.

#### `plot_shap_vs_value_loop.R`

For each variable in vars_to_plot, this function creates a scatterplot of SHAP values vs predictor values colored by z-scaled predictor values, saves individual plots, and optionally creates a multi-panel figure.

#### `weekly_sum_variables.R`

Weekly (Year + Week) variant of `date_sum_variables.R`: same max/min/depth-at-extreme/value-at-DCM summaries but aggregated to weekly resolution. Not used by the current manuscript pipeline; retained as a utility for weekly-resolution exploration.
