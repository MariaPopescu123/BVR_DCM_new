#This file downloads all the necessary functions and datasets for this project.
#
# Inputs:
# - Internet connection for EDI/GitHub downloads.
# - Local Functions/*.R files in this repository.
#
# Outputs:
# - Populates core in-memory objects used by downstream scripts.
# - Writes source datasets to CSVs/.
# - Defines variable_labels used by plotting/ML scripts.
#
# Dependencies:
# - R package pacman plus all packages loaded by pacman::p_load().
# - This script should be run first in a fresh R session.

if (!requireNamespace("pacman", quietly = TRUE)) {
  stop("Package 'pacman' is required. Install with install.packages('pacman') and rerun 01_DataDownload.R.")
}

dir.create("CSVs", showWarnings = FALSE, recursive = TRUE)
dir.create("Figs", showWarnings = FALSE, recursive = TRUE)

# Downloading data and functions here

pacman::p_load(tidyverse, patchwork, lubridate, akima, reshape2, pracma,
               gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
               reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr,
               forecast, ggthemes, splines, readr, ggbeeswarm,
               knitr, fastshap, here, ISOweek, ragg, scales, rlang, randomForest)


source("Functions/interpolate_variable.R")
source("Functions/data_availability_function.R")
source("Functions/weekly_sum_variables.R")
source("Functions/new_var_importance_shap_plots.R") #function for running RandomForest and visualizing variable importance and shap values
source("Functions/jackknife.R")
source("Functions/final_data_availability_plot.R")
source("Functions/plot_shap_vs_value_loop.R")
source("Functions/find_depths.R")


#### Loading Data  #### 
#Note! This will take a while as some of these files are quite large.

#Updated to include 2024
#waterlevel data using the pressure sensor (platform data) https://portal.edirepository.org/nis/codeGeneration?packageId=edi.725.5&statisticalFileType=r
#BVR water level from the staff gauge and converted to reservoir depth
wtrlvl <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/4/43476abff348c81ef37f5803986ee6e1")
write.csv(wtrlvl, "CSVs/wtrlvl.csv", row.names = FALSE)

#water level data for years post-2020 using pressure sensors
BVRplatform <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e")
write.csv(BVRplatform, "CSVs/BVRplatform.csv", row.names = FALSE)

#ctd data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.200.15&statisticalFileType=r
#updated 2025
options(timeout = 999999)
url  <- "https://pasta.lternet.edu/package/data/eml/edi/200/15/9d741c9cced69cfd609c473ada2812b1"
dest <- "CSVs/CTD.csv"
dir.create("CSVs", showWarnings = FALSE)
download.file(url, dest, mode = "wb")
CTD <- read_csv(dest)

#flora data https://portal.edirepository.org/nis/mapbrowse?packageid=edi.272.10
#published 2026
phytos_df <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/272/10/6d7576cc758ca378fe004ad0ac9eed85")
write.csv(phytos_df, "CSVs/phytos_df.csv", row.names = FALSE)

# metals data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.455.9&statisticalFileType=r
#updated 2025
metalsdf <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5")
write.csv(metalsdf, "CSVs/metalsdf.csv", row.names = FALSE)
#removed flags for 68, per the data creator (Cece Wood)'s recommendation

#secchi data https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=198&revision=13
#updated 2025
secchiframe <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/198/13/3ee0ddb9f2183ad4d8c955d50d1b8fba")
write.csv(secchiframe, "CSVs/secchiframe.csv", row.names = FALSE)

#ysi https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=198&revision=13
#updated 2025
ysi_profiles <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/198/13/e50a50d062ee73f4d85e4f20b360ce4f")
write.csv(ysi_profiles, "CSVs/ysi_profiles.csv", row.names = FALSE)

##chemistry: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.199.13&statisticalFileType=r
#updated 2025
chemistry <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081") 
write.csv(chemistry, "CSVs/chemistry.csv", row.names = FALSE)

#meteorological data from FCR https://portal.edirepository.org/nis/codeGeneration?packageId=edi.389.10&statisticalFileType=r
options(timeout = 9999999)
metdata <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/389/10/d3f3d2fa40c41fdcd505ae49b2fdcf8b")
EDImet <- metdata
EDImetC <- EDImet %>%
  select(
    Reservoir,
    Site,
    DateTime,
    Rain_Total_mm,
    WindSpeed_Average_m_s,
    AirTemp_C_Average,
    Flag_WindSpeed_Average_m_s,
    Flag_AirTemp_C_Average,
    Flag_Rain_Total_mm
  )
write.csv(EDImetC, "CSVs/EDImetC.csv", row.names = FALSE)


#bathymetry data for BVR https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1254.1
bath <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184")
bath<- bath|>
  filter(Reservoir == "BVR")

write.csv(bath, "CSVs/BVRbath.csv", row.names = FALSE)


#BVR depth offsets (used in 04_photic_temp_thermo.R)
download.file(
  "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
  destfile = "BVR_Depth_offsets.csv"
)

#### Variable labels used across scripts ####
variable_labels <- c(
  max_conc = "DCM Magnitude (\u00b5g/L)",
  DCM_depth = "DCM Depth (m)",
  WaterLevel_m = "Water Level (m)",
  PZ = "Photic Zone Depth (m)",
  PZ_prop = "PZ/Water Level Proportion",
  N_at_DCM = "Buoyancy Frequency at DCM (s\u207b\u00b9)",
  schmidt_stability = "Schmidt Stability (J/m\u00b2)",
  surface_temp   = "Surface Temperature at 0.5m (\u00b0C)",
  temp_at_DCM    = "Temperature at DCM (\u00b0C)",
  thermocline_depth = "Thermocline Depth (m)",
  SFe_mgL_at_DCM = "SFe (mg/L) at DCM",
  SRP_ugL_at_DCM = "SRP (\u00b5g/L) at DCM",
  NH4_ugL_at_DCM = "NH\u2084\u207a (\u00b5g/L) at DCM",
  NO3NO2_ugL_at_DCM = "NO\u2083\u207b/NO\u2082\u207b at DCM",
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  depth_NH4_ugL_max = "Depth of Max NH\u2084\u207a (m)",
  depth_NO3NO2_ugL_max = "Depth of Max NO\u2083\u207b/NO\u2082\u207b (m)",
  Precip_Weekly  = "Precipitation Weekly Sum (mm)",
  precip_lag1    = "Precipitation Weekly Sum mm (Lag 1 wk)",
  precip_lag2    = "Precipitation Weekly Sum mm (Lag 2 wk)",
  AirTemp_Avg    = "Air Temperature Weekly Average (\u00b0C)",
  airtemp_lag1   = "Air Temperature Weekly Average \u00b0C (Lag 1 wk)",
  airtemp_lag2   = "Air Temperature Weekly Average \u00b0C (Lag 2 wk)",
  WindSpeed_Avg  = "Wind Speed Weekly Average (m/s)",
  wind_lag1      = "Wind Speed Weekly Average m/s (Lag 1 wk)",
  wind_lag2      = "Wind Speed Weekly Average m/s (Lag 2 wk)"
)

#### Read in CSVs without re-downloading ####
# Run this section if you want to load data from saved CSVs
# after having already downloaded and saved above
# instead of re-downloading from EDI

wtrlvl <- read_csv("CSVs/wtrlvl.csv")
BVRplatform <- read_csv("CSVs/BVRplatform.csv")
CTD <- read_csv("CSVs/CTD.csv")
phytos_df <- read_csv("CSVs/phytos_df.csv")
metalsdf <- read_csv("CSVs/metalsdf.csv")
secchiframe <- read_csv("CSVs/secchiframe.csv")
ysi_profiles <- read_csv("CSVs/ysi_profiles.csv")
chemistry <- read_csv("CSVs/chemistry.csv")
EDImetC <- read_csv("CSVs/EDImetC.csv")
bath <- read_csv("CSVs/BVRbath.csv")
