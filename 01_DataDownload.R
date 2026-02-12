#This file downloads all the necessary functions and datasets for this project

# Downloading data and functions here

pacman::p_load(tidyverse, patchwork, lubridate, akima, reshape2, pracma,
               gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
               reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr,
               forecast, ggthemes, splines, readr, ggbeeswarm,
               knitr, fastshap, here)


source("Functions/interpolate_variable.R")
source("Functions/data_availability_function.R")
source("Functions/weekly_sum_variables.R")
source("Functions/new_var_importance_shap_plots.R") #function for running RandomForest and visualizing variable importance and shap values
source("Functions/jackknife.R")
source("Functions/final_data_availability_plot.R")
source("Functions/plot_shap_vs_value_loop.R")
source("Functions/find_depths.R")


#### Loading Data  #### 

#Updated to incude 2024
#waterlevel data using the pressure sensor (platform data) https://portal.edirepository.org/nis/codeGeneration?packageId=edi.725.5&statisticalFileType=r
#BVR water level from the staff gauge and converted to reservoir depth
wtrlvl <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/725/4/43476abff348c81ef37f5803986ee6e1") 

#for past 2020 using pressure sensors
BVRplatform <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e")

#ctd data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.200.15&statisticalFileType=r
#updated 2025
options(timeout = 999999)
url  <- "https://pasta.lternet.edu/package/data/eml/edi/200/15/9d741c9cced69cfd609c473ada2812b1"
dest <- "CSVs/CTD.csv"
dir.create("CSVs", showWarnings = FALSE)
download.file(url, dest, mode = "wb")
CTD <- read.csv(dest)

#flora data https://portal.edirepository.org/nis/mapbrowse?packageid=edi.272.10
#published 2026
phytos_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/10/6d7576cc758ca378fe004ad0ac9eed85")

# metals data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.455.9&statisticalFileType=r
#updated 2025
metalsdf <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5")
#removed flags for 68 as per Cece's advice

#secchi data https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=198&revision=13
#updated 2025
secchiframe <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/198/13/3ee0ddb9f2183ad4d8c955d50d1b8fba")

#ysi https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=198&revision=13
#updated 2025
ysi_profiles <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/198/13/e50a50d062ee73f4d85e4f20b360ce4f")
write.csv(ysi_profiles, "CSVs/ysi_profiles.csv")

##chemistry: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.199.13&statisticalFileType=r
#updated 2025
chemistry <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081") 

#meteorological data from FCR https://portal.edirepository.org/nis/codeGeneration?packageId=edi.389.10&statisticalFileType=r
options(timeout = 9999999)
metdata <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/389/10/d3f3d2fa40c41fdcd505ae49b2fdcf8b")
EDImet <- metdata

#bathymetry data for BVR https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1254.1
bath <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184")

BVRbath<- bath|>
  filter(Reservoir == "BVR")

