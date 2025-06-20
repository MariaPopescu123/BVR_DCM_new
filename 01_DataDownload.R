# Downloading data and functions here

pacman::p_load(tidyverse, patchwork, lubridate, akima, reshape2, pracma,
               gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
               reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes, splines)

source("02_Functions/interpolate_variable.R")
source("02_Functions/data_availability_function.R")
source("02_Functions/weekly_sum_variables.R")

#### Loading Data  #### 04July2025

#ctd data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.200.15&statisticalFileType=r
#updated 2025
CTD <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/200/15/9d741c9cced69cfd609c473ada2812b1")

#flora data https://portal.edirepository.org/nis/mapbrowse?packageid=edi.272.9
#published 2024
#current_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/8/0359840d24028e6522f8998bd41b544e")

#published 2025
phytos_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/9/f246b36c591a888cc70ebc87a5abbcb7")

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

#data from here https://portal.edirepository.org/nis/mapbrowse?packageid=edi.199.12
chemistry <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/199/12/a33a5283120c56e90ea414e76d5b7ddb")

#meteorological data from FCR https://portal.edirepository.org/nis/mapbrowse?packageid=edi.389.8
#options(timeout = 300)
#metdata <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

#bathymetry data for BVR https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1254.1
bath <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184")

BVRbath<- bath|>
  filter(Reservoir == "BVR")

