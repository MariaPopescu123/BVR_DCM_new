
#### Nutrients  ####

chemistry_filtered <- chemistry |> #loaded in from DataDownload
  filter(Reservoir == "BVR", Site == 50)|>
  mutate(Date = as_date(DateTime), 
         DateTime = as.POSIXct(DateTime))|>
  mutate(
    TN_ugL = if_else(Flag_TN_ugL == 9, NA_real_, TN_ugL),
    TP_ugL = if_else(Flag_TP_ugL == 9, NA_real_, TP_ugL),
    SRP_ugL = if_else(Flag_TP_ugL == 9, NA_real_, SRP_ugL),
    NH4_ugL = if_else(Flag_NH4_ugL == 9, NA_real_, NH4_ugL),
    NO3NO2_ugL = if_else(Flag_NO3NO2_ugL == 9, NA_real_, NO3NO2_ugL),
    DIC_mgL = if_else(Flag_DIC_mgL == 9, NA_real_, DIC_mgL)
  ) |>
  select(Date, Depth_m, TN_ugL, TP_ugL, NH4_ugL, NO3NO2_ugL, DIC_mgL, SRP_ugL)


variables <- c("TN_ugL", "TP_ugL","SRP_ugL", "NH4_ugL", "NO3NO2_ugL", 
               "DIC_mgL")

#raw data availability 
plot <- data_availability(chemistry_filtered, variables)
print(plot)
ggsave("Figs/Data_availability/raw_chem_availability.png", plot = plot, width = 20, height = 15, dpi = 300)

#### NP ratio  ####

calculate_np_ratio <- function(tn, tp) {
  # Convert concentrations from µg/L to mg/L
  tn_mgL <- tn / 1000
  tp_mgL <- tp / 1000
  
  # Convert mg/L to moles/L
  tn_molL <- tn_mgL / 14.01
  tp_molL <- tp_mgL / 30.97
  
  # Calculate N:P ratio
  calcnp_ratio <- ifelse(is.na(tn_molL) | is.na(tp_molL), NA, tn_molL / tp_molL)
  
  return(calcnp_ratio)
}

# added np ratio to dataframe
chemistry_filtered_np <- chemistry_filtered %>%
  mutate(np_ratio = calculate_np_ratio(TN_ugL,TP_ugL))|>
  relocate(np_ratio, .before = TN_ugL)|>
  mutate(Week = week(Date), 
         Year = year(Date))


#before I run this I need to have DCM depth calculated so I can see what the 
#concentration of the nutrients are at DCM depth

#just date and DCMdepth

chem_w_DCM <- chemistry_filtered_np|>
  left_join(final_phytos, by = c("Week", "Year"))|>
  mutate(Reservoir = "BVR", Site = 50) # need to this here for interpolation function

#interpolate so that I can get a value for DCM depth
variables <- c("SRP_ugL", "NH4_ugL", "np_ratio")
chem_interpolated <- interpolate_variable(chem_w_DCM, variables)

chem_interpolated2 <- chem_interpolated |>
  left_join(final_phytos, by = c("Week", "Year")) 
chem_interpolated3 <- chem_interpolated2|>
  mutate(Reservoir = "BVR", Site = 50) |>
  filter(Year > 2014) |>
  group_by(Date) |>
  tidyr::fill(DCM_depth, .direction = "downup") |>
  ungroup()|>
  filter(!is.na(DCM_depth))

####summarize variables
variables <- c("SRP_ugL", "NH4_ugL", "np_ratio")
chem_weekly_sum <- weekly_sum_variables(chem_interpolated3, variables)

#join to frame with correct dates
final_chem <- frame_weeks|>
  left_join(chem_weekly_sum, by = c("Week", "Year"))|>
  select(-WaterLevel_m)

write.csv(final_chem, "CSVs/final_chem.csv", row.names = FALSE)


####SFe joined####
# metals data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.455.9&statisticalFileType=r
#updated 2025
metalsdf <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5")
#removed flags for 68 as per Cece's advice
metals_updated <- metalsdf |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Site == 50, Reservoir == "BVR", Flag_SFe_mgL != 68) |>
  select(Date, Depth_m, SFe_mgL)|>
  mutate(Week = week(Date), 
         Year = year(Date))

metaswDCM <- metals_updated|>
  left_join(final_phytos, by = c("Week", "Year"))|>
  mutate(Reservoir = "BVR", Site = 50) # need to this here for interpolation function

#interpolate so that I can get a value for DCM depth
variables <- c("SFe_mgL")
metals_interpolated <- interpolate_variable(metaswDCM, variables)

metals_interpolated2 <- metals_interpolated |>
  left_join(final_phytos, by = c("Week", "Year")) 
metals_interpolated3 <- metals_interpolated2|>
  mutate(Reservoir = "BVR", Site = 50) |>
  filter(Year > 2014) |>
  group_by(Date) |>
  tidyr::fill(DCM_depth, .direction = "downup") |>
  ungroup()|>
  filter(!is.na(DCM_depth))
####summarize variables
variables <- c("SFe_mgL")
metals_weekly_sum <- weekly_sum_variables(metals_interpolated3, variables)

final_metals <- frame_weeks|> #random forest frame with metals
  left_join(metals_weekly_sum, by = c("Week", "Year"))|>
  select(-WaterLevel_m)

write.csv(final_metals, "CSVs/final_metals.csv", row.names = FALSE)


