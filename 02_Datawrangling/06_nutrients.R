#Maria Popescu

#This script:
# 1. Cleans the chemistry data
# 2. Interpolates and summarizes chemistry data for final data frame to be used in RF analysis
# 3. Cleans the SFe data
# 4. Interpolate and summarizes SFe (metals) data for final dataframe to be used in RF analysis

# 1. Clean chemistry ####
chemistry_filtered <- chemistry |>
  filter(Reservoir == "BVR", Site == 50)|>
  mutate(Date = as_date(DateTime), 
         DateTime = as.POSIXct(DateTime))|>
  select(Date, Depth_m, NH4_ugL, SRP_ugL, NO3NO2_ugL)|>
  mutate(Week = week(Date), 
         Year = year(Date))

variables <- c("SRP_ugL", "NH4_ugL", "NO3NO2_ugL")
data_availability(chemistry_filtered, variables)

chem_w_DCM <- chemistry_filtered|>
  left_join(final_phytos, by = c("Week", "Year"))|> 
  mutate(Reservoir = "BVR", Site = 50)

# 2. Interpolate and summarize ----
variables <- c("SRP_ugL", "NH4_ugL", "NO3NO2_ugL")
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

variables <- c("SRP_ugL", "NH4_ugL", "NO3NO2_ugL")
chem_weekly_sum <- weekly_sum_variables(chem_interpolated3, variables)

final_chem <- chem_weekly_sum

write.csv(final_chem, "CSVs/final_chem.csv", row.names = FALSE)

# 3. Prepare the SFe data ----------------
metals_updated <- metalsdf |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Site == 50, Reservoir == "BVR") |>
  select(Date, Depth_m, SFe_mgL)|>
  mutate(Week = week(Date), 
         Year = year(Date))

metalswDCM <- metals_updated|>
  left_join(final_phytos, by = c("Week", "Year"))|>
  mutate(Reservoir = "BVR", Site = 50)

# 4. Interpolate and summarize SFe ----
variables <- c("SFe_mgL")
metals_interpolated <- interpolate_variable(metalswDCM, variables)

metals_interpolated2 <- metals_interpolated |>
  left_join(final_phytos, by = c("Week", "Year")) 
metals_interpolated3 <- metals_interpolated2|>
  mutate(Reservoir = "BVR", Site = 50) |>
  filter(Year > 2014) |>
  group_by(Date) |>
  tidyr::fill(DCM_depth, .direction = "downup") |>
  ungroup()|>
  filter(!is.na(DCM_depth))

variables <- c("SFe_mgL")
metals_weekly_sum <- weekly_sum_variables(metals_interpolated3, variables)

final_metals <- metals_weekly_sum

write.csv(final_metals, "CSVs/final_metals.csv", row.names = FALSE)
