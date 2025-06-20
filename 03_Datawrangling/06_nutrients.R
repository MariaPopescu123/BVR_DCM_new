
#### Nutrients  ####

chemistry_filtered <- chemistry |> #loaded in from DataDownload
  filter(Reservoir == "BVR", Site == 50)|>
  mutate(Date = as_date(DateTime), 
         DateTime = as.POSIXct(DateTime))|>
  filter((hour(DateTime) >= 8), (hour(DateTime) <= 18))|>
  mutate(
    TN_ugL = if_else(Flag_TN_ugL == 9, NA_real_, TN_ugL),
    TP_ugL = if_else(Flag_TP_ugL == 9, NA_real_, TP_ugL),
    NH4_ugL = if_else(Flag_NH4_ugL == 9, NA_real_, NH4_ugL),
    NO3NO2_ugL = if_else(Flag_NO3NO2_ugL == 9, NA_real_, NO3NO2_ugL),
    DIC_mgL = if_else(Flag_DIC_mgL == 9, NA_real_, DIC_mgL)
  ) |>
  select(Date, Depth_m, TN_ugL, TP_ugL, NH4_ugL, NO3NO2_ugL, DIC_mgL)


variables <- c("TN_ugL", "TP_ugL", "NH4_ugL", "NO3NO2_ugL", 
               "DIC_mgL")

#raw data availability 
plot <- data_availability(chemistry_filtered, variables)

ggsave("raw_chem_availability.png", plot = plot, width = 20, height = 15, dpi = 300)

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
  relocate(np_ratio, .before = TN_ugL)

variables <- c("TN_ugL", "TP_ugL", "NH4_ugL", "NO3NO2_ugL", 
               "DIC_mgL", "np_ratio")

chem_weekly_sum <- weekly_sum_variables(chemistry_filtered_np, variables)

#join to frame with correct dates
final_chem <- frame_weeks|>
  left_join(chem_weekly_sum, by = c("Week", "Year"))|>
  select(-WaterLevel_m)

write.csv(final_chem, "CSVs/final_chem.csv", row.names = FALSE)
