# buoyancy_freq
# CHANGE THIS SO IT'S MAXIMUM BUOYANCY FREQUENCY

temp_depths_cleaned <- read.csv("CSVs/temp_depths_cleaned.csv")

####Buoyancy Frequency ####
buoyancy_frame <- temp_depths_cleaned|> #temp_depths_cleaned was loaded in from 04_photic_temp_thermo
  mutate(buoyancy_freq = c(buoyancy.freq(Temp_C, Depth_m), NA))|>#added for padding for the last value
  select(Date, buoyancy_freq, Depth_m)|>
  mutate(Week = week(Date), 
         Year = year(Date))

joined_df <- buoyancy_frame |>
  left_join(final_phytos|> select(Year, Week, DCM_depth), by = c("Week", "Year"))|> #final phytos is loaded from 02_phytos
  filter(!is.na(DCM_depth))

buoyancy_with_dcm <- joined_df |>
  group_by(Week, Year) |>
  mutate(
    depth_diff = abs(Depth_m - DCM_depth),
    N_at_DCM = buoyancy_freq[which.min(depth_diff)]
  ) |>
  ungroup() |>
  select(Week, Year, Depth_m, buoyancy_freq, DCM_depth, N_at_DCM)|>
  select(Week, Year, N_at_DCM)|>
  group_by(Week, Year)|>
  summarise(N_at_DCM = mean(N_at_DCM, na.rm = TRUE))|>
  ungroup()

final_buoyancy <- frame_weeks|>
  left_join(buoyancy_with_dcm, by = c("Week", "Year"))|>
  select(-WaterLevel_m)

write.csv(final_buoyancy, "CSVs/final_buoyancy.csv", row.names = FALSE)


#checking within year and across year variability
var_summary <- final_buoyancy %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(N_at_DCM, na.rm = TRUE),
    sd_within = sd(N_at_DCM, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )

across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across


