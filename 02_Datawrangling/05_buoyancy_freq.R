# code to calculate weekly buoyancy_freq at the DCM from 2014-2024


####Buoyancy Frequency ####
buoyancy_frame <- temp_depths_interp|> #temp_depths_interp was loaded in from 04_photic_temp_thermo
  group_by(Date) |>
  mutate(buoyancy_freq = c(buoyancy.freq(Temp_C, Depth_m), NA))|>#added for padding for the last value
  ungroup() |>
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

final_buoyancy <- buoyancy_with_dcm

write.csv(final_buoyancy, "CSVs/final_buoyancy.csv", row.names = FALSE)
