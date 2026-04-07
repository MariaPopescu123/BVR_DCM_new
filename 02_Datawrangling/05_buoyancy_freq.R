# Maria Popescu
#
# This script calculates weekly buoyancy frequency at DCM depth.
# Inputs expected in the workspace from prior steps:
# - temp_depths_interp (from 04_photic_temp_thermo.R)
# - final_phytos with DCM_depth by Year/Week (from 02_Phytos_dataframe.R)
# Output file will be written: CSVs/final_buoyancy.csv

required_objects <- c("temp_depths_interp", "final_phytos")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for 05_buoyancy_freq.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 02_Phytos_dataframe.R and 04_photic_temp_thermo.R first.")
}


####Buoyancy Frequency ####
buoyancy_frame <- temp_depths_interp|>
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
