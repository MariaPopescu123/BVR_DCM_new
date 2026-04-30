# code to calculate weekly buoyancy_freq at the DCM from 2014-2024
library(data.table)

####Buoyancy Frequency ####
buoyancy_frame <- temp_depths_interp|> #temp_depths_interp was loaded in from 04_photic_temp_thermo
  group_by(Date) |>
  mutate(buoyancy_freq = c(buoyancy.freq(Temp_C, Depth_m), NA))|>#added for padding for the last value
  ungroup() |>
  select(Date, buoyancy_freq, Depth_m)|>
  filter(!is.na(buoyancy_freq))


# Get the unique dates available in buoyancy_frame (one row per date)
buoyancy_dates <- buoyancy_frame |>
  distinct(Date) |>
  arrange(Date)

# For each final_phytos Date, find the nearest buoyancy Date (either direction)
phytos_dt <- as.data.table(final_phytos)
buoy_dates_dt <- as.data.table(buoyancy_dates)

phytos_dt[, Date := as.Date(Date)]
buoy_dates_dt[, Date := as.Date(Date)]
buoy_dates_dt[, buoyancy_Date := Date]   # stash before the rolling join overwrites

setkey(buoy_dates_dt, Date)
setkey(phytos_dt, Date)

phytos_with_buoy_date <- buoy_dates_dt[phytos_dt, on = "Date", roll = "nearest"] |>
  as_tibble()
# phytos_with_buoy_date now has: Year, Week, Date (phyto date), DCM_depth, max_conc, buoyancy_Date

# Pull all buoyancy measurements for the matched dates, then within each
# (phyto Date, buoyancy_Date) pair, pick the depth closest to DCM_depth
final_buoyancy <- phytos_with_buoy_date |>
  left_join(buoyancy_frame, by = c("buoyancy_Date" = "Date")) |>
  group_by(Date) |>     # group by phyto Date — one final row per phyto observation
  slice_min(abs(Depth_m - DCM_depth), n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(date_gap = as.numeric(abs(Date - buoyancy_Date))) |>
  select(Year, Date, DCM_depth, N_at_DCM = buoyancy_freq, date_gap)|>
  filter(date_gap < 7)|>
  select(Date, N_at_DCM)
  
write.csv(final_buoyancy, "CSVs/final_buoyancy.csv", row.names = FALSE)
