#Maria Popescu

#This script:
# 1. Cleans the chemistry data
# 2. Interpolates and summarizes chemistry data for final data frame to be used in RF analysis
# 3. Cleans the SFe data (soluble iron)
# 4. Interpolate and summarizes SFe (metals) data for final dataframe to be used in RF analysis
#
# Inputs expected in the workspace from prior scripts:
# - chemistry, metalsdf (loaded in 01_DataDownload.R)
# - final_phytos (created in 02_Phytos_dataframe.R)
# - helper functions interpolate_variable(), date_sum_variables(), data_availability()
# Outputs written: CSVs/final_chem.csv and CSVs/final_metals.csv

required_objects <- c("chemistry", "metalsdf", "final_phytos")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for 06_nutrients.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 01_DataDownload.R and 02_Phytos_dataframe.R first.")
}

# 1. Clean chemistry ####
chemistry_filtered <- chemistry |>
  filter(Reservoir == "BVR", Site == 50)|>
  mutate(Date = as_date(DateTime),
         DateTime = as.POSIXct(DateTime))|>
  select(Date, Depth_m, NH4_ugL, SRP_ugL, NO3NO2_ugL)|>
  mutate(Year = year(Date))

variables <- c("SRP_ugL", "NH4_ugL", "NO3NO2_ugL")
data_availability(chemistry_filtered, variables)

library(data.table)
#rolling nearest chem observation to phyto data
phytos_dt <- as.data.table(final_phytos)
chem_dt   <- as.data.table(chemistry_filtered)

phytos_dt[, Date := as.Date(Date)]
chem_dt[,   Date := as.Date(Date)]

# Stash chem's original date so we can compute the gap after the join
chem_dt[, chem_Date := Date]

setkey(chem_dt, Date)
setkey(phytos_dt, Date)

# Rolling join: nearest chem row to each phyto row, in either direction
chem_w_DCM <- chem_dt[phytos_dt, on = "Date", roll = "nearest"]

# Filter to matches within 4 days, then clean up
chem_w_DCM <- chem_w_DCM |>
  as_tibble() |>
  mutate(date_gap = as.numeric(abs(Date - chem_Date))) |>
  filter(date_gap <= 4) |>
  select(-chem_Date, -date_gap) |>
  mutate(Reservoir = "BVR", Site = 50)

# 2. Interpolate and summarize ----
variables <- c("SRP_ugL", "NH4_ugL", "NO3NO2_ugL")
chem_interpolated <- interpolate_variable(chem_w_DCM, variables)

chem_interpolated2 <- chem_interpolated |>
  left_join(final_phytos, by = c("Year","Date"))
chem_interpolated3 <- chem_interpolated2|>
  mutate(Reservoir = "BVR", Site = 50) |>
  filter(Year > 2014) |>
  group_by(Date) |>
  tidyr::fill(DCM_depth, .direction = "downup") |>
  ungroup()

#per-Date summaries on phyto cast Dates
final_chem <- chem_interpolated3 |>
  filter(Date %in% final_phytos$Date) |>
  date_sum_variables(variables)

write.csv(final_chem, "CSVs/final_chem.csv", row.names = FALSE)

# 3. Prepare the SFe data ----------------
metals_updated <- metalsdf |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Site == 50, Reservoir == "BVR") |>
  select(Date, Depth_m, SFe_mgL)|>
  mutate(Year = year(Date))

#rolling nearest metals observation to phyto data (mirrors the chem block above)
metals_dt <- as.data.table(metals_updated)
metals_dt[, Date := as.Date(Date)]

# Stash metals' original date so we can compute the gap after the join
metals_dt[, metals_Date := Date]

setkey(metals_dt, Date)
# phytos_dt was created and keyed on Date earlier in the chem section

# Rolling join: nearest metals row to each phyto row, in either direction
metalswDCM <- metals_dt[phytos_dt, on = "Date", roll = "nearest"]

# Filter to matches within 4 days, then clean up
metalswDCM <- metalswDCM |>
  as_tibble() |>
  mutate(date_gap = as.numeric(abs(Date - metals_Date))) |>
  filter(date_gap <= 4) |>
  select(-metals_Date, -date_gap) |>
  mutate(Reservoir = "BVR", Site = 50)

# 4. Interpolate and summarize SFe ----
variables <- c("SFe_mgL")
metals_interpolated <- interpolate_variable(metalswDCM, variables)

metals_interpolated2 <- metals_interpolated |>
  left_join(final_phytos, by = c("Year","Date"))
metals_interpolated3 <- metals_interpolated2|>
  mutate(Reservoir = "BVR", Site = 50) |>
  filter(Year > 2014) |>
  group_by(Date) |>
  tidyr::fill(DCM_depth, .direction = "downup") |>
  ungroup()

#per-Date summaries on phyto cast Dates
final_metals <- metals_interpolated3 |>
  filter(Date %in% final_phytos$Date) |>
  date_sum_variables(variables)

write.csv(final_metals, "CSVs/final_metals.csv", row.names = FALSE)
