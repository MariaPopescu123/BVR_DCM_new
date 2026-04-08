#joining all the dataframes together from the data wrangling file
#
# Inputs:
# - Weekly CSV outputs from previous wrangling steps (01-08), especially frame_weeks.csv
#   created in 02_Phytos_dataframe.R.
#
# Outputs:
# - In-memory data frame: full_weekly_data.
# - CSV: CSVs/full_weekly_data.csv.
#
# Dependencies:
# - Run after scripts 01-08 have finished successfully.

required_csvs <- c(
  "CSVs/frame_weeks.csv",
  "CSVs/final_phytos.csv",
  "CSVs/weekly_water_level.csv",
  "CSVs/final_metals.csv",
  "CSVs/final_photic_thermo.csv",
  "CSVs/final_buoyancy.csv",
  "CSVs/final_chem.csv",
  "CSVs/final_schmidt.csv",
  "CSVs/final_metdata.csv"
)

missing_csvs <- required_csvs[!file.exists(required_csvs)]
if (length(missing_csvs) > 0) {
  stop("Missing required CSV inputs for 09_join_all_frames.R: ",
       paste(missing_csvs, collapse = ", "),
       ". Run upstream scripts 01-08 first.")
}

#read in csvs
frame_weeks <- read.csv("CSVs/frame_weeks.csv")
final_phytos <- read.csv("CSVs/final_phytos.csv")
weekly_water_level <- read.csv("CSVs/weekly_water_level.csv")
final_metals <- read.csv("CSVs/final_metals.csv")|>
  select(-Date)
final_photic_thermo <- read.csv("CSVs/final_photic_thermo.csv")|>
  select(-Date)
final_buoyancy <- read.csv("CSVs/final_buoyancy.csv")
final_chem <- read.csv("CSVs/final_chem.csv")|>
  select(-Date)
final_schmidt <- read.csv("CSVs/final_schmidt.csv")
final_metdata <- read.csv("CSVs/final_metdata.csv")

full_weekly_data1 <- frame_weeks %>%
  left_join(final_phytos, by = c("Year", "Week")) %>%
  left_join(weekly_water_level, by = c("Year", "Week"))%>%
  left_join(final_metals, by = c("Year", "Week")) %>%
  left_join(final_photic_thermo, by = c("Year", "Week")) %>%
  left_join(final_buoyancy, by = c("Year", "Week")) %>%
  left_join(final_chem, by = c("Year", "Week")) %>%
  left_join(final_schmidt, by = c("Year", "Week"))|>
  left_join(final_metdata, by = c("Year", "Week"))

full_weekly_data <- full_weekly_data1 |>
  mutate(DOY = yday(Date))|>
  filter(DOY >= 133, DOY <= 286)|>
  select(-DOY)|>
  select(-Year, -Secchi_m, -sec_K_d) |>
  relocate(Date, .before = "Week") |>
  relocate(WaterLevel_m, .before = "PZ") |>
  rename(
    AirTemp_Avg   = weekly_airtempavg,
    WindSpeed_Avg = WindSpeed_Weekly_Average_m_s,
    Precip_Weekly    = precip_weekly
  ) |>
  relocate(wind_lag1, wind_lag2, .after = WindSpeed_Avg) |>
  relocate(precip_lag1, precip_lag2, .after = Precip_Weekly) |>
  relocate(airtemp_lag1, airtemp_lag2, .after = AirTemp_Avg)|>
  filter(year(Date)>2014 & year(Date)<2025)


write.csv(full_weekly_data, "CSVs/full_weekly_data.csv", row.names = FALSE)

