#joining all the dataframes together from the data wrangling file
#
# Inputs:
# - Date-keyed CSV outputs from previous wrangling steps (01-08), with
#   final_phytos.csv (created in 02_Phytos_dataframe.R) as the spine.
#
# Outputs:
# - In-memory data frame: full_data.
# - CSV: CSVs/full_data.csv.
#
# Dependencies:
# - Run after scripts 01-08 have finished successfully.

required_csvs <- c(
  "CSVs/final_phytos.csv",
  "CSVs/water_level.csv",
  "CSVs/final_metals.csv",
  "CSVs/final_photic_thermo.csv",
  "CSVs/final_buoyancy.csv",
  "CSVs/final_chem.csv",
  "CSVs/final_schmidt.csv",
  "CSVs/final_metdata.csv",
  "CSVs/temp_at_DCM.csv"
)

missing_csvs <- required_csvs[!file.exists(required_csvs)]
if (length(missing_csvs) > 0) {
  stop("Missing required CSV inputs for 09_join_all_frames.R: ",
       paste(missing_csvs, collapse = ", "),
       ". Run upstream scripts 01-08 first.")
}

#read in csvs (all keyed on Date)
final_phytos <- read_csv("CSVs/final_phytos.csv")

water_level <- read_csv("CSVs/water_level.csv")|>
  group_by(Date)|>
  summarize(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE)) |>
  ungroup()
final_metals <- read_csv("CSVs/final_metals.csv")|>
  mutate(Date = as_date(Date))
final_photic_thermo <- read_csv("CSVs/final_photic_thermo.csv")|>
  mutate(Date = as_date(Date))
final_buoyancy <- read_csv("CSVs/final_buoyancy.csv")|>
  mutate(Date = as_date(Date))
final_chem <- read_csv("CSVs/final_chem.csv")|>
  mutate(Date = as_date(Date))
final_schmidt <- read_csv("CSVs/final_schmidt.csv")|>
  mutate(Date = as_date(Date))
final_metdata <- read_csv("CSVs/final_metdata.csv")|>
  mutate(Date = as_date(Date))
temp_at_DCM <- read_csv("CSVs/temp_at_DCM.csv")|>
  mutate(Date = as_date(Date))

#not joining right
full_data1 <- final_phytos %>%
  left_join(water_level,by = "Date") %>%
  left_join(final_metals,by = "Date") %>%
  left_join(final_photic_thermo,by = "Date") %>%
  left_join(final_buoyancy,by = "Date") %>%
  left_join(final_chem,by = "Date") %>%
  left_join(final_schmidt,by = "Date") %>%
  left_join(final_metdata,by = "Date") %>%
  left_join(temp_at_DCM,by = "Date")

full_data <- full_data1 |>
  mutate(DOY = yday(Date))|>
  filter(DOY >= 133, DOY <= 286)|>
  select(-DOY)|>
  mutate(Year = Year.x,
         temp_at_DCM = temp_at_DCM.x,
         WaterLevel_m = WaterLevel_m.x) |>
  relocate(Date) |>
  relocate(WaterLevel_m, .before = "PZ") |>
  filter(year(Date)>2014 & year(Date)<2025)


write.csv(full_data, "CSVs/full_data.csv", row.names = FALSE)

