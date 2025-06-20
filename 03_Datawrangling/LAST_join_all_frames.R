#joining all the dataframes together from the data wrangling file

#read in csvs
frame_weeks <- read.csv("CSVs/frame_weeks.csv")
final_phytos <- read.csv("CSVs/final_phytos.csv")
final_metals <- read.csv("CSVs/final_metals.csv")
final_photic_thermo <- read.csv("CSVs/final_photic_thermo.csv")
final_buoyancy <- read.csv("CSVs/final_chem.csv")
final_chem <- read.csv("CSVs/final_chem.csv")
final_schmidt <- read.csv("CSVs/final_schmidt.csv")


full_weekly_data <- frame_weeks %>%
  left_join(final_phytos, by = c("Year", "Week")) %>%
  left_join(final_metals, by = c("Year", "Week")) %>%
  left_join(final_photic_thermo, by = c("Year", "Week")) %>%
  left_join(final_buoyancy, by = c("Year", "Week")) %>%
  left_join(final_chem, by = c("Year", "Week")) %>%
  left_join(final_schmidt, by = c("Year", "Week"))


full_weekly_data <- full_weekly_data |>
  mutate(Date = Date.x)|>
  select(-Year, -Date.y, -Secchi_m, -sec_K_d)|>
  relocate(Date, .before = "Week")

write.csv(full_weekly_data, "CSVs/full_weekly_data.csv", row.names = FALSE)
