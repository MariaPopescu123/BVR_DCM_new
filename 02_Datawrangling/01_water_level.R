#Maria Popescu

#This script compiles Water level for Beaverdam using sensor data
#and historical observation records
#
# Inputs:
# - wtrlvl and BVRplatform objects loaded in 01_DataDownload.R.
#
# Outputs:
# - In-memory data frames: water_level, weekly_water_level.
# - CSVs: CSVs/water_level.csv and CSVs/weekly_water_level.csv.
#
# Dependencies:
# - Run after 01_DataDownload.R in the same R session.

required_objects <- c("wtrlvl", "BVRplatform")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for 01_water_level.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 01_DataDownload.R first.")
}

#list of DOY for interpolation purpose
DOY_list <- 32:334  # DOYs from February 1 to November 30
years <- 2015:2024 #we are looking within this range
DOY_year_ref <- expand.grid(Year = years, DOY = DOY_list)|>
  arrange(Year, DOY)

#add water level to data frame to use as the max depth for creating
#sequence of depths to interpolate each cast to

####Waterlevel####
wtrlvl <- wtrlvl |> 
  mutate(Date = as_date(DateTime))

#Add DOY and Year columns to wtrlvl2, then join with DOY_year_ref
wtrlvl2 <- wtrlvl |>
  mutate(Year = year(Date), DOY = yday(Date))

#join and interpolate WaterLevel_m for each DOY in each year
wtrlvl2_interpolated <- DOY_year_ref %>%
  left_join(wtrlvl2, by = c("Year", "DOY")) %>%
  group_by(Year, DOY) %>%
  summarise(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(
    WaterLevel_m = {
      if (all(is.na(WaterLevel_m))) {
        WaterLevel_m   # leave as all NA for that year
      } else {
        ifelse(
          DOY < min(DOY[!is.na(WaterLevel_m)]) |
            DOY > max(DOY[!is.na(WaterLevel_m)]),
          NA_real_,
          zoo::na.spline(WaterLevel_m, x = DOY, na.rm = FALSE)
        )
      }
    }
  ) %>%
  filter(Year > 2014) %>% 
  arrange(Year, DOY) %>%
  select(Year, DOY, WaterLevel_m)
#ignore the years past 2020 for now they will be fixed
#with the next code (not enough data for interpolation), but still using earlier values of 2020


#now for past 2020 
#Add DOY and Year columns to wtrlvl2, then join with DOY_year_ref
BVRplatform2 <- BVRplatform |>
  filter(Flag_LvlPressure_psi_13 != 5)|>#filter flags, questionable value but left in the dataset
  mutate(Date = as.Date(DateTime))|>
  mutate(Year = year(Date), DOY = yday(Date))

#join and interpolate WaterLevel_m for each DOY in each year
BVRplatform2_interpolated <- DOY_year_ref |>
  left_join(BVRplatform2, by = c("Year" = "Year", "DOY" = "DOY")) |>
  group_by(Year) |>
  mutate(
    LvlDepth_m_13 = zoo::na.spline(LvlDepth_m_13, x = DOY, na.rm = FALSE)
  )|>
  filter(Year > 2019, Site == 50)|>
  arrange(Year, DOY)|>
  select(Year, DOY, DateTime, LvlDepth_m_13)


#make data frame for waterlevels
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2024-12-31")

daily_dates <- data.frame(
  Date_fake = seq.Date(from = start_date, to = end_date, by = "day")
) %>%
  mutate(Year = year(Date_fake))|>
  mutate(Depth_m = NA)

Depth_fake = seq(0, 13, by = 0.1)

# Expand grid to get each date with each depth
expanded_dates <- expand_grid(Date_fake = daily_dates$Date_fake, Depth_m = Depth_fake)

# Add year info to the expanded data
expanded_dates <- expanded_dates %>%
  mutate(Year = year(Date_fake),
         DOY = yday(Date_fake),
         Date = Date_fake)|>
  select(-Date_fake)

water_levelsjoined <- expanded_dates|>
  left_join(BVRplatform2_interpolated, by = c("Year", "DOY"), relationship = "many-to-many")|>
  left_join(wtrlvl2_interpolated, by = c("Year", "DOY"), relationship = "many-to-many")|>
  filter(Year>2014)

water_levelscoalesced<- water_levelsjoined|>
  mutate(WaterLevel_m = coalesce(LvlDepth_m_13,WaterLevel_m))|>
  select(Year, DOY, WaterLevel_m)|>
  group_by(Year, DOY)|>
  summarise(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE), .groups = "drop")

#final data frame for analysis: water_level
#this has all the depths and all the days so I can still use this for depth profile
#calculations later
water_level <- expanded_dates|>
  left_join(water_levelscoalesced, by = c("Year", "DOY"))|>
  filter(!is.na(WaterLevel_m))

#not used in manuscript, just used for diagnostics
wtrlvl_by_year <- ggplot(water_levelscoalesced, aes(x = DOY, y = WaterLevel_m, color = factor(Year))) +
  geom_line(size = 1) +  
  labs(
    title = "Water Level 2015-2024",
    x = "Week of Year",
    y = "Water Level (m)",
    color = "Year"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 25, hjust = .5),  # Left-align title
    plot.title.position = "plot",  # Positions the title relative to the entire plot area
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 18),     # <-- Increase legend text size
    legend.title = element_text(size = 18),
    axis.line = element_line(color = "black"),  # both x and y axes
  )

print(wtrlvl_by_year)

#final csvs
write.csv(water_level, "CSVs/water_level.csv", row.names = FALSE)


#additional stats for paper
#water level ranges before and after 2022
wl_by_date <- water_level |> distinct(Date, Year, WaterLevel_m)
before2022 <- wl_by_date %>%
  filter(Year < 2022)
after2022 <- wl_by_date %>%
  filter(Year >= 2022)
before2022 %>%
  summarise(
    min_waterlevel = min(WaterLevel_m, na.rm = TRUE),
    max_waterlevel = max(WaterLevel_m, na.rm = TRUE)
  )
after2022 %>%
  summarise(
    min_waterlevel = min(WaterLevel_m, na.rm = TRUE),
    max_waterlevel = max(WaterLevel_m, na.rm = TRUE)
  )

