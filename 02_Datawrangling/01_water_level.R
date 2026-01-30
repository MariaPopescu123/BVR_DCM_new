#Maria Popescu
#Water level for Beaverdam 

#Updated to incude 2024
#waterlevel data
wtrlvl <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/725/4/43476abff348c81ef37f5803986ee6e1") 

#waterlevel data using the pressure sensor (platform data) https://portal.edirepository.org/nis/codeGeneration?packageId=edi.725.5&statisticalFileType=r
#for past 2020
BVRplatform <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e")

#list of DOY for interpolation purpose
DOY_list <- 32:334  # DOYs from February 1 to November 30
years <- 2015:2024
DOY_year_ref <- expand.grid(Year = years, DOY = DOY_list)|>
  arrange(Year, DOY)

#add water level to data frame to use as the max depth for creating sequence of depths to interpolate each cast to
####Waterlevel####
wtrlvl <- wtrlvl |> 
  mutate(Date = as_date(DateTime))

#Add DOY and Year columns to wtrlvl2, then join with DOY_year_ref
wtrlvl2 <- wtrlvl |>
  mutate(Year = year(Date), DOY = yday(Date))

#join and interpolate WaterLevel_m for each DOY in each year
wtrlvl2_interpolated <- DOY_year_ref %>%
  left_join(wtrlvl2, by = c("Year", "DOY")) %>%
  group_by(Year) %>%
  mutate(
    WaterLevel_m = zoo::na.spline(WaterLevel_m, x = DOY, na.rm = FALSE)
  ) %>%
  filter(Year > 2013) %>%
  arrange(Year, DOY)|>
  select(Year, DOY, WaterLevel_m)

#-----------------------------------------------------------------------------#
# QUICK WL DIAGNOSTIC PLOT
ggplot(wtrlvl2_interpolated, aes(x = DOY, y = WaterLevel_m)) +
  geom_line() +
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "Day of Year", y = "Water Level (m)")
#note that there is something weird happening from 2021-2023 
#if these years aren't being used from this dataset, I'd subset and make a note above so it's clear
#-----------------------------------------------------------------------------#

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
start_date <- as.Date("2014-01-01")
end_date <- as.Date("2024-12-31")

weekly_dates <- data.frame(
  Date_fake = seq.Date(from = start_date, to = end_date, by = "week")
) %>%
  mutate(Year = year(Date_fake),
         Week = week(Date_fake))|>
  mutate(Depth_m = NA)

Depth_fake = seq(0, 13, by = 0.1)

# Expand grid to get each date with each depth
expanded_dates <- expand_grid(Date_fake = weekly_dates$Date_fake, Depth_m = Depth_fake)

# Add year and week info to the expanded data
expanded_dates <- expanded_dates %>%
  mutate(Year = year(Date_fake),
         Week = week(Date_fake),
         DOY = yday(Date_fake), 
         Date = Date_fake)|>
  select(-Date_fake)

water_levelsjoined <- expanded_dates|>
  left_join(BVRplatform2_interpolated, by = c("Year", "DOY"), relationship = "many-to-many")|>
  left_join(wtrlvl2_interpolated, by = c("Year", "DOY"), relationship = "many-to-many")|>
  filter(Year>2013)

water_levelscoalesced<- water_levelsjoined|>
  mutate(WaterLevel_m = coalesce(LvlDepth_m_13,WaterLevel_m))|>
  select(Year, DOY, WaterLevel_m)|>
  group_by(Year, DOY)|>
  summarise(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE), .groups = "drop")

#this has all the depths and all the days
water_level <- expanded_dates|>
  left_join(water_levelscoalesced, by = c("Year", "DOY"))|>
  filter(!is.na(WaterLevel_m))

weekly_water_level <- water_level |> 
  group_by(Year, Week) |> 
  slice(1) |> 
  ungroup()|>
  select(Year, Week, WaterLevel_m)

wtrlvl_by_year <- ggplot(water_level, aes(x = Week, y = WaterLevel_m, color = factor(Year))) +
  geom_line(size = 1) +  # Optional: connect points by year
  labs(
    title = "Water Level 2014-2024",
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

ggsave("Figs/WaterLevel_colored_by_year.png", wtrlvl_by_year, width = 8, height = 5, dpi = 600, bg = "white")


#final csv
write.csv(water_level, "CSVs/water_level.csv", row.names = FALSE)

#additional stats for paper 

#How much did water level drop in 2022 
water2022 <- water_level|>
  filter(Year == 2022)

#checking within year and across year variability
var_summary <- water_level %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(WaterLevel_m, na.rm = TRUE),
    sd_within = sd(WaterLevel_m, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )
across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across

#water level ranges before and after 2022
before2022 <- water_levelscoalesced %>%
  filter(Year < 2022)
after2022 <- water_levelscoalesced %>%
  filter(Year > 2022)
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

