#Maria Popescu

#this script:
# 1. calculates schmidt stability for BVR across all years
# 2. provides a diagnostic plot to visualize schmidt stability
# 3. makes a dataframe (final_schmidt) that will be used in final analysis
# 4. calculates statistics included in the paper


#calculating schmidt stability 
BVRbath <- bath|>
  filter(Reservoir == "BVR")|>
  select(Depth_m, SA_m2)


# Prepare data frame
weekly_temp_profiles <- temp_depths_cleaned |>
  mutate(
    Week = week(Date),
    Day = day(Date),
    Year = year(Date),
    RoundedDepth = round(Depth_m)  # Round to the nearest meter
  ) |>
  filter(DOY >= 133, DOY <= 285)|>
  group_by(Date, RoundedDepth) |>
  slice_min(abs(Depth_m - RoundedDepth), with_ties = FALSE) |>  # Select depth closest to that rounded meter
  ungroup()|>
  left_join(weekly_water_level, by = c("Week", "Year"), relationship = "many-to-many")|>
  mutate(Date = as.Date(Date))


# Need to make correction for different water levels and bathymetry.
# Currently the Bath frame is 0-14. 
# For each Date the max depth needs to be subtracted from 14. ex: BathDiff = 14 - max(Depth_m) [example: 2]
# Then make a column called BathAdj = Roundeddepth + BathDiff
# Then left join BVRbath to BathAdj

new_bath <- weekly_temp_profiles|>
  group_by(Date)|>
  mutate(BathDiff = 14 - max(RoundedDepth), 
         BathAdj = RoundedDepth + BathDiff)|>
  ungroup()|>
  left_join(BVRbath, by = c("BathAdj" = "Depth_m"))|>
  select(-RoundedDepth, -WaterLevel_m, -BathDiff, -BathAdj)

# Now we have different bathymetry for each Date
# Let's calculate Schmidt stability:
# Higher Schmidt stability → more energy needed to mix → stronger stratification
# Lower Schmidt stability → less energy needed → weaker or no stratification

schmidt_frame <- new_bath|>
  group_by(Date)|>
  filter(!is.na(Temp_C))|>
  mutate(schmidt_stability = schmidt.stability(Temp_C, Depth_m, SA_m2, Depth_m))|>
  ungroup()|>
  group_by(Year, Week)|>
  summarise(schmidt_stability = mean(schmidt_stability))|>
  ungroup()

final_schmidt <- frame_weeks|>
  left_join(schmidt_frame, by = c("Week", "Year"))

#diagnostic plot to visualize schmidt stability
schmidt_plot <- ggplot(final_schmidt, aes(x = Week, y = schmidt_stability, color = factor(Year), group = Year)) +
  geom_point() +
  geom_line() +  
  labs(
    x = "Week of Year",
    y = "Schmidt Stability (J/m²)",
    color = "Year"
  ) +
  theme_minimal()

ggsave(filename = "Figs/schmidt.png", plot = schmidt_plot, width = 10, height = 6, dpi = 300)

#this is the final dataframe that will be used in RF analysis
write.csv(final_schmidt, here::here("CSVs", "final_schmidt.csv"), row.names = FALSE)

#checking within year and across year variability
var_summary <- final_schmidt %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(schmidt_stability, na.rm = TRUE),
    sd_within = sd(schmidt_stability, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )

across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across



#Additional Stats

#checking within year and across year variability
var_summary <- final_schmidt %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(schmidt_stability, na.rm = TRUE),
    sd_within = sd(schmidt_stability, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )
across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across

print(var_summary)
print(variability_ratio)

