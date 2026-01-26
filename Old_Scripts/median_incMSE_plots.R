#---PZ and PZ prop median----

#annual median values of PZ
median_PZ <- read.csv("CSVs/final_photic_thermo.csv")|>
  group_by(Year)|>
  summarise(medianPZ = median(PZ, na.rm = TRUE), meanPZ = mean(PZ, na.rm = TRUE))|>
  ungroup()

annual_PZ_incMSE <- depth_jackknife$summary|>
  filter(Variable == "PZ")|>
  left_join(median_PZ, by = c("Year"))

ggplot(annual_PZ_incMSE, aes(x = mean_incMSE)) +
  geom_line(aes(y = meanPZ, color = "Mean PZ")) +
  geom_line(aes(y = medianPZ, color = "Median PZ")) +
  labs(
    x = "Mean incMSE",
    y = "Photic Zone Depth (m)",
    color = "Variable"
  ) +
  scale_color_manual(
    values = c("Mean PZ" = "firebrick", "Median PZ" = "steelblue")
  ) +
  theme_minimal()



#---Annual median and mean values of PZ_prop---
median_PZ_prop <- read.csv("CSVs/final_photic_thermo.csv") |>
  group_by(Year) |>
  summarise(
    medianPZ_prop = median(PZ_prop, na.rm = TRUE),
    meanPZ_prop   = mean(PZ_prop, na.rm = TRUE)
  ) |>
  ungroup()

# --- Merge with incMSE summary for PZ_prop ---
annual_PZ_prop_incMSE <- depth_jackknife$summary |>
  filter(Variable == "PZ_prop") |>
  left_join(median_PZ_prop, by = c("Year"))

# --- Plot mean and median PZ_prop vs mean_incMSE ---
ggplot(annual_PZ_prop_incMSE, aes(x = mean_incMSE)) +
  geom_line(aes(y = meanPZ_prop, color = "Mean PZ_prop")) +
  geom_line(aes(y = medianPZ_prop, color = "Median PZ_prop")) +
  labs(
    x = "Mean incMSE",
    y = "Photic Zone (Proportional)",
    color = "Variable"
  ) +
  scale_color_manual(
    values = c("Mean PZ_prop" = "firebrick", "Median PZ_prop" = "steelblue"),
    labels = c("Mean PZ_prop", "Median PZ_prop")
  ) +
  theme_minimal()

#---WaterLevel_m------####

# --- Annual median and mean values of WaterLevel_m ---
median_WaterLevel_m <- read.csv("CSVs/water_level.csv") |>
  group_by(Year) |>
  summarise(
    medianWaterLevel_m = median(WaterLevel_m, na.rm = TRUE),
    meanWaterLevel_m   = mean(WaterLevel_m, na.rm = TRUE)
  ) |>
  ungroup()

# --- Merge with incMSE summary for WaterLevel_m ---
annual_WaterLevel_incMSE <- depth_jackknife$summary |>
  filter(Variable == "WaterLevel_m") |>
  left_join(median_WaterLevel_m, by = c("Year"))

# --- Plot mean and median WaterLevel_m vs mean_incMSE ---
ggplot(annual_WaterLevel_incMSE, aes(x = mean_incMSE)) +
  geom_line(aes(y = meanWaterLevel_m, color = "Mean Water Level")) +
  geom_line(aes(y = medianWaterLevel_m, color = "Median Water Level")) +
  labs(
    x = "Mean incMSE",
    y = "Water Level (m)",
    color = "Variable"
  ) +
  scale_color_manual(
    values = c("Mean Water Level" = "firebrick", "Median Water Level" = "steelblue"),
    labels = c("Mean Water Level", "Median Water Level")
  ) +
  theme_minimal()

#---Schmidt_stability------####

# --- Annual median and mean values of schmidt_stability ---
median_schmidt_stability <- read.csv("CSVs/final_schmidt.csv") |>
  group_by(Year) |>
  summarise(
    medianschmidt_stability = median(schmidt_stability, na.rm = TRUE),
    meanschmidt_stability   = mean(schmidt_stability, na.rm = TRUE)
  ) |>
  ungroup()

# --- Merge with incMSE summary for schmidt_stability ---
annual_schmidt_incMSE <- depth_jackknife$summary |>
  filter(Variable == "schmidt_stability") |>
  left_join(median_schmidt_stability, by = c("Year"))

# --- Plot mean and median schmidt_stability vs mean_incMSE ---
ggplot(annual_schmidt_incMSE, aes(x = mean_incMSE)) +
  geom_line(aes(y = meanschmidt_stability, color = "Mean schmidt Stability")) +
  geom_line(aes(y = medianschmidt_stability, color = "Median schmidt Stability")) +
  labs(
    x = "Mean incMSE",
    y = "schmidt Stability (J/m²)",
    color = "Variable"
  ) +
  scale_color_manual(
    values = c("Mean schmidt Stability" = "firebrick",
               "Median schmidt Stability" = "steelblue"),
    labels = c("Mean schmidt Stability", "Median schmidt Stability")
  ) +
  theme_minimal()

#---depth_NH4_ugL_max------####

# --- Annual median and mean values of depth_NH4_ugL_max ---
median_depth_NH4 <- read.csv("CSVs/final_chem.csv") |>
  group_by(Year) |>
  summarise(
    mediandepth_NH4_ugL_max = median(depth_NH4_ugL_max, na.rm = TRUE),
    meandepth_NH4_ugL_max   = mean(depth_NH4_ugL_max, na.rm = TRUE)
  ) |>
  ungroup()

# --- Merge with incMSE summary for depth_NH4_ugL_max ---
annual_depth_NH4_incMSE <- depth_jackknife$summary |>s
  filter(Variable == "depth_NH4_ugL_max") |>
  left_join(median_depth_NH4, by = "Year")

# --- Plot mean and median depth_NH4_ugL_max vs mean_incMSE ---
ggplot(annual_depth_NH4_incMSE, aes(x = mean_incMSE)) +
  geom_line(aes(y = meandepth_NH4_ugL_max,   color = "Mean NH4")) +
  geom_line(aes(y = mediandepth_NH4_ugL_max, color = "Median NH4")) +
  labs(
    x = "Mean incMSE",
    y = "NH4 (µg/L)",
    color = "Variable"
  ) +
  scale_color_manual(
    values = c("Mean NH4" = "firebrick", "Median NH4" = "steelblue")
  ) +
  theme_minimal()
