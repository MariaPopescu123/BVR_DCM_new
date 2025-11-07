####visualize chosen variables####

chosen_variables <- full_weekly_data |>
  select(
    Date,
    max_conc,                     # Maximum phytoplankton concentration (µg/L)
    DCM_depth,                    # Deep Chlorophyll Maximum depth (m)
    WaterLevel_m,                 # Reservoir water level (m)
    PZ,                           # Photic zone depth (m)
    schmidt_stability,            # Schmidt stability (J/m²)
    thermocline_depth,            # Thermocline depth (m)
    TFe_mgL_max_val,              # Max total Fe (mg/L)
    SRP_ugL_max_val,              # Max soluble reactive phosphorus (µg/L)
    NH4_ugL_max_val,              # Max ammonium (µg/L)
    depth_SFe_mgL_max,            # Depth of max soluble Fe (m)
    depth_SRP_ugL_max,            # Depth of max SRP (m)
    depth_NH4_ugL_max,            # Depth of max ammonium (m)
    depth_np_ratio_max,           # Depth of max N:P ratio
    precip_lag1,                  # 1-week lag precipitation (mm)
    airtemp_lag2,                 # 2-week lag air temperature (°C)
    wind_lag1                     # 1-week lag wind speed (m/s)
  )

variable_labels <- c(
  max_conc = "Max Total Phytoplankton (µg/L)",
  DCM_depth = "DCM Depth (m)",
  WaterLevel_m = "Water Level (m)",
  PZ = "Photic Zone Depth (m)",
  schmidt_stability = "Schmidt Stability (J/m²)",
  thermocline_depth = "Thermocline Depth (m)",
  TFe_mgL_max_val = "Max Total Fe (mg/L)",
  SRP_ugL_max_val = "Max SRP (µg/L)",
  NH4_ugL_max_val = "Max NH₄⁺ (µg/L)",
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  depth_NH4_ugL_max = "Depth of Max NH₄⁺ (m)",
  depth_np_ratio_max = "Depth of Max N:P Ratio",
  precip_lag1 = "Precipitation (Lag 1 wk)",
  airtemp_lag2 = "Air Temperature (Lag 2 wk)",
  wind_lag1 = "Wind Speed (Lag 1 wk)"
)



# ---All variable visualization plot-----
variable_labels2 <- c(
  WaterLevel_m        = "Water Level (m)",
  PZ                  = "Photic Zone Depth (m)",
  schmidt_stability   = "Schmidt Stability (J/m²)",
  thermocline_depth   = "Thermocline Depth (m)",
  TFe_mgL_max_val     = "Max Total Fe (mg/L)",
  SRP_ugL_max_val     = "Max SRP (µg/L)",
  NH4_ugL_max_val     = "Max NH\u2084\u207A (µg/L)",
  depth_SFe_mgL_max   = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max   = "Depth of Max SRP (m)",
  depth_NH4_ugL_max   = "Depth of Max NH\u2084\u207A (m)",
  Precip_Weekly       = "Weekly Precipitation (mm)",
  AirTemp_Avg         = "Air Temperature (°C)",
  WindSpeed_Avg       = "Wind Speed (m/s)",
  DCM_depth           = "DCM Depth (m)",
  max_conc            = "Max Chlorophyll-a (µg/L)"
)

variables_plot <- full_weekly_data |>
  dplyr::select(
    Date,
    DCM_depth,
    max_conc,
    WaterLevel_m,
    PZ,
    schmidt_stability,
    thermocline_depth,
    TFe_mgL_max_val,
    SRP_ugL_max_val,
    NH4_ugL_max_val,
    depth_SFe_mgL_max,
    depth_SRP_ugL_max,
    depth_NH4_ugL_max,
    Precip_Weekly,
    AirTemp_Avg,
    WindSpeed_Avg
  ) |>
  dplyr::filter(lubridate::year(Date) > 2014)

color_df <- ggplot_build(wtrlvl_by_year)$data[[1]] |>
  dplyr::select(group, colour) |>
  dplyr::distinct() |>
  dplyr::arrange(group)

year_levels <- as.character(2015:2024)
year_colors <- setNames(
  color_df$colour[seq_along(year_levels) %% nrow(color_df) + 1],
  year_levels
)

variables_plot <- variables_plot |>
  dplyr::mutate(
    Date = as.Date(Date),
    Year_num = lubridate::year(Date),
    Year = factor(as.character(Year_num), levels = year_levels)
  )

facet_vars <- c(
  "WaterLevel_m", "PZ", "schmidt_stability", "thermocline_depth",
  "TFe_mgL_max_val", "SRP_ugL_max_val", "NH4_ugL_max_val",
  "depth_SFe_mgL_max", "depth_SRP_ugL_max", "depth_NH4_ugL_max",
  "Precip_Weekly", "AirTemp_Avg", "WindSpeed_Avg",
  "DCM_depth", "max_conc"
)

variables_plot_long <- variables_plot |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(facet_vars),
    names_to  = "Variable",
    values_to = "Value"
  ) |>
  dplyr::mutate(
    Variable_f = factor(
      Variable,
      levels = names(variable_labels2),
      labels = unname(variable_labels2)
    )
  )

variables_plot_long$Variable_f <- factor(
  variables_plot_long$Variable_f,
  levels = unname(variable_labels2)
)

all_variables_visualize <- ggplot(
  variables_plot_long,
  aes(x = Date, y = Value, color = Year, group = Year)
) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  facet_wrap(~ Variable_f, scales = "free_y", ncol = 5, nrow = 3) +
  scale_color_manual(
    values = year_colors,
    name   = "Year",
    limits = year_levels,
    drop   = FALSE
  ) +
  scale_x_date(
    breaks = seq(as.Date("2015-01-01"), as.Date("2024-12-31"), by = "1 year"),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Environmental Variables and Depth Metrics (2015–2024)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background   = element_rect(fill = "grey92", color = "grey70"),
    strip.text         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x       = element_text(margin = ggplot2::margin(t = 6)),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    plot.title         = element_text(face = "bold", hjust = 0.5)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(alpha = 1)))

ggsave(
  filename = "Figs/all_variables_by_yearly_color.png",
  plot     = all_variables_visualize,
  width    = 20, height = 12, units = "in", dpi = 300, bg = "white"
)

#----overlapping years----
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

variable_labels2 <- c(
  WaterLevel_m        = "Water Level (m)",
  PZ                  = "Photic Zone Depth (m)",
  schmidt_stability   = "Schmidt Stability (J/m²)",
  thermocline_depth   = "Thermocline Depth (m)",
  TFe_mgL_max_val     = "Max Total Fe (mg/L)",
  SRP_ugL_max_val     = "Max SRP (µg/L)",
  NH4_ugL_max_val     = "Max NH\u2084\u207A (µg/L)",
  depth_SFe_mgL_max   = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max   = "Depth of Max SRP (m)",
  depth_NH4_ugL_max   = "Depth of Max NH\u2084\u207A (m)",
  Precip_Weekly       = "Weekly Precipitation (mm)",
  AirTemp_Avg         = "Air Temperature (°C)",
  WindSpeed_Avg       = "Wind Speed (m/s)",
  DCM_depth           = "DCM Depth (m)",
  max_conc            = "Max Chlorophyll-a (µg/L)"
)

variables_plot <- full_weekly_data %>%
  select(
    Date, DCM_depth, max_conc,
    WaterLevel_m, PZ, schmidt_stability, thermocline_depth,
    TFe_mgL_max_val, SRP_ugL_max_val, NH4_ugL_max_val,
    depth_SFe_mgL_max, depth_SRP_ugL_max, depth_NH4_ugL_max,
    Precip_Weekly, AirTemp_Avg, WindSpeed_Avg
  ) %>%
  filter(year(Date) > 2014) %>%
  mutate(
    Date   = as.Date(Date),
    Year   = factor(year(Date)),
    DOY    = yday(Date),
    Leap   = leap_year(year(Date)),
    DOY_season = if_else(Leap & DOY > 59, DOY - 1L, DOY)
  ) %>%
  filter(DOY_season > 133, DOY_season < 285)   # ← restrict to May–Oct window

facet_vars <- c(
  "WaterLevel_m","PZ","schmidt_stability","thermocline_depth",
  "TFe_mgL_max_val","SRP_ugL_max_val","NH4_ugL_max_val",
  "depth_SFe_mgL_max","depth_SRP_ugL_max","depth_NH4_ugL_max",
  "Precip_Weekly","AirTemp_Avg","WindSpeed_Avg",
  "DCM_depth","max_conc"
)

variables_plot_long <- variables_plot %>%
  pivot_longer(cols = all_of(facet_vars),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable_f = factor(
    Variable,
    levels = names(variable_labels2),
    labels = unname(variable_labels2)
  ))

month_starts <- yday(as.Date(paste0("2001-", 5:10, "-01")))  # May–Oct
month_labs   <- month.abb[5:10]

ggplot(variables_plot_long,
       aes(x = DOY_season, y = Value, color = Year, group = Year)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  facet_wrap(~ Variable_f, scales = "free_y", ncol = 5, nrow = 3) +
  # Optional custom year palette (uncomment if you have one)
  # scale_color_manual(values = year_colors, name = "Year") +
  scale_x_continuous(
    breaks = month_starts,
    labels = month_labs,
    limits = c(133, 285),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Month (Growing Season)",
    y = NULL,
    title = "Seasonal Overlap (DOY 133–285) of Environmental and Depth Metrics by Year"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background   = element_rect(fill = "grey92", color = "grey70"),
    strip.text         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    plot.title         = element_text(face = "bold", hjust = 0.5)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(alpha = 1)))

ggsave(
  filename = "Figs/all_variables_season_DOY133_285.png",
  width = 20, height = 12, units = "in", dpi = 300, bg = "white"
)

#-----with points------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

variable_labels2 <- c(
  WaterLevel_m        = "Water Level (m)",
  PZ                  = "Photic Zone Depth (m)",
  schmidt_stability   = "Schmidt Stability (J/m²)",
  thermocline_depth   = "Thermocline Depth (m)",
  TFe_mgL_max_val     = "Max Total Fe (mg/L)",
  SRP_ugL_max_val     = "Max SRP (µg/L)",
  NH4_ugL_max_val     = "Max NH\u2084\u207A (µg/L)",
  depth_SFe_mgL_max   = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max   = "Depth of Max SRP (m)",
  depth_NH4_ugL_max   = "Depth of Max NH\u2084\u207A (m)",
  Precip_Weekly       = "Weekly Precipitation (mm)",
  AirTemp_Avg         = "Air Temperature (°C)",
  WindSpeed_Avg       = "Wind Speed (m/s)",
  DCM_depth           = "DCM Depth (m)",
  max_conc            = "Max Chlorophyll-a (µg/L)"
)

variables_plot <- full_weekly_data %>%
  select(
    Date, DCM_depth, max_conc,
    WaterLevel_m, PZ, schmidt_stability, thermocline_depth,
    TFe_mgL_max_val, SRP_ugL_max_val, NH4_ugL_max_val,
    depth_SFe_mgL_max, depth_SRP_ugL_max, depth_NH4_ugL_max,
    Precip_Weekly, AirTemp_Avg, WindSpeed_Avg
  ) %>%
  filter(year(Date) > 2014) %>%
  mutate(
    Date   = as.Date(Date),
    Year   = factor(year(Date)),
    DOY    = yday(Date),
    Leap   = leap_year(year(Date)),
    DOY_season = if_else(Leap & DOY > 59, DOY - 1L, DOY)
  ) %>%
  filter(DOY_season > 133, DOY_season < 285)   # ← restrict to May–Oct window

facet_vars <- c(
  "WaterLevel_m","PZ","schmidt_stability","thermocline_depth",
  "TFe_mgL_max_val","SRP_ugL_max_val","NH4_ugL_max_val",
  "depth_SFe_mgL_max","depth_SRP_ugL_max","depth_NH4_ugL_max",
  "Precip_Weekly","AirTemp_Avg","WindSpeed_Avg",
  "DCM_depth","max_conc"
)

variables_plot_long <- variables_plot %>%
  pivot_longer(cols = all_of(facet_vars),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable_f = factor(
    Variable,
    levels = names(variable_labels2),
    labels = unname(variable_labels2)
  ))

month_starts <- yday(as.Date(paste0("2001-", 5:10, "-01")))  # May–Oct
month_labs   <- month.abb[5:10]

ggplot(variables_plot_long,
       aes(x = DOY_season, y = Value, color = Year)) +
  geom_point(size = 1.8, alpha = 0.8) +                      # ← points only
  facet_wrap(~ Variable_f, scales = "free_y", ncol = 5, nrow = 3) +
  # Optional custom year palette (uncomment if you have one)
  # scale_color_manual(values = year_colors, name = "Year") +
  scale_x_continuous(
    breaks = month_starts,
    labels = month_labs,
    limits = c(133, 285),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Month (Growing Season)",
    y = NULL,
    title = "Seasonal Overlap (DOY 133–285) of Environmental and Depth Metrics by Year"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background   = element_rect(fill = "grey92", color = "grey70"),
    strip.text         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    plot.title         = element_text(face = "bold", hjust = 0.5)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(alpha = 1)))

ggsave(
  filename = "Figs/all_variables_season_DOY133_285_points.png",
  width = 20, height = 12, units = "in", dpi = 300, bg = "white"
)







####data availability for chosen variables####

final_data_availability_fig <- full_weekly_data |>
  select(
    Date,
    max_conc,
    DCM_depth,
    WaterLevel_m,
    PZ,
    schmidt_stability,
    thermocline_depth,
    TFe_mgL_max_val,
    SRP_ugL_max_val,
    NH4_ugL_max_val,
    Precip_Weekly,
    AirTemp_Avg,
    WindSpeed_Avg
  ) |>
  filter(year(Date) > 2014) |>
  rename(
    `Max Chlorophyll (µg/L)`       = max_conc,
    `Deep Chlorophyll Max Depth (m)` = DCM_depth,
    `Water Level (m)`              = WaterLevel_m,
    `Photic Zone Depth (m)`        = PZ,
    `Schmidt Stability (J/m²)`     = schmidt_stability,
    `Thermocline Depth (m)`        = thermocline_depth,
    `Total Fe (mg/L)`              = TFe_mgL_max_val,
    `SRP (µg/L)`                   = SRP_ugL_max_val,
    `NH₄⁺ (µg/L)`                  = NH4_ugL_max_val,
    `Precipitation (mm/day)`       = Precip_Weekly,
    `Air Temperature (°C)`         = AirTemp_Avg,
    `Wind Speed (m/s)`             = WindSpeed_Avg
  )

variables <- c(
  "Max Chlorophyll (µg/L)",
  "Deep Chlorophyll Max Depth (m)",
  "Water Level (m)",
  "Photic Zone Depth (m)",
  "Schmidt Stability (J/m²)",
  "Thermocline Depth (m)",
  "Total Fe (mg/L)",
  "SRP (µg/L)",
  "NH₄⁺ (µg/L)",
  "Precipitation (mm/day)",
  "Air Temperature (°C)",
  "Wind Speed (m/s)"
)

plot <- final_data_availability_plot(final_data_availability_fig, variables)  

ggsave("Figs/Final_Analysis_Data_availability.png", plot = plot, width = 15, height = 15)
