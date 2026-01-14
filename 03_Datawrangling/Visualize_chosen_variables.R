####visualize chosen variables####

chosen_variables <- full_weekly_data |>
  select(
    Date,
    max_conc,                     # Maximum phytoplankton concentration (µg/L)
    DCM_depth,                    # Deep Chlorophyll Maximum depth (m)
    WaterLevel_m,                 # Reservoir water level (m)
    PZ,                           # Photic zone depth (m)
    N_at_DCM,                     # Buoyancy Frequency at DCM (s⁻¹)
    schmidt_stability,            # Schmidt stability (J/m²)
    thermocline_depth,            # Thermocline depth (m)
    SFe_mgL_at_DCM,               # SFe (mg/L) at DCM
    SRP_ugL_at_DCM,              # SRP (µg/L) at DCM
    NH4_ugL_at_DCM,              # NH4 (µg/L) at DCM
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
  N_at_DCM = "Buoyancy Frequency at DCM (s⁻¹)",
  schmidt_stability = "Schmidt Stability (J/m²)",
  thermocline_depth = "Thermocline Depth (m)",
  SFe_mgL_at_DCM = "SFe (mg/L) at DCM",
  SRP_ugL_at_DCM = "SRP (µg/L) at DCM",
  NH4_ugL_at_DCM = "NH₄⁺ (µg/L) at DCM",
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  depth_NH4_ugL_max = "Depth of Max NH₄⁺ (m)",
  Precip_Weekly  = "Precipitation Weekly Sum",
  precip_lag1    = "Precipitation Weekly Sum (Lag 1 wk)",
  precip_lag2    = "Precipitation Weekly Sum (Lag 2 wk)",
  AirTemp_Avg    = "Air Temperature Weekly Average",
  airtemp_lag1   = "Air Temperature Weekly Average (Lag 1 wk)",
  airtemp_lag2   = "Air Temperature Weekly Average (Lag 2 wk)",
  WindSpeed_Avg  = "Wind Speed Weekly Average",
  wind_lag1      = "Wind Speed Weekly Average (Lag 1 wk)",
  wind_lag2      = "Wind Speed Weekly Average (Lag 2 wk)"
)


#----overlapping years with lines----
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)

variable_labels2 <- c(
  WaterLevel_m        = "Water Level (m)",
  PZ                  = "Photic Zone Depth (m)",
  schmidt_stability   = "Schmidt Stability (J/m²)",
  thermocline_depth   = "Thermocline Depth (m)",
  SFe_mgL_at_DCM      = "SFe (mg/L) at DCM",
  SRP_ugL_at_DCM      = "SRP (µg/L) at DCM",
  NH4_ugL_at_DCM      = "NH\u2084\u207A (µg/L) at DCM",
  depth_SFe_mgL_max   = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max   = "Depth of Max SRP (m)",
  depth_NH4_ugL_max   = "Depth of Max NH\u2084\u207A (m)",
  Precip_Weekly       = "Weekly Precipitation (mm)",
  AirTemp_Avg         = "Air Temperature (°C)",
  WindSpeed_Avg       = "Wind Speed (m/s)",
  DCM_depth           = "DCM Depth (m)",
  max_conc            = "Total Phytoplankton (µg/L)"
)

variables_plot <- full_weekly_data %>%
  select(
    Date, DCM_depth, max_conc,
    WaterLevel_m, PZ, schmidt_stability, thermocline_depth,
    SFe_mgL_at_DCM, SRP_ugL_at_DCM, NH4_ugL_at_DCM,
    depth_SFe_mgL_max, depth_SRP_ugL_max, depth_NH4_ugL_max,
    Precip_Weekly, AirTemp_Avg, WindSpeed_Avg
  ) %>%
  filter(year(Date) > 2014) %>%
  mutate(
    Date       = as.Date(Date),
    Year       = factor(year(Date)),
    DOY        = yday(Date),
    Leap       = leap_year(year(Date)),
    DOY_season = if_else(Leap & DOY > 59, DOY - 1L, DOY)
  ) %>%
  filter(DOY_season > 133, DOY_season < 285)

facet_vars <- names(variable_labels2)

variables_plot_long <- variables_plot %>%
  pivot_longer(
    cols = all_of(facet_vars),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable_label = factor(
      Variable,
      levels = names(variable_labels2),
      labels = unname(variable_labels2)
    )
  )

# Month ticks for May–Oct
month_starts <- yday(as.Date(paste0("2001-", 5:10, "-01")))
month_labs   <- month.abb[5:10]

# Which panels should be reversed?
rev_labels <- c(
  "Water Level (m)",
  "Photic Zone Depth (m)",
  "Thermocline Depth (m)",
  "DCM Depth (m)",
  "Depth of Max Soluble Fe (m)",
  "Depth of Max SRP (m)",
  "Depth of Max NH\u2084\u207A (m)"
)

# Helper: single-panel plot
one_panel <- function(label_text, reverse_y = FALSE, show_legend = FALSE) {
  d <- variables_plot_long %>%
    dplyr::filter(Variable_label == label_text)
  
  p <- ggplot(d, aes(DOY_season, Value, color = Year, group = Year)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    scale_x_continuous(
      breaks = month_starts,
      labels = month_labs,
      limits = c(133, 285),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(x = NULL, y = NULL, title = label_text) +
    theme_bw(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      legend.position    = if (show_legend) "right" else "none"
    )
  
  if (reverse_y) p <- p + scale_y_reverse()
  p
}

R <- function(lbl, show_legend = FALSE) one_panel(lbl, lbl %in% rev_labels, show_legend)

# Row 1: Water level, PZ, Thermocline Depth, Schmidt Stability
row1 <- (
  R("Water Level (m)", show_legend = TRUE) +
    R("Photic Zone Depth (m)") +
    R("Thermocline Depth (m)") +
    R("Schmidt Stability (J/m²)")
) +
  plot_layout(ncol = 4)

row2 <- (
  plot_spacer() +
    R("Weekly Precipitation (mm)") +
    R("Air Temperature (°C)") +
    R("Wind Speed (m/s)")
) +
  plot_layout(ncol = 4)

row3 <- (
  R("DCM Depth (m)") +
    R("Depth of Max Soluble Fe (m)") +
    R("Depth of Max SRP (m)") +
    R("Depth of Max NH\u2084\u207A (m)")
) +
  plot_layout(ncol = 4)

row4 <- (
  R("Total Phytoplankton (µg/L)") +
    R("SFe (mg/L) at DCM") +
    R("SRP (µg/L) at DCM") +
    R("NH\u2084\u207A (µg/L) at DCM")
) +
  plot_layout(ncol = 4)

p_final <- (row1 / row2 / row3 / row4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Seasonal Overlap (DOY 133–285) of Environmental and Depth Metrics by Year",
    theme = theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  )

print(p_final)

ggsave(
  filename = "Figs/all_variables_season_DOY133_285_custom.png",
  plot = p_final,
  width = 20, height = 14, units = "in", dpi = 300, bg = "white"
)























####With points##############
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)

variable_labels2 <- c(
  WaterLevel_m        = "Water Level (m)",
  PZ                  = "Photic Zone Depth (m)",
  schmidt_stability   = "Schmidt Stability (J/m²)",
  thermocline_depth   = "Thermocline Depth (m)",
  SFe_mgL_max_val     = "Max Soluble Fe (mg/L)",
  SRP_ugL_max_val     = "Max SRP (µg/L)",
  NH4_ugL_max_val     = "Max NH\u2084\u207A (µg/L)",
  depth_SFe_mgL_max   = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max   = "Depth of Max SRP (m)",
  depth_NH4_ugL_max   = "Depth of Max NH\u2084\u207A (m)",
  Precip_Weekly       = "Weekly Precipitation (mm)",
  AirTemp_Avg         = "Air Temperature (°C)",
  WindSpeed_Avg       = "Wind Speed (m/s)",
  DCM_depth           = "DCM Depth (m)",
  max_conc            = "Total Phytoplankton (µg/L)"
)

variables_plot <- full_weekly_data %>%
  select(
    Date, DCM_depth, max_conc,
    WaterLevel_m, PZ, schmidt_stability, thermocline_depth,
    SFe_mgL_max_val, SRP_ugL_max_val, NH4_ugL_max_val,
    depth_SFe_mgL_max, depth_SRP_ugL_max, depth_NH4_ugL_max,
    Precip_Weekly, AirTemp_Avg, WindSpeed_Avg
  ) %>%
  filter(year(Date) > 2014) %>%
  mutate(
    Date       = as.Date(Date),
    Year       = factor(year(Date)),
    DOY        = yday(Date),
    Leap       = leap_year(year(Date)),
    DOY_season = if_else(Leap & DOY > 59, DOY - 1L, DOY)
  ) %>%
  filter(DOY_season > 133, DOY_season < 285)

facet_vars <- names(variable_labels2)

variables_plot_long <- variables_plot %>%
  pivot_longer(cols = all_of(facet_vars),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable_label = factor(
    Variable,
    levels = names(variable_labels2),
    labels = unname(variable_labels2)
  ))

# Month ticks for May–Oct
month_starts <- yday(as.Date(paste0("2001-", 5:10, "-01")))
month_labs   <- month.abb[5:10]

# Helper: single-panel plot (POINTS)
one_panel <- function(label_text, reverse_y = FALSE) {
  d <- variables_plot_long %>%
    mutate(Variable_label = factor(
      Variable,
      levels = names(variable_labels2),
      labels = unname(variable_labels2)
    )) %>%
    dplyr::filter(Variable_label == label_text)
  
  p <- ggplot(d, aes(DOY_season, Value, color = Year)) +
    geom_point(size = 1.8, alpha = 0.85) +
    scale_x_continuous(
      breaks = month_starts, labels = month_labs,
      limits = c(133, 285), expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(x = NULL, y = NULL, title = label_text) +
    theme_bw(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      legend.position    = "none"
    )
  if (reverse_y) p + scale_y_reverse() else p
}

# Panels to reverse
rev_labels <- c(
  "Water Level (m)",
  "Photic Zone Depth (m)",
  "Thermocline Depth (m)",
  "DCM Depth (m)",
  "Depth of Max Soluble Fe (m)",
  "Depth of Max SRP (m)",
  "Depth of Max NH\u2084\u207A (m)"
)

R <- function(lbl) one_panel(lbl, lbl %in% rev_labels)

# ---- EXACT LAYOUT (4x4, row 2 centered) ----
row1 <- ( R("Water Level (m)")
          + R("Photic Zone Depth (m)")
          + R("Thermocline Depth (m)")
          + R("Schmidt Stability (J/m²)") ) + plot_layout(ncol = 4)

row2 <- ( plot_spacer()
          + R("Weekly Precipitation (mm)")
          + R("Air Temperature (°C)")
          + R("Wind Speed (m/s)") ) + plot_layout(ncol = 4)

row3 <- ( R("DCM Depth (m)")
          + R("Depth of Max Soluble Fe (m)")
          + R("Depth of Max SRP (m)")
          + R("Depth of Max NH\u2084\u207A (m)") ) + plot_layout(ncol = 4)

row4 <- ( R("Total Phytoplankton (µg/L)")
          + R("Max Soluble Fe (mg/L)")
          + R("Max SRP (µg/L)")
          + R("Max NH\u2084\u207A (µg/L)") ) + plot_layout(ncol = 4)

p_final <- (row1 / row2 / row3 / row4) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

p_final <- p_final + plot_annotation(
  title = "Seasonal Overlap (DOY 133–285) of Environmental and Depth Metrics by Year",
  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
)

print(p_final)

ggsave(
  filename = "Figs/all_variables_season_DOY133_285_points.png",
  plot = p_final,
  width = 20, height = 14, units = "in", dpi = 300, bg = "white"
)

####----table display important metrics----####
tabledata <- full_weekly_data %>%
 select(
    Date,
    max_conc,                     # Maximum phytoplankton concentration (µg/L)
    DCM_depth,                    # Deep Chlorophyll Maximum depth (m)
    WaterLevel_m,                 # Reservoir water level (m)
    PZ,                           # Photic zone depth (m)
    schmidt_stability,            # Schmidt stability (J/m²)
    thermocline_depth,            # Thermocline depth (m)
    SFe_mgL_max_val,              # Max Soluble Fe (mg/L)
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

variables <- c(
  "max_conc",             # Maximum phytoplankton concentration (µg/L)
  "DCM_depth",            # Deep Chlorophyll Maximum depth (m)
  "WaterLevel_m",         # Reservoir water level (m)
  "PZ",                   # Photic zone depth (m)
  "schmidt_stability",    # Schmidt stability (J/m²)
  "thermocline_depth",    # Thermocline depth (m)
  "SFe_mgL_max_val",      # Max Soluble Fe (mg/L)
  "SRP_ugL_max_val",      # Max soluble reactive phosphorus (µg/L)
  "NH4_ugL_max_val",      # Max ammonium (µg/L)
  "depth_SFe_mgL_max",    # Depth of max soluble Fe (m)
  "depth_SRP_ugL_max",    # Depth of max SRP (m)
  "depth_NH4_ugL_max",    # Depth of max ammonium (m)
  "depth_np_ratio_max",   # Depth of max N:P ratio
  "precip_lag1",          # 1-week lag precipitation (mm)
  "airtemp_lag2",         # 2-week lag air temperature (°C)
  "wind_lag1"             # 1-week lag wind speed (m/s)
)

tabledataprep <- tabledata %>%
  mutate(Year = lubridate::year(Date)) %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "Variable",
    values_to = "value"
  ) %>%
  group_by(Year, Variable) %>%
  summarise(
    Max   = max(value, na.rm = TRUE),
    Min   = min(value, na.rm = TRUE),
    Range = Max - Min,
    Mean  = mean(value, na.rm = TRUE),
    SD    = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Variable = variable_labels[Variable] %||% Variable
  )


# make sure metrics in a nice order
tabledataprep_long <- tabledataprep %>%
  pivot_longer(
    cols = c(Max, Min, Range, Mean, SD),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, levels = c("Max", "Min", "Range", "Mean", "SD"))
  )

# wide table: one row per Year, one column per Variable × Metric
tabledataprep_wide <- tabledataprep_long %>%
  unite("colname", Variable, Metric, sep = " - ") %>%   # e.g. "DCM depth (m) - Max"
  pivot_wider(
    names_from = colname,
    values_from = Value
  ) %>%
  arrange(Year)







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
    SFe_mgL_max_val,
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
    `Soluble Fe (mg/L)`              = SFe_mgL_max_val,
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
  "Soluble Fe (mg/L)",
  "SRP (µg/L)",
  "NH₄⁺ (µg/L)",
  "Precipitation (mm/day)",
  "Air Temperature (°C)",
  "Wind Speed (m/s)"
)

plot <- final_data_availability_plot(final_data_availability_fig, variables)  

ggsave("Figs/Final_Analysis_Data_availability.png", plot = plot, width = 15, height = 15)


check <- full_weekly_data|>
  filter(DCM_depth < .05*WaterLevel_m)
