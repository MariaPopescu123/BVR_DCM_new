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
    Precip_Weekly,                  # 1-week lag precipitation (mm)
    AirTemp_Avg,                 # 2-week lag air temperature (°C)
    WindSpeed_Avg                     # 1-week lag wind speed (m/s)
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
  wind_lag2      = "Wind Speed Weekly Average (Lag 2 wk)", 
  SO4_ugL_max_val = "Sulfate Max (µg/L)",
  SO4_ugL_min_val = "Sulfate Min (µg/L)",
  SO4_ugL_at_DCM  = "Sulfate (µg/L) at DCM",
  depth_SO4_ugL_min = "Depth of Min Sulfate (m)",
  depth_SO4_ugL_max = "Depth of Max Sulfate (m)"
)


#----overlapping years with lines----
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)


# Subset the relevant variables and filter by DOY
variables_plot <- full_weekly_data %>%
  select(
    Date, DCM_depth, max_conc,
    WaterLevel_m, PZ, schmidt_stability, thermocline_depth, N_at_DCM,
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

# Keep only variable labels that exist in the dataset
facet_vars <- intersect(names(variable_labels), colnames(variables_plot))

# Pivot to long format and map pretty labels
variables_plot_long <- variables_plot %>%
  pivot_longer(
    cols = all_of(facet_vars),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable_label = factor(
      Variable,
      levels = facet_vars,
      labels = variable_labels[facet_vars]
    )
  )

# Month ticks for May–October
month_starts <- yday(as.Date(paste0("2001-", 5:10, "-01")))
month_labs   <- month.abb[5:10]

# Panels to reverse
rev_labels <- c(
  "Water Level (m)",
  "Photic Zone Depth (m)",
  "Thermocline Depth (m)",
  "DCM Depth (m)",
  "Depth of Max Soluble Fe (m)",
  "Depth of Max SRP (m)",
  "Depth of Max NH₄⁺ (m)"
)

# Single-panel plotting helper
one_panel <- function(label_text, reverse_y = FALSE, show_legend = FALSE) {
  d <- variables_plot_long %>%
    filter(Variable_label == label_text)
  
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

# ---- Build 4x4 layout ----

# Row 1
row1 <- (
  R("Water Level (m)", show_legend = TRUE) +
    R("Photic Zone Depth (m)") +
    R("Thermocline Depth (m)") +
    R("Schmidt Stability (J/m²)")
) + plot_layout(ncol = 4)

# Row 2
row2 <- (
  R("Buoyancy Frequency at DCM (s⁻¹)") +
    R("Precipitation Weekly Sum") +
    R("Air Temperature Weekly Average") +
    R("Wind Speed Weekly Average")
) + plot_layout(ncol = 4)

# Row 3
row3 <- (
  R("DCM Depth (m)") +
    R("Depth of Max Soluble Fe (m)") +
    R("Depth of Max SRP (m)") +
    R("Depth of Max NH₄⁺ (m)")
) + plot_layout(ncol = 4)

# Row 4
row4 <- (
  R("Max Total Phytoplankton (µg/L)") +
    R("SFe (mg/L) at DCM") +
    R("SRP (µg/L) at DCM") +
    R("NH₄⁺ (µg/L) at DCM")
) + plot_layout(ncol = 4)

# Final combined plot
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


