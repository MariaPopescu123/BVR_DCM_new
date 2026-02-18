####visualize chosen variables####
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)

variable_labels <- c(
  max_conc = "DCM Magnitude (µg/L)",
  DCM_depth = "DCM Depth (m)",
  WaterLevel_m = "Water Level (m)",
  PZ = "Photic Zone Depth (m)",
  N_at_DCM = "Buoyancy Frequency at DCM (s⁻¹)",
  schmidt_stability = "Schmidt Stability (J/m²)",
  thermocline_depth = "Thermocline Depth (m)",
  SFe_mgL_at_DCM = "SFe (mg/L) at DCM",
  SRP_ugL_at_DCM = "SRP (µg/L) at DCM",
  NH4_ugL_at_DCM = "NH₄⁺ (µg/L) at DCM",
  NO3NO2_ugL_at_DCM = "NO₃⁻/NO₂⁻ (µg/L) at DCM",
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  depth_NH4_ugL_max = "Depth of Max NH₄⁺ (m)",
  depth_NO3NO2_ugL_max = "Depth of Max NO₃⁻/NO₂⁻ (m)",
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

# ---- helper: standard DOY_season + filtering ----
prep_for_plot <- function(df, date_col = "Date") {
  df %>%
    mutate(
      Date       = as.Date(.data[[date_col]]),
      Week       = week(Date),
      Year       = factor(year(Date)),
      DOY        = yday(Date),
      Leap       = leap_year(year(Date)),
      DOY_season = (Week - 1) * 7 + 4
    ) %>%
    filter(year(Date) > 2014, year(Date) < 2025) %>%
    filter(DOY_season > 133, DOY_season < 285)
}

make_long <- function(df, vars) {
  df %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(
      Variable_label = factor(Variable,
                              levels = vars,
                              labels = variable_labels[vars]
      )
    )
}

# ---- 1. Met variables (~52 wks/yr) ----
met_vars <- c("Precip_Weekly", "AirTemp_Avg", "WindSpeed_Avg")

met_long <- final_metdata %>%
  rename(
    AirTemp_Avg   = weekly_airtempavg,
    WindSpeed_Avg = WindSpeed_Weekly_Average_m_s,
    Precip_Weekly = precip_weekly
  ) %>%
  prep_for_plot() %>%
  make_long(met_vars) %>%
  mutate(max_conc = NA_real_)

# ---- 2. Photic zone & thermocline (53 wks/yr) ----
photic_vars <- c("PZ", "thermocline_depth")

photic_long <- final_photic_thermo %>%
  mutate(Date = as.Date(paste0(Year, "-01-01")) + weeks(Week - 1)) %>%
  prep_for_plot() %>%
  make_long(photic_vars) %>%
  mutate(max_conc = NA_real_)

# ---- 3. Schmidt stability (~19-22 wks/yr) ----
schmidt_vars <- c("schmidt_stability")

schmidt_long <- final_schmidt %>%
  # schmidt has no Date, build one from Year + Week
  mutate(Date = as.Date(paste0(Year, "-01-01")) + weeks(Week - 1)) %>%
  prep_for_plot() %>%
  make_long(schmidt_vars) %>%
  mutate(max_conc = NA_real_)

# ---- 4. Sampling-frequency variables (from full_weekly_data) ----
sampling_vars <- c(
  "DCM_depth", "WaterLevel_m", "N_at_DCM",
  "SFe_mgL_at_DCM", "SRP_ugL_at_DCM", "NH4_ugL_at_DCM",
  "depth_SFe_mgL_max", "depth_SRP_ugL_max", "depth_NH4_ugL_max"
)

sampling_base <- full_weekly_data %>% prep_for_plot()

sampling_long <- sampling_base %>% make_long(sampling_vars)

# max_conc as its own panel
max_conc_rows <- sampling_base %>%
  transmute(
    Date, Year, DOY, Leap, DOY_season, max_conc,
    Variable = "max_conc",
    Value = max_conc,
    Variable_label = factor("DCM Magnitude (µg/L)")
  )

# ---- 5. Combine all ----
variables_plot_long <- bind_rows(
  sampling_long, max_conc_rows,
  met_long, photic_long, schmidt_long
)

# ---- Plotting ----
rev_labels <- c(
  "Water Level (m)",
  "Photic Zone Depth (m)",
  "Thermocline Depth (m)",
  "DCM Depth (m)",
  "Depth of Max Soluble Fe (m)",
  "Depth of Max SRP (m)",
  "Depth of Max NH₄⁺ (m)"
)

one_panel <- function(label_text, reverse_y = FALSE, show_legend = FALSE) {
  d <- variables_plot_long %>%
    filter(Variable_label == label_text)
  
  if (label_text == "DCM Depth (m)") {
    d <- d %>% filter(max_conc > 20)
  }
  
  p <- ggplot(d, aes(DOY_season, Value, color = Year, group = Year)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    scale_x_continuous(
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
row1 <- (
  R("Water Level (m)", show_legend = TRUE) +
    R("Photic Zone Depth (m)") +
    R("Thermocline Depth (m)") +
    R("Schmidt Stability (J/m²)")
) + plot_layout(ncol = 4)

row2 <- (
  R("Buoyancy Frequency at DCM (s⁻¹)") +
    R("Precipitation Weekly Sum") +
    R("Air Temperature Weekly Average") +
    R("Wind Speed Weekly Average")
) + plot_layout(ncol = 4)

row3 <- (
  R("DCM Depth (m)") +
    R("Depth of Max Soluble Fe (m)") +
    R("Depth of Max SRP (m)") +
    R("Depth of Max NH₄⁺ (m)")
) + plot_layout(ncol = 4)

row4 <- (
  R("DCM Magnitude (µg/L)") +
    R("SFe (mg/L) at DCM") +
    R("SRP (µg/L) at DCM") +
    R("NH₄⁺ (µg/L) at DCM")
) + plot_layout(ncol = 4)

p_final <- (row1 / row2 / row3 / row4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Seasonal Overlap (DOY 133 - 285) of Environmental and Depth Metrics",
    theme = theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  )

ggsave(
  filename = "Figs/all_variables_visualized.png",
  plot = p_final,
  width = 20, height = 14, units = "in", dpi = 300, bg = "white"
)
# warnings are ok