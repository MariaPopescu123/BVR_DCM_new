####visualize chosen variables####
####compute statistics####
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
  Precip_Weekly  = "Precipitation Weekly Sum (mm)",
  precip_lag1    = "Precipitation Weekly Sum mm (Lag 1 wk)",
  precip_lag2    = "Precipitation Weekly Sum mm (Lag 2 wk)",
  AirTemp_Avg    = "Air Temperature Weekly Average (\u00b0C)",
  airtemp_lag1   = "Air Temperature Weekly Average \u00b0C (Lag 1 wk)",
  airtemp_lag2   = "Air Temperature Weekly Average \u00b0C (Lag 2 wk)",
  WindSpeed_Avg  = "Wind Speed Weekly Average (m/s)",
  wind_lag1      = "Wind Speed Weekly Average m/s (Lag 1 wk)",
  wind_lag2      = "Wind Speed Weekly Average m/s (Lag 2 wk)"
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
    R("Precipitation Weekly Sum (mm)") +
    R("Air Temperature Weekly Average (\u00b0C)") +
    R("Wind Speed Weekly Average (m/s)")
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

####Summary Statistics (Table S2 & S3)####
# Uses a full stratified-season spine (all Year+Week combos within DOY 133-285)
# so that environmental variables get their full data coverage,
# not limited to weeks when phyto casts were taken.
# DCM depth and magnitude will be NA for non-phyto weeks (correct behaviour).

# ── Full spine of all stratified-season weeks 2015-2024 ──
stats_spine <- expand.grid(Year = 2015:2024, Week = 1:53) %>%
  mutate(Date = as.Date(paste0(Year, "-01-01")) + weeks(Week - 1),
         DOY  = yday(Date)) %>%
  filter(DOY > 133, DOY < 286) %>%
  select(Year, Week)

# ── Read source CSVs ──
stats_waterlevel <- read.csv("CSVs/water_level.csv") %>%
  group_by(Year, Week) %>%
  slice(1) %>%
  ungroup() %>%
  select(Year, Week, WaterLevel_m)

stats_photic  <- read.csv("CSVs/final_photic_thermo.csv") %>%
  select(Year, Week, PZ, thermocline_depth)

stats_buoyancy <- read.csv("CSVs/final_buoyancy.csv") %>%
  select(-any_of("Date"))

stats_chem    <- read.csv("CSVs/final_chem.csv") %>%
  select(-any_of("Date"))

stats_metals  <- read.csv("CSVs/final_metals.csv") %>%
  select(-any_of("Date"))

stats_schmidt <- read.csv("CSVs/final_schmidt.csv") %>%
  select(-any_of("Date"))

stats_metdata <- read.csv("CSVs/final_metdata.csv") %>%
  rename(
    AirTemp_Avg   = weekly_airtempavg,
    WindSpeed_Avg = WindSpeed_Weekly_Average_m_s,
    Precip_Weekly = precip_weekly
  )

stats_phytos  <- read.csv("CSVs/final_phytos.csv") %>%
  select(Year, Week, DCM_depth, max_conc)

# ── Join everything to full spine ──
full_stats_data <- stats_spine %>%
  left_join(stats_phytos,    by = c("Year", "Week")) %>%
  left_join(stats_waterlevel, by = c("Year", "Week")) %>%
  left_join(stats_metals,    by = c("Year", "Week")) %>%
  left_join(stats_photic,    by = c("Year", "Week")) %>%
  left_join(stats_buoyancy,  by = c("Year", "Week")) %>%
  left_join(stats_chem,      by = c("Year", "Week")) %>%
  left_join(stats_schmidt,   by = c("Year", "Week")) %>%
  left_join(stats_metdata,   by = c("Year", "Week")) %>%
  mutate(Year = as.integer(Year))

vars_to_use <- names(variable_labels)[names(variable_labels) %in% names(full_stats_data)]

# ── Overall summary statistics ──
#NEED TO FILTER SO THAT ITS WITHIN THE STUDY PERIOD
overall_summary <- bind_rows(lapply(vars_to_use, function(v) {

  if (v == "DCM_depth") {
    data_used <- full_stats_data %>% filter(max_conc > 20)
    var_label <- paste0(variable_labels[v], "*")
  } else {
    data_used <- full_stats_data
    var_label <- variable_labels[v]
  }

  s       <- data_used[[v]]
  s_clean <- s[!is.na(s)]
  if (length(s_clean) == 0) return(NULL)

  yearly <- data_used %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Year) %>%
    summarise(yr_mean = mean(.data[[v]], na.rm = TRUE),
              yr_sd   = sd(.data[[v]],   na.rm = TRUE),
              .groups = "drop")

  min_row <- data_used %>%
    filter(!is.na(.data[[v]]), .data[[v]] == min(.data[[v]], na.rm = TRUE)) %>% slice(1)
  max_row <- data_used %>%
    filter(!is.na(.data[[v]]), .data[[v]] == max(.data[[v]], na.rm = TRUE)) %>% slice(1)

  lowest_year  <- yearly %>% filter(yr_mean == min(yr_mean, na.rm = TRUE)) %>% slice(1)
  highest_year <- yearly %>% filter(yr_mean == max(yr_mean, na.rm = TRUE)) %>% slice(1)

  tibble(
    Variable              = unname(var_label),
    Mean                  = round(mean(s_clean), 3),
    Median                = round(median(s_clean), 3),
    SD                    = round(sd(s_clean), 3),
    `CV (%)`              = round(sd(s_clean) / mean(s_clean) * 100, 1),
    Min                   = paste0(round(min_row[[v]], 3), " (", min_row$Year, ")"),
    Max                   = paste0(round(max_row[[v]], 3), " (", max_row$Year, ")"),
    Range                 = round(max(s_clean) - min(s_clean), 3),
    Q25                   = round(quantile(s_clean, 0.25), 3),
    Q75                   = round(quantile(s_clean, 0.75), 3),
    IQR                   = round(IQR(s_clean), 3),
    `Within-Year SD`      = round(mean(yearly$yr_sd, na.rm = TRUE), 3),
    `Between-Year SD`     = round(sd(yearly$yr_mean, na.rm = TRUE), 3),
    `Lowest Yearly Mean`  = paste0(lowest_year$Year,  " (", round(lowest_year$yr_mean,  3),
                                   " \u00b1 ", round(lowest_year$yr_sd,  3), ")"),
    `Highest Yearly Mean` = paste0(highest_year$Year, " (", round(highest_year$yr_mean, 3),
                                   " \u00b1 ", round(highest_year$yr_sd, 3), ")")
  )
}))

write.csv(overall_summary, "CSVs/summary_statistics_overall.csv", row.names = FALSE)

# ── Yearly stats ──
yearly_stats <- bind_rows(lapply(vars_to_use, function(v) {
  if (v == "DCM_depth") {
    data_used <- full_stats_data %>% filter(max_conc > 20)
  } else {
    data_used <- full_stats_data
  }
  data_used %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Year) %>%
    summarise(
      Variable   = v,
      N          = n(),
      Mean       = mean(.data[[v]], na.rm = TRUE),
      SD         = sd(.data[[v]], na.rm = TRUE),
      CV_percent = (SD / Mean) * 100,
      Min        = min(.data[[v]], na.rm = TRUE),
      Max        = max(.data[[v]], na.rm = TRUE),
      Range      = Max - Min,
      Median     = median(.data[[v]], na.rm = TRUE),
      Q25        = quantile(.data[[v]], 0.25, na.rm = TRUE),
      Q75        = quantile(.data[[v]], 0.75, na.rm = TRUE),
      IQR        = IQR(.data[[v]], na.rm = TRUE),
      .groups    = "drop"
    )
}))

write.csv(yearly_stats, "CSVs/yearly_variable_stats.csv", row.names = FALSE)