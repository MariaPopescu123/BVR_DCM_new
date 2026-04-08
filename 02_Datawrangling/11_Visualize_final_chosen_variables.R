#This script:
# 1. Visualizes the variables going into the analysis
# 2. Calculates statistics for the variables
#
# Inputs:
# - full_weekly_data, final_metdata, final_photic_thermo, final_schmidt
#   (created by upstream data-wrangling scripts)
# - variable_labels (created in 01_DataDownload.R)
# Output figure: Figs/all_variables_visualized.png

# packages loaded in 01_DataDownload.R
# variable_labels defined in 01_DataDownload.R

required_objects <- c("full_weekly_data", "final_metdata", "final_photic_thermo", "final_schmidt", "variable_labels")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for 11_Visualize_final_chosen_variables.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 01_DataDownload.R and 01-10 in 02_Datawrangling first.")
}

####compute statistics####

# Helper: standard date parsing, seasonal filter, and shared week/year fields.
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
    filter(DOY >= 133, DOY <= 286)
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

# ---- 1. Met variables ----
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

# ---- 2. Photic zone & thermocline ----
photic_vars <- c("PZ", "thermocline_depth")

photic_long <- final_photic_thermo %>%
  mutate(Date = ISOweek2date(paste0(Year, "-W", sprintf("%02d", Week), "-1"))) %>%
  prep_for_plot() %>%
  make_long(photic_vars) %>%
  mutate(max_conc = NA_real_)

# ---- 3. Schmidt stability ----
schmidt_vars <- c("schmidt_stability")

schmidt_long <- final_schmidt %>%
  mutate(Date = ISOweek2date(paste0(Year, "-W", sprintf("%02d", Week), "-1"))) %>%
  prep_for_plot() %>%
  make_long(schmidt_vars) %>%
  mutate(max_conc = NA_real_)

# ---- 4. Sampling-frequency variables ----
sampling_vars <- c(
  "DCM_depth", "WaterLevel_m", "N_at_DCM",
  "SFe_mgL_at_DCM", "SRP_ugL_at_DCM", "NH4_ugL_at_DCM",
  "NO3NO2_ugL_at_DCM",
  "depth_SFe_mgL_max", "depth_SRP_ugL_max", "depth_NH4_ugL_max",
  "depth_NO3NO2_ugL_max"
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
  "Photic Zone Depth (m)",
  "Thermocline Depth (m)",
  "DCM Depth (m)",
  "Depth of Max Soluble Fe (m)",
  "Depth of Max SRP (m)",
  "Depth of Max NH₄⁺ (m)",
  "Depth of Max NO₃⁻/NO₂⁻ (m)"
)

one_panel <- function(label_text, reverse_y = FALSE, show_legend = FALSE, panel_letter = NULL) {
  d <- variables_plot_long %>%
    filter(Variable_label == label_text)

  if (label_text == "DCM Depth (m)") {
    d <- d %>% filter(max_conc > 20)
  }

  p <- ggplot(d, aes(DOY_season, Value, color = Year, group = Year)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    scale_x_continuous(
      limits = c(133, 286),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(x = NULL, y = NULL, title = label_text, tag = panel_letter) +
    theme_bw(base_size = 16) +
    theme(
      plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      legend.position    = if (show_legend) "right" else "none",
      plot.tag           = element_text(face = "bold", size = 14),
      plot.tag.position  = c(0.02, 0.98)
    )

  if (reverse_y) p <- p + scale_y_reverse()
  p
}

R <- function(lbl, show_legend = FALSE, panel_letter = NULL) one_panel(lbl, lbl %in% rev_labels, show_legend, panel_letter)

# ---- Build 4x4 layout ----
row1 <- (
  R("Water Level (m)", show_legend = TRUE, panel_letter = "a") +
    R("Photic Zone Depth (m)", panel_letter = "b") +
    R("Thermocline Depth (m)", panel_letter = "c") +
    R("Schmidt Stability (J/m²)", panel_letter = "d")
) + plot_layout(ncol = 4)

row2 <- (
  R("Buoyancy Frequency at DCM (s⁻¹)", panel_letter = "e") +
    R("Precipitation Weekly Sum (mm)", panel_letter = "f") +
    R("Air Temperature Weekly Average (\u00b0C)", panel_letter = "g") +
    R("Wind Speed Weekly Average (m/s)", panel_letter = "h")
) + plot_layout(ncol = 4)

row3 <- (
  R("Depth of Max NO₃⁻/NO₂⁻ (m)", panel_letter = "i") +
    R("Depth of Max Soluble Fe (m)", panel_letter = "j") +
    R("Depth of Max SRP (m)", panel_letter = "k") +
    R("Depth of Max NH₄⁺ (m)", panel_letter = "l")
) + plot_layout(ncol = 4)

row4 <- (
  R("NO₃⁻/NO₂⁻ (µg/L) at DCM", panel_letter = "m") +
    R("SFe (mg/L) at DCM", panel_letter = "n") +
    R("SRP (µg/L) at DCM", panel_letter = "o") +
    R("NH₄⁺ (µg/L) at DCM", panel_letter = "p")
) + plot_layout(ncol = 4)

p_final <- (row1 / row2 / row3 / row4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Seasonal Overlap (DOY 133 - 286) of Environmental and Depth Metrics",
    theme = theme(
      legend.position = "right",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      legend.key.size = unit(1.5, "lines"),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
    )
  )

ggsave(
  filename = "Figs/all_variables_visualized.png",
  plot = p_final,
  width = 20, height = 14, units = "in", dpi = 1200, bg = "white"
)

####Summary Statistics (for SI)####
# Uses all Year+Week combos within DOY 133-286
# so that environmental variables get their full data coverage,
# not limited to weeks when phyto casts were taken.
# DCM depth and magnitude will be NA for non-phyto weeks.

# ── Full spine of all stratified-season weeks 2015-2024 ──
stats_spine <- expand.grid(Year = 2015:2024, Week = 1:53) %>%
  mutate(Date = ISOweek2date(paste0(Year, "-W", sprintf("%02d", Week), "-1")),
         DOY  = yday(Date)) %>%
  filter(DOY >= 133, DOY <= 286) %>%
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

####Yearly summary stats figure (Figure S_)####
# Same layout as all_variables_visualized but:
#   - DCM Depth    replaced by Depth of Max NO3/NO2
#   - DCM Magnitude replaced by NO3/NO2 at DCM
# Style matches Figure S6: median + IQR ribbon + mean +/- SE, by year

yearly_stats_rev_labels <- c(
  "Photic Zone Depth (m)",
  "Thermocline Depth (m)",
  "Depth of Max NO\u2083\u207b/NO\u2082\u207b (m)",
  "Depth of Max Soluble Fe (m)",
  "Depth of Max SRP (m)",
  "Depth of Max NH\u2084\u207a (m)"
)

one_panel_yearly <- function(var_name, show_legend = FALSE, panel_letter = NULL) {
  label     <- unname(variable_labels[var_name])
  reverse_y <- label %in% yearly_stats_rev_labels

  d <- full_stats_data %>%
    filter(!is.na(.data[[var_name]])) %>%
    group_by(Year) %>%
    summarise(
      median = median(.data[[var_name]], na.rm = TRUE),
      q25    = quantile(.data[[var_name]], 0.25, na.rm = TRUE),
      q75    = quantile(.data[[var_name]], 0.75, na.rm = TRUE),
      mean   = mean(.data[[var_name]], na.rm = TRUE),
      sd     = sd(.data[[var_name]], na.rm = TRUE),
      min    = min(.data[[var_name]], na.rm = TRUE),
      max    = max(.data[[var_name]], na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot(d, aes(x = factor(Year))) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = "Range", group = 1), alpha = 0.25) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = "IQR", group = 1), alpha = 0.4) +
    geom_line(aes(y = median, color = "Median", group = 1), linewidth = 1.2) +
    geom_point(aes(y = median, color = "Median"), size = 2.5) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = "Mean \u00b1 SD", group = 1),
                  width = 0.15, alpha = 0.6) +
    geom_point(aes(y = mean, color = "Mean \u00b1 SD"), shape = 21, fill = "white", size = 2) +
    scale_color_manual(name = NULL, values = c("Median" = "blue", "Mean \u00b1 SD" = "red")) +
    scale_fill_manual(name = NULL, values = c("IQR" = "grey80", "Range" = "lightblue")) +
    labs(x = NULL, y = NULL, title = label, tag = panel_letter) +
    theme_bw(base_size = 16) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      legend.position  = if (show_legend) "right" else "none",
      plot.tag          = element_text(face = "bold", size = 14),
      plot.tag.position = c(0.02, 0.98)
    )

  if (reverse_y) p <- p + scale_y_reverse()
  p
}

Ry <- function(v, show_legend = FALSE, panel_letter = NULL) one_panel_yearly(v, show_legend, panel_letter)

yrow1 <- (
  Ry("WaterLevel_m", show_legend = TRUE, panel_letter = "a") +
  Ry("PZ", panel_letter = "b") +
  Ry("thermocline_depth", panel_letter = "c") +
  Ry("schmidt_stability", panel_letter = "d")
) + plot_layout(ncol = 4)

yrow2 <- (
  Ry("N_at_DCM", panel_letter = "e") +
  Ry("Precip_Weekly", panel_letter = "f") +
  Ry("AirTemp_Avg", panel_letter = "g") +
  Ry("WindSpeed_Avg", panel_letter = "h")
) + plot_layout(ncol = 4)

yrow3 <- (
  Ry("depth_NO3NO2_ugL_max", panel_letter = "i") +
  Ry("depth_SFe_mgL_max", panel_letter = "j") +
  Ry("depth_SRP_ugL_max", panel_letter = "k") +
  Ry("depth_NH4_ugL_max", panel_letter = "l")
) + plot_layout(ncol = 4)

yrow4 <- (
  Ry("NO3NO2_ugL_at_DCM", panel_letter = "m") +
  Ry("SFe_mgL_at_DCM", panel_letter = "n") +
  Ry("SRP_ugL_at_DCM", panel_letter = "o") +
  Ry("NH4_ugL_at_DCM", panel_letter = "p")
) + plot_layout(ncol = 4)

p_yearly <- (yrow1 / yrow2 / yrow3 / yrow4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Yearly Median and Mean \u00b1 SD (DOY 133\u2013286)",
    theme = theme(
      legend.position = "right",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      legend.key.size = unit(1.5, "lines"),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
    )
  )

ggsave(
  filename = "Figs/all_variables_yearly_stats.png",
  plot = p_yearly,
  width = 20, height = 14, units = "in", dpi = 1200, bg = "white"
)

#stats figure