#Maria Popescu

#This script quantifies how far the interpolated variables used in the RF
#analysis sit from the original (raw) measurements that produced them.
#
#For every variable that is interpolated and then summarized onto the
#phytoplankton cast Dates (full_data.csv), it reports:
# 1. which used analysis Dates are interpolated (gap > 0 to nearest raw obs)
#    vs directly measured (gap == 0)
# 2. the staleness gap (days from each used Date to the NEAREST raw measurement
#    of that variable, within the same year) - median / min / max
# 3. how many used Dates are more than 7 days from the nearest raw measurement
#
#Interpolation is always within-year and bounded to the first/last valid day
#of data in that year (see Functions/interpolate_variable.R), so gaps are
#computed within each calendar year and used Dates outside a year's data range
#are NA (not used).
#
#Inputs (all written by earlier scripts, so this runs standalone after them):
# - CSVs/full_data.csv      (final analysis frame, one row per cast Date)
# - CSVs/chemistry.csv, CSVs/metalsdf.csv, CSVs/secchiframe.csv,
#   CSVs/CTD.csv, CSVs/ysi_profiles.csv, CSVs/BVRplatform.csv, CSVs/wtrlvl.csv
#
#Outputs:
# - CSVs/interpolation_gap_summary.csv   (one row per variable)
# - CSVs/interpolation_gap_by_date.csv   (one row per used Date x variable)
# - Figs/Data_availability/interpolation_gap_summary.png      (rendered table)
# - Figs/Data_availability/interpolation_availability_panels.png (paneled plot)

library(dplyr)
library(lubridate)
library(tidyr)
library(gridExtra)
library(grid)
library(ggplot2)

full <- read.csv("CSVs/full_data.csv") |> mutate(Date = as.Date(Date))

#raw measurement Dates feeding each interpolated variable (BVR, Site 50).
#Temperature is stitched from CTD/YSI/sensorstring by year exactly as in
#04_photic_temp_thermo.R; water level from staff gauge + platform as in
#01_water_level.R.
read_raw <- function(path) read.csv(path)

chem <- read_raw("CSVs/chemistry.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10))) |>
  filter(Reservoir == "BVR", Site == 50)

metals <- read_raw("CSVs/metalsdf.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10))) |>
  filter(Reservoir == "BVR", Site == 50)

secchi <- read_raw("CSVs/secchiframe.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10))) |>
  filter(Reservoir == "BVR", Site == 50, !is.na(Secchi_m))

ctd <- read_raw("CSVs/CTD.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10))) |>
  filter(Reservoir == "BVR", Site == 50, !is.na(Temp_C))

ysi <- read_raw("CSVs/ysi_profiles.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10))) |>
  filter(Reservoir == "BVR", Site == 50, !is.na(Temp_C))

bvr <- read_raw("CSVs/BVRplatform.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10)))

wtrlvl <- read_raw("CSVs/wtrlvl.csv") |>
  mutate(Date = as.Date(substr(DateTime, 1, 10))) |>
  filter(Reservoir == "BVR", Site == 50, !is.na(WaterLevel_m))

#temperature raw cast Dates by the instrument used each year
temp_dates <- sort(unique(c(
  ctd |> filter(year(Date) %in% c(2015, 2016, 2019, 2022, 2023, 2024)) |> pull(Date),
  ysi |> filter(year(Date) %in% c(2017, 2018, 2020, 2019))           |> pull(Date),
  bvr |> filter(year(Date) == 2021, !is.na(ThermistorTemp_C_5))      |> pull(Date)
)))

#water level raw Dates: staff gauge (<=2020 era) + platform pressure sensor
wl_dates <- sort(unique(c(
  wtrlvl$Date,
  bvr |> filter(!is.na(LvlDepth_m_13)) |> pull(Date)
)))

#variable registry: the column in full_data that flags a used Date, and the
#raw measurement Dates behind it
registry <- list(
  list(var = "SRP_ugL",      used_col = "SRP_ugL_at_DCM",
       raw = chem |> filter(!is.na(SRP_ugL))      |> pull(Date) |> unique()),
  list(var = "NH4_ugL",      used_col = "NH4_ugL_at_DCM",
       raw = chem |> filter(!is.na(NH4_ugL))      |> pull(Date) |> unique()),
  list(var = "NO3NO2_ugL",   used_col = "NO3NO2_ugL_at_DCM",
       raw = chem |> filter(!is.na(NO3NO2_ugL))   |> pull(Date) |> unique()),
  list(var = "SFe_mgL",      used_col = "SFe_mgL_at_DCM",
       raw = metals |> filter(!is.na(SFe_mgL))    |> pull(Date) |> unique()),
  list(var = "Secchi_PZ",    used_col = "PZ",            raw = unique(secchi$Date)),
  list(var = "Temp_C",       used_col = "temp_at_DCM",   raw = temp_dates),
  list(var = "WaterLevel_m", used_col = "WaterLevel_m",  raw = wl_dates)
)

#staleness: days from a used Date to the nearest raw obs in the same year
gap_to_nearest <- function(used_dates, raw_dates) {
  raw_dates <- sort(unique(raw_dates))
  vapply(used_dates, function(d) {
    same_yr <- raw_dates[year(raw_dates) == year(d)]
    if (length(same_yr) == 0) return(NA_real_)
    min(abs(as.numeric(d - same_yr)))
  }, numeric(1))
}

#build the per-Date detail table
by_date <- lapply(registry, function(e) {
  used <- full$Date[!is.na(full[[e$used_col]])]
  tibble(
    variable = e$var,
    Date     = used,
    Year     = year(used),
    gap_days = gap_to_nearest(used, e$raw)
  )
}) |> bind_rows() |>
  filter(!is.na(gap_days)) |>
  mutate(interpolated = gap_days > 0) |>
  arrange(variable, Date)

write.csv(by_date, "CSVs/interpolation_gap_by_date.csv", row.names = FALSE)

#per-variable summary
summary_tbl <- by_date |>
  group_by(variable) |>
  summarise(
    n_used          = n(),
    n_interpolated  = sum(interpolated),
    pct_interpolated = round(100 * mean(interpolated), 1),
    median_gap_days = median(gap_days),
    min_gap_days    = min(gap_days),
    max_gap_days    = max(gap_days),
    max_gap_date    = Date[which.max(gap_days)],
    n_gap_gt7       = sum(gap_days > 7),
    .groups = "drop"
  ) |>
  arrange(desc(max_gap_days))

write.csv(summary_tbl, "CSVs/interpolation_gap_summary.csv", row.names = FALSE)

#render the summary as a table figure for the manuscript
if (!dir.exists("Figs/Data_availability")) {
  dir.create("Figs/Data_availability", recursive = TRUE)
}

table_for_fig <- summary_tbl |>
  transmute(
    Variable          = variable,
    `Used dates`      = n_used,
    `Interpolated`    = n_interpolated,
    `% interp`        = pct_interpolated,
    `Median gap (d)`  = median_gap_days,
    `Min gap (d)`     = min_gap_days,
    `Max gap (d)`     = max_gap_days,
    `Max gap date`    = as.character(max_gap_date),
    `# dates >7d`     = n_gap_gt7
  )

tt <- ttheme_minimal(
  core    = list(fg_params = list(cex = 0.8),
                 bg_params = list(fill = c("white", "grey95"), col = NA)),
  colhead = list(fg_params = list(cex = 0.85, fontface = "bold"),
                 bg_params = list(fill = "grey85", col = NA))
)

tg <- tableGrob(table_for_fig, rows = NULL, theme = tt)
title <- textGrob(
  "Interpolation gaps for variables used in the RF analysis",
  gp = gpar(fontsize = 14, fontface = "bold"))
subtitle <- textGrob(
  paste("gap = days from a used cast Date to the nearest raw measurement;",
        "# dates >7d = used Dates more than a week from any real measurement"),
  gp = gpar(fontsize = 9, col = "grey30"))

table_fig <- gtable::gtable_add_rows(tg, heights = grobHeight(title) + unit(6, "mm"), pos = 0)
table_fig <- gtable::gtable_add_grob(table_fig, title, t = 1, l = 1, r = ncol(tg))
table_fig <- gtable::gtable_add_rows(table_fig, heights = grobHeight(subtitle) + unit(4, "mm"), pos = 1)
table_fig <- gtable::gtable_add_grob(table_fig, subtitle, t = 2, l = 1, r = ncol(tg))

ggplot2::ggsave(
  "Figs/Data_availability/interpolation_gap_summary.png",
  table_fig, width = 13, height = 3.2, dpi = 300, bg = "white")

#### Paneled availability plot ####
#One panel per variable (DOY x Year, like the FluoroProbe availability figure):
# black points   = raw measurement availability for that variable
# red points     = FluoroProbe casts actually fed to a Random Forest for this var
# blue open tri  = used Date where raw data existed that same Date (gap == 0)
# orange open tri= used Date whose value had to be interpolated (gap > 0)
#
#"Used" = an RF model input. A Date counts for a variable only if it survives the
#exact RF cleaning in 03_Machine_learning/RF_and_SHAP.R (drop columns >25% NA,
#then complete cases) for a model whose predictors include a column derived from
#that variable. Interpolated values that never reach a model are NOT shown.
plot_years <- 2015:2024
var_levels <- vapply(registry, function(e) e$var, character(1))

lvl <- c("Raw data available", "FluoroProbe cast (used)",
         "Available (not interpolated)", "Interpolated")

#replicate the RF row-cleaning to find which Dates each model actually fits on,
#and which predictor columns survive the >25% NA drop
rf_used <- function(df, response) {
  d <- df |> mutate(Date = as.Date(Date), Year = year(Date)) |>
    filter(Year >= min(plot_years), Year <= max(plot_years))
  keep <- d |> select(-Date, -Year) |> select(where(is.numeric)) |> names()
  m <- d |> select(Date, Year, all_of(response), any_of(setdiff(keep, response))) |>
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  na_frac   <- vapply(m, function(x) mean(is.na(x)), numeric(1))
  drop_cols <- setdiff(names(na_frac)[na_frac > 0.25], c("Date", "Year", response))
  m <- m |> select(-any_of(drop_cols)) |> tidyr::drop_na()
  list(dates = as.Date(m$Date),
       cols  = setdiff(names(m), c("Date", "Year", response)))
}

depth_rf <- rf_used(
  full |> filter(max_conc > 20) |>
    select(Date, DCM_depth, PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
           depth_NH4_ugL_max, depth_NO3NO2_ugL_max, depth_SRP_ugL_max, depth_SFe_mgL_max,
           air_temp_week_avg, wind_week_avg, precip_week_sum),
  "DCM_depth")

mag_rf <- rf_used(
  full |>
    select(Date, max_conc, WaterLevel_m, PZ, schmidt_stability, thermocline_depth, N_at_DCM,
           SFe_mgL_at_DCM, SRP_ugL_at_DCM, NH4_ugL_at_DCM, NO3NO2_ugL_at_DCM,
           air_temp_week_avg, wind_week_avg, precip_week_sum),
  "max_conc")

#panel variable -> the RF predictor column(s) it feeds, per model
var_model_cols <- list(
  SRP_ugL      = list(depth = "depth_SRP_ugL_max",    mag = "SRP_ugL_at_DCM"),
  NH4_ugL      = list(depth = "depth_NH4_ugL_max",    mag = "NH4_ugL_at_DCM"),
  NO3NO2_ugL   = list(depth = "depth_NO3NO2_ugL_max", mag = "NO3NO2_ugL_at_DCM"),
  SFe_mgL      = list(depth = "depth_SFe_mgL_max",    mag = "SFe_mgL_at_DCM"),
  Secchi_PZ    = list(depth = "PZ",                   mag = "PZ"),
  Temp_C       = list(depth = c("thermocline_depth", "schmidt_stability"),
                      mag   = c("thermocline_depth", "schmidt_stability", "N_at_DCM")),
  WaterLevel_m = list(depth = "WaterLevel_m",         mag = "WaterLevel_m")
)

#Dates on which a variable is actually an RF input (union across the models that
#kept a column derived from it)
rf_used_dates <- function(varname) {
  m <- var_model_cols[[varname]]
  sort(unique(c(
    if (any(m$depth %in% depth_rf$cols)) depth_rf$dates,
    if (any(m$mag   %in% mag_rf$cols))   mag_rf$dates
  )))
}

avail_long <- lapply(registry, function(e) {
  used <- rf_used_dates(e$var)
  used <- used[year(used) %in% plot_years]
  gaps <- gap_to_nearest(used, e$raw)          #classify each used Date
  bind_rows(
    #raw availability stays full (context layer), not restricted to RF inputs
    tibble(Date = as.Date(e$raw), variable = e$var, layer = "Raw data available"),
    tibble(Date = used, variable = e$var, layer = "FluoroProbe cast (used)"),
    tibble(Date = used, variable = e$var,
           layer = ifelse(gaps > 0, "Interpolated", "Available (not interpolated)"))
  )
}) |>
  bind_rows() |>
  mutate(Year = year(Date), DayOfYear = yday(Date)) |>
  filter(Year %in% plot_years) |>
  mutate(
    layer    = factor(layer, levels = lvl),
    variable = factor(variable, levels = var_levels),
    Year     = factor(Year, levels = as.character(plot_years))
  ) |>
  #draw order (bottom -> top): red casts at the very back, then the open
  #triangles, then the black raw points on top
  arrange(match(layer, c("FluoroProbe cast (used)",
                         "Available (not interpolated)",
                         "Interpolated",
                         "Raw data available")))

avail_panels <- ggplot(avail_long,
                       aes(x = DayOfYear, y = Year,
                           shape = layer, color = layer, size = layer)) +
  #thin line tracing each year's raw sampling sequence
  geom_line(data = ~ filter(.x, layer == "Raw data available"),
            mapping = aes(x = DayOfYear, y = Year, group = Year),
            color = "grey75", linewidth = 0.3, inherit.aes = FALSE) +
  geom_vline(xintercept = c(133, 286), linetype = "dashed", color = "red") +
  geom_point(stroke = 0.8, fill = NA) +
  facet_wrap(~ variable, ncol = 2) +
  #open shapes everywhere except the filled black raw points (shape 16);
  #red = open circle (1), triangles = open up-triangle (2)
  scale_shape_manual(values = c("Raw data available" = 16,
                                "FluoroProbe cast (used)" = 1,
                                "Available (not interpolated)" = 2,
                                "Interpolated" = 2)) +
  scale_color_manual(values = c("Raw data available" = "black",
                                "FluoroProbe cast (used)" = "red",
                                "Available (not interpolated)" = "blue",
                                "Interpolated" = "orange")) +
  #red circle largest so it reads as a ring behind the triangles
  scale_size_manual(values = c("Raw data available" = 1.2,
                               "FluoroProbe cast (used)" = 4.0,
                               "Available (not interpolated)" = 2.5,
                               "Interpolated" = 2.5)) +
  scale_x_continuous(breaks = seq(1, 365, by = 30), limits = c(91, 331)) +
  labs(x = "Day of Year", y = "Year", shape = NULL, color = NULL, size = NULL,
       title = "Interpolation vs. raw data availability for analysis variables") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))

ggsave("Figs/Data_availability/interpolation_availability_panels.png",
       avail_panels, width = 13, height = 12, dpi = 300, bg = "white")

#console report
cat("\n==== Interpolation gap summary (used analysis Dates) ====\n")
cat("gap_days   = days from a used Date to the NEAREST raw measurement (staleness)\n")
cat("n_gap_gt7  = used Dates more than 7 days from any real measurement\n\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

biggest <- summary_tbl |> slice_max(max_gap_days, n = 1)
cat(sprintf(
  "\nBiggest staleness gap: %s = %d days (used Date %s)\n\n",
  biggest$variable, biggest$max_gap_days, as.character(biggest$max_gap_date)))

cat("Dates carrying interpolated (gap > 0) values, by variable:\n")
by_date |>
  filter(interpolated) |>
  group_by(variable) |>
  summarise(interpolated_dates = paste(sprintf("%s(%dd)", Date, gap_days),
                                       collapse = ", "), .groups = "drop") |>
  rowwise() |>
  group_walk(~ cat(sprintf("\n%s:\n  %s\n", .x$variable, .x$interpolated_dates)))
