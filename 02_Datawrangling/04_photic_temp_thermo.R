#Maria Popescu

#this script calculates:
# 1. photic zone
# 2. temp_depths_interp (also used for schmidt stability and buoyancy frequency)
# 3. thermocline
# 4. final_photic_thermo for RF analysis
#
# Inputs:
# - secchiframe, ysi_profiles, CTD, BVRplatform, bath (from 01_DataDownload.R)
# - final_phytos (from 02_Phytos_dataframe.R)
# - helper function find_depths() sourced by 01_DataDownload.R
#
# Outputs:
# - In-memory data frame: temp_depths_interp.
# - CSVs/final_photic_thermo.csv and supporting CSV/figure outputs.
#
# Dependencies:
# - Run after 01_DataDownload.R and 02_Phytos_dataframe.R in the same session.

required_objects <- c("secchiframe", "ysi_profiles", "CTD", "BVRplatform", "bath", "final_phytos")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for 04_photic_temp_thermo.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 01_DataDownload.R and 02_Phytos_dataframe.R first.")
}

if (!exists("find_depths", mode = "function", inherits = TRUE)) {
  stop("Missing required function find_depths(). Re-run 01_DataDownload.R to source Functions/find_depths.R.")
}

library(ISOweek)


if (!dir.exists("Figs/Daily_interp_Casts")) {
  dir.create("Figs/Daily_interp_Casts", recursive = TRUE)
}

if (!dir.exists("Figs/Thermocline")) {
  dir.create("Figs/Thermocline", recursive = TRUE)
}

#### Photic zone from Secchi ####

{
  secchi_df <- secchiframe |>
    mutate(Date = as_date(DateTime)) |>
    group_by(Date, Reservoir, Site) |>
    summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE), .groups = "drop") |>
    filter(Reservoir == "BVR" & Site == 50) |>
    mutate(Year = year(Date), DOY = yday(Date))

  variables <- c("Secchi_m")
  data_availability(secchi_df, variables)

  DOY_year_ref <- expand.grid(Year = unique(secchi_df$Year), DOY = 1:366) |>
    filter(!(Year %% 4 != 0 & DOY == 366)) |>
    mutate(Date = as.Date(DOY - 1, origin = paste0(Year, "-01-01")))

  #interpolate secchi to daily resolution
  secchi_interpolated <- DOY_year_ref %>%
    mutate(
      Date = as.Date(paste(Year, DOY), format = "%Y %j")
    ) |>
    left_join(secchi_df, by = c("Year", "DOY"), suffix = c("", "_secchi")) |>
    filter(Year > 2013, DOY != 207) |>
    group_by(Year) |>
    mutate(
      first_valid_DOY = min(DOY[!is.na(Secchi_m)], na.rm = TRUE),
      last_valid_DOY  = max(DOY[!is.na(Secchi_m)], na.rm = TRUE),
      Secchi_m = ifelse(
        DOY >= first_valid_DOY & DOY <= last_valid_DOY,
        na.approx(Secchi_m, x = DOY, na.rm = FALSE),
        NA_real_
      )
    ) |>
    arrange(Year, DOY) |>
    select(Date, Year, Secchi_m) |>
    ungroup() |>
    group_by(Date) |>
    summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE))

  #K_d and photic zone from secchi (Kirk 1994)
  photic_zone_frame <- secchi_interpolated |>
    mutate(sec_K_d = 1.7/Secchi_m) |>
    mutate(PZ = 4.605 /sec_K_d)|>
    group_by(Date)|>
    filter(year(Date) >2014, year(Date) <2025)
}

#### Temperature data: choosing instruments by availability ####

#####YSI#####
ysi_profiles <- ysi_profiles|>
  filter(Reservoir == "BVR", Site == 50)|>
  mutate(Date = as_date(DateTime))
variables <- c("DO_mgL", "PAR_umolm2s", "DOsat_percent", "Cond_uScm", "ORP_mV", "pH", "Temp_C")
data_availability(ysi_profiles, variables)

#only Temp_C has sufficient coverage across years

#select cast closest to noon each day
ysi_noon <- ysi_profiles |>
  mutate(DateTime = as.POSIXct(DateTime)) |>
  filter(!is.na(DateTime)) |>
  arrange(DateTime) |>
  mutate(
    Date = as.Date(DateTime),
    cast_id = cumsum(c(TRUE, diff(as.numeric(DateTime)) > 3600))
  ) |>
  group_by(cast_id) |>
  mutate(
    cast_median = as.POSIXct(median(as.numeric(DateTime)), origin = "1970-01-01", tz = tz(DateTime)),
    noon_diff = abs(as.numeric(difftime(cast_median,
                                        as.POSIXct(paste(Date, "12:00:00"), tz = tz(DateTime)), units = "mins")))
  ) |>
  group_by(Date) |>
  filter(noon_diff == min(noon_diff)) |>
  ungroup() |>
  select(-cast_id, -cast_median, -noon_diff)

ysi <- ysi_noon|>
  select(-ORP_mV, -Cond_uScm, -pH)|>
  filter((hour(DateTime) >= 8), (hour(DateTime) <= 18))
variables <- c("Temp_C")
data_availability(ysi, variables)

#####CTD#####
CTD_noon <- CTD |>
  filter(Reservoir == "BVR", Site == 50) |>
  mutate(DateTime = as.POSIXct(DateTime)) |>
  filter(!is.na(DateTime)) |>
  arrange(DateTime) |>
  mutate(
    Date = as.Date(DateTime),
    cast_id = cumsum(c(TRUE, diff(as.numeric(DateTime)) > 3600))
  ) |>
  group_by(cast_id) |>
  mutate(
    cast_median = as.POSIXct(median(as.numeric(DateTime)), origin = "1970-01-01", tz = tz(DateTime)),
    noon_diff = abs(as.numeric(difftime(cast_median,
                                        as.POSIXct(paste(Date, "12:00:00"), tz = tz(DateTime)), units = "mins")))
  ) |>
  group_by(Date) |>
  filter(noon_diff == min(noon_diff)) |>
  ungroup() |>
  select(-cast_id, -cast_median, -noon_diff)

CTDfiltered <- CTD_noon |>
  mutate(
    Date = as_date(DateTime)
  ) |>
  filter(hour(DateTime) >= 8, hour(DateTime) <= 18)

variables <- c("DO_mgL", "PAR_umolm2s", "DOsat_percent", "Cond_uScm", "ORP_mV",
               "pH", "Temp_C")
data_availability(CTDfiltered, variables)
#Temp from CTD: 2015, 2016, 2019, 2022, 2023, 2024
#Temp from YSI: 2017, 2018, 2020
#Temp from sensorstring: 2021

CTDtemp<- CTDfiltered|>
  mutate(Year = year(Date))|>
  filter(Year %in% c(2015, 2016, 2019, 2022, 2023, 2024))|>
  select(Date, Year, Temp_C, Depth_m)

variables<- c("Temp_C")
data_availability(CTDtemp, variables)

ysitemp<- ysi%>%
  mutate(Year = year(Date))|>
  filter(Year %in% c(2017, 2018, 2020))|>
  select(Date, Year, Temp_C, Depth_m)
data_availability(ysitemp, variables)

temp_depths_coalesced <- full_join(ysitemp, CTDtemp, by = c("Date", "Year", "Depth_m"))|>
  group_by(Date)|>
  mutate(Temp_C = coalesce(Temp_C.x, Temp_C.y))|>
  ungroup()|>
  filter(Depth_m > 0.09)|>
  select(-Temp_C.y, -Temp_C.x)

variables<- c("Temp_C")
data_availability(temp_depths_coalesced, variables)

#early 2019 missing from CTD, fill from YSI before DOY 170
ysitemp2019_clean <- ysi |>
  mutate(Date = as_date(Date),
         Year = year(Date),
         DOY = yday(Date)) |>
  filter(Year == 2019, DOY < 170) |>
  select(Date, Year, Depth_m, Temp_C)

temp_depths_coalesced <- bind_rows(temp_depths_coalesced, ysitemp2019_clean) |>
  arrange(Date, Depth_m)

#####Sensorstrings (2021)#####
# BVR_Depth_offsets.csv downloaded in 01_DataDownload.R

bvrdatasensorstring <- find_depths(data_file = BVRplatform,
                                   depth_offset = "BVR_Depth_offsets.csv",
                                   output = NULL,
                                   round_digits = 2,
                                   bin_width = 0.1,
                                   wide_data = F)

bvrdatasensorstring2 <- bvrdatasensorstring |>
  filter(variable == "ThermistorTemp",
         year(DateTime) == 2021) |>
  mutate(Temp_C = observation,
         Depth_m = rounded_depth,
         Year = year(DateTime),
         Date = as.Date(DateTime),
         mins_from_noon = abs(as.numeric(difftime(DateTime,
                                                  as.POSIXct(paste(Date, "12:00:00"), tz = tz(DateTime)),
                                                  units = "mins")))) |>
  group_by(Date) |>
  filter(mins_from_noon == min(mins_from_noon)) |>
  ungroup() |>
  select(Date, DateTime, Depth_m, Temp_C)

data_availability(bvrdatasensorstring2, "Temp_C")

all_years_temp <- bind_rows(temp_depths_coalesced, bvrdatasensorstring2) |>
  arrange(Date, Depth_m)

data_availability(all_years_temp, variables)


#### Temp profile cleaning ####
#bin to 0.1m, average duplicates, flag reversal spikes (>2C direction change)
temp_depths_cleaned <- all_years_temp |>
  filter(!is.na(Temp_C)) |>
  mutate(Depth_m = floor(Depth_m * 10) / 10) |>
  group_by(Date, Depth_m) |>
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE), .groups = "drop") |>
  group_by(Date) |>
  arrange(Depth_m, .by_group = TRUE) |>
  mutate(
    temp_diff = Temp_C - lag(Temp_C),
    prev_diff = lag(temp_diff),
    reversal = !is.na(temp_diff) & !is.na(prev_diff) &
      sign(temp_diff) != sign(prev_diff) &
      abs(temp_diff) > 2
  ) |>
  filter(!reversal) |>
  ungroup() |>
  select(-temp_diff, -prev_diff, -reversal) |>
  mutate(DOY = yday(Date)) |>
  mutate(Reservoir = "BVR", Site = 50, DateTime = Date) |>
  filter(DOY >= 133, DOY <= 286)

#rolling median filter: remove points >2C from local median (window=7)
cleaned <- temp_depths_cleaned |>
  group_by(Date) |>
  arrange(Depth_m, .by_group = TRUE) |>
  mutate(
    Temp_rolling_median = rollapply(Temp_C, width = 7, FUN = median,
                                    fill = NA, align = "center"),
    temp_deviation = abs(Temp_C - Temp_rolling_median)
  ) |>
  filter(is.na(temp_deviation) | temp_deviation < 2) |>
  ungroup() |>
  select(-Temp_rolling_median, -temp_deviation)

#diagnostic: plot daily casts
for (yr in years) {
  test <- cleaned |> filter(year(Date) == yr)
  if (nrow(test) == 0) next

  plot_casts <- ggplot(test, aes(x = Temp_C, y = Depth_m)) +
    geom_path() +
    geom_point(size = 0.8, alpha = 0.8) +
    geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1),
               color = "lightblue", linetype = "dotted", linewidth = 0.3) +
    scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
    theme_bw() +
    facet_wrap(vars(Date)) +
    xlab("Temp") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "Temp Profiles"))

  ggsave(filename = paste0("Figs/Daily_interp_Casts/", yr, "_raw_casts.png"),
         plot = plot_casts, width = 12, height = 10, dpi = 300)
}

#remove incomplete 2020-09-19 cast
temp_depths_cleaned2 <- cleaned|>
  filter(!(Date == as.Date("2020-09-19")))

variables <- c("Temp_C")
temp_depths_interp <- interpolate_variable(temp_depths_cleaned2, variables)

#recheck after interpolation. temp_depths_interp is now daily, so restrict the
#diagnostic facets to Dates with an actual cast.
cast_dates <- temp_depths_cleaned2 |> distinct(Date) |> pull(Date)

for (yr in years) {
  test <- temp_depths_interp |> filter(year(Date) == yr, Date %in% cast_dates)
  if (nrow(test) == 0) next

  plot_casts <- ggplot(test, aes(x = Temp_C, y = Depth_m)) +
    geom_path() +
    geom_point(size = 0.8, alpha = 0.8) +
    geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1),
               color = "lightblue", linetype = "dotted", linewidth = 0.3) +
    scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
    theme_bw() +
    facet_wrap(vars(Date)) +
    xlab("Temp") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "Temp Profiles"))

  ggsave(filename = paste0("Figs/Daily_interp_Casts/", yr, "_raw_casts.png"),
         plot = plot_casts, width = 12, height = 10, dpi = 300)
}

#### Thermocline ####

just_thermocline <- temp_depths_interp |>
  filter(!is.na(Temp_C)) |>
  group_by(Date, Depth_m) |>
  summarize(Temp_C = mean(Temp_C, na.rm = TRUE), .groups = "drop") |>
  group_by(Date) |>
  group_modify(~ {
    max_depth <- max(.x$Depth_m, na.rm = TRUE)

    #tunable thresholds for shallow reservoir thermocline detection
    shallow_threshold <- 0.2
    deep_filter       <- 0.25
    reject_shallow    <- 0.15
    reject_deep       <- 0.85
    smin_default      <- 1
    smin_relaxed      <- 0.5

    #first pass
    thermocline_depth <- thermo.depth(
      .x$Temp_C,
      .x$Depth_m,
      Smin = smin_default,
      seasonal = TRUE,
      index = FALSE,
      mixed.cutoff = deep_filter * max_depth
    )

    method <- "standard"

    #if too shallow, recalculate ignoring surface
    if (!is.na(thermocline_depth) && thermocline_depth < shallow_threshold * max_depth) {
      .x_deep <- .x |> filter(Depth_m > deep_filter * max_depth)

      if (nrow(.x_deep) >= 3) {
        thermocline_depth <- tryCatch(
          thermo.depth(
            .x_deep$Temp_C,
            .x_deep$Depth_m,
            Smin = smin_relaxed,
            seasonal = TRUE,
            index = FALSE
          ),
          error = function(e) NA_real_
        )
        method <- "deep_recalc"
      }
    }

    #reject implausibly shallow results, try mid-column
    if (!is.na(thermocline_depth) &&
        thermocline_depth < reject_shallow * max_depth) {
      .x_mid <- .x |> filter(Depth_m > reject_shallow * max_depth,
                             Depth_m < reject_deep * max_depth)
      if (nrow(.x_mid) >= 3) {
        thermocline_depth <- tryCatch(
          thermo.depth(
            .x_mid$Temp_C,
            .x_mid$Depth_m,
            Smin = smin_relaxed,
            seasonal = TRUE,
            index = FALSE
          ),
          error = function(e) NA_real_
        )
        method <- "reject_recalc_shallow"
      } else {
        thermocline_depth <- NA_real_
        method <- "rejected"
      }
    }

    if (!is.na(thermocline_depth) &&
        thermocline_depth > reject_deep * max_depth) {
      .x_upper <- .x |> filter(Depth_m < reject_deep * max_depth)
      if (nrow(.x_upper) >= 3) {
        thermocline_depth <- tryCatch(
          thermo.depth(
            .x_upper$Temp_C,
            .x_upper$Depth_m,
            Smin = smin_relaxed,
            seasonal = TRUE,
            index = FALSE
          ),
          error = function(e) NA_real_
        )
        method <- "reject_recalc_deep"
      } else {
        thermocline_depth <- NA_real_
        method <- "rejected"
      }
    }

    tibble(thermocline_depth = thermocline_depth, method = method)
  }) |>
  ungroup() |>
  mutate(Year = year(Date))

#####diagnostic: thermocline overlaid on profiles####
thermocline_and_depth_profiles <- temp_depths_interp|>
  left_join(just_thermocline, by = c("Date"))|>
  group_by(Date)|>
  fill(thermocline_depth, .direction = "updown")|>
  ungroup()|>
  mutate(year = year(Date))

for (yr in years) {
  test <- thermocline_and_depth_profiles |>
    filter(year(Date) == yr, Date %in% cast_dates)
  if (nrow(test) == 0) next

  plot_casts <- ggplot(test, aes(x = Temp_C, y = Depth_m)) +
    geom_path() +
    geom_point(size = 0.8, alpha = 0.8) +
    geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1),
               color = "lightblue", linetype = "dotted", linewidth = 0.3) +
    geom_hline(aes(yintercept = thermocline_depth), linetype = "dashed", color = "red") +
    scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
    theme_bw() +
    facet_wrap(vars(Date)) +
    xlab("Temp") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "Temp Profiles"))+
    geom_text(aes(label = round(thermocline_depth, 1), x = Inf, y = thermocline_depth),
              color = "black", hjust = 1.1, size = 3)

  ggsave(filename = paste0("Figs/Thermocline/", yr, "_raw_casts.png"),
         plot = plot_casts, width = 12, height = 10, dpi = 300)
}

#Daily average for analysis (may-october window only)
just_thermocline <- just_thermocline|>
  group_by(Date)|>
  summarise(thermocline_depth = mean(thermocline_depth, na.rm = TRUE))

#### Surface temp and temp at DCM ####
surface_temp <- temp_depths_interp |>
  filter(Depth_m == 0.5) |>
  group_by(Date) |>
  summarise(surface_temp = mean(Temp_C, na.rm = TRUE), .groups = "drop")

#temp_depths_interp is daily, so each phyto Date matches exactly. Pick the
#interpolated depth closest to that Date's DCM_depth.
temp_at_DCM <- temp_depths_interp |>
  inner_join(final_phytos |> select(Date, DCM_depth), by = "Date") |>
  filter(!is.na(DCM_depth)) |>
  group_by(Date) |>
  slice_min(abs(Depth_m - DCM_depth), n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(Date, temp_at_DCM = Temp_C)

write.csv(temp_at_DCM, "CSVs/temp_at_DCM.csv", row.names = FALSE)

#water_level has 130 depth-bin rows per Date with the same WaterLevel_m;
#collapse to one row per Date before joining.
water_level_daily <- water_level |> distinct(Date, WaterLevel_m)

final_photic_thermo <- photic_zone_frame |>
  ungroup() |>
  full_join(just_thermocline,  by = "Date") |>
  full_join(water_level_daily, by = "Date") |>
  full_join(surface_temp,      by = "Date") |>
  full_join(temp_at_DCM,       by = "Date") |>
  mutate(
    PZ      = pmin(PZ, WaterLevel_m),
    PZ_prop = PZ / WaterLevel_m
  ) |>
  select(-Secchi_m, -sec_K_d) |>
  filter(if_any(-Date, ~ !is.na(.x)))

ggplot(final_photic_thermo, aes(x = Date)) +
  geom_line(aes(y = WaterLevel_m), color = "blue") +
  geom_line(aes(y = PZ), color = "red")

write.csv(final_photic_thermo, "CSVs/final_photic_thermo.csv", row.names = FALSE)

