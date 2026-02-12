#EDI met data goes from mid 2015 - 2024
#NLDAS from Heather Wander goes until 2021
#this script down scales the NLDAS data and joins it to the EDI met data

#1 bring in EDI met data and summarise it for weekly averages
#2 bring in NLDAS met data and summarise it for weekly averages
#3 regression lm for NLDAS and EDI
#4 Downscale
#5 Merge full dataset
#6 create lagged variables
#7 visualize 2015 to check new data 
#8 Plot all years
#9 export the data as a csv for use in machine learning 


pacman::p_load(tidyverse, lubridate, RColorBrewer, lubridate,
               dplyr, tidyr, ggplot2, ggthemes, patchwork)

####1 EDI met data####
EDImetC <- EDImet %>%
  select(
    Reservoir,
    Site,
    DateTime,
    Rain_Total_mm,
    WindSpeed_Average_m_s,
    AirTemp_C_Average,
    Flag_WindSpeed_Average_m_s,
    Flag_AirTemp_C_Average,
    Flag_Rain_Total_mm
  )

#Weekly values
# 1) Add date/year/week
EDImet_wk <- EDImetC |>
  mutate(
    Date = as_date(DateTime),
    Year = year(DateTime),
    Week = isoweek(Date)   # use week(Date) if you prefer; isoweek is usually more consistent
  )

# 2) Compute missingness per (Year, Week), drop weeks with >10% missing
wk_qc <- EDImet_wk |>
  group_by(Year, Week) |>
  summarise(
    n_rows = n(),
    
    # % missing for each field used downstream
    miss_dt   = mean(is.na(DateTime)),
    miss_rain = mean(is.na(Rain_Total_mm)),
    miss_tair = mean(is.na(AirTemp_C_Average)),
    miss_wind = mean(is.na(WindSpeed_Average_m_s)),
    
    # worst-case missingness across required fields
    miss_any_required = pmax(miss_dt, miss_rain, miss_tair, miss_wind),
    
    drop_week = miss_any_required > 0.10,
    .groups = "drop"
  )

# 3) Print dropped weeks per year
dropped <- wk_qc |>
  filter(drop_week) |>
  transmute(
    Year, Week,
    pct_missing = round(100 * miss_any_required, 1)
  ) |>
  arrange(Year, Week)

if (nrow(dropped) == 0) {
  message("No weeks dropped (no weeks exceeded 10% missing data).")
} else {
  dropped |>
    group_by(Year) |>
    summarise(
      dropped_weeks = paste0(Week, " (", pct_missing, "%)", collapse = ", "),
      .groups = "drop"
    ) |>
    print(n = Inf)
}

#these weeks are not within the time frame that we are analyzing so it's ok!


# 4) Keep only good weeks and compute weekly summaries
EDImet_0 <- EDImet_wk |>
  inner_join(wk_qc |> filter(!drop_week) |> select(Year, Week), by = c("Year", "Week")) |>
  group_by(Year, Week) |>
  summarise(
    Date = min(Date, na.rm = TRUE),
    precip_weekly = sum(Rain_Total_mm, na.rm = TRUE),
    weekly_airtempavg = mean(AirTemp_C_Average, na.rm = TRUE),
    WindSpeed_Weekly_Average_m_s = mean(WindSpeed_Average_m_s, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(EDImet_0, "CSVs/EDImet0.csv")

####2 NLDAS met data####

#Heather Wander generated data
#for full repository https://github.com/hlwander/interannual_zoops/tree/a6f8b9f2fb2cacf09e5994eb7c0f73435cce9b89
heathergeneratedNLDAS <- "https://raw.githubusercontent.com/hlwander/interannual_zoops/a6f8b9f2fb2cacf09e5994eb7c0f73435cce9b89/inputs/BVR_GLM_NLDAS_010113_123121_GMTadjusted.csv"
NLDAS <- read.csv(heathergeneratedNLDAS)
write.csv(NLDAS, "CSVs/NLDAS.csv", row.names = FALSE)
#heather's precipitation is given in m per day
#windspeed in m per day
#air temp is hourly

NLDAS <- read.csv("CSVs/NLDAS.csv")
  
  # ---- 1) Add Date/Year/Week + unit conversions (rowwise, before weekly QC) ----
  NLDAS_wk <- NLDAS %>%
    mutate(
      Date = as_date(time),
      Year = year(Date),
      Week = isoweek(Date),        # swap to week(Date) if you prefer
      Rain = Rain * 1000,          # m -> mm (to match EDI)
      WindSpeed = WindSpeed / 86400 # m/day -> m/s (to match EDI)
    )
  
  # ---- 2) Compute missingness per variable by (Year, Week) ----
  wk_miss <- NLDAS_wk %>%
    group_by(Year, Week) %>%
    summarise(
      n_rows = n(),
      miss_rain = mean(is.na(Rain)),
      miss_tair = mean(is.na(AirTemp)),
      miss_wind = mean(is.na(WindSpeed)),
      .groups = "drop"
    ) %>%
    mutate(
      bad_rain = miss_rain > 0.10,
      bad_tair = miss_tair > 0.10,
      bad_wind = miss_wind > 0.10
    )
  
  # ---- 3) Report which weeks were "removed" (NA’d) per year, per variable ----
  report_one <- function(df, flag_col, label) {
    df %>%
      filter(.data[[flag_col]]) %>%
      transmute(
        Year, Week,
        pct_missing = round(100 * .data[[sub("bad_", "miss_", flag_col)]], 1),
        variable = label
      ) %>%
      arrange(Year, Week)
  }
  
  removed_tbl <- bind_rows(
    wk_miss %>% filter(bad_rain) %>% transmute(Year, Week, variable = "Rain (mm)", pct_missing = round(100*miss_rain, 1)),
    wk_miss %>% filter(bad_tair) %>% transmute(Year, Week, variable = "AirTemp",   pct_missing = round(100*miss_tair, 1)),
    wk_miss %>% filter(bad_wind) %>% transmute(Year, Week, variable = "WindSpeed (m/s)", pct_missing = round(100*miss_wind, 1))
  )
  
  if (nrow(removed_tbl) == 0) {
    message("No (Year, Week) exceeded 10% missingness for any variable.")
  } else {
    removed_tbl %>%
      group_by(Year, variable) %>%
      summarise(
        weeks_removed = paste0(Week, " (", pct_missing, "%)", collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(Year, variable) %>%
      print(n = Inf)
  }
  
  # ---- 4) Join flags back + set only the bad-variable values to NA within those weeks ----
  NLDAS_qc <- NLDAS_wk %>%
    left_join(wk_miss %>% select(Year, Week, bad_rain, bad_tair, bad_wind), by = c("Year", "Week")) %>%
    mutate(
      Rain = if_else(bad_rain, NA_real_, Rain),
      AirTemp = if_else(bad_tair, NA_real_, AirTemp),
      WindSpeed = if_else(bad_wind, NA_real_, WindSpeed)
    )
  
  # ---- 5) Weekly summaries (weeks still exist; some variables may be NA for that week) ----
  NLDAS_0 <- NLDAS_qc %>%
    group_by(Year, Week) %>%
    summarise(
      Date = min(Date, na.rm = TRUE),
      precip_weekly = sum(Rain, na.rm = TRUE),
      weekly_airtempavg = mean(AirTemp, na.rm = TRUE),
      WindSpeed_Weekly_Average_m_s = mean(WindSpeed, na.rm = TRUE),
      .groups = "drop"
    )
  
####3. Build regressions for downscaling####
# overlap data
overlap <- inner_join(
  EDImet_0 |> 
    select(Year, Week, EDI_airtemp = weekly_airtempavg,
           EDI_precip = precip_weekly,
           EDI_wind   = WindSpeed_Weekly_Average_m_s),
  NLDAS_0 |> 
    select(Year, Week, NLDAS_airtemp = weekly_airtempavg,
           NLDAS_precip = precip_weekly,
           NLDAS_wind   = WindSpeed_Weekly_Average_m_s),
  by = c("Year", "Week")
)

# fit regression models
model_temp   <- lm(EDI_airtemp ~ NLDAS_airtemp, data = overlap)
model_precip <- lm(EDI_precip ~ NLDAS_precip, data = overlap)
model_wind   <- lm(EDI_wind   ~ NLDAS_wind,   data = overlap)

####4. Downscale NLDAS for missing early 2015####
NLDAS_2015fill <- NLDAS_0 |>
  filter(Year == 2015) |>
  anti_join(EDImet_0, by = c("Year", "Week")) |>  # only missing weeks
  mutate(
    weekly_airtempavg = predict(model_temp, 
                                newdata = data.frame(NLDAS_airtemp = weekly_airtempavg)),
    precip_weekly = predict(model_precip, 
                            newdata = data.frame(NLDAS_precip = precip_weekly)),
    WindSpeed_Weekly_Average_m_s = predict(model_wind, 
                                           newdata = data.frame(NLDAS_wind = WindSpeed_Weekly_Average_m_s))
  ) |>
  select(Year, Week, Date, precip_weekly, weekly_airtempavg, WindSpeed_Weekly_Average_m_s)

####5. Merge full dataset####
full_met <- bind_rows(EDImet_0, NLDAS_2015fill) |>
  arrange(Year, Week)

####6. Lagged variables####
full_met <- full_met |>
  group_by(Year) |> 
  arrange(Week) |> 
  mutate(
    airtemp_lag1 = lag(weekly_airtempavg, 1),
    precip_lag1  = lag(precip_weekly, 1),
    wind_lag1    = lag(WindSpeed_Weekly_Average_m_s, 1), 
    airtemp_lag2 = lag(weekly_airtempavg, 2),
    precip_lag2  = lag(precip_weekly, 2),
    wind_lag2    = lag(WindSpeed_Weekly_Average_m_s, 2)
  ) |> 
  ungroup()

####7. Quick check: plot 2015####
p1 <- ggplot(full_met |> filter(Year == 2015), 
             aes(Date, weekly_airtempavg)) +
  geom_line(color="red") +
  labs(title="Weekly Air Temp (2015)", y="°C")

p2 <- ggplot(full_met |> filter(Year == 2015), 
             aes(Date, precip_weekly)) +
  geom_line(color="blue") +
  labs(title="Weekly Precip (2015)", y="mm")

p3 <- ggplot(full_met |> filter(Year == 2015), 
             aes(Date, WindSpeed_Weekly_Average_m_s)) +
  geom_line(color="green") +
  labs(title="Weekly Wind Speed (2015)", y="m/s")

joined_data_2015 <- (p1 / p2 / p3) + plot_layout(heights = c(1,1,1)) 
print(joined_data_2015)
if (!dir.exists("Figs/metdata")) {
  dir.create("Figs/metdata", recursive = TRUE)
}


####8. Plot all years for each variable just for QAQC check####
# 1. Precipitation plot
p1 <- ggplot(full_met, aes(x = Week, y = precip_weekly, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly Precipitation", y = "Precipitation (mm)", color = "Year") +
  theme_minimal()

# 2. Weekly Air Temperature plot
p2 <- ggplot(full_met, aes(x = Week, y = weekly_airtempavg, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly Air Temperature", y = "Air Temp (°C)", color = "Year") +
  theme_minimal()

# 3. Wind Speed Weekly Average plot
p3 <- ggplot(full_met, aes(x = Week, y = WindSpeed_Weekly_Average_m_s, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly Wind Speed Average", y = "Wind Speed (m/s)", color = "Year") +
  theme_minimal()

combined_plot <- (p1 | p2) / (p3)
print(combined_plot)
ggsave("Figs/metdata/NLDAS.png", combined_plot, width = 12, height = 8, dpi = 300)

#checking to make sure I have full coverage for the time frame we are looking at 
variables <- c("precip_weekly","weekly_airtempavg","WindSpeed_Weekly_Average_m_s")
data_availability(full_met, variables)


####9. Export as CSV####
write.csv(full_met, "CSVs/final_metdata.csv", row.names = FALSE)

#checking within year and across year variability
var_summary <- full_met %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(WindSpeed_Weekly_Average_m_s, na.rm = TRUE),
    sd_within = sd(WindSpeed_Weekly_Average_m_s, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )

across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across


#now for air temp
var_summary <- full_met %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(airtemp_lag2, na.rm = TRUE),
    sd_within = sd(airtemp_lag2, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )

across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across
