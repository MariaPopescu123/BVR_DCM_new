#Maria Popescu

#this script calculates:
# 1. photic zone
# 2. temp_depths_interp dataframe that will later also be used to calculate schdmit stability and buoyancy frequency
# 3. thermocline
# 4. and then produces the data frame final_photic_thermo which will be used in RF analysis

library(ISOweek)

#will save figures to these files
if (!dir.exists("Figs/Daily_interp_Casts")) {
  dir.create("Figs/Daily_interp_Casts", recursive = TRUE)
}

if (!dir.exists("Figs/Daily_interp_Casts")) {
  dir.create("Figs/Daily_interp_Casts", recursive = TRUE)
}

#### secchi PZ  ####
#checking for secchi data availability  

{
  secchi_df <- secchiframe |>
    mutate(Date = as_date(DateTime)) |>
    group_by(Date, Reservoir, Site) |>
    summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE), .groups = "drop") |>
    filter(Reservoir == "BVR" & Site == 50) |>
    mutate(Year = year(Date), DOY = yday(Date))
  
  variables <- c("Secchi_m")
  
  data_availability(secchi_df, variables) #see how much raw secchi data is available
  
  # Ensure DOY_year_ref contains all DOY values for each Year
  DOY_year_ref <- expand.grid(Year = unique(secchi_df$Year), DOY = 1:366) |> # Handle leap years
    filter(!(Year %% 4 != 0 & DOY == 366)) |>  # Remove DOY 366 for non-leap years
    mutate(Date = as.Date(DOY - 1, origin = paste0(Year, "-01-01")))  # Ensure Date matches DOY
  
  # Perform interpolation so that we have secchi for each day
  secchi_interpolated <- DOY_year_ref %>%
    mutate(Week = week(Date))|>
    left_join(secchi_df, by = c("Year", "DOY")) %>%
    filter(Year > 2013, DOY != 207) %>% #filter out incorrect secchi obseration (doesn't make sense) 
    group_by(Year) %>%
    mutate(
      first_valid_DOY = min(DOY[!is.na(Secchi_m)], na.rm = TRUE),
      last_valid_DOY = max(DOY[!is.na(Secchi_m)], na.rm = TRUE),
      Secchi_m = ifelse(
        DOY >= first_valid_DOY & DOY <= last_valid_DOY,
        na.approx(Secchi_m, x = DOY, na.rm = FALSE),
        NA_real_
      )
    ) |>
    arrange(Year, DOY) |>
    select(Year, Week, Secchi_m)|>
    ungroup()|>
    group_by(Year ,Week)|>
    summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE))
  
  
  # Adding Secchi
  frame_weeks <- read.csv("CSVs/frame_weeks.csv")
  weekly_secchi <- frame_weeks|> #RF frame w metals and secchi
    left_join(secchi_interpolated, by = c("Year", "Week"))
  
  
  # Calculating K_d and light availability from secchi
  
  photic_zone_frame <- weekly_secchi |> #add light
    mutate(sec_K_d = 1.7/Secchi_m) |>
    mutate(PZ = 4.065 /sec_K_d)|>
    group_by(Year, Week)|>
    mutate(PZ = if_else(PZ > 9.0, 9.0, PZ))|>
    filter(Year >2014, Year <2025)
}

photic_zone_frame$Date <- ISOweek2date(paste0(photic_zone_frame$Year, "-W", sprintf("%02d", photic_zone_frame$Week), "-1"))
write.csv(photic_zone_frame, "CSVs/photic_zone_frame.csv", row.names = FALSE)

#diagnostic plot of photic zone depth from 2014-2025
ggplot(photic_zone_frame, aes(x = Date, y = PZ)) +
  geom_line(aes(group = factor(year(Date)))) +
  scale_y_reverse()

####choosing data based on data availability####

#####YSI#####
ysi_profiles <- ysi_profiles|>
  filter(Reservoir == "BVR", Site == 50)|>
  mutate(Date = as_date(DateTime))
variables <- c("DO_mgL", "PAR_umolm2s", "DOsat_percent", "Cond_uScm", "ORP_mV", "pH", "Temp_C")
data_availability(ysi_profiles, variables)

#removing PAR, ORP, cond, oxygen, and pH due to limited data availability
#keeping temp because YSI has the most temp

ysi_noon <- ysi_profiles |>
  mutate(DateTime = as.POSIXct(DateTime)) |>
  filter(!is.na(DateTime)) |>
  arrange(DateTime) |>
  mutate(
    Date = as.Date(DateTime),
    cast_id = cumsum(c(TRUE, diff(as.numeric(DateTime)) > 3600)) #an hour between the last measurement and new indicates new cast
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
    cast_id = cumsum(c(TRUE, diff(as.numeric(DateTime)) > 3600)) #an hour between the last measurement and new indicates new cast
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
  ) 

variables <- c("DO_mgL", "PAR_umolm2s", "DOsat_percent", "Cond_uScm", "ORP_mV", 
               "pH", "Temp_C")
data_availability(CTDfiltered, variables)
#can't use many of the variables because not enough data for every year
#can use Temp from CTD for 2015, 2016, 2019, 2021, 2022, 2023, and 2024
#can use Temp from YSI for 2017, 2018, 2020, 2021

CTDtemp<- CTDfiltered|>
  mutate(Year = year(Date), Week = week(Date))|>
  filter(Year %in% c(2015, 2016, 2019, 2021, 2022, 2023, 2024))|> 
  select(Date, Year, Week, Temp_C, Depth_m)

ysitemp<- ysi%>%
  mutate(Year = year(Date), Week = week(Date))|>
  filter(Year %in% c(2017, 2018, 2020, 2021))|>
  select(Date, Year, Week, Temp_C, Depth_m)
#coalesce 

temp_depths_coalesced <- full_join(ysitemp, CTDtemp, by = c("Date", "Year", "Depth_m", "Week"))|>
  group_by(Date)|>
  mutate(Temp_C = coalesce(Temp_C.x, Temp_C.y))|>
  ungroup()|>
  filter(Depth_m > 0.09)|>
  select(-Temp_C.y, -Temp_C.x)

variables<- c("Temp_C")
data_availability(temp_depths_coalesced, variables)

#the beginning of 2019 is missing from within the season we want, so grabbing from ysi data
#before DOY 160
ysitemp2019_clean <- ysi |>
  mutate(Date = as_date(Date),
         Year = year(Date),
         Week = week(Date),
         DOY = yday(Date)) |>
  filter(Year == 2019, DOY < 170) |>
  select(Date, Year, Week, Depth_m, Temp_C)

# append into coalesced dataframe
temp_depths_coalesced <- bind_rows(temp_depths_coalesced, ysitemp2019_clean) |>
  arrange(Date, Depth_m)

data_availability(temp_depths_coalesced, variables)


####Temp calculations####
temp_depths_cleaned <- temp_depths_coalesced |>
  filter(!is.na(Temp_C)) |>
  mutate(Depth_m = floor(Depth_m * 10) / 10) |>
  group_by(Date, Depth_m) |>
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE), .groups = "drop") |>
  group_by(Date) |>
  arrange(Depth_m, .by_group = TRUE) |>
  mutate(
    temp_diff = Temp_C - lag(Temp_C),
    prev_diff = lag(temp_diff),
    # Flag: direction reversed AND the reversal is large (points that don't make sense and are obvious error removed)
    reversal = !is.na(temp_diff) & !is.na(prev_diff) &
      sign(temp_diff) != sign(prev_diff) &
      abs(temp_diff) > 2
  ) |>
  filter(!reversal) |>
  ungroup() |>
  select(-temp_diff, -prev_diff, -reversal) |>
  mutate(DOY = yday(Date)) |>
  mutate(Reservoir = "BVR", Site = 50, DateTime = Date) |>
  filter(DOY >= 133, DOY <= 285)

#removing outliers 
cleaned <- temp_depths_cleaned |>
  group_by(Date) |>
  arrange(Depth_m, .by_group = TRUE) |>
  mutate(
    # Rolling median of Temp_C over a window of nearby depth observations
    Temp_rolling_median = rollapply(Temp_C, width = 7, FUN = median, 
                                    fill = NA, align = "center"),
    # Absolute deviation from the local median
    temp_deviation = abs(Temp_C - Temp_rolling_median)
  ) |>
  # Remove points that deviate more than 2°C from the local median
  filter(is.na(temp_deviation) | temp_deviation < 2) |>
  ungroup() |>
  select(-Temp_rolling_median, -temp_deviation)

#write.csv(temp_depths_cleaned, "CSVs/temp_depths_cleaned.csv")

#look at the daily casts
for (yr in years) {
  
  # Filter data for the year
  test <- cleaned |>
    filter(year(Date) == yr)
  
  # Skip if there's no data
  if (nrow(test) == 0) next
  
  # Create plot
  plot_casts <- ggplot(test, aes(x = Temp_C, y = Depth_m)) +
    geom_path() +
    geom_point(size = 0.8, alpha = 0.8) +  # Small points at each observation
    # Light blue grid lines for every whole meter
    geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1), 
               color = "lightblue", linetype = "dotted", linewidth = 0.3) +
    # Vertical lines for concentrations
    scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
    theme_bw() +
    facet_wrap(vars(Date)) +
    xlab("Temp") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "Temp Profiles"))
  
  # Save plot
  ggsave(filename = paste0("Figs/Daily_interp_Casts/", yr, "_raw_casts.png"),
         plot = plot_casts,
         width = 12,
         height = 10,
         dpi = 300)
}


#incomplete cast remove
#2020-09-19

temp_depths_cleaned2 <- cleaned|> #remove the weirdos
  filter(!(Date == as.Date("2020-09-19")))

variables <- c("Temp_C")
temp_depths_interp <- interpolate_variable(temp_depths_cleaned2, variables) #this will be used for buoyancy frequency

#look at the daily casts
for (yr in years) {
  
  # Filter data for the year
  test <- temp_depths_interp |>
    filter(year(Date) == yr)
  
  # Skip if there's no data
  if (nrow(test) == 0) next
  
  # Create plot
  plot_casts <- ggplot(test, aes(x = Temp_C, y = Depth_m)) +
    geom_path() +
    geom_point(size = 0.8, alpha = 0.8) +  # Small points at each observation
    # Light blue grid lines for every whole meter
    geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1), 
               color = "lightblue", linetype = "dotted", linewidth = 0.3) +
    # Vertical lines for concentrations
    scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
    theme_bw() +
    facet_wrap(vars(Date)) +
    xlab("Temp") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "Temp Profiles"))
  
  # Save plot
  ggsave(filename = paste0("Figs/Daily_interp_Casts/", yr, "_raw_casts.png"),
         plot = plot_casts,
         width = 12,
         height = 10,
         dpi = 300)
}
#warnings are ok

####Thermocline####

# Dataframe with thermocline
just_thermocline <- temp_depths_interp |>
  filter(!is.na(Temp_C)) |>
  group_by(Date, Depth_m) |>
  mutate(Temp_C = mean(Temp_C), na.rm = TRUE) |>
  ungroup() |>
  group_by(Date) |>
  group_modify(~ {
    max_depth <- max(.x$Depth_m[!is.na(.x$Temp_C)], na.rm = TRUE)
    
    # --- 1) First pass: standard thermo.depth ---
    thermocline_depth <- thermo.depth(
      .x$Temp_C,
      .x$Depth_m,
      Smin = 1,
      seasonal = TRUE,
      index = FALSE,
      mixed.cutoff = 0.25 * max_depth
    )
    
    # --- 2) If it's too shallow, ignore surface and recalc deeper ---
    if (!is.na(thermocline_depth) && thermocline_depth < 0.2 * max_depth) {
      
      # Remove mixed layer entirely
      .x_deep <- .x |> filter(Depth_m > 0.25 * max_depth)
      
      if (nrow(.x_deep) >= 3) {
        thermocline_depth <- tryCatch(
          thermo.depth(
            .x_deep$Temp_C,
            .x_deep$Depth_m,
            Smin = 0.5,
            seasonal = TRUE,
            index = FALSE
          ),
          error = function(e) NA_real_
        )
      }
    }
    
    # --- 3) Final sanity: still reject noise ---
    thermocline_depth <- ifelse(
      is.na(thermocline_depth) | thermocline_depth < 0.15 * max_depth,
      NA,
      thermocline_depth
    )
    
    tibble(Date = .x$Date[1], thermocline_depth = thermocline_depth)
  }) |>
  ungroup() |>
  mutate(
    Week = week(Date),
    Year = year(Date)
  )

#####individual date thermocline check####
#join thermocline to temp profiles so that I can plot them 
thermocline_and_depth_profiles <- temp_depths_interp|>
  left_join(just_thermocline, by = c("Date"))|>
  group_by(Date)|>
  fill(thermocline_depth, .direction = "updown")|>
  ungroup()|>
  mutate(year = year(Date))


for (yr in years) {
  
  # Filter data for the year
  test <- thermocline_and_depth_profiles |>
    filter(year(Date) == yr)
  
  # Skip if there's no data
  if (nrow(test) == 0) next
  
  # Create plot
  plot_casts <- ggplot(test, aes(x = Temp_C, y = Depth_m)) +
    geom_path() +
    geom_point(size = 0.8, alpha = 0.8) +  # Small points at each observation
    # Light blue grid lines for every whole meter
    geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1), 
               color = "lightblue", linetype = "dotted", linewidth = 0.3) +
    # Horizontal lines for depths
    geom_hline(aes(yintercept = thermocline_depth), linetype = "dashed", color = "red") +
    # Vertical lines for concentrations
    scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
    theme_bw() +
    facet_wrap(vars(Date)) +
    xlab("Temp") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "Temp Profiles"))+
    geom_text(aes(label = round(thermocline_depth, 1), x = Inf, y = thermocline_depth), 
              color = "black", hjust = 1.1, size = 3)
  
  # Save plot
  ggsave(filename = paste0("Figs/Thermocline/", yr, "_raw_casts.png"),
         plot = plot_casts,
         width = 12,
         height = 10,
         dpi = 300)
}




#keep in mind I will only be analyzing data within the may-october window
just_thermocline <- just_thermocline|>
  group_by(Week, Year)|>
  summarise(thermocline_depth = mean(thermocline_depth, na.rm = TRUE))

final_photic_thermo <- photic_zone_frame|>
  left_join(just_thermocline, by = c("Week", "Year"))|>
  left_join(weekly_water_level, by = c("Week", "Year"))|>
  mutate(PZ_prop = PZ/WaterLevel_m)|>
  select(-WaterLevel_m)

variables <- c("PZ")

data_availability(final_photic_thermo, variables)

write.csv(final_photic_thermo, "CSVs/final_photic_thermo.csv", row.names = FALSE)

#checking within and across year variability 

#checking within year and across year variability
var_summary <- photic_zone_frame %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_val = mean(PZ, na.rm = TRUE),
    sd_within = sd(PZ, na.rm = TRUE),
    cv_within = sd_within / mean_val
  )

across_year <- var_summary %>%
  summarise(
    mean_across = mean(mean_val),
    sd_across   = sd(mean_val),
    cv_across   = sd_across / mean_across
  )

variability_ratio <- mean(var_summary$sd_within) / across_year$sd_across
