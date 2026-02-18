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

#will save figures to these files
if (!dir.exists("Figs/Thermocline")) {
  dir.create("Figs/Thermocline", recursive = TRUE)
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
  
  # Calculating K_d and light availability from secchi
  
  photic_zone_frame <- secchi_interpolated |> #add light
    mutate(sec_K_d = 1.7/Secchi_m) |>
    mutate(PZ = 4.605 /sec_K_d)|>
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
#warnings ok

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
#can't use many of the variables because not enough data for every year. 
#can use Temp from CTD for 2015, 2016, 2019, 2022, 2023, and 2024
#can use Temp from YSI for 2017, 2018, 2020
#will use Temp from BVR platform sensorstring

CTDtemp<- CTDfiltered|>
  mutate(Year = year(Date), Week = week(Date))|>
  filter(Year %in% c(2015, 2016, 2019, 2022, 2023, 2024))|> 
  select(Date, Year, Week, Temp_C, Depth_m)

variables<- c("Temp_C")
data_availability(CTDtemp, variables)

ysitemp<- ysi%>%
  mutate(Year = year(Date), Week = week(Date))|>
  filter(Year %in% c(2017, 2018, 2020))|>
  select(Date, Year, Week, Temp_C, Depth_m)
data_availability(ysitemp, variables)

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

#Sensorstrings------
#using BVR sensorstrings for what is available (2021-2024)
bvrdatasensorstring <- find_depths (data_file = BVRplatform, # data_file = the file of most recent data either from EDI or GitHub. Currently reads in the L1 file
                                    depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",  # depth_offset = the file of depth of each sensor relative to each other. This file for BVR is on GitHub
                                    output = NULL, # output = the path where you would like the data saved
                                    round_digits = 2, #round_digits = number of digits you would like to round to
                                    bin_width = 0.1, # bin width in m
                                    wide_data = F)  

#choosing the cast closest to noon
bvrdatasensorstring2 <- bvrdatasensorstring |>
  filter(variable == "ThermistorTemp", 
         year(DateTime) == 2021) |>
  mutate(Temp_C = observation, 
         Depth_m = rounded_depth,
         Year = year(DateTime),
         Date = as.Date(DateTime),
         Week = week(Date),
         mins_from_noon = abs(as.numeric(difftime(DateTime, 
                                                  as.POSIXct(paste(Date, "12:00:00"), tz = tz(DateTime)), 
                                                  units = "mins")))) |>
  group_by(Year, Week) |>
  filter(Date == min(Date)) |> #to get the first date for each week
  group_by(Week, Date) |>
  filter(mins_from_noon == min(mins_from_noon)) |>
  ungroup() |>
  select(Date, DateTime, Depth_m, Temp_C)

data_availability(bvrdatasensorstring2, "Temp_C")

#bind sensorstring data to the rest
all_years_temp <- bind_rows(temp_depths_coalesced, bvrdatasensorstring2) |>
  arrange(Date, Depth_m)

data_availability(all_years_temp, variables)


####Temp calculations####
# Clean temperature profiles for BVR Site 50 during stratified season
temp_depths_cleaned <- all_years_temp |>
  # Remove missing temperature readings
  filter(!is.na(Temp_C)) |>
  # Round depths to nearest 0.1m to bin close measurements together
  mutate(Depth_m = floor(Depth_m * 10) / 10) |>
  # Average duplicate readings at the same date and depth
  group_by(Date, Depth_m) |>
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE), .groups = "drop") |>
  # Within each date, sort by depth and detect temperature spike artifacts
  group_by(Date) |>
  arrange(Depth_m, .by_group = TRUE) |>
  mutate(
    # Temperature change from previous depth
    temp_diff = Temp_C - lag(Temp_C),
    # Temperature change at the depth above
    prev_diff = lag(temp_diff),
    # Flag points where temperature sharply reverses direction (>2°C)
    # relative to the previous gradient — likely sensor errors
    reversal = !is.na(temp_diff) & !is.na(prev_diff) &
      sign(temp_diff) != sign(prev_diff) &
      abs(temp_diff) > 2
  ) |>
  # Remove flagged spike points
  filter(!reversal) |>
  ungroup() |>
  select(-temp_diff, -prev_diff, -reversal) |>
  # Add day of year and metadata columns
  mutate(DOY = yday(Date)) |>
  mutate(Reservoir = "BVR", Site = 50, DateTime = Date) |>
  # Restrict to stratified season (~May 13 to ~Oct 12)
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

#look at the daily casts to check again and make sure they are clean before calculating thermocline
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
  summarize(Temp_C = mean(Temp_C, na.rm = TRUE), .groups = "drop") |>
  group_by(Date) |>
  group_modify(~ {
    max_depth <- max(.x$Depth_m, na.rm = TRUE)
    
    # --- Tunable parameters ---
    shallow_threshold <- 0.2
    deep_filter       <- 0.25
    reject_shallow    <- 0.15
    reject_deep       <- 0.85
    smin_default      <- 1
    smin_relaxed      <- 0.5
    
    # --- 1) First pass: standard thermo.depth ---
    thermocline_depth <- thermo.depth(
      .x$Temp_C,
      .x$Depth_m,
      Smin = smin_default,
      seasonal = TRUE,
      index = FALSE,
      mixed.cutoff = deep_filter * max_depth
    )
    
    method <- "standard"
    
    # --- 2) If too shallow, ignore surface and recalc deeper ---
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
    
    # --- 3) Final sanity: reject noise, but try to recalculate ---
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
#warnings are ok


#keep in mind I will only be analyzing data within the may-october window
just_thermocline <- just_thermocline|>
  group_by(Week, Year)|>
  summarise(thermocline_depth = mean(thermocline_depth, na.rm = TRUE))

final_photic_thermo <- photic_zone_frame|>
  left_join(just_thermocline, by = c("Week", "Year"))|>
  left_join(weekly_water_level, by = c("Week", "Year"))|>
  mutate(PZ_prop = PZ/WaterLevel_m)|>
  select(-WaterLevel_m)

data_availability(final_photic_thermo, "thermocline_depth")

write.csv(final_photic_thermo, "CSVs/final_photic_thermo.csv", row.names = FALSE)
