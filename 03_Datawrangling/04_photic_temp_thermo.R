#this calculates
# 1. photic zone
# 2. temperature dataframe
# 3. thermocline

#need to have run the DataDownload first

#### secchi PZ  ####

#need to load in frame weeks 

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
    mutate(PZ = if_else(PZ > WaterLevel_m, WaterLevel_m, PZ))
}

library(ISOweek)
photic_zone_frame$Date <- ISOweek2date(paste0(photic_zone_frame$Year, "-W", sprintf("%02d", photic_zone_frame$Week), "-1"))

write.csv(photic_zone_frame, "CSVs/photic_zone_frame.csv", row.names = FALSE)

ggplot(photic_zone_frame, aes(x = Date, y = PZ)) +
  geom_line(aes(group = factor(year(Date)))) +
  scale_y_reverse()

####Adding PAR, DO, DOsat_percent, cond, ORP, pH, temp ####
#I don't end up using any of these variables, because we don't have consistent data through all years, 
#but also they are very correlated to other variables that are included
#####YSI#####
ysi_profiles <- ysi_profiles|>
  mutate(Date = as_date(DateTime))

#come back to remove flags
variables <- c("DO_mgL", "PAR_umolm2s", "DOsat_percent", "Cond_uScm", "ORP_mV", "pH", "Temp_C")

data_availability(ysi_profiles, variables)
# Generate the plot
#plot <- data_availability(ysi_profiles, variables)  
# Save the plot with specific dimensions
#ggsave("raw_ysi_availability.png", plot = plot, width = 20, height = 15, dpi = 300)

#removing PAR, ORP, cond, and pH due to limited data availability
#keeping temp because YSI has the most temp

variables <- c("DO_mgL","DOsat_percent", "Temp_C")
ysi <- ysi_profiles|>
  select(-PAR_umolm2s, -ORP_mV, -Cond_uScm, -pH)|>
  filter(Reservoir == "BVR", Site == 50)|>
  filter((hour(DateTime) >= 8), (hour(DateTime) <= 18))
#remove flags
data_availability(ysi, variables)

#####CTD#####
CTDfiltered <- CTD |>
  filter(Reservoir == "BVR", Site == 50) |>
  filter(!if_any(starts_with("Flag"), ~ . == 68)) |>
  mutate(
    DateTime = ymd_hms(DateTime, tz = "UTC"),
    Date = as_date(DateTime)
  ) |>
  filter(hour(DateTime) >= 8, hour(DateTime) <= 18)

variables <- c("DO_mgL", "PAR_umolm2s", "DOsat_percent", "Cond_uScm", "ORP_mV", 
               "pH", "Temp_C")
data_availability(CTDfiltered, variables)

#can't use many of the variables because not enough data for every year
#can use Temp from CTD for 2014, 2015, 2016, 2019, 2022, 2023, and 2024
#can use Temp from YSI for 2017, 2018, 2020

CTDtemp<- CTDfiltered|>
  mutate(Year = year(Date), Week = week(Date))|>
  filter(Year %in% c(2014, 2015, 2016, 2019, 2022, 2023, 2024))|> #remove flags
  select(Date, Year, Week, Temp_C, Depth_m)

ysitemp<- ysi%>%
  mutate(Year = year(Date), Week = week(Date))|>
  filter(Year %in% c(2017, 2018, 2020))|>
  select(Date, Year, Week, Temp_C, Depth_m)

#coalesce 

temp_depths_coalesced <- full_join(ysitemp, CTDtemp, by = c("Date", "Year", "Depth_m", "Week"))|>
  group_by(Date)|>
  mutate(Temp_C = coalesce(Temp_C.x, Temp_C.y))|>
  ungroup()|>
  filter(Depth_m > 0.09)|>
  select(-Temp_C.y, -Temp_C.x)

#variables <- ("Temp_C")
#temp_depths_coalesced_summarised <- weekly_sum_variables(temp_depths_coalesced, variables)

#probably this isn't the most useful information for Temp^^^
#instead I will calculate: VWT, SurfTemp (average of first meter),
#thermocline depth, epilimnion, metalimnion, hypolimnion mean temperatures
#temp range (mean(top 1m)- mean(bottom 1m))
#standard deviation of temp 
#max and min temp
#schmidt stability
#buoyancy freq
####Temp calculations####
temp_depths_cleaned <- temp_depths_coalesced |> #adding buoyancy freq here
  filter(!is.na(Temp_C)) |>
  group_by(Date, Depth_m) |>
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE), .groups = "drop")|>
  mutate(buoyancy_freq = c(buoyancy.freq(Temp_C, Depth_m), NA))#added for padding for the last value

temp_weekly_sum <- weekly_sum_variables(temp_depths_cleaned, "Temp_C")

####Thermocline####

# Dataframe with thermocline
just_thermocline <- temp_depths_cleaned |>
  filter(!is.na(Temp_C)) |>
  group_by(Date, Depth_m)|>
  mutate(Temp_C = mean(Temp_C), na.rm = TRUE)|>
  ungroup()|>
  group_by(Date) |>
  group_modify(~ {
    # Calculate max depth where Temp_C is not NA
    max_depth <- max(.x$Depth_m[!is.na(.x$Temp_C)], na.rm = TRUE)
    
    # Compute thermocline depth
    thermocline_depth <- thermo.depth(
      .x$Temp_C, 
      .x$Depth_m, 
      Smin = 1, 
      seasonal = TRUE, 
      index = FALSE, 
      mixed.cutoff = 0.25 * max_depth  # Use 25% of max depth for mixed layer
    )
    
    # Apply cutoff: Remove thermocline depths deeper than 40% of max depth
    #    thermocline_depth <- ifelse(thermocline_depth > 0.6 * max_depth, NA, thermocline_depth)
    #    thermocline_depth <- ifelse(thermocline_depth < 0.25 * max_depth, NA, thermocline_depth)
    
    # Ensure Date is a single value and return the summarised dataframe
    tibble(Date = .x$Date[1], thermocline_depth = thermocline_depth)
  }) |>
  
  ungroup()|>
  mutate(Week = week(Date), 
         Year = year(Date))

#####individual date thermocline check####
#join thermocline to temp profiles so that I can plot them 
thermocline_and_depth_profiles <- temp_depths_cleaned|>
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

#need to recalculate some of them will come back later for now just add to main dataframe

just_thermocline <- just_thermocline|>
  group_by(Week, Year)|>
  summarise(thermocline_depth = mean(thermocline_depth, na.rm = TRUE))

final_photic_thermo <- photic_zone_frame|>
  left_join(just_thermocline, by = c("Week", "Year"))|>
  select(-WaterLevel_m)

write.csv(final_photic_thermo, "CSVs/final_photic_thermo.csv", row.names = FALSE)
