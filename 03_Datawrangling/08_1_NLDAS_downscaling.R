#EDI met data goes from mid 2015 - 2024
#NLDAS from Heather Wander goes until 2021
#this script down scales the NLDAS data and joins it to the EDI met data

#1 bring in EDI met data and summarise it for weekly averages
#2 bring in NLDAS met data and summarise it for weekly averages
#3 downscale NLDAS data to obtain the rest of 2015 values
#4 join the full dataset together
#5 create lagged variables
#6 export the data as a csv for use in mmachine learning 

pacman::p_load(tidyverse, lubridate, RColorBrewer, dplyr, tidyr, ggplot2, ggthemes, patchwork)

####1 EDI met data####
#meteorological data from FCR https://portal.edirepository.org/nis/mapbrowse?packageid=edi.389.8
#2015-2024
options(timeout = 300)
EDImet <- read.csv( "https://pasta.lternet.edu/package/data/eml/edi/389/9/62647ecf8525cdfc069b8aaee14c0478")

#summarise to get weekly values 
##### Weekly values ####

EDImet_0 <- EDImet |>
  mutate(Date = as_date(DateTime))|>
  mutate(Year = year(DateTime),
         Week = week(Date)) |>
  group_by(Year, Week) |>
  summarise(
    Date = min(Date),  
    precip_weekly = mean(Rain_Total_mm, na.rm = TRUE),
    weekly_airtempavg = mean(AirTemp_C_Average, na.rm = TRUE),
    WindSpeed_Weekly_Average_m_s = mean(WindSpeed_Average_m_s, na.rm = TRUE),
    .groups = "drop"
  )
####2 NLDAS met data####

#Heather Wander generated data####
#for full repository https://github.com/hlwander/interannual_zoops/tree/a6f8b9f2fb2cacf09e5994eb7c0f73435cce9b89
#heathergeneratedNLDAS <- "https://raw.githubusercontent.com/hlwander/interannual_zoops/a6f8b9f2fb2cacf09e5994eb7c0f73435cce9b89/inputs/BVR_GLM_NLDAS_010113_123121_GMTadjusted.csv"
#NLDAS <- read.csv(heathergeneratedNLDAS)
#write.csv(NLDAS, "CSVs/NLDAS.csv", row.names = FALSE)
NLDAS <- read.csv("CSVs/NLDAS.csv")

NLDAS_0 <- NLDAS|>
  mutate(Date = as_date(time))|>
  mutate(Year = year(time),
         Week = week(time)) |>
  group_by(Year, Week) |>
  summarise(
    Date = min(Date),  
    precip_weekly = mean(Rain, na.rm = TRUE), #average
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
ggsave("Figs/metdata/joined_data_2015.png", joined_data_2015)


####8. plot all years for each variable
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

####9. Visualize met variable availability####
variables <- c("precip_weekly","weekly_airtempavg","WindSpeed_Weekly_Average_m_s")
data_availability(full_met, variables)

write.csv(full_met, "CSVs/final_metdata.csv", row.names = FALSE)
