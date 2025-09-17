#Met data

#meteorological data from FCR https://portal.edirepository.org/nis/mapbrowse?packageid=edi.389.8
#2015-2023
options(timeout = 300)
metdata <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

#met data for 2013->


# Visualizing metdata####
metdata0 <- metdata|>
 mutate(Date = as_date(DateTime))|>
 mutate(DOY = yday(Date))|>
   relocate(DOY, .before = DateTime)|>
   relocate(Date, .before = DateTime)

 
##### Weekly values ####

metdata_summed <- metdata0 |>
  mutate(Year = year(DateTime),
         Week = week(Date)) |>
  group_by(Year, Week) |>
  summarise(
    Date = min(Date),  
    precip_weekly = sum(Rain_Total_mm, na.rm = TRUE),
    weekly_airtempavg = mean(AirTemp_C_Average, na.rm = TRUE),
    PAR_umolm2s_Weekly_Average = mean(PAR_umolm2s_Average, na.rm = TRUE),
    WindSpeed_Weekly_Average_m_s = mean(WindSpeed_Average_m_s, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(metdata_summed, "CSVs/metdata_summed.csv", row.names = FALSE)
metdata_summed <- read.csv("CSVs/metdata_summed.csv")

metdata_summed <- metdata_summed|>
  mutate(Date = as_date(Date))

####plot variables#####
# 1. Precipitation plot
p1 <- ggplot(metdata_summed, aes(x = Week, y = precip_weekly, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly Precipitation", y = "Precipitation (mm)", color = "Year") +
  theme_minimal()

# 2. Weekly Air Temperature plot
p2 <- ggplot(metdata_summed, aes(x = Week, y = weekly_airtempavg, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly Air Temperature", y = "Air Temp (°C)", color = "Year") +
  theme_minimal()

# 3. PAR Weekly Average plot
p3 <- ggplot(metdata_summed, aes(x = Week, y = PAR_umolm2s_Weekly_Average, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly PAR Average", y = "PAR (µmol/m²/s)", color = "Year") +
  theme_minimal()

# 4. Wind Speed Weekly Average plot
p4 <- ggplot(metdata_summed, aes(x = Week, y = WindSpeed_Weekly_Average_m_s, color = factor(Year), group = Year)) +
  geom_line() +
  labs(title = "Weekly Wind Speed Average", y = "Wind Speed (m/s)", color = "Year") +
  theme_minimal()

# If you want to show them all together, install and load patchwork
# install.packages("patchwork")
library(patchwork)
combined_plot <- (p1 | p2) / (p3 | p4)
ggsave("Figs/metdata/metdata.png", combined_plot, width = 12, height = 8, dpi = 300)


##### Function for plotting meteorological variables #### 
metplots <- function(yearz, variable, maxx = NULL){
  
  metviz <- metdata_summed |>
    filter(year(Date) == yearz)  # filtering for the year specified
  
  ggplot(metviz, aes(x = Date, y = {{variable}})) +
    geom_path() +
    ggtitle(paste(deparse(substitute(variable)), yearz)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, maxx)) +  # setting consistent y-axis limits
    scale_x_date(date_labels = "%b",  # show abbreviated month names on x-axis
                 date_breaks = "1 month", 
                 expand = expansion(mult = c(0.01, 0.01))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # optional: tilt labels for readability
}


####viz precip####
years <- 2015:2023
precip_plots <- lapply(years, function(y) metplots(y, precip_weekly, maxx = 80))
precips <- do.call(plot_grid, c(precip_plots, ncol = 3))
precips <- precips + theme(plot.background = element_rect(fill = "white", color = NA))
print(precips)
ggsave("Figs/metdata/precip_all_years.png", precips, width = 12, height = 8, dpi = 300)

# ##### weekly_airtempavg #### 
precip_plots <- lapply(years, function(y) metplots(y, weekly_airtempavg, maxx = 40))
precips <- do.call(plot_grid, c(precip_plots, ncol = 3))
precips <- precips + theme(plot.background = element_rect(fill = "white", color = NA))
print(precips)
ggsave("Figs/metdata/weekly_temp_all_years.png", precips, width = 12, height = 8, dpi = 300)

####PAR_umolm2s_Weekly_Average####
precip_plots <- lapply(years, function(y) metplots(y, PAR_umolm2s_Weekly_Average, maxx = 650))
precips <- do.call(plot_grid, c(precip_plots, ncol = 3))
precips <- precips + theme(plot.background = element_rect(fill = "white", color = NA))
print(precips)
ggsave("Figs/metdata/PAR_umolm2s_Weekly_Average.png", precips, width = 12, height = 8, dpi = 300)

####WindSpeed_Weekly_Average_m_s####
precip_plots <- lapply(years, function(y) metplots(y, WindSpeed_Weekly_Average_m_s, maxx = 4))
precips <- do.call(plot_grid, c(precip_plots, ncol = 3))
precips <- precips + theme(plot.background = element_rect(fill = "white", color = NA))
print(precips)
ggsave("Figs/metdata/WindSpeed_Weekly_Average_m_s.png", precips, width = 12, height = 8, dpi = 300)

#for the week I'm going to change each one to be the week prior to add a lag effect 
metdata_final<- metdata_summed|>
  select(-Date)|>
  mutate(Week = Week + 1,
         Precip_avg_lagged = precip_weekly, 
         avg_airtemp_lagged = weekly_airtempavg, 
         PAR_umolm2s_Weekly_Avg_lagged = PAR_umolm2s_Weekly_Average, 
         WindSpeed_Weekly_Average_m_s_lagged = WindSpeed_Weekly_Average_m_s)|>
  select(-precip_weekly, -weekly_airtempavg, -PAR_umolm2s_Weekly_Average, -WindSpeed_Weekly_Average_m_s)

final_metdata <- frame_weeks|>
  left_join(metdata_final, by = c("Week", "Year"))|>
  select(-WaterLevel_m)  

write.csv(final_metdata, "CSVs/final_metdata.csv", row.names = FALSE)

####Data generated by Heather Wander
#for full repository https://github.com/hlwander/interannual_zoops/tree/a6f8b9f2fb2cacf09e5994eb7c0f73435cce9b89
heathergeneratedNLDAS <- "https://raw.githubusercontent.com/hlwander/interannual_zoops/a6f8b9f2fb2cacf09e5994eb7c0f73435cce9b89/inputs/BVR_GLM_NLDAS_010113_123121_GMTadjusted.csv"
NLDAS <- read.csv(heathergeneratedNLDAS)
write.csv(NLDAS, "CSVs/NLDAS.csv", row.names = FALSE)
