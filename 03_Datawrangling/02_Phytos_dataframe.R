#Phytoplankton metrics and visualization
#published 2025
current_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/9/f246b36c591a888cc70ebc87a5abbcb7")

#published 2024
#current_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/8/0359840d24028e6522f8998bd41b544e")

#adding columns with total_conc max and the depth at which it occurs
phytos <- current_df %>% 
  filter(Reservoir == "BVR", Site == 50)%>%
  mutate(Date  = as_date(DateTime)) |> 
  filter((hour(DateTime) >= 8), (hour(DateTime) <= 18))|>
  filter(!(CastID == 592))|> #filter out weird drop in 2017
  filter(!(CastID == 395))|> #weird drop in 2016
  #filter(Flag_TotalConc_ugL != 2,Flag_TotalConc_ugL != 3)|> #2 is instrument malfunction and #3 is low transmission value
  mutate(Week = week(Date))|>
  mutate(Year = year(Date))|>
  mutate(DOY = yday(Date))

write.csv(phytos, "CSVs/phytos.csv", row.names = FALSE)
phytos <- read.csv("CSVs/phytos.csv")

####flora instrument data availability####

#days on the x axis, years on the y axis
plot_dat <- phytos %>%
  filter(!is.na(TotalConc_ugL)) %>%
  mutate(Year = year(Date), 
         DayOfYear = yday(Date))|> # Extract year and day of the year
  select(Date, Year, Week, DayOfYear, TotalConc_ugL, Depth_m)

# Find the maximum TotalConc_ugL value for each year
max_totals_per_year <- plot_dat %>%
  group_by(year(Date)) %>%
  slice(which.max(TotalConc_ugL)) %>%
  ungroup()

# Main plot
phytosplot <- # Plot: x-axis is DayOfYear, y-axis is Year, with a line and highlighted points
  ggplot(plot_dat, aes(x = DayOfYear, y = as.factor(Year), group = Year)) +
  geom_line() +  # Line for each year
  geom_point() +  # Data points
  geom_point(data = max_totals_per_year, aes(x = DayOfYear, y = as.factor(Year)), 
             color = "red", size = 3) +  # Highlight max points in red
  geom_text(data = max_totals_per_year, 
            aes(x = DayOfYear, y = as.factor(Year), 
                label = paste0("Max: ", round(TotalConc_ugL, 2), " µg/L\nDepth: ", Depth_m, " m")), 
            vjust = 1.5, hjust = 0.5, color = "black", size = 3) +  # Smaller text and place below the point
  theme_bw() +
  labs(x = "Day of Year", y = "Year", title = "Fluoroprobe Data Availability 2025 Pub") +
  scale_x_continuous(breaks = seq(1, 365, by = 30), limits = c(1, 365)) +  # Set x-axis limits and breaks
  theme(panel.grid.minor = element_blank())+  # Optional: remove minor grid lines
  geom_vline(xintercept = 133, linetype = "dashed", color = "red") +  # Vertical dashed line at DayOfYear 133
  geom_vline(xintercept = 286, linetype = "dashed", color = "red")  # Vertical dashed line at DayOfYear 286
print(phytosplot)

ggsave("Figs/Data_availability/TotalPhytos2025pub.png",phytosplot)

#see that the data is dispersed at random intervals
####choosing casts and calculating peaks####
#1. Look at every cast for every year and remove casts that do not make sense
#2. Calculate peak metrics for each cast (peak depth, width, and magnitude)
#3. Visually check all casts and each metric to make sure it makes sense
#4. Average the peak metrics together (if appropriate)
#5. New data frame with one set of peak metrics for each week that we have data


#plot each one for cast selection 

# Make sure the Figs directory exists
if (!dir.exists("Figs")) {
  dir.create("Figs")
}

# Prepare your data with FacetID
DCM_metrics <- phytos |>
  select(Reservoir, Site, Date, Week, CastID, Depth_m, TotalConc_ugL) |>
  group_by(Reservoir, Date, Site) |>
  mutate(FacetID = paste(CastID, Reservoir, Site, Date, "Week", Week, sep = " ")) |>
  ungroup()

# Get unique years in the dataset
years <- unique(year(DCM_metrics$Date))

# Loop over each year
for (yr in years) {
  
  # Filter data for the year
  test <- DCM_metrics |>
    filter(year(Date) == yr)
  
  # Skip if there's no data
  if (nrow(test) == 0) next
  
  # Create plot
  plot_casts <- ggplot(test, aes(x = TotalConc_ugL, y = Depth_m)) +
    geom_path() +
    scale_y_reverse() +
    theme_bw() +
    facet_wrap(vars(FacetID)) +
    xlab("micrograms per liter") +
    ylab("Depth (m)") +
    ggtitle(paste(yr, "raw casts"))
  
  # Save plot
  ggsave(filename = paste0("Figs/raw_flora_casts/", yr, "_raw_casts.png"),
         plot = plot_casts,
         width = 12,
         height = 10,
         dpi = 300)
}

#notes on casts
#casts to remove: 467, 814, 856, 920, 1149 

DCM_metrics_filtered <- DCM_metrics |>
  filter(!CastID %in% c(467, 814, 856, 920, 1149)) |>
  mutate(CastID = case_when(
    CastID == 485 ~ 484,  # Change 485 to 484
    CastID == 492 ~ 493,  # Combine 492 and 493
    CastID == 499 ~ 500,  # Combine 499 and 500
    CastID == 603 ~ 604,  # Combine 603 and 604
    TRUE ~ CastID          # Keep other values unchanged
  ))|>
  mutate(DOY = yday(Date), Year = year(Date))|>
  filter(DOY > 133, DOY < 285)

#question about how to address casts that don't qualify as a bloom. They won't calculate metrics
#will RandomForest assume a bloom just didn't happen?
#does there need to be a component that first predicts whether or not a bloom will happen?
#and then if yes 

#join waterlevel because this will be important for peak metrics
water_level <- read.csv("CSVs/water_level.csv") |>
  mutate(Date = as_date(Date)) |>
  select(Week, Year, WaterLevel_m) |>
  group_by(Week, Year) |>
  summarise(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE), .groups = "drop")

DCM_metrics_filtered <- DCM_metrics_filtered|>
  left_join(water_level, by = c("Week", "Year"))

####Peak.depth and max_conc####
DCM_metrics_depth <- DCM_metrics_filtered|>
  group_by(CastID) %>%
  mutate(max_conc = max(TotalConc_ugL, na.rm = TRUE))|> #concentration of totals at totals DCM
  mutate(DCM_depth = ifelse(TotalConc_ugL == max_conc, Depth_m, NA_real_))|>
  fill(max_conc, .direction = "downup")|>
  fill(DCM_depth, .direction = "downup")|>
  ungroup()

####Peak.width####
#use Totals_mean
#calculate the metrics on the actual observed profiles 
#don't interpolate 

#Using mean as per Mary's recommendation
# 
# for_peaks <- DCM_metrics_depth|>
#   group_by(CastID) %>%
#   mutate(
#     totals_med = median(TotalConc_ugL, na.rm = TRUE),  # Calculate the median, excluding NA values
#     totals_mean = mean(TotalConc_ugL, na.rm = TRUE),   # Calculate the mean
#     peak.top = as.integer(Depth_m <= DCM_depth & TotalConc_ugL >= totals_mean),  # Create binary indicator
#     peak.bottom = as.integer(Depth_m >= DCM_depth & TotalConc_ugL >= totals_mean),
#     # Apply condition: If Totals_DCM_conc < 30, set peak.top and peak.bottom to 0
#     peak.top = if_else(max_conc < 30, 0, peak.top),
#     peak.bottom = if_else(max_conc < 30, 0, peak.bottom),
#     # Replace peak.top and peak.bottom with Depth_m if indicator is 1
#     peak.top = if_else(peak.top == 1, Depth_m, 0),
#     peak.bottom = if_else(peak.bottom == 1, Depth_m, 0),
#     # Get the minimum peak.top value, replace Inf with NA if all are NA or 0
#     peak.top = if_else(any(peak.top != 0), 
#                        min(peak.top[peak.top != 0], na.rm = TRUE), 
#                        NA_real_),
#     # Get the maximum peak.bottom value, replace -Inf with NA if all are NA or 0
#     peak.bottom = if_else(any(peak.bottom != 0), 
#                           max(peak.bottom[peak.bottom != 0], na.rm = TRUE), 
#                           NA_real_),
#     # Calculate peak width and handle infinite values by replacing them with NA
#     peak.width = peak.bottom - peak.top,
#     peak.width = if_else(is.na(peak.top) | is.na(peak.bottom), NA_real_, peak.width)
#   ) %>%
#   ungroup()|>  # Ungroup after mutations
#   select(CastID, Depth_m, peak.top, peak.bottom, peak.width, totals_mean, totals_med)
# 
# final_DCM_metrics <- DCM_metrics_depth |> #with peak calculations
#   left_join(for_peaks, by = c("CastID", "Depth_m")) |>
#   #mutate(peak.width = if_else(peak.width < .3*WaterLevel_m, peak.width, NA_real_)) |>
#   filter(DOY > 133, DOY < 286)
# 
####visualize DCM metrics####
# Loop over each year
# for (yr in years) {
#   
#   # Filter data for the year
#   test <- final_DCM_metrics |>
#     filter(year(Date) == yr)
#   
#   # Skip if there's no data
#   if (nrow(test) == 0) next
#   
#   # Create plot
#   plot_casts <- ggplot(test, aes(x = TotalConc_ugL, y = Depth_m)) +
#     geom_path() +
#     # Light blue grid lines for every whole meter
#     geom_hline(yintercept = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1), 
#                color = "lightblue", linetype = "dotted", linewidth = 0.3) +
#     # Horizontal lines for depths
#     geom_hline(aes(yintercept = peak.top), linetype = "dashed", color = "red") +
#     geom_hline(aes(yintercept = DCM_depth), linetype = "solid", color = "red") +
#     geom_hline(aes(yintercept = peak.bottom), linetype = "dashed", color = "red") +
#     # Vertical lines for concentrations
#     geom_vline(aes(xintercept = totals_mean), color = "red") +
#     scale_y_reverse(breaks = seq(0, max(test$Depth_m, na.rm = TRUE), by = 1)) +
#     theme_bw() +
#     facet_wrap(vars(FacetID)) +
#     xlab("micrograms per liter") +
#     ylab("Depth (m)") +
#     ggtitle(paste(yr, "raw casts"))+
#     geom_text(aes(label = round(DCM_depth, 1), x = Inf, y = DCM_depth), 
#               color = "black", hjust = 1.1, size = 3)
#   
#   # Save plot
#   ggsave(filename = paste0("Figs/raw_flora_casts/", yr, "_raw_casts.png"),
#          plot = plot_casts,
#          width = 12,
#          height = 10,
#          dpi = 300)
# }
# 
#for now not going to keep peak calculations in analysis 
#will come back to this at a later date

final_DCM_metrics<- final_DCM_metrics|>
  #select(-peak.top, -peak.bottom, -peak.width)|>#can remove this once have peak calculations figured out
  group_by(CastID)|>
  mutate(Q3 = quantile(TotalConc_ugL, 0.75)) #25% of data falls above this value 

####boxplots depth of DCM####

#need to use raw data for this to work 

#for june, july, august
boxplot_Data <- final_DCM_metrics |>
  filter(max_conc > 20) |>
  filter(month(Date)>5, month(Date)<9) |>
  mutate(Year = year(Date), Month = month(Date))|>
  group_by(Year, Month)|>
  mutate(monthly_avg = mean(max_conc))

# Calculate max_legend_value for the color scale limits
max_legend_value <- max(boxplot_Data$max_conc, na.rm = TRUE)

# Create the multi-panel boxplot with an overlay of colored points for Totals_DCM_conc
ggplot(boxplot_Data, aes(x = factor(Month, labels = c("June", "July", "August")), 
                         y = DCM_depth, 
                         fill = monthly_avg)) +
  geom_boxplot() +  # Boxplot with filled colors based on Totals_DCM_conc
  facet_wrap(~ Year) +  # Create a panel for each year
  scale_fill_gradientn(colours = blue2green2red(60), na.value = "gray", limits = c(NA, max_legend_value)) +  # Apply color gradient to boxes
  scale_y_reverse(name = "DCM Depth (inverted)") +  # Reverse the y-axis
  ylim(10, 0) +  # Set the y-axis limits, reversing the range
  labs(x = "Month", y = "DCM Depth", fill = "Total's µg/L") +  # Label the legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
#visualizing just one box per year

#filtering so that max_conc is greater than 20 because otherwise we can't call it a deep chlorophyll maxima 
boxplot_Data <- final_DCM_metrics|>
 # filter(max_conc > 20) |> #or change this so that it's only displaying points greater than 20ugL
  mutate(DayOfYear = yday(Date))|>
  filter(DayOfYear>133, DayOfYear<286) |>
  mutate(Year = year(Date), Month = month(Date))|>
  select(CastID, Date,Year,Month, DCM_depth, max_conc)|>
  group_by(CastID, Date,Year,Month,)|>
  summarise(
    DCM_depth = mean(DCM_depth, na.rm = TRUE),
    max_conc = mean(max_conc, na.rm = TRUE),
    .groups = "drop"
  )

label_data <- boxplot_Data %>%
  group_by(Year) %>%
  summarise(n = n())  # Calculate the number of data points per year

# Plot with labels for the number of data points
last_plot <- ggplot(boxplot_Data, aes(x = factor(Year), y = DCM_depth)) +
  geom_boxplot() +
  geom_point(aes(color = max_conc), position = position_jitter(width = 0.2), size = 2) +  # Add points with color representing concentration
  scale_color_gradientn(
    colours = c("blue","cyan", "green","yellow", "red", "red3"),
    values = scales::rescale(c(0,40, 75, 100, 200, max_legend_value)),  # Make red hit at 150 µg/L
    na.value = "gray",
    limits = c(20, max_legend_value), 
    breaks = c(20, 100, 200, 300, 380)
  ) +  scale_y_reverse(name = "DCM Depth (inverted)") +  # Reverse the y-axis
  ggtitle(label = "DCM Depths all data points") + #or change this so that it's only displaying points greater than 20ugL
  ylim(10, 0) +  # Set the y-axis limits, reversing the range
  labs(x = "Year", y = "DCM Depth", color = "totals ugL") +  # Label the legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = label_data, aes(x = factor(Year), y = 0.5, label = paste0("n = ", n)), 
            vjust = -0.5)  # Add labels at the top of each column

print(last_plot)

ggsave(
  filename = "DCM_depths_annual_boxplot_all_data_points.png",
  plot = last_plot(),
  path = "Figs/Phytos_viz",
  width = 10,   # width in inches
  height = 5,   # height in inches
  dpi = 300     # optional: high resolution
)


####boxplot width of DCM####

# boxplot_Data <- DCM_final |>
#   filter(Totals_DCM_conc > 20) |>
#   filter(month(Date)>5, month(Date)<9) |>
#   mutate(Year = year(Date), Month = month(Date))|>
#   filter(peak.width<2.5)

# # Create the multi-panel boxplot with an overlay of colored points for width of DCM
# ggplot(boxplot_Data, aes(x = factor(Month, labels = c("June", "July", "August")), 
#                          y = peak.width)) +
#   geom_boxplot() +
#   geom_point(aes(color = Totals_DCM_conc), position = position_jitter(width = 0.2), size = 2) +  # Add points with color representing concentration
#   facet_wrap(~ Year) +  # Create a panel for each year
#   scale_color_gradientn(colours = blue2green2red(60), na.value = "gray", limits = c(NA, max_legend_value)) +  # Apply color gradient to points
#   labs(x = "Month", y = "Peak Width", color = "totals ugL") +  # Label the legend
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# #one box per year
# boxplot_Data <- DCM_final |>
#   filter(Totals_DCM_conc > 20) |>
#   mutate(DayOfYear = yday(Date))|>
#   filter(DayOfYear>133, DayOfYear<286) |>
#   mutate(Year = year(Date), Month = month(Date))|>
#   filter(peak.width<2.5)
# 
# label_data <- boxplot_Data %>%
#   group_by(Year) %>%
#   summarise(n = n())  # Calculate the number of data points per year
# 
# ggplot(boxplot_Data, aes(x = factor(Year), y = peak.width)) +
#   geom_boxplot() +
#   geom_point(aes(color = Totals_DCM_conc), position = position_jitter(width = 0.2), size = 2) +  # Add points with color representing concentration
#   scale_color_gradientn(colours = blue2green2red(60), na.value = "gray", limits = c(NA, max_legend_value)) +  # Apply color gradient to points
#   ggtitle(label = "Peak Width only displaying totals > 20") +
#   ylim(0, 5) +  # Set the y-axis limits
#   labs(x = "Year", y = "Peak Width", color = "totals ugL") +  # Label the legend
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   geom_text(data = label_data, aes(x = factor(Year), y = 4.5, label = paste0("n = ", n)), 
#             vjust = -0.5)  # Adjust y position for labels at the top
# 

####boxplots magnitude of DCM####

#for June-August

boxplot_Data <- final_DCM_metrics |>
  filter(max_conc > 20) |>
  filter(month(Date)>5, month(Date)<9, year(Date)>2014) |>
  mutate(Year = year(Date), Month = month(Date))|>
  select(CastID, Date,Year,Month, DCM_depth, max_conc)|>
  group_by(CastID, Date,Year,Month)|>
  summarise(
    DCM_depth = mean(DCM_depth, na.rm = TRUE),
    max_conc = mean(max_conc, na.rm = TRUE),
    .groups = "drop"
  )

summer_conc <- ggplot(boxplot_Data, aes(x = factor(Month, labels = c("June", "July", "August")), 
                         y = max_conc)) +
  geom_boxplot() +
  geom_point(aes(color = max_conc), position = position_jitter(width = 0.2), size = 2) +  # Add points with color representing concentration
  facet_wrap(~ Year, nrow = 2, ncol = 5) +  # Create a panel for each year
  scale_color_gradientn(colours = blue2green2red(60), na.value = "gray", limits = c(NA, max_legend_value)) +  # Apply color gradient to points
  labs(x = "Month", y = "Peak Magnitude", color = "totals ugL") +  # Label the legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(summer_conc)

ggsave(
  filename = "DCM_concentrations_summer_months.png",
  plot = last_plot(),
  path = "Figs/Phytos_viz",
  width = 10,   # width in inches
  height = 5,   # height in inches
  dpi = 300     # optional: high resolution
)

#visualizing just one box per year

boxplot_Data <- final_DCM_metrics |>
  filter(max_conc > 20) |>
  mutate(DayOfYear = yday(Date))|>
  filter(DayOfYear>133, DayOfYear<286) |>
  mutate(Year = year(Date), Month = month(Date))

ggplot(boxplot_Data, aes(x = factor(Year), y = max_conc)) +
  geom_boxplot() +
  geom_point(aes(color = max_conc), position = position_jitter(width = 0.2), size = 2) +  # Add points with color representing concentration
  scale_color_gradientn(colours = blue2green2red(60), na.value = "gray", limits = c(NA, max_legend_value)) +  # Apply color gradient to points
  ggtitle(label = "Peak Magnitudes only displaying totals > 20")+
  ylim(0, 385) +  # Set the y-axis limits, reversing the range
  labs(x = "Year", y = "Peak Magnitude", color = "totals ugL") +  # Label the legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For heatmaps go to "Extra Helpful Scripts/DCMBVRheatmaps"


#frame that will be added to for RF analysis at the end
#one measurement for each week that we have data available for 
final_phytos <- final_DCM_metrics|>
  group_by(Year,Week,WaterLevel_m)|>
  summarise(
    DCM_depth = mean(DCM_depth, na.rm = TRUE),
    max_conc = mean(max_conc, na.rm = TRUE),
    totals_mean = mean(totals_mean),
    totals_med = mean(totals_med),
    .groups = "drop"
  )|>
  select(-WaterLevel_m)

#everything else will be joined to this dataframe
frame_weeks <- final_phytos|>
  distinct(Year, Week)|>
  left_join(water_level, by = c("Week", "Year"))

write.csv(frame_weeks, "CSVs/frame_weeks.csv", row.names = FALSE)
write.csv(final_phytos, "CSVs/final_phytos.csv", row.names = FALSE)
#metrics for each variable that needs to be calculated 
#depth_var_max
#depth_var_min
#var_max_val
#var_min_val
#var_mean
#var_range (max-main)

