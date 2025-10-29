library(ggplot2)
library(dplyr)
library(tidyr)

full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

####Function for running correlation and making plots 
plot_correlation_matrix <- function(df, file_name, cutoff = 0.6, title = "Correlation Matrix Heatmap") {
  library(ggplot2)
  library(dplyr)
  
  # Compute correlation matrix
  cor_matrix <- cor(df[, 2:ncol(df)], 
                    method = "spearman", 
                    use = "pairwise.complete.obs")
  
  # Convert to long format
  cor_long <- as.data.frame(as.table(cor_matrix)) %>%
    rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)
  
  # Set factor levels
  var_names <- colnames(cor_matrix)
  cor_long$Var1 <- factor(cor_long$Var1, levels = var_names)
  cor_long$Var2 <- factor(cor_long$Var2, levels = var_names)
  nvars <- length(var_names)
  
  # Scaling factor
  scale_factor <- sqrt(42 / nvars)
  
  # Subset for "X" marks where abs(correlation) >= cutoff
  cor_x <- cor_long %>% filter(abs(Correlation) >= cutoff)
  
  # Build the plot
  p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation, size = abs(Correlation))) +
    geom_point(shape = 22, color = "white") +
    # Add "X" on tiles above cutoff
    geom_point(data = cor_x, aes(x = Var1, y = Var2), shape = 4, color = "black", size = 6 * scale_factor, stroke = 1.2) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                         limits = c(-1, 1), name = "Correlation") +
    scale_size(range = c(2 * scale_factor, 12 * scale_factor), guide = "none") +
    geom_vline(xintercept = seq(0.5, nvars + 0.5, by = 1), 
               linetype = "dashed", color = "black", size = 0.2) +
    geom_hline(yintercept = seq(0.5, nvars + 0.5, by = 1), 
               linetype = "dashed", color = "black", size = 0.2) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 12 * scale_factor),
      axis.text.y = element_text(size = 12 * scale_factor),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 20 * scale_factor, hjust = 0.5)
    ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(var_names)) +
    coord_fixed() +
    labs(title = title)
  
  # Save the plot
  ggsave(file_name, plot = p, width = 20, height = 20, units = "in", dpi = 300, bg = "white")
  
  return(p)
}

#### CM depth analysis####
depth_analysis <- full_weekly_data |>
  select(-ends_with("max_val"), -ends_with("min_val"), -ends_with("range"), 
         -max_conc, -totals_mean, -totals_med, -N_at_DCM, -Week, -Date.x, -Date.x.x, -Date.y.y)
write.csv(depth_analysis, "CSVs/depth_analysis_frame.csv", row.names = FALSE)
plot_correlation_matrix(depth_analysis, "Figs/DCM_Depth_Correlations/DCM_Depth_Correlation_Matrix.png", 
                        title = "DCM Depth Variables Correlation Matrix Heatmap")

#####Metals#####
metals_depth_corr <- depth_analysis |>
  select(Date, DCM_depth,
         depth_SFe_mgL_max, depth_SFe_mgL_min, depth_mean_SFe_mgL,
         depth_TFe_mgL_max, depth_TFe_mgL_min, depth_mean_TFe_mgL,
         depth_SMn_mgL_max, depth_SMn_mgL_min, depth_mean_SMn_mgL
  )
plot_correlation_matrix(
  metals_depth_corr,
  "Figs/DCM_Depth_Correlations/metals_depth_corr_Matrix.png",
  title = "Metals Depth Variables Correlation Matrix Heatmap"
)

#####Chemistry#####
chem_depth_corr <- depth_analysis |>
  select(
    Date,
    DCM_depth,
    depth_TN_ugL_max,
    depth_TN_ugL_min,
    depth_mean_TN_ugL,
    depth_TP_ugL_max,
    depth_TP_ugL_min,
    depth_SRP_ugL_max, 
    depth_SRP_ugL_min, 
    depth_mean_SRP_ugL,
    depth_mean_TP_ugL,
    depth_NH4_ugL_max,
    depth_NH4_ugL_min,
    depth_mean_NH4_ugL,
    depth_NO3NO2_ugL_max,
    depth_NO3NO2_ugL_min,
    depth_mean_NO3NO2_ugL,
    depth_DIC_mgL_max,
    depth_DIC_mgL_min,
    depth_mean_DIC_mgL,
    depth_np_ratio_max,
    depth_np_ratio_min,
    depth_mean_np_ratio
  )
plot_correlation_matrix(
  chem_depth_corr,
  "Figs/DCM_Depth_Correlations/Chem_depth_corr_Matrix.png",
  title = "Chem Depth Variables Correlation Matrix Heatmap"
)

#####Met Data#####
met_depth_corr <- depth_analysis |>
  select(Date, DCM_depth,
         "Precip_Weekly", "precip_lag1", "precip_lag2",
         "AirTemp_Avg", "airtemp_lag1", "airtemp_lag2",
         "WindSpeed_Avg", "wind_lag1", "wind_lag2"
  )
plot_correlation_matrix(
  met_depth_corr,
  "Figs/DCM_Depth_Correlations/Met_depth_corr_Matrix.png",
  title = "Met Depth Variables Correlation Matrix Heatmap"
)

#####Physics#####
physics_depth_corr <- depth_analysis |>
  select(Date, DCM_depth, thermocline_depth, schmidt_stability
  )
plot_correlation_matrix(
  physics_depth_corr,
  "Figs/DCM_Depth_Correlations/physics_depth_corr_Matrix.png",
  title = "Physics Depth Variables Correlation Matrix Heatmap"
)


#####Phytos#####
#may do this


####chosen variables####
depth_analysis <- read.csv("CSVs/depth_analysis_frame.csv")

depth_analysis_revised <- depth_analysis |>
  select(Date, DCM_depth,
         WaterLevel_m,
         PZ,
         depth_SFe_mgL_max,
         depth_np_ratio_max,
         depth_NH4_ugL_max,
         depth_SRP_ugL_max,
         thermocline_depth,
         schmidt_stability,
         precip_lag1,
         precip_lag2,
         airtemp_lag1,
         airtemp_lag2,
         wind_lag1,
         wind_lag2)

write.csv(depth_analysis_revised, "CSVs/depth_analysis_revised.csv", row.names = FALSE)



#####BOTH Met Data##### these are variables for both DCM depth and DCM magnitude
met_depth_corr <- full_weekly_data |>
  select(Date, DCM_depth, max_conc,
         "Precip_Weekly", "precip_lag1", "precip_lag2",
         "AirTemp_Avg", "airtemp_lag1", "airtemp_lag2",
         "WindSpeed_Avg", "wind_lag1", "wind_lag2"
  )
plot_correlation_matrix(
  met_depth_corr,
  "Figs/DCM_Depth_Correlations/Met_depth_corr_Matrix.png",
  title = "Met Depth Variables Correlation Matrix Heatmap"
)

full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

#### CM magnitude analysis####
#prep dataframe
magnitude_analysis <- full_weekly_data |>
  select(-starts_with("depth_"), -Week, -totals_mean, -DCM_depth,
         -totals_med, -Date.x, -Date.y.y, -Date.x.x)
write.csv(magnitude_analysis, "CSVs/magnitude_analysis_frame.csv", row.names = FALSE)
#correlation matrix
plot_correlation_matrix(magnitude_analysis, "Figs/DCM_Magnitude_Correlations/DCM_Magnitude_Correlation_Matrix.png", 
                        title = "DCM Magnitude Variables Correlation Matrix Heatmap")

#####Metals#####
metals_magnitude_analysis <- magnitude_analysis |>
  select(Date, max_conc,
         SFe_mgL_max_val, SFe_mgL_min_val, SFe_mgL_range,
         TFe_mgL_max_val, TFe_mgL_min_val, TFe_mgL_range,
         SMn_mgL_max_val, SMn_mgL_min_val, SMn_mgL_range)
#correlation matrix
plot_correlation_matrix(metals_magnitude_analysis,
                        "Figs/DCM_Magnitude_Correlations/METALS_magnitude_Correlation_Matrix.png", 
                        title = "METALS Magnitude Variables Correlation Matrix Heatmap")


#####Chemistry#####
CHEM_magnitude_analysis <- magnitude_analysis |>
  select(Date, max_conc,
         TN_ugL_max_val, TN_ugL_min_val, TN_ugL_range,
         TP_ugL_max_val, TP_ugL_min_val, TP_ugL_range,
         NH4_ugL_max_val, NH4_ugL_min_val, NH4_ugL_range,
         NO3NO2_ugL_max_val, NO3NO2_ugL_min_val, NO3NO2_ugL_range,
         DIC_mgL_max_val, DIC_mgL_min_val, DIC_mgL_range,
         np_ratio_max_val, np_ratio_min_val, np_ratio_range)
#correlation matrix
plot_correlation_matrix(CHEM_magnitude_analysis,
                        "Figs/DCM_Magnitude_Correlations/CHEM_magnitude_Correlation_Matrix.png", 
                        title = "CHEM Magnitude Variables Correlation Matrix Heatmap")
#####Physics#####
physics_magnitude_analysis <- magnitude_analysis |>
  select(Date, max_conc,
         N_at_DCM, schmidt_stability, thermocline_depth)
#correlation matrix
plot_correlation_matrix(physics_magnitude_analysis,
                        "Figs/DCM_Magnitude_Correlations/physics_magnitude_Correlation_Matrix.png", 
                        title = "Physics Magnitude Variables Correlation Matrix Heatmap")


#####Phytos#####

####chosen variables####
magnitude_analysis <- read.csv("CSVs/magnitude_analysis_frame.csv")

magnitude_analysis_revised <- magnitude_analysis |>
  select(Date, max_conc, WaterLevel_m, PZ, schmidt_stability, thermocline_depth,
         TFe_mgL_max_val,SRP_ugL_max_val, NH4_ugL_max_val, 
         precip_lag1, precip_lag2, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
write.csv(magnitude_analysis_revised, "CSVs/magnitude_analysis_revised.csv", row.names = FALSE)

####visualize chosen variables####

######MAGNITUDE#####

magnitude_variables_plot <- full_weekly_data |>
  select(
    Date,
    WaterLevel_m,
    PZ,
    schmidt_stability,
    thermocline_depth,
    TFe_mgL_max_val,
    SRP_ugL_max_val,
    NH4_ugL_max_val,
    Precip_Weekly,        
    AirTemp_Avg,       
    WindSpeed_Avg
  )|>
  filter(year(Date) >2014)

# --- 1) Build a named palette from the water-level plot ----------------------
color_df <- ggplot_build(wtrlvl_by_year)$data[[1]] %>%
  dplyr::select(group, colour) %>%
  distinct() %>%
  arrange(group)
year_levels <- as.character(2015:2024)
year_colors <- setNames(color_df$colour[seq_along(year_levels) %% nrow(color_df) + 1],
                        year_levels)


# --- 2) Prepare data ---------------------------------------------------------
magnitude_variables_plot <- magnitude_variables_plot %>%
  mutate(
    Date = as.Date(Date),
    Year_num = year(Date),
    DOY  = yday(Date),
    Year = factor(as.character(Year_num), levels = year_levels)
  )

magnitude_long <- magnitude_variables_plot %>%
  pivot_longer(
    cols = c(WaterLevel_m,
             PZ,
             schmidt_stability,
             thermocline_depth,
             TFe_mgL_max_val,
             SRP_ugL_max_val,
             NH4_ugL_max_val,
             Precip_Weekly,        
             AirTemp_Avg,       
             WindSpeed_Avg),
    names_to  = "Variable", values_to = "Value"
  )

DCM_magnitude_visualize <- ggplot(magnitude_long, aes(DOY, Value, color = Year, group = Year)) +
  geom_line(size = 0.8, alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 5, nrow = 2) +   # ✅ 5 columns × 2 rows
  scale_color_manual(
    values = year_colors,
    name   = "Year",
    limits = year_levels,
    drop   = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 365, by = 30),
    labels = function(x) month.abb[(x %/% 30) %% 12 + 1]
  ) +
  labs(
    x = "Month",
    y = "Value",
    title = "DCM Magnitude Environmental Variables"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "grey70"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "right",        # ✅ legend on the right
    legend.direction = "vertical",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 9),
    legend.box.margin = margin(t = 0, r = 10, b = 0, l = 5)  # spacing
  ) +
  guides(color = guide_legend(ncol = 1))   # single vertical column of years

ggsave("Figs/DCM_magnitude_variables_by_yearly_color.png", plot = DCM_magnitude_visualize, width = 20, height = 10, units = "in", dpi = 300, bg = "white")

####data availability for chosen variables####

final_data_availability_fig <- full_weekly_data |>
  select(
    Date,
    max_conc,
    DCM_depth,
    WaterLevel_m,
    PZ,
    schmidt_stability,
    thermocline_depth,
    TFe_mgL_max_val,
    SRP_ugL_max_val,
    NH4_ugL_max_val,
    Precip_Weekly,
    AirTemp_Avg,
    WindSpeed_Avg
  ) |>
  filter(year(Date) > 2014) |>
  rename(
    `Max Chlorophyll (µg/L)`       = max_conc,
    `Deep Chlorophyll Max Depth (m)` = DCM_depth,
    `Water Level (m)`              = WaterLevel_m,
    `Photic Zone Depth (m)`        = PZ,
    `Schmidt Stability (J/m²)`     = schmidt_stability,
    `Thermocline Depth (m)`        = thermocline_depth,
    `Total Fe (mg/L)`              = TFe_mgL_max_val,
    `SRP (µg/L)`                   = SRP_ugL_max_val,
    `NH₄⁺ (µg/L)`                  = NH4_ugL_max_val,
    `Precipitation (mm/day)`       = Precip_Weekly,
    `Air Temperature (°C)`         = AirTemp_Avg,
    `Wind Speed (m/s)`             = WindSpeed_Avg
  )

variables <- c(
  "Max Chlorophyll (µg/L)",
  "Deep Chlorophyll Max Depth (m)",
  "Water Level (m)",
  "Photic Zone Depth (m)",
  "Schmidt Stability (J/m²)",
  "Thermocline Depth (m)",
  "Total Fe (mg/L)",
  "SRP (µg/L)",
  "NH₄⁺ (µg/L)",
  "Precipitation (mm/day)",
  "Air Temperature (°C)",
  "Wind Speed (m/s)"
)

plot <- final_data_availability_plot(final_data_availability_fig, variables)  

ggsave("Figs/Final_Analysis_Data_availability.png", plot = plot, width = 15, height = 15)
