# #this script:
# 1) creates labels for ML figures
# 2) visualizes chosen variables for figure S1
# 3) summarizes statistics for Table S2 and Table S3


full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

if (!dir.exists("Figs/correlations")){
  dir.create("Figs/correlations")
}

####visualize chosen variables####

#REMEMBER TO INCREASE LEGEND SIZE FOR THE CORRELATION MATRIX AND INCLUDE NO2/NO3!!!

variable_labels <- c(
  max_conc = "DCM Magnitude (µg/L)",
  DCM_depth = "DCM Depth (m)",
  WaterLevel_m = "Water Level (m)",
  PZ = "Photic Zone Depth (m)",
  PZ_prop = "PZ/Water Level Proportion",
  N_at_DCM = "Buoyancy Frequency at DCM (s⁻¹)",
  schmidt_stability = "Schmidt Stability (J/m²)",
  thermocline_depth = "Thermocline Depth (m)",
  SFe_mgL_at_DCM = "SFe (mg/L) at DCM",
  SRP_ugL_at_DCM = "SRP (µg/L) at DCM",
  NH4_ugL_at_DCM = "NH₄⁺ (µg/L) at DCM",
  NO3NO2_ugL_at_DCM = "NO₃⁻/NO₂⁻ at DCM", 
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  depth_NH4_ugL_max = "Depth of Max NH₄⁺ (m)",
  depth_NO3NO2_ugL_max = "Depth of Max NO₃⁻/NO₂⁻ (m)", 
  Precip_Weekly  = "Precipitation Weekly Sum (mm)",
  precip_lag1    = "Precipitation Weekly Sum mm (Lag 1 wk)",
  precip_lag2    = "Precipitation Weekly Sum mm (Lag 2 wk)",
  AirTemp_Avg    = "Air Temperature Weekly Average (\u00b0C)",
  airtemp_lag1   = "Air Temperature Weekly Average \u00b0C (Lag 1 wk)",
  airtemp_lag2   = "Air Temperature Weekly Average \u00b0C (Lag 2 wk)",
  WindSpeed_Avg  = "Wind Speed Weekly Average (m/s)",
  wind_lag1      = "Wind Speed Weekly Average m/s (Lag 1 wk)",
  wind_lag2      = "Wind Speed Weekly Average m/s (Lag 2 wk)"
)


####Function for running correlation and making plots 

plot_correlation_matrix <- function(df, file_name, cutoff = 0.6, 
                                    title = "Correlation Matrix Heatmap", 
                                    variable_labels = NULL) {
  
  # Compute correlation matrix (exclude first column, assuming it's Date)
  cor_matrix <- cor(df[, 2:ncol(df)], 
                    method = "spearman", 
                    use = "pairwise.complete.obs")
  
  # Convert to long format
  cor_long <- as.data.frame(as.table(cor_matrix)) %>%
    rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)
  
  var_names <- colnames(cor_matrix)
  
  # Subset variable_labels to only variables in the data
  if(!is.null(variable_labels)) {
    selected_labels <- variable_labels[names(variable_labels) %in% var_names]
    
    # Map raw names to pretty labels
    cor_long <- cor_long %>%
      mutate(
        Var1 = factor(Var1, levels = names(selected_labels), labels = selected_labels),
        Var2 = factor(Var2, levels = names(selected_labels), labels = selected_labels)
      )
  } else {
    cor_long <- cor_long %>%
      mutate(
        Var1 = factor(Var1, levels = var_names),
        Var2 = factor(Var2, levels = var_names)
      )
  }
  
  nvars <- length(unique(cor_long$Var1))
  scale_factor <- sqrt(42 / nvars)
  
  # Legend sizing: target ~half the matrix height
  legend_height <- nvars / 2
  legend_key_height <- unit(legend_height / 5, "lines")
  legend_key_width <- unit(legend_height / 8, "lines")
  
  # Subset for "X" marks where abs(correlation) >= cutoff
  cor_x <- cor_long %>% filter(abs(Correlation) >= cutoff)
  
  # Build plot
  p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation, size = abs(Correlation))) +
    geom_tile(color = NA) +
    geom_point(data = cor_x, aes(x = Var1, y = Var2), shape = 4, color = "black", size = 6 * scale_factor, stroke = 1.2) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                         limits = c(-1, 1), name = "Correlation") +
    scale_size(range = c(2 * scale_factor, 12 * scale_factor), guide = "none") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 12 * scale_factor),
      axis.text.y = element_text(size = 12 * scale_factor),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 20 * scale_factor, hjust = 0.5),
      # Legend sizing - approximately half the matrix height
      legend.key.height = legend_key_height,
      legend.key.width = legend_key_width,
      legend.title = element_text(size = 14 * scale_factor),
      legend.text = element_text(size = 12 * scale_factor),
      legend.margin = ggplot2::margin(l = 10)
    ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(cor_long$Var2))) +
    coord_fixed() +
    labs(title = title)
  
  ggsave(file_name, plot = p, width = 20, height = 20, units = "in", dpi = 300, bg = "white")
  return(list(plot = p, correlations = cor_long))
  
}

#ADD NO2/NO3 HERE
#looking at the remainder of the variables to choose which are redundant
####chosen variables####
full_weekly_data <- full_weekly_data|>
  select(
    "Date", "WaterLevel_m", "DCM_depth", "max_conc",
     "PZ", "thermocline_depth","schmidt_stability",
    "PZ_prop", "N_at_DCM", "depth_SRP_ugL_max", "SRP_ugL_at_DCM",
    "depth_NH4_ugL_max", "NH4_ugL_at_DCM","depth_NO3NO2_ugL_max", "NO3NO2_ugL_at_DCM",
    "depth_SFe_mgL_max", "SFe_mgL_at_DCM",
    "Precip_Weekly", "precip_lag1", "precip_lag2",
    "AirTemp_Avg", "airtemp_lag1", "airtemp_lag2",
    "WindSpeed_Avg", "wind_lag1", "wind_lag2")

check <- plot_correlation_matrix(
  full_weekly_data,
  "Figs/correlations/full_corr_Matrix.png",
  title = "Correlation Matrix Heatmap for Variable Selection", 
  variable_labels = variable_labels,
)
View(check$correlations)


#Stats to produce Table S2 summary statistics----
# ── Variable labels (display order) ──
variable_labels <- c(
  DCM_depth = "DCM Depth (m)",
  max_conc = "DCM Magnitude (µg/L)",
  WaterLevel_m = "Water Level (m)",
  PZ = "Photic Zone Depth (m)",
  PZ_prop = "PZ / Water Level Proportion",
  thermocline_depth = "Thermocline Depth (m)",
  schmidt_stability = "Schmidt Stability (J/m²)",
  N_at_DCM = "Buoyancy Frequency at DCM (s⁻¹)",
  SFe_mgL_at_DCM = "SFe (mg/L) at DCM",
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  SRP_ugL_at_DCM = "SRP (µg/L) at DCM",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  NH4_ugL_at_DCM = "NH₄⁺ (µg/L) at DCM",
  depth_NH4_ugL_max = "Depth of Max NH₄⁺ (m)",
  NO3NO2_ugL_at_DCM = "NO₃⁻/NO₂⁻ (µg/L) at DCM",
  depth_NO3NO2_ugL_max = "Depth of Max NO₃⁻/NO₂⁻ (m)",
  Precip_Weekly = "Weekly Precipitation Sum (mm)",
  AirTemp_Avg = "Air Temperature Weekly Avg (\u00b0C)",
  WindSpeed_Avg = "Wind Speed Weekly Avg (m/s)")

# ── Read data ──
full_weekly_data <- full_weekly_data %>%
  mutate(Date = as.Date(Date),
         Year = year(Date))

# ── Compute overall summary ──
vars_to_use <- names(variable_labels)[names(variable_labels) %in% names(full_weekly_data)]
overall_summary <- bind_rows(lapply(vars_to_use, function(v) {
  
  # Apply filter ONLY for DCM depth
  if (v == "DCM_depth") {
    data_used <- full_weekly_data %>%
      filter(max_conc > 20)
    
    var_label <- paste0(variable_labels[v], "*")
    
  } else {
    data_used <- full_weekly_data
    var_label <- variable_labels[v]
  }
  
  s <- data_used[[v]]
  s_clean <- s[!is.na(s)]
  
  # Yearly stats
  yearly <- data_used %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Year) %>%
    summarise(
      yr_mean = mean(.data[[v]], na.rm = TRUE),
      yr_sd = sd(.data[[v]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Min row
  min_row <- data_used %>%
    filter(!is.na(.data[[v]])) %>%
    filter(.data[[v]] == min(.data[[v]], na.rm = TRUE)) %>%
    slice(1)
  
  # Max row
  max_row <- data_used %>%
    filter(!is.na(.data[[v]])) %>%
    filter(.data[[v]] == max(.data[[v]], na.rm = TRUE)) %>%
    slice(1)
  
  min_val <- round(min_row[[v]], 3)
  min_year <- min_row$Year
  
  max_val <- round(max_row[[v]], 3)
  max_year <- max_row$Year
  
  # Lowest yearly mean
  lowest_year <- yearly %>%
    filter(yr_mean == min(yr_mean, na.rm = TRUE)) %>%
    slice(1)
  
  lowest_year_text <- paste0(
    lowest_year$Year, " (",
    round(lowest_year$yr_mean, 3), " ± ",
    round(lowest_year$yr_sd, 3), ")"
  )
  
  # Highest yearly mean
  highest_year <- yearly %>%
    filter(yr_mean == max(yr_mean, na.rm = TRUE)) %>%
    slice(1)
  
  highest_year_text <- paste0(
    highest_year$Year, " (",
    round(highest_year$yr_mean, 3), " ± ",
    round(highest_year$yr_sd, 3), ")"
  )
  
  tibble(
    Variable = unname(var_label),
    Mean = round(mean(s_clean), 3),
    Median = round(median(s_clean), 3),
    SD = round(sd(s_clean), 3),
    `CV (%)` = round(sd(s_clean) / mean(s_clean) * 100, 1),
    
    Min = paste0(min_val, " (", min_year, ")"),
    Max = paste0(max_val, " (", max_year, ")"),
    
    Range = round(max(s_clean) - min(s_clean), 3),
    Q25 = round(quantile(s_clean, 0.25), 3),
    Q75 = round(quantile(s_clean, 0.75), 3),
    IQR = round(IQR(s_clean), 3),
    
    `Within-Year SD` = round(mean(yearly$yr_sd, na.rm = TRUE), 3),
    `Between-Year SD` = round(sd(yearly$yr_mean, na.rm = TRUE), 3),
    
    `Lowest Yearly Mean` = lowest_year_text,
    `Highest Yearly Mean` = highest_year_text
  )
  
}))

# save as CSV for Table S2
write.csv(overall_summary, "CSVs/summary_statistics_overall.csv", row.names = FALSE)

#yearly stats----

stats <- full_weekly_data|>
  mutate(Year = year(Date))

yearly_stats <- bind_rows(lapply(vars_to_use, function(v) {
  # Apply filter ONLY for DCM_depth
  if (v == "DCM_depth") {
    data_used <- stats %>%
      filter(max_conc > 20)
  } else {
    data_used <- stats
  }
  data_used %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Year) %>%
    summarise(
      Variable = v,
      N = n(),
      Mean = mean(.data[[v]], na.rm = TRUE),
      SD = sd(.data[[v]], na.rm = TRUE),
      CV_percent = (SD / Mean) * 100,
      Min = min(.data[[v]], na.rm = TRUE),
      Max = max(.data[[v]], na.rm = TRUE),
      Range = Max - Min,
      Median = median(.data[[v]], na.rm = TRUE),
      Q25 = quantile(.data[[v]], 0.25, na.rm = TRUE),
      Q75 = quantile(.data[[v]], 0.75, na.rm = TRUE),
      IQR = IQR(.data[[v]], na.rm = TRUE),
      .groups = "drop"
    )
}))

#save as CSV for Table S3
write.csv(
  yearly_stats,
  "CSVs/yearly_variable_stats.csv",
  row.names = FALSE
)


