library(ggplot2)
library(dplyr)
library(tidyr)

full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

####visualize chosen variables####

variable_labels <- c(
  max_conc = "Max Total Phytoplankton (µg/L)",
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
  depth_SFe_mgL_max = "Depth of Max Soluble Fe (m)",
  depth_SRP_ugL_max = "Depth of Max SRP (m)",
  depth_NH4_ugL_max = "Depth of Max NH₄⁺ (m)",
  Precip_Weekly  = "Precipitation Weekly Sum",
  precip_lag1    = "Precipitation Weekly Sum (Lag 1 wk)",
  precip_lag2    = "Precipitation Weekly Sum (Lag 2 wk)",
  AirTemp_Avg    = "Air Temperature Weekly Average",
  airtemp_lag1   = "Air Temperature Weekly Average (Lag 1 wk)",
  airtemp_lag2   = "Air Temperature Weekly Average (Lag 2 wk)",
  WindSpeed_Avg  = "Wind Speed Weekly Average",
  wind_lag1      = "Wind Speed Weekly Average (Lag 1 wk)",
  wind_lag2      = "Wind Speed Weekly Average (Lag 2 wk)"
)


####Function for running correlation and making plots 

plot_correlation_matrix <- function(df, file_name, cutoff = 0.6, 
                                    title = "Correlation Matrix Heatmap", 
                                    variable_labels = NULL) {
  library(ggplot2)
  library(dplyr)
  
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
      plot.title = element_text(size = 20 * scale_factor, hjust = 0.5)
    ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(cor_long$Var2))) +  # use the factor levels for y-axis
    coord_fixed() +
    labs(title = title)
  
  ggsave(file_name, plot = p, width = 20, height = 20, units = "in", dpi = 300, bg = "white")
  
  return(p)
}



#####Met Data#####
met_depth_corr <- full_weekly_data |>
  select(Date, DCM_depth, max_conc, 
         "Precip_Weekly", "precip_lag1", "precip_lag2",
         "AirTemp_Avg", "airtemp_lag1", "airtemp_lag2",
         "WindSpeed_Avg", "wind_lag1", "wind_lag2"
  )

if (!dir.exists("Figs/correlations")) {
  dir.create("Figs/correlations", recursive = TRUE)
}

plot_correlation_matrix(
  met_depth_corr,
  "Figs/correlations/Met_depth_corr_Matrix.png",
  title = "Meteorological Variables Correlation Matrix Heatmap", 
  variable_labels = variable_labels,
)

#####Physics#####
physics_depth_corr <- full_weekly_data |>
  select(Date, DCM_depth, max_conc, thermocline_depth, schmidt_stability, N_at_DCM 
  )
plot_correlation_matrix(
  physics_depth_corr,
  "Figs/correlations/physics_corr_Matrix.png",
  title = "Physical Variables Correlation Matrix Heatmap", 
  variable_labels = variable_labels,
)

####chosen variables####
full_weekly_data <- full_weekly_data|>
  select(
    "Date", "WaterLevel_m", "DCM_depth", "max_conc",
     "PZ", "thermocline_depth","schmidt_stability",
    "PZ_prop", "N_at_DCM", "depth_SRP_ugL_max", "SRP_ugL_at_DCM",
    "depth_NH4_ugL_max", "NH4_ugL_at_DCM",
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

