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
  metals_depth_corr,
  "Figs/DCM_Depth_Correlations/Chem_depth_corr_Matrix.png",
  title = "Chem Depth Variables Correlation Matrix Heatmap"
)

#####Met Data#####
met_depth_corr <- depth_analysis |>
  select(
    "Precip_Avg", "precip_lag1", "precip_lag2",
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
  select(thermocline_depth, schmidt_stability
  )
plot_correlation_matrix(
  physics_depth_corr,
  "Figs/DCM_Depth_Correlations/physics_depth_corr_Matrix.png",
  title = "Physics Depth Variables Correlation Matrix Heatmap"
)

#####Phytos#####
#may do this



#### CM magnitude analysis####
#prep dataframe
magnitude_analysis <- full_weekly_data |>
  select(-starts_with("depth_"), -Week, -totals_mean, -totals_med, -Date.x, -Date.y.y, -Date.x.x)
write.csv(magnitude_analysis, "CSVs/magnitude_analysis_frame.csv", row.names = FALSE)
#correlation matrix
plot_correlation_matrix(magnitude_analysis, "Figs/DCM_Magnitude_Correlations/DCM_Magnitude_Correlation_Matrix.png", 
                        title = "DCM Magnitude Variables Correlation Matrix Heatmap")
