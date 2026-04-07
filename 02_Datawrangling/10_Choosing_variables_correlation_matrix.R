# #this script:
# 1) creates labels for ML figures
# 2) makes correlation matrix
# 3) summarizes statistics for Table S2 and Table S3
#
# Inputs:
# - CSVs/full_weekly_data.csv (created by 09_join_all_frames.R)
# - variable_labels object (created in 01_DataDownload.R)
# Output figures will be written to: Figs/correlations/

if (!exists("variable_labels", inherits = TRUE)) {
  stop("variable_labels is missing. Run 01_DataDownload.R before 10_Choosing_variables_correlation_matrix.R.")
}


full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

if (!dir.exists("Figs/correlations")){
  dir.create("Figs/correlations")
}

####visualize chosen variables####
# variable_labels defined in 01_DataDownload.R


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

#looking at the remainder of the variables to choose which are redundant
####chosen variables####
full_weekly_data <- full_weekly_data|>
  select(
    "Date", "WaterLevel_m", "DCM_depth", "max_conc",
     "PZ", "thermocline_depth", "schmidt_stability",
    "surface_temp", "temp_at_DCM",
    "PZ_prop", "N_at_DCM", "depth_SRP_ugL_max", "SRP_ugL_at_DCM",
    "depth_NH4_ugL_max", "NH4_ugL_at_DCM", "depth_NO3NO2_ugL_max", "NO3NO2_ugL_at_DCM",
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
# check$correlations  # uncomment to view interactively

# Checking collinear metrics
# Buoyancy Frequency, Schmidt Stability, surface temp, and temperature at DCM are
# all metrics of temperature and stability so let's check the actual scores and see
# which ones are most collinear and which ones are most correlated with DCM depth and DCM magnitude
# adding air temperature to show collinearity between temp metrics 

thermal_vars <- c("DCM_depth", "max_conc", "N_at_DCM", "schmidt_stability", "surface_temp", "temp_at_DCM", "AirTemp_Avg", "airtemp_lag1", "airtemp_lag2")
thermal_cor <- cor(full_weekly_data[, thermal_vars], method = "spearman", use = "pairwise.complete.obs")

# All possible pairings of the 4 thermal/stability variables
pairings <- list(
  c("temp_at_DCM", "N_at_DCM"),
  c("surface_temp", "schmidt_stability"),
  c("temp_at_DCM", "surface_temp"),
  c("temp_at_DCM", "schmidt_stability"),
  c("N_at_DCM", "surface_temp"),
  c("N_at_DCM", "schmidt_stability"),
  c("AirTemp_Avg", "surface_temp"),
  c("airtemp_lag1", "surface_temp"),
  c("airtemp_lag2", "surface_temp")
)

pairing_table <- do.call(rbind, lapply(pairings, function(p) {
  data.frame(
    Var1 = p[1],
    Var2 = p[2],
    Inter_correlation = round(thermal_cor[p[1], p[2]], 3),
    Var1_vs_DCM_depth = round(thermal_cor[p[1], "DCM_depth"], 3),
    Var2_vs_DCM_depth = round(thermal_cor[p[2], "DCM_depth"], 3),
    Var1_vs_max_conc = round(thermal_cor[p[1], "max_conc"], 3),
    Var2_vs_max_conc = round(thermal_cor[p[2], "max_conc"], 3)
  )
}))

print(pairing_table)

# Export pairing table as a figure
library(gridExtra)

# Use readable labels for the table
pairing_display <- pairing_table
pairing_display$Var1 <- variable_labels[pairing_display$Var1]
pairing_display$Var2 <- variable_labels[pairing_display$Var2]
colnames(pairing_display) <- c(
  "Variable 1", "Variable 2", "Inter-\ncorrelation",
  "Var1 vs\nDCM Depth", "Var2 vs\nDCM Depth",
  "Var1 vs\nDCM Magnitude", "Var2 vs\nDCM Magnitude"
)

tt <- ttheme_minimal(
  core    = list(fg_params = list(fontsize = 9, hjust = 0.5, x = 0.5),
                 padding   = unit(c(6, 4), "mm")),
  colhead = list(fg_params = list(fontsize = 9, fontface = "bold", hjust = 0.5, x = 0.5),
                 padding   = unit(c(6, 4), "mm"))
)

tbl_grob <- tableGrob(pairing_display, rows = NULL, theme = tt)

ggsave(
  filename = "Figs/correlations/thermal_pairing_table.png",
  plot = tbl_grob,
  width = 12, height = 3, units = "in", dpi = 300, bg = "white"
)

# For DCM depth: schmidt_stability will be used for temp metrics
# surface temp is highly correlated with air temperature which is already being used in the model 
# For DCM magnitude: N_at_DCM + schmidt stability are most correlated w DCM magnitude and
# have low collinearity
