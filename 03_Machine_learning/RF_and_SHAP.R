#RF and SHAP 
#This script runs Random Forest + SHAP workflows for two response variables: DCM depth and DCM magnitude.
# - DCM_depth (using weeks where max_conc > 20)
# - max_conc (all available phytoplankton weeks)
#
# It creates:
# - variable-importance and SHAP summary plots
# - jackknife robustness heatmaps
# - SHAP-vs-value interaction panels for manuscript + supplement
#
# Inputs expected in the workspace from prior scripts:
# - full_data, variable_labels
# - helper functions sourced in 01_DataDownload.R:
#   var_importance_shap_plots(), jackknife_incMSE_heatmap(), plot_shap_vs_value_loop()

required_objects <- c("full_data", "variable_labels")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for RF_and_SHAP.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 01_DataDownload.R and 02_Datawrangling/09_join_all_frames.R first.")
}

required_functions <- c("var_importance_shap_plots", "jackknife_incMSE_heatmap", "plot_shap_vs_value_loop")
missing_functions <- required_functions[!vapply(required_functions, exists, logical(1), mode = "function", inherits = TRUE)]
if (length(missing_functions) > 0) {
  stop("Missing required helper functions for RF_and_SHAP.R: ",
       paste(missing_functions, collapse = ", "),
       ". Re-run 01_DataDownload.R to source Functions/*.R.")
}

dirs <- c(
  "Figs/MachineLearning",
  "Figs/MachineLearning/Depth",
  "Figs/MachineLearning/Magnitude",
  "Figs/MachineLearning/SHAP_interaction/Depth",
  "Figs/MachineLearning/SHAP_interaction/Magnitude"
)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

####Final Plots####
#------depth------####
depth_analysis_over20 <- full_data|>
  filter(max_conc>20)|>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max,depth_NO3NO2_ugL_max,depth_SRP_ugL_max, depth_SFe_mgL_max,
    air_temp_week_avg, wind_week_avg, precip_week_sum)

finaldepthRF_over20 <- var_importance_shap_plots(
  Xdataframe      = depth_analysis_over20,
  XYear           = 2015,
  XYear2          = 2024,
  whichvars       = "",
  response_var    = "DCM_depth",
  save_dir        = "Depth",
  variable_labels = variable_labels
)

#####Jackknife#####
#this is for viewing to see how robust the model is across years
depth_jackknife_over20 <- jackknife_incMSE_heatmap(
  Xdataframe     = depth_analysis_over20,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Depth",
  var_order = finaldepthRF_over20$var_order,
  response_var   = "DCM_depth",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","Depth","Jackknife_Heatmap_over20.png"), 
  variable_labels = variable_labels, 
  panel_label = "A"
)

print(depth_jackknife_over20$plot)
print(depth_jackknife_over20$imp_summary)
print(depth_jackknife_over20$plot_df)
print(depth_jackknife_over20$imp_long)
print(depth_jackknife_over20$overall_importance)
print(depth_jackknife_over20$year_stats)

####------magnitude-------####
magnitude_analysis <- full_data|>
  select(Date, max_conc, WaterLevel_m, PZ, schmidt_stability, thermocline_depth, N_at_DCM,
         SFe_mgL_at_DCM,SRP_ugL_at_DCM, NH4_ugL_at_DCM, NO3NO2_ugL_at_DCM,
         air_temp_week_avg, wind_week_avg, precip_week_sum)

finalmagnitudeRF <- var_importance_shap_plots(
  Xdataframe      = magnitude_analysis,
  XYear           = 2015,
  XYear2          = 2024,
  whichvars       = "",
  response_var    = "max_conc",
  save_dir        = "Magnitude",
  variable_labels = variable_labels
)


magnitude_jackknife <- jackknife_incMSE_heatmap(
  Xdataframe     = magnitude_analysis,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Magnitude",
  var_order = finalmagnitudeRF$var_order,
  response_var   = "max_conc",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","Magnitude","MagnitudeJackknife_Heatmap.png"), 
  variable_labels = variable_labels, 
  panel_label = "B"
)
#if you want to view
#res$plot
#res$summary %>% as.data.frame() %>% head()

tag_theme <- theme(
  plot.tag = element_text(size = 16, face = "bold"),
  plot.tag.position = c(0.3, .98)
)

other_theme <- theme(
  plot.tag = element_text(size = 16, face = "bold"),
  plot.tag.position = c(0.2, .98)
)

p1 <- finaldepthRF_over20$plots$importance + labs(tag = "A") + tag_theme
p2 <- finaldepthRF_over20$plots$shap + labs(tag = "B") + other_theme
p3 <- finalmagnitudeRF$plots$importance + labs(tag = "C") + tag_theme
p4 <- finalmagnitudeRF$plots$shap + labs(tag = "D") + other_theme

combined_RF_panels <- (p1 + p2) / (p3 + p4)

ggsave(
  filename = here::here("Figs", "MachineLearning", "FinalCombined_Depth_Magnitude_RF.png"),
  plot = combined_RF_panels,
  width = 18, height = 10, dpi = 900, bg = "white"
)

print(magnitude_jackknife$plot)
print(magnitude_jackknife$imp_summary)
print(magnitude_jackknife$plot_df)
print(magnitude_jackknife$imp_long)
print(magnitude_jackknife$overall_importance)
print(magnitude_jackknife$year_stats)


####model information####
print(finaldepthRF_over20$tuning_scores)
print(finaldepthRF_over20$importance_table) #most helpful
print(finaldepthRF_over20$shap_long)
print(finaldepthRF_over20$shap_long_filtered)
print(finaldepthRF_over20$model_details) #this too
print(finaldepthRF_over20$counts)




print(finalmagnitudeRF$tuning_scores)
print(finalmagnitudeRF$importance_table) #most helpful
print(finalmagnitudeRF$shap_long)
print(finalmagnitudeRF$shap_long_filtered)
print(finalmagnitudeRF$model_details)
print(finalmagnitudeRF$counts)

#to see the number of observations that are used to calculate OOB
cat("Depth OOB R2 observations:", finaldepthRF_over20$model_details$n, "\n")
cat("Magnitude OOB R2 observations:", finalmagnitudeRF$model_details$n, "\n")


####-----interaction plots for SHAP vs value----####

####----depth####
#for main manuscript figure 6----
depthshap <- finaldepthRF_over20$shap_long
vars_to_plot <- c(
  "PZ",
  "WaterLevel_m",
  "depth_NH4_ugL_max",
  "depth_SFe_mgL_max",
  "depth_SRP_ugL_max",
  "thermocline_depth"
)


plot_shap_vs_value_loop(
  shap_df = depthshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Depth"),
  prefix = "depth",
  analysis_label = "Predictor Effects on DCM Depth Prediction", 
  var_labels = variable_labels 
)

#for supplemental figure (all variables) S8----
vars_to_plot <- c(
  "PZ",
  "WaterLevel_m",
  "depth_NH4_ugL_max",
  "depth_SFe_mgL_max",
  "depth_SRP_ugL_max",
  "thermocline_depth",
  "schmidt_stability",
  "N_at_DCM",
  "depth_NO3NO2_ugL_max",
  "wind_week_avg",
  "air_temp_week_avg"
)

plot_shap_vs_value_loop(
  shap_df = depthshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Depth"),
  prefix = "SIdepth",
  analysis_label = "Predictor Effects on DCM Depth Prediction", 
  panel_ncol = 4,
  var_labels = variable_labels 
)


#----magnitude----
magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- c(
  "WaterLevel_m",
  "N_at_DCM",
  "wind_week_avg",
  "PZ",
  "thermocline_depth",
  "NH4_ugL_at_DCM"
)

plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "magnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction", 
  var_labels = variable_labels, 
  panel_ncol = 3
)

#for all variables S9----

magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- c(
  "WaterLevel_m",
  "N_at_DCM",
  "wind_week_avg",
  "PZ",
  "thermocline_depth",
  "NH4_ugL_at_DCM",
  "schmidt_stability",
  "SFe_mgL_at_DCM",
  "NO3NO2_ugL_at_DCM",
  "SRP_ugL_at_DCM",
  "air_temp_week_avg"
)


plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "SImagnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction", 
  var_labels = variable_labels, 
  panel_ncol = 4
)

####ALL ANALYSIS WITHOUT WATERLEVEL####

no_wl_dirs <- c(
  "Figs/MachineLearning/NO WATER LEVEL",
  "Figs/MachineLearning/NO WATER LEVEL/Depth",
  "Figs/MachineLearning/NO WATER LEVEL/Magnitude",
  "Figs/MachineLearning/NO WATER LEVEL/SHAP_interaction/Depth",
  "Figs/MachineLearning/NO WATER LEVEL/SHAP_interaction/Magnitude"
)

for (d in no_wl_dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

#------depth------####
depth_analysis_over20_noWL <- full_data|>
  filter(max_conc>20)|>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability,
    depth_NH4_ugL_max,depth_NO3NO2_ugL_max,depth_SRP_ugL_max, depth_SFe_mgL_max,
    air_temp_week_avg, wind_week_avg, precip_week_sum)

finaldepthRF_over20_noWL <- var_importance_shap_plots(
  Xdataframe      = depth_analysis_over20_noWL,
  XYear           = 2015,
  XYear2          = 2024,
  whichvars       = "",
  response_var    = "DCM_depth",
  save_dir        = "NO WATER LEVEL/Depth",
  variable_labels = variable_labels
)

#####Jackknife#####
depth_jackknife_over20_noWL <- jackknife_incMSE_heatmap(
  Xdataframe     = depth_analysis_over20_noWL,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Depth",
  var_order = finaldepthRF_over20_noWL$var_order,
  response_var   = "DCM_depth",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","NO WATER LEVEL","Depth","Jackknife_Heatmap_over20.png"),
  variable_labels = variable_labels,
  panel_label = "A"
)

print(depth_jackknife_over20_noWL$plot)
print(depth_jackknife_over20_noWL$imp_summary)
print(depth_jackknife_over20_noWL$plot_df)
print(depth_jackknife_over20_noWL$imp_long)
print(depth_jackknife_over20_noWL$overall_importance)
print(depth_jackknife_over20_noWL$year_stats)

####------magnitude-------####
magnitude_analysis_noWL <- full_data|>
  select(Date, max_conc, PZ, schmidt_stability, thermocline_depth, N_at_DCM,
         SFe_mgL_at_DCM,SRP_ugL_at_DCM, NH4_ugL_at_DCM, NO3NO2_ugL_at_DCM,
         air_temp_week_avg, wind_week_avg, precip_week_sum)

finalmagnitudeRF_noWL <- var_importance_shap_plots(
  Xdataframe      = magnitude_analysis_noWL,
  XYear           = 2015,
  XYear2          = 2024,
  whichvars       = "",
  response_var    = "max_conc",
  save_dir        = "NO WATER LEVEL/Magnitude",
  variable_labels = variable_labels
)


magnitude_jackknife_noWL <- jackknife_incMSE_heatmap(
  Xdataframe     = magnitude_analysis_noWL,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Magnitude",
  var_order = finalmagnitudeRF_noWL$var_order,
  response_var   = "max_conc",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","NO WATER LEVEL","Magnitude","MagnitudeJackknife_Heatmap.png"),
  variable_labels = variable_labels,
  panel_label = "B"
)

p1 <- finaldepthRF_over20_noWL$plots$importance + labs(tag = "A") + tag_theme
p2 <- finaldepthRF_over20_noWL$plots$shap + labs(tag = "B") + other_theme
p3 <- finalmagnitudeRF_noWL$plots$importance + labs(tag = "C") + tag_theme
p4 <- finalmagnitudeRF_noWL$plots$shap + labs(tag = "D") + other_theme

combined_RF_panels_noWL <- (p1 + p2) / (p3 + p4)

ggsave(
  filename = here::here("Figs", "MachineLearning", "NO WATER LEVEL", "FinalCombined_Depth_Magnitude_RF.png"),
  plot = combined_RF_panels_noWL,
  width = 18, height = 10, dpi = 900, bg = "white"
)

print(magnitude_jackknife_noWL$plot)
print(magnitude_jackknife_noWL$imp_summary)
print(magnitude_jackknife_noWL$plot_df)
print(magnitude_jackknife_noWL$imp_long)
print(magnitude_jackknife_noWL$overall_importance)
print(magnitude_jackknife_noWL$year_stats)


####model information####
print(finaldepthRF_over20_noWL$tuning_scores)
print(finaldepthRF_over20_noWL$importance_table) #most helpful
print(finaldepthRF_over20_noWL$shap_long)
print(finaldepthRF_over20_noWL$shap_long_filtered)
print(finaldepthRF_over20_noWL$model_details) #this too
print(finaldepthRF_over20_noWL$counts)


print(finalmagnitudeRF_noWL$tuning_scores)
print(finalmagnitudeRF_noWL$importance_table) #most helpful
print(finalmagnitudeRF_noWL$shap_long)
print(finalmagnitudeRF_noWL$shap_long_filtered)
print(finalmagnitudeRF_noWL$model_details)
print(finalmagnitudeRF_noWL$counts)

cat("Depth OOB R2 observations (no WL):", finaldepthRF_over20_noWL$model_details$n, "\n")
cat("Magnitude OOB R2 observations (no WL):", finalmagnitudeRF_noWL$model_details$n, "\n")


####-----interaction plots for SHAP vs value----####

####----depth####
#for main manuscript figure 6----
depthshap_noWL <- finaldepthRF_over20_noWL$shap_long
vars_to_plot <- c(
  "PZ",
  "depth_NH4_ugL_max",
  "depth_SFe_mgL_max",
  "depth_SRP_ugL_max",
  "thermocline_depth"
)


plot_shap_vs_value_loop(
  shap_df = depthshap_noWL,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/NO WATER LEVEL/SHAP_interaction/Depth"),
  prefix = "depth",
  analysis_label = "Predictor Effects on DCM Depth Prediction",
  var_labels = variable_labels
)

#for supplemental figure (all variables) S8----
vars_to_plot <- c(
  "PZ",
  "depth_NH4_ugL_max",
  "depth_SFe_mgL_max",
  "depth_SRP_ugL_max",
  "thermocline_depth",
  "schmidt_stability",
  "N_at_DCM",
  "depth_NO3NO2_ugL_max",
  "wind_week_avg",
  "air_temp_week_avg"
)

plot_shap_vs_value_loop(
  shap_df = depthshap_noWL,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/NO WATER LEVEL/SHAP_interaction/Depth"),
  prefix = "SIdepth",
  analysis_label = "Predictor Effects on DCM Depth Prediction",
  panel_ncol = 4,
  var_labels = variable_labels
)


#----magnitude----
magnitudeshap_noWL <- finalmagnitudeRF_noWL$shap_long
vars_to_plot <- c(
  "N_at_DCM",
  "wind_week_avg",
  "PZ",
  "thermocline_depth",
  "NH4_ugL_at_DCM"
)

plot_shap_vs_value_loop(
  shap_df = magnitudeshap_noWL,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/NO WATER LEVEL/SHAP_interaction/Magnitude"),
  prefix = "magnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction",
  var_labels = variable_labels,
  panel_ncol = 3
)

#for all variables S9----

magnitudeshap_noWL <- finalmagnitudeRF_noWL$shap_long
vars_to_plot <- c(
  "N_at_DCM",
  "wind_week_avg",
  "PZ",
  "thermocline_depth",
  "NH4_ugL_at_DCM",
  "schmidt_stability",
  "SFe_mgL_at_DCM",
  "NO3NO2_ugL_at_DCM",
  "SRP_ugL_at_DCM",
  "air_temp_week_avg"
)


plot_shap_vs_value_loop(
  shap_df = magnitudeshap_noWL,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/NO WATER LEVEL/SHAP_interaction/Magnitude"),
  prefix = "SImagnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction",
  var_labels = variable_labels,
  panel_ncol = 4
)

