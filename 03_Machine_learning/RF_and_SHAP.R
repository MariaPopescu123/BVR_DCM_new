#RF and SHAP 
#This script runs Random Forest + SHAP workflows for two response variables: DCM depth and DCM magnitude.
# - DCM_depth (using weeks where max_conc > 20)
# - max_conc (all available phytoplankton weeks)
#
# It creates:
# - variable-importance with SHAP score plots (Fig. 5)
# - jackknife robustness heatmaps (Fig. S6)
# - SHAP-vs-value interaction panels for manuscript + Supporting Information (Fig. 6, Fig. 7, Fig. S9, Fig. S10)
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
#this is for viewing to see how robust the model is across years Fig. S6
#### Supporting Information Figure S6a (DCM depth jackknife heatmap) ####
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
  panel_label = "a"
)
# copy to Corresponding Figure folder
ggsave(here::here("Figs","Supporting Information","Figure S6a.png"),
       plot = depth_jackknife_over20$plot,
       width = 12, height = 8, dpi = 400, bg = "white")

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


#### Supporting Information Figure S6b (DCM magnitude jackknife heatmap) ####
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
  panel_label = "b"
)
# copy to Corresponding Figure folder
ggsave(here::here("Figs","Supporting Information","Figure S6b.png"),
       plot = magnitude_jackknife$plot,
       width = 12, height = 8, dpi = 400, bg = "white")
#if you want to view
#res$plot
#res$summary %>% as.data.frame() %>% head()

tag_theme <- theme(
  plot.tag = element_text(size = 10, face = "bold"),
  plot.tag.position = c(0.05, .98)
)

other_theme <- theme(
  plot.tag = element_text(size = 10, face = "bold"),
  plot.tag.position = c(0.05, .98)
)

p1 <- finaldepthRF_over20$plots$importance + labs(tag = "a") + tag_theme
p2 <- finaldepthRF_over20$plots$shap + labs(tag = "b") + other_theme
p3 <- finalmagnitudeRF$plots$importance + labs(tag = "c") + tag_theme
p4 <- finalmagnitudeRF$plots$shap + labs(tag = "d") + other_theme

make_rf_caption <- function(label, md) {
  sprintf("%s: OOB R² = %.2f | OOB RMSE = %.2f | n = %d | ntree = %d | mtry = %d | nodesize = %d",
          label, md$oob_r2, md$oob_rmse, md$n,
          md$best_params$ntree, md$best_params$mtry, md$best_params$nodesize)
}
fig5_caption <- paste(
  make_rf_caption("Depth model",     finaldepthRF_over20$model_details),
  make_rf_caption("Magnitude model", finalmagnitudeRF$model_details),
  sep = "\n"
)

combined_RF_panels <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    caption = fig5_caption,
    theme = theme(plot.caption = element_text(size = 10, hjust = 0, lineheight = 1.1))
  )

#### Main Manuscript Figure 5 ####
ggsave(
  filename = here::here("Figs", "MachineLearning", "Fig5_FinalCombined_Depth_Magnitude_RF.png"),
  plot = combined_RF_panels,
  width = 9, height = 5, dpi = 900, bg = "white"
)
# copy to Corresponding Figure folder
ggsave(
  filename = here::here("Figs", "Main Manuscript", "Figure5.png"),
  plot = combined_RF_panels,
  width = 9, height = 5, dpi = 900, bg = "white"
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
#### Main Manuscript Figure 6 ####
depthshap <- finaldepthRF_over20$shap_long
vars_to_plot <- head(finaldepthRF_over20$var_order, 6)


fig6 <- plot_shap_vs_value_loop(
  shap_df = depthshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Depth"),
  prefix = "depth",
  analysis_label = "Predictor Effects on DCM Depth Prediction",
  var_labels = variable_labels,
  panel_ncol = 3,
  panel_width = 9,
  panel_height = 5
)
# copy to Corresponding Figure folder
ggsave(here::here("Figs","Main Manuscript","Figure6.png"),
       plot = fig6$panel_plot,
       width = 9, height = 5, dpi = 1200, bg = "white")

#for Supporting Information figure (all variables) S9----
#### Supporting Information Figure S9 ####
vars_to_plot <- finaldepthRF_over20$var_order

figS9 <- plot_shap_vs_value_loop(
  shap_df = depthshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Depth"),
  prefix = "SIdepth",
  analysis_label = "Predictor Effects on DCM Depth Prediction",
  panel_ncol = 4,
  var_labels = variable_labels,
  # Save near print size so native font size ~= on-page size; text_scale lifts
  # the smallest fonts to ~9 pt so they render >= 8 pt in the manuscript.
  # Larger canvas gives each panel more room so the titles don't overlap.
  panel_width = 13,
  panel_height = 7,
  text_scale = 1.15
)
# copy to Corresponding Figure folder
ggsave(here::here("Figs","Supporting Information","Figure S9.png"),
       plot = figS9$panel_plot,
       width = 13, height = 7, dpi = 1200, bg = "white")


#----magnitude----
#for main manuscript figure 7----
#### Main Manuscript Figure 7 ####
magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- head(finalmagnitudeRF$var_order, 6)

fig7 <- plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "magnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction",
  var_labels = variable_labels,
  panel_ncol = 3,
  panel_width = 9,
  panel_height = 5
)
# copy to Corresponding Figure folder
ggsave(here::here("Figs","Main Manuscript","Figure7.png"),
       plot = fig7$panel_plot,
       width = 9, height = 5, dpi = 1200, bg = "white")

#for all variables figure S10----
#### Supporting Information Figure S10 ####
magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- finalmagnitudeRF$var_order


figS10 <- plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "SImagnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction",
  var_labels = variable_labels,
  panel_ncol = 4,
  panel_width = 13,
  panel_height = 7,
  text_scale = 1.15
)
# copy to Supporting Information folder
ggsave(here::here("Figs","Supporting Information","Figure S10.png"),
       plot = figS10$panel_plot,
       width = 13, height = 7, dpi = 1200, bg = "white")
