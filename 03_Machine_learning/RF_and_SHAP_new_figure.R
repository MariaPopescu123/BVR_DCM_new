#RF and SHAP 
#This script runs Random Forest + SHAP workflows for two response variables: DCM depth and DCM magnitude.
# - DCM_depth (using weeks where max_conc > 20)
# - max_conc (all available phytoplankton weeks)
#
# It creates:
# - variable-importance with SHAP score plots (Fig. 5)
# - jackknife robustness heatmaps (Fig. S8)
# - SHAP-vs-value interaction panels for manuscript + supplement (Fig. 6, Fig. 7, Fig. S9, Fig. S10)
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

####Figure 5 — 2-panel layout: SHAP swarm + (%IncMSE) score column for each response####
# Built locally from RF helper outputs (does not modify new_var_importance_shap_plots.R).
# Layout per row: [SHAP value distribution]  [text column of %IncMSE scores]
# One shared z-scaled colour legend collected to the right.

pretty_lab_fn <- function(v) {
  if (is.null(variable_labels)) return(v)
  out <- unname(variable_labels[as.character(v)])
  out[is.na(out)] <- v[is.na(out)]
  out
}

build_score_col <- function(rf_result) {
  reversed_vars <- rev(rf_result$var_order)
  score_df <- rf_result$importance_table |>
    dplyr::filter(Variable %in% reversed_vars) |>
    dplyr::mutate(Variable = factor(Variable, levels = reversed_vars))

  ggplot(score_df, aes(x = 1, y = Variable,
                       label = sprintf("%.2f", `%IncMSE`))) +
    geom_text(size = 2.8) +
    scale_x_continuous(limits = c(0.5, 1.5)) +
    scale_y_discrete(drop = FALSE) +
    labs(x = "(%IncMSE)", y = NULL) +
    theme_classic(base_size = 10) +
    theme(
      panel.border    = element_blank(),
      axis.line.x     = element_blank(),
      axis.line.y     = element_line(colour = "grey50", linewidth = 0.4),
      axis.ticks      = element_blank(),
      axis.text.y     = element_blank(),
      axis.text.x     = element_text(colour = "transparent", size = 8),
      axis.title.x    = element_text(size = 9),
      axis.title.y    = element_blank(),
      plot.margin     = ggplot2::margin(5, 5, 5, 6)
    )
}

tag_style <- theme(
  plot.tag          = element_text(size = 11, face = "bold"),
  plot.tag.position = c(0.02, 0.99)
)

shap_depth  <- finaldepthRF_over20$plots$shap + labs(tag = "a") + tag_style
shap_mag    <- finalmagnitudeRF$plots$shap   + labs(tag = "b") + tag_style
score_depth <- build_score_col(finaldepthRF_over20)
score_mag   <- build_score_col(finalmagnitudeRF)

make_rf_caption <- function(label, md) {
  sprintf("%s: OOB R² = %.3f | n = %d | ntree = %d | mtry = %d | nodesize = %d",
          label, md$oob_r2, md$n,
          md$best_params$ntree, md$best_params$mtry, md$best_params$nodesize)
}
fig5_caption <- paste(
  make_rf_caption("Depth model",     finaldepthRF_over20$model_details),
  make_rf_caption("Magnitude model", finalmagnitudeRF$model_details),
  sep = "\n"
)

fig5_new <- (patchwork::wrap_plots(
  shap_depth, score_depth,
  shap_mag,   score_mag,
  ncol   = 2,
  widths = c(5, 1),
  guides = "collect"
) +
  plot_annotation(
    caption = fig5_caption,
    theme   = theme(plot.caption = element_text(size = 8, hjust = 0, lineheight = 1.1))
  )) &
  theme(legend.position = "right", legend.justification = "center")

ggsave(
  filename = here::here("Figs", "MachineLearning", "FinalCombined_Depth_Magnitude_RF.png"),
  plot     = fig5_new,
  width    = 5, height = 6, dpi = 900, bg = "white"
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
vars_to_plot <- head(finaldepthRF_over20$var_order, 6)


plot_shap_vs_value_loop(
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

#for supplemental figure (all variables) S9----
vars_to_plot <- finaldepthRF_over20$var_order

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
#for main manuscript figure 7----
magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- head(finalmagnitudeRF$var_order, 6)

plot_shap_vs_value_loop(
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

#for all variables figure S10----

magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- finalmagnitudeRF$var_order


plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "SImagnitude",
  analysis_label = "Predictor Effects on DCM Magnitude Prediction",
  var_labels = variable_labels,
  panel_ncol = 4
)
