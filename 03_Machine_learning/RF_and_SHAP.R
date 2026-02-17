#RF and SHAP 

#How to use the function
#xdataframe: either magnitude_analysis or depth_analysis
#XYear (range of dates you want to look at, if you want just one year then put same date for both)
#XYear2
#whichvars (this label helps name the file and title so that I can keep straight what I did differently)
#response_var (either DCM_depth or max_conc)
#save_dir = either "Depth" or "Magnitude"

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

####DEPTH weather lags to produce Figure S4####
met_lags <- full_weekly_data |>
  filter(max_conc>20)|>
  select(Date, DCM_depth, precip_lag1, precip_lag2,
         airtemp_lag1, airtemp_lag2,
         wind_lag1, wind_lag2)
depth_metRF <- var_importance_shap_plots(
  Xdataframe    = met_lags,
  XYear         = 2015,
  XYear2        = 2024,
  whichvars     = "Meteorological Lags",
  response_var  = "DCM_depth",
  save_dir      = "Depth",
  variable_labels = variable_labels
)
#------------------------------------------------------------------------------#
####MAGNITUDE weather lags####
met_lags <- full_weekly_data |>
  select(Date, max_conc,
         precip_lag1, precip_lag2,
         airtemp_lag1, airtemp_lag2,
         wind_lag1, wind_lag2)
magnitudemetRF <- var_importance_shap_plots(
  Xdataframe     = met_lags,
  XYear          = 2015,
  XYear2         = 2024,
  whichvars      = "Meteorological Lags",
  response_var   = "max_conc",
  save_dir       = "Magnitude",
  variable_labels = variable_labels
)

tag_theme <- theme(
  plot.tag = element_text(size = 16, face = "bold"),
  plot.tag.position = c(0.3, .98)
)

other_theme <- theme(
  plot.tag = element_text(size = 16, face = "bold"),
  plot.tag.position = c(0.2, .98)
)

p1 <- depth_metRF$plots$importance + labs(tag = "A") + tag_theme
p2 <- depth_metRF$plots$shap + labs(tag = "B") + other_theme
p3 <- magnitudemetRF$plots$importance + labs(tag = "C") + tag_theme
p4 <- magnitudemetRF$plots$shap + labs(tag = "D") + other_theme

combined_RF_panels <- (p1 + p2) / (p3 + p4)

ggsave(
  filename = here::here("Figs", "MachineLearning", "Met_combinedRF.png"),
  plot = combined_RF_panels,
  width = 18, height = 10, dpi = 600, bg = "white"
)


####Final Plots####
#------depth------####
depth_analysis_over20 <- full_weekly_data|>
  filter(max_conc>20)|>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max,depth_NO3NO2_ugL_max,depth_SRP_ugL_max, depth_SFe_mgL_max,
    airtemp_lag2, wind_lag2)

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

####------magnitude-------####
magnitude_analysis <- full_weekly_data|>
  select(Date, max_conc, WaterLevel_m, PZ, schmidt_stability, thermocline_depth,N_at_DCM,
         SFe_mgL_at_DCM,SRP_ugL_at_DCM, NH4_ugL_at_DCM, NO3NO2_ugL_at_DCM, 
         airtemp_lag1, wind_lag1)

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
  width = 18, height = 10, dpi = 600, bg = "white"
)

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
  analysis_label = "Depth Analysis", 
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
  "depth_NO3NO2_ugL_max",
  "wind_lag2",
  "airtemp_lag2"
)

plot_shap_vs_value_loop(
  shap_df = depthshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Depth"),
  prefix = "SIdepth",
  analysis_label = "Depth Analysis", 
  panel_ncol = 2,
  var_labels = variable_labels 
)


#----magnitude----
magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- c(
  "WaterLevel_m",
  "N_at_DCM",
  "wind_lag1",
  "PZ",
  "thermocline_depth",
  "NH4_ugL_at_DCM"
)

plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "magnitude",
  analysis_label = "Magnitude Analysis", 
  var_labels = variable_labels
)

#for all variables S9----

magnitudeshap <- finalmagnitudeRF$shap_long
vars_to_plot <- c(
  "WaterLevel_m",
  "N_at_DCM",
  "wind_lag1",
  "PZ",
  "thermocline_depth",
  "NH4_ugL_at_DCM",
  "schmidt_stability",
  "SFe_mgL_at_DCM",
  "NO3NO2_ugL_at_DCM",
  "SRP_ugL_at_DCM",
  "airtemp_lag1"
)




plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "SImagnitude",
  analysis_label = "Magnitude Analysis", 
  var_labels = variable_labels, 
  panel_ncol = 4
)

