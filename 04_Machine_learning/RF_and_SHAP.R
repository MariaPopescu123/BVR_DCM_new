#RF and SHAP 

#How to use the function
#xdataframe: either magnitude_analysis or depth_analysis
#XYear (range of dates you want to look at, if you want just one year then put same date for both)
#XYear2
#whichvars (this label helps name the file and title so that I can keep straight what I did differently)
#response_var (either DCM_depth or max_conc)
#save_dir = either "Depth" or "Magnitude"

####DEPTH weather lags####
met_lags <- full_weekly_data |>
  filter(max_conc>20)|>
  select(Date, DCM_depth,
         Precip_Weekly, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
depth_metRF <- var_importance_shap_plots(Xdataframe = met_lags, 2015, 2024, "MET LAGS", "DCM_depth", "Depth", variable_labels = variable_labels)

#------------------------------------------------------------------------------#
####MAGNITUDEweather lags####
met_lags <- full_weekly_data |>
  select(Date, max_conc,
         Precip_Weekly, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
magnitudemetRF <- var_importance_shap_plots(Xdataframe = met_lags,
                                            2015, 2024, "MET LAGS", "max_conc", "Magnitude", variable_labels = variable_labels)

#combine the plots
combined_RF_panels <- depth_metRF$plots$combined /
  magnitudemetRF$plots$combined

ggsave(
  filename = here::here("Figs", "MachineLearning", "Met_combinedRF.png"),
  plot = combined_RF_panels,
  width = 14, height = 10, dpi = 600, bg = "white"
)

####Final Plots####
#######With max_conc >20######

#------depth------####
depth_analysis_over20 <- full_weekly_data|>
  filter(max_conc>20)|>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max, depth_SRP_ugL_max, depth_SFe_mgL_max,
    wind_lag2, airtemp_lag2)

finaldepthRF_over20 <- var_importance_shap_plots(Xdataframe = depth_analysis_over20, 2015, 2024,
                                          "","DCM_depth", "Depth", variable_labels = variable_labels)
#####Jackknife#####
#this is for viewing to see how robust the model is across years
depth_jackknife_over20 <- jackknife_incMSE_heatmap(
  Xdataframe     = depth_analysis_over20,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Depth",
  var_order = finaldepthRF_over20$var_order,
  response_var   = "DCM_depth",
  whichvars_label= "over20",
  save_path      = here::here("Figs","MachineLearning","Depth","Jackknife_Heatmap_over20_5.png"), 
  variable_labels = variable_labels
)

####------magnitude-------####
magnitude_analysis_over20 <- full_weekly_data|>
  select(Date, max_conc, WaterLevel_m, PZ, schmidt_stability, thermocline_depth,N_at_DCM,
         SFe_mgL_at_DCM,SRP_ugL_at_DCM, NH4_ugL_at_DCM, 
         Precip_Weekly, AirTemp_Avg, wind_lag1)|>
  filter(max_conc>20)

finalmagnitudeRF_over20 <- var_importance_shap_plots(magnitude_analysis_over20,
                                              2015, 2024,
                                              "", "max_conc", "Magnitude",  
                                              variable_labels = variable_labels)


magnitude_jackknife_over20 <- jackknife_incMSE_heatmap(
  Xdataframe     = magnitude_analysis_over20,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Magnitude",
  var_order = finalmagnitudeRF_over20$var_order,
  response_var   = "max_conc",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","Magnitude","OVER20MagnitudeJackknife_Heatmap.png"), 
  variable_labels = variable_labels
)
#if you want to view
#res$plot
#res$summary %>% as.data.frame() %>% head()

#now to combine the two plots from depth and magnitude
combined_RF_panels <- finaldepthRF_over20$plots$combined /
  finalmagnitudeRF_over20$plots$combined
print(combined_RF_panels)

ggsave(
  filename = here::here("Figs", "MachineLearning", "OVER20Combined_Depth_Magnitude_RF.png"),
  plot = combined_RF_panels,
  width = 14, height = 10, dpi = 600, bg = "white"
)

####model information####
print(finaldepthRF_over20$tuning_scores)
print(finaldepthRF_over20$importance_table) #most helpful
print(finaldepthRF_over20$shap_long)
print(finaldepthRF_over20$shap_long_filtered)
print(finaldepthRF_over20$model_details) #this too
print(finaldepthRF_over20$counts)




print(finalmagnitudeRF_over20$tuning_scores)
print(finalmagnitudeRF_over20$importance_table) #most helpful
print(finalmagnitudeRF_over20$shap_long)
print(finalmagnitudeRF_over20$shap_long_filtered)
print(finalmagnitudeRF_over20$model_details)
print(finalmagnitudeRF_over20$counts)







####-----interaction plots for SHAP vs value----####

####----depth####
depthshap <- finaldepthRF_over20$shap_long
vars_to_plot <- c(
  "PZ",
  "thermocline_depth",
  "schmidt_stability",
  "WaterLevel_m",
  "depth_NH4_ugL_max",
  "depth_SRP_ugL_max",
  "depth_SFe_mgL_max",
  "wind_lag2",
  "airtemp_lag2"
)
plot_shap_vs_value_loop(
  shap_df = depthshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Depth"),
  prefix = "depth",
  analysis_label = "Depth Analysis", 
  var_labels = variable_labels 
)


#----magnitude----
magnitudeshap <- finalmagnitudeRF_over20$shap_long
vars_to_plot <- c(
  "WaterLevel_m",
  "PZ",
  "schmidt_stability",
  "thermocline_depth",
  "N_at_DCM",
  "SFe_mgL_at_DCM",
  "NH4_ugL_at_DCM",
  "SRP_ugL_at_DCM",
  "Precip_Weekly",
  "AirTemp_Avg",
  "wind_lag1"
)
plot_shap_vs_value_loop(
  shap_df = magnitudeshap,
  vars_to_plot = vars_to_plot,
  out_dir = here::here("Figs/MachineLearning/SHAP_interaction/Magnitude"),
  prefix = "magnitude",
  analysis_label = "Magnitude Analysis", 
  var_labels = variable_labels
)
