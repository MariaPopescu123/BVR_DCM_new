#RF and SHAP 

#How to use the function
#xdataframe: either magnitude_analysis or depth_analysis
#XYear (range of dates you want to look at, if you want just one year then put same date for both)
#XYear2
#whichvars (this label helps name the file and title so that I can keep straight what I did differently)
#response_var (either DCM_depth or max_conc)
#save_dir = either "Depth" or "Magnitude"

####Depth Analysis####
#to inform decisions on which variables will go into the final model
#####ALL VARS####
depth_analysis <- read.csv("CSVs/depth_analysis_revised.csv")
depth_ML <- var_importance_shap_plots(Xdataframe = depth_analysis, 2015, 2024, "","DCM_depth", "Depth")
depth_ML
#to see model_details
print(depth_ML$model_details)
#Variable Importance & SHAP: DCM_depth (2015-2024) — all variables
#Best RF: ntree=200, mtry=3, nodesize=2 | n=133 | R²=0.938

#####all vars individual years#####
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = depth_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = " ",
    response_var = "DCM_depth",
    save_dir = "Depth"
  )
  all_plots[[as.character(i)]] <- p
}
# Combine all 10 plots in a grid (e.g., 2 rows x 5 columns)
combined_all <- wrap_plots(all_plots, ncol = 1)
# Save all together
ggsave(
  here::here("Figs", "MachineLearning", "Depth","all years combined", "AllYears_Combined_ALL_VARIABLES.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

#--------------Final Variables all years----------------------
final_depth_analysis <- depth_analysis |>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max, depth_SRP_ugL_max, depth_SFe_mgL_max,
    wind_lag1, airtemp_lag2, precip_lag1
  )
#write.csv(selected_depth_analysis, "CSVs/selected_depth_analysis.csv", row.names = FALSE)

finaldepthRF <- var_importance_shap_plots(Xdataframe = final_depth_analysis, 2015, 2024,
                                          "","DCM_depth", "Depth", variable_labels = variable_labels)

#I want this function to also return a list called var_order

#####Selected variables individual years#####
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = final_depth_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = "final",
    response_var = "DCM_depth",
    save_dir = "Depth", 
    variable_labels = variable_labels
  )
  all_plots[[as.character(i)]] <- p[[1]]
}
combined_all <- wrap_plots(all_plots, ncol = 1)
ggsave(
  here::here("Figs", "MachineLearning", "Depth","all years combined", "AllYears_Combined_Final.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

#####Jacknife#####
#this is for viewing to see how robust the model is across years
depth_jackknife <- jackknife_incMSE_heatmap(
  Xdataframe     = final_depth_analysis,
  year_min       = 2015,
  year_max       = 2024,
  var_order = finaldepthRF$var_order,
  response_var   = "DCM_depth",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","Depth","Jackknife_Heatmap.png"), 
  variable_labels = variable_labels
)


####weather lags####
met_lags <- full_weekly_data |>
  filter(max_conc>20)|>
  select(Date, DCM_depth,
         Precip_Weekly, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
met_RFSHAP <- var_importance_shap_plots(Xdataframe = met_lags, 2015, 2024, "MET LAGS", "DCM_depth", "Depth", variable_labels = variable_labels)


all_plots <- list()

for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = met_lags,
    XYear = i,
    XYear2 = i,
    whichvars = "ALL MET LAGS",
    response_var = "DCM_depth",
    save_dir = "Depth"
  )
  all_plots[[as.character(i)]] <- p
}

combined_all <- wrap_plots(all_plots, ncol = 1)

ggsave(
  here::here("Figs", "MachineLearning", "Depth","all years combined", "AllYears_Combined_selected_variables.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

#------------------------------------------------------------------------------#
####Magnitude Analysis####
magnitude_analysis <- read.csv("CSVs/magnitude_analysis_revised.csv")

####weather lags####
met_lags <- full_weekly_data |>
  select(Date, max_conc,
         Precip_Weekly, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
var_importance_shap_plots(Xdataframe = met_lags, 2015, 2024, "MET LAGS", "max_conc", "Magnitude")

####Final Plots####

finalmagnitudeRF <- var_importance_shap_plots(magnitude_analysis,
                                              2015, 2024,
                                              "", "max_conc", "Magnitude",  
                                              variable_labels = variable_labels)

#individual years
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = final_magnitude_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = "",
    response_var = "max_conc",
    save_dir = "Magnitude"
  )
  all_plots[[as.character(i)]] <- p[[1]]
}
combined_all <- wrap_plots(all_plots, ncol = 1)
ggsave(
  here::here("Figs", "MachineLearning", "Magnitude","all years combined", "Final.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

magnitude_jackknife <- jackknife_incMSE_heatmap(
  Xdataframe     = magnitude_analysis,
  year_min       = 2015,
  year_max       = 2024,
  metric = "DCM Magnitude Full Data",
  var_order = finalmagnitudeRF$var_order,
  response_var   = "max_conc",
  whichvars_label= "",
  save_path      = here::here("Figs","MachineLearning","Magnitude","FULLDATAMagnitudeJackknife_Heatmap.png"), 
  variable_labels = variable_labels
)
#if you want to view
#res$plot
#res$summary %>% as.data.frame() %>% head()

#now to combine the two plots from depth and magnitude
combined_RF_panels <- finaldepthRF$plots$combined /
  finalmagnitudeRF$plots$combined

ggsave(
  filename = here::here("Figs", "MachineLearning", "Combined_Depth_Magnitude_RF.png"),
  plot = combined_RF_panels,
  width = 14, height = 10, dpi = 600, bg = "white"
)


#######With max_conc >20######

#------depth------#
depth_analysis_over20 <- full_weekly_data|>
  filter(max_conc>20)|>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max, depth_SRP_ugL_max, depth_SFe_mgL_max,
    wind_lag1, airtemp_lag2, precip_lag1  
  )

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
  save_path      = here::here("Figs","MachineLearning","Depth","Jackknife_Heatmap_over20_3.png"), 
  variable_labels = variable_labels
)

#------magnitude-------#
magnitude_analysis_over20 <- full_weekly_data|>
  select(Date, max_conc, WaterLevel_m, PZ, schmidt_stability, thermocline_depth,N_at_DCM,
         SFe_mgL_max_val,SRP_ugL_max_val, NH4_ugL_max_val, 
         precip_lag1, airtemp_lag2, wind_lag1)|>
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
