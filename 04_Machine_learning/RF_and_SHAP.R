#RF and SHAP 

####Depth Analysis####

######All variables#####
#to inform decisions on which variables will go into the final model
depth_analysis <- read.csv("CSVs/depth_analysis_frame.csv")
depth_var_importance_shap_plots(Xdataframe = depth_analysis, 2015, 2024, "TEST ALL VARIABLES")

#all variables excluding 2022 the drawdowm 
depth_var_importance_shap_plots(Xdataframe = depth_analysis, 2015, 2021, "ALL VARS excl 2022")

#selected variables
selected_depth_analysis <- depth_analysis |>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max, depth_SRP_ugL_max, depth_SFe_mgL_max,
    wind_lag1, airtemp_lag2, precip_lag1
  )

depth_var_importance_shap_plots(Xdataframe = selected_depth_analysis, 2015, 2024, "selected variables")


#To determine weather lags
met_lags <- depth_analysis |>
  select(Date, DCM_depth,
         Precip_Avg, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
var_importance_shap_plots(Xdataframe = met_lags, 2015, 2024, "ALL MET LAGS")
