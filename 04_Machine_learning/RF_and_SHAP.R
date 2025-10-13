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
depth_analysis <- read.csv("CSVs/depth_analysis_frame.csv")
var_importance_shap_plots(Xdataframe = depth_analysis, 2015, 2024, "all variables","DCM_depth", "Depth")

#####all vars individual years#####
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = depth_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = "all variables",
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

#####Selected variables All Years####
selected_depth_analysis <- depth_analysis |>
  select(
    Date, DCM_depth,
    PZ, thermocline_depth, schmidt_stability, WaterLevel_m,
    depth_NH4_ugL_max, depth_SRP_ugL_max, depth_SFe_mgL_max,
    wind_lag1, airtemp_lag2, precip_lag1
  )
#write.csv(selected_depth_analysis, "CSVs/selected_depth_analysis.csv", row.names = FALSE)

var_importance_shap_plots(Xdataframe = selected_depth_analysis, 2015, 2024, "selected vars","DCM_depth", "Depth")

#####Selected variables individual years#####
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = selected_depth_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = "selected variables",
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





####weather lags####
met_lags <- depth_analysis |>
  select(Date, DCM_depth,
         Precip_Avg, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
var_importance_shap_plots(Xdataframe = met_lags, 2015, 2024, "ALL MET LAGS", "DCM_depth", "Depth")

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
#####all variables all years#####
var_importance_shap_plots(magnitude_analysis, 2015, 2024, "all_vars", "max_conc", "Magnitude")
#individual years
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = magnitude_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = "all variables",
    response_var = "max_conc",
    save_dir = "Magnitude"
  )
  all_plots[[as.character(i)]] <- p
}
# Combine all 10 plots in a grid (e.g., 2 rows x 5 columns)
combined_all <- wrap_plots(all_plots, ncol = 1)
# Save all together
ggsave(
  here::here("Figs", "MachineLearning", "Magnitude","all years combined", "AllYears_Combined_ALL_VARIABLES.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)


####selected variables####

var_importance_shap_plots(magnitude_analysis_revised, 2015, 2024, "selected vars", "max_conc", "Magnitude")
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = magnitude_analysis_revised,
    XYear = i,
    XYear2 = i,
    whichvars = "all variables",
    response_var = "max_conc",
    save_dir = "Magnitude"
  )
  all_plots[[as.character(i)]] <- p
}
combined_all <- wrap_plots(all_plots, ncol = 1)
ggsave(
  here::here("Figs", "MachineLearning", "Magnitude","all years combined", "AllYears_Combined_selected_vars.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

####weather lags####
met_lags <- magnitude_analysis |>
  select(Date, max_conc,
         Precip_Avg, precip_lag1, precip_lag2,
         AirTemp_Avg, airtemp_lag1, airtemp_lag2,
         WindSpeed_Avg, wind_lag1, wind_lag2)
var_importance_shap_plots(Xdataframe = met_lags, 2015, 2024, "ALL MET LAGS", "max_conc", "Magnitude")

#individual years
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = met_lags,
    XYear = i,
    XYear2 = i,
    whichvars = "ALL MET LAGS",
    response_var = "max_conc",
    save_dir = "Magnitude"
  )
  all_plots[[as.character(i)]] <- p
}
combined_all <- wrap_plots(all_plots, ncol = 1)
ggsave(
  here::here("Figs", "MachineLearning", "Magnitude","all years combined", "AllYears_combined_MET_LAGS.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

####Final Plots####
final_magnitude_analysis <- magnitude_analysis_revised |>
  select(
    Date,
    max_conc,
    WaterLevel_m,
    PZ,
    N_at_DCM,
    schmidt_stability,
    thermocline_depth,
    TFe_mgL_max_val,
    SRP_ugL_min_val,
    NH4_ugL_max_val,
    NO3NO2_ugL_max_val,
    precip_lag1,
    airtemp_lag2,
    wind_lag1
  )

var_importance_shap_plots(final_magnitude_analysis, 2015, 2024, "final", "max_conc", "Magnitude")

#individual years
all_plots <- list()
for (i in 2015:2024) {
  p <- var_importance_shap_plots(
    Xdataframe = final_magnitude_analysis,
    XYear = i,
    XYear2 = i,
    whichvars = "final",
    response_var = "max_conc",
    save_dir = "Magnitude"
  )
  all_plots[[as.character(i)]] <- p
}
combined_all <- wrap_plots(all_plots, ncol = 1)
ggsave(
  here::here("Figs", "MachineLearning", "Magnitude","all years combined", "final plot.png"),
  plot = combined_all,
  width = 12,
  height = 30,
  dpi = 600,
  bg = "white"
)

