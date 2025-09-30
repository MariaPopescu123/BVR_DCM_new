#### metals  ####
# metals data https://portal.edirepository.org/nis/codeGeneration?packageId=edi.455.9&statisticalFileType=r
#updated 2025
metalsdf <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5")
#removed flags for 68 as per Cece's advice

metalsdf <- metalsdf|>
  mutate(Date = as.Date(DateTime))

variables <- c("SFe_mgL", "TFe_mgL", "SMn_mgL")

metals_summarised <- weekly_sum_variables(metalsdf, variables)

final_metals <- frame_weeks|> #random forest frame with metals
  left_join(metals_summarised, by = c("Week", "Year"))|>
  select(-WaterLevel_m)

write.csv(final_metals, "CSVs/final_metals.csv", row.names = FALSE)

#how much raw metal data do we have?This is just to visualize
# #variables <- c(
#   "depth_SFe_mgL_max", "depth_SFe_mgL_min", "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
#   "depth_TFe_mgL_max", "depth_TFe_mgL_min", "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
#   "depth_SMn_mgL_max", "depth_SMn_mgL_min", "SMn_mgL_max_val", "SMn_mgL_min_val", "SMn_mgL_range")
# data_availability(metals_summarised, variables)
#metals_interpolated <- interpolate_variable(metalsdf_filtered, variables, expanded_dates) no need for this anymore
#data_availability(metals_interpolated, variables)
