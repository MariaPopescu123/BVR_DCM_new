#Maria BVR_DCM
#before running this script first run 
#01_DataDownload
#03_Datawrangling: then go through all the files within chronologically
#then you can run this script

#this script is messy, old, and incorrect but I am keeping it here, because 
#I want to refer to it when I make a new correlation matrix

# 
full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

#print(colnames(full_weekly_data_clean))
depth_analysis <- full_weekly_data |>
  select(-ends_with("max_val"), -ends_with("min_val"), -ends_with("range"), 
         -max_conc, -totals_mean, -totals_med, -N_at_DCM, -Week, -Date.x)

magnitude_analysis <- full_weekly_data|>
  select(-starts_with("depth_"), -Week, -totals_mean, -totals_med, -Date.x)

#####correlation function----

#for depth analysis
correlations <- function(year1, year2) {
  DCM_final_cor <- depth_analysis |>
    filter(year(Date) >= {{year1}}, year(Date) <= {{year2}}) |>
    filter(month(Date) > 4, month(Date) < 10)
  
  drivers_cor <- cor(DCM_final_cor[,c(2:37)],
                     method = "spearman", use = "pairwise.complete.obs")
 
  list(drivers_cor = drivers_cor, DCM_final_cor = DCM_final_cor)

}

# correlation for all years for depth analysis

{results <- correlations(2014, 2023)
final_data_cor_results <- results$drivers_cor
final_data_cor_results[lower.tri(final_data_cor_results)] = ""
final_data_cor <- results$DCM_final_cor
final_data_cor_results <- results$drivers_cor

final_data_cor_results[lower.tri(final_data_cor_results)] <- NA
diag(final_data_cor_results) <- NA

# Flatten the correlation matrix into a long format
final_data_cor_long <- as.data.frame(as.table(final_data_cor_results)) |>
  filter(!is.na(Freq))  # Remove NAs introduced by setting the lower triangle to NA

final_data_cor_long$Freq <- as.numeric(as.character(final_data_cor_long$Freq))

significant_correlations <- final_data_cor_long |> # Filter correlations based on the cutoff of 0.65
  filter(abs(Freq) >= 0.5) |>  # Apply cutoff for correlation
  arrange(desc(abs(Freq)))# Sort by absolute correlation values

colnames(significant_correlations) <- c("Variable1", "Variable2", "Correlation") # Rename columns for clarity

significant_correlations <- significant_correlations |>
  filter(Variable1 %in% c("DCM_depth"))|>
  filter(!Variable2 %in% c("peak.top", "peak.bottom"))|>
  mutate(Combined = paste(Variable1, "vs", Variable2))

# Plot to visualize correlations
ggplot(significant_correlations, aes(x = Correlation, y = reorder(Combined, Correlation))) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use a bar plot
  geom_text(aes(label = round(Correlation, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "white") +  # Add correlation values as text on bars
  labs(title = "Significant Correlations (Cutoff 0.65) 2014-2023",
       x = "Correlation Value",
       y = "Variable Pairs") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5))  # Center the title
}

####DEPTH ANALYSIS CORRELATIONS AMDIST VARIABLES####

correlations <- function(year1, year2, cutoff = 0.6) {
  # Filter your data
  DCM_final_cor <- depth_analysis |>
    filter(year(Date) >= year1, year(Date) <= year2) |>
    filter(month(Date) > 4, month(Date) < 10)
  
  # Keep only numeric columns
  numeric_data <- DCM_final_cor[, sapply(DCM_final_cor, is.numeric)]
  
  # Calculate Spearman correlation
  cor_matrix <- cor(numeric_data, method = "spearman", use = "pairwise.complete.obs")
  
  # Remove redundant pairs (lower triangle and diagonal)
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  
  # Convert to long format
  cor_long <- as.data.frame(as.table(cor_matrix)) |>
    filter(!is.na(Freq)) |>              # Remove NAs from lower triangle and diag
    filter(abs(Freq) >= cutoff) |>       # Apply correlation cutoff
    arrange(desc(abs(Freq)))             # Sort by correlation magnitude
  
  # Rename columns
  colnames(cor_long) <- c("Variable1", "Variable2", "Correlation")
  
  return(cor_long)
}

# Run the function
significant_correlations <- correlations(2014, 2023, cutoff = 0.6)

# View or export the result
print(significant_correlations)
# write.csv(significant_correlations, "significant_correlations.csv", row.names = FALSE)

# Optional: visualize with a barplot of top pairs
ggplot(significant_correlations, aes(x = Correlation, y = reorder(paste(Variable1, "vs", Variable2), Correlation))) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(Correlation, 2)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  labs(title = "Highly Correlated Variable Pairs (|r| ≥ 0.6), 2014–2023",
       x = "Spearman Correlation",
       y = "Variable Pair") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5))

####MAGNITUDE ANALYSIS CORRELATIONS AMIDST VARIABLES####

correlations <- function(year1, year2, cutoff = 0.6) {
  # Filter your data
  DCM_final_cor <- magnitude_analysis |>
    filter(year(Date) >= year1, year(Date) <= year2) |>
    filter(month(Date) > 4, month(Date) < 10)
  
  # Keep only numeric columns
  numeric_data <- DCM_final_cor[, sapply(DCM_final_cor, is.numeric)]
  
  # Calculate Spearman correlation
  cor_matrix <- cor(numeric_data, method = "spearman", use = "pairwise.complete.obs")
  
  # Remove redundant pairs (lower triangle and diagonal)
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  
  # Convert to long format
  cor_long <- as.data.frame(as.table(cor_matrix)) |>
    filter(!is.na(Freq)) |>              # Remove NAs from lower triangle and diag
    filter(abs(Freq) >= cutoff) |>       # Apply correlation cutoff
    arrange(desc(abs(Freq)))             # Sort by correlation magnitude
  
  # Rename columns
  colnames(cor_long) <- c("Variable1", "Variable2", "Correlation")
  
  return(cor_long)
}

# Run the function
significant_correlations <- correlations(2014, 2023, cutoff = 0.6)

# View or export the result
print(significant_correlations)
# write.csv(significant_correlations, "significant_correlations.csv", row.names = FALSE)

# Optional: visualize with a barplot of top pairs
ggplot(significant_correlations, aes(x = Correlation, y = reorder(paste(Variable1, "vs", Variable2), Correlation))) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(Correlation, 2)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  labs(title = "Highly Correlated Variable Pairs (|r| ≥ 0.6), 2014–2023",
       x = "Spearman Correlation",
       y = "Variable Pair") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5))










#####correlations across years,max day each year####

DCM_final_maxdays_cor<- DCM_final|>
  filter(Date %in% c("2014-08-13", "2015-08-08", "2016-06-16", "2017-07-20", "2018-08-16", "2019-06-06", "2020-09-16", "2021-08-09", "2022-08-01", "2023-07-31"))

maxdayscor <- cor(DCM_final_maxdays_cor[,c(2:64)], method = "spearman", use = "pairwise.complete.obs")

maxdayscor[lower.tri(maxdayscor)] <- NA
diag(maxdayscor) <- NA

# Flatten the correlation matrix into a long format
maxdayscor_long <- as.data.frame(as.table(maxdayscor)) |>
  filter(!is.na(Freq))  # Remove NAs introduced by setting the lower triangle to NA

maxdayscor_long$Freq <- as.numeric(as.character(maxdayscor_long$Freq))

significant_correlations <- maxdayscor_long |> # Filter correlations based on the cutoff of 0.65
  filter(abs(Freq) >= 0.7) |>  # Apply cutoff for correlation
  arrange(desc(abs(Freq)))# Sort by absolute correlation values

colnames(significant_correlations) <- c("Variable1", "Variable2", "Correlation") # Rename columns for clarity

significant_correlations <- significant_correlations |>
  filter(Variable1 %in% c("DCM_depth"))|>
  filter(!Variable2 %in% c("peak.top", "peak.bottom"))|>
  mutate(Combined = paste(Variable1, "vs", Variable2))


significant_correlations$Combined <- paste(significant_correlations$Variable1, significant_correlations$Variable2, sep = " - ")

ggplot(significant_correlations, aes(x = Correlation, y = reorder(Combined, Correlation))) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use a bar plot
  geom_text(aes(label = round(Correlation, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "white") +  # Add correlation values as text on bars
  labs(title = "Significant Correlations (Cutoff 0.65) 2014-2023",
       x = "Correlation Value",
       y = "Variable Pairs") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5))  # Center the title

#####daily correlation, for choosing specific day####

#these are the days that the max TotalConc_ugL occurs. The biggest bloom. 
blooms <- DCM_final|>
  group_by(year(Date))|>
  mutate(bloommax = if_else(TotalConc_ugL == max(TotalConc_ugL), TRUE, NA_real_))|>
  ungroup()|>
  filter(bloommax == TRUE)|>
  group_by(Date)|>
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) |>
  ungroup()
  

#"2014-08-13" "2015-08-08" "2016-06-16" "2017-07-20" "2018-08-16" "2019-06-06" "2020-09-16" "2021-08-09" "2022-08-01" "2023-07-31"
#change date to see correlations for the singular day that max was the biggest
daily_cor <- pwmgslyccntbp|>
  filter(Date %in% c(""))|>#change this
  select("Depth_m", "TotalConc_ugL", "TotalConc_ugL", "SFe_mgL", "TFe_mgL", "SMn_mgL", "SCa_mgL",
         "TCa_mgL", "TCu_mgL", "SBa_mgL", "TBa_mgL",
         "CO2_umolL", "CH4_umolL", "DO_mgL",
         "DOsat_percent", "np_ratio", "TN_ugL", "TP_ugL", 
         "NH4_ugL", "NO3NO2_ugL", "SRP_ugL", "DOC_mgL", "DIC_mgL", 
         "DC_mgL", "Temp_C", "buoyancy_freq")

daily_cor_result <- cor(daily_cor[,c(6:32)], method = "spearman", use = "pairwise.complete.obs")
  
daily_cor_result[lower.tri(daily_cor_result)] = ""

daily_cor_long <- as.data.frame(as.table(daily_cor_result)) |>
  filter(!is.na(Freq))  # Remove NAs introduced by setting the lower triangle to NA

daily_cor_long$Freq <- as.numeric(as.character(daily_cor_long$Freq))

significant_correlations <- daily_cor_long |> # Filter correlations based on the cutoff of 0.65
  filter(abs(Freq) >= 0.65) |>  # Apply cutoff for correlation
  arrange(desc(abs(Freq)))# Sort by absolute correlation values

colnames(significant_correlations) <- c("Variable1", "Variable2", "Correlation") # Rename columns for clarity
significant_correlations_sorted <- significant_correlations[order(significant_correlations$Variable1), ] #variable 1 sorted alphabetically 

significant_correlations$Combined <- paste(significant_correlations$Variable1, significant_correlations$Variable2, sep = " - ")

significant_correlations <- significant_correlations |>
  filter(Variable1 %in% c("TotalConc_ugL", "TotalConc_ugL"))|>
  filter(!Variable2 %in% c("Depth_m"))|>
  mutate(Combined = paste(Variable1, "vs", Variable2))

ggplot(significant_correlations, aes(x = Correlation, y = reorder(Combined, Correlation))) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use a bar plot
  geom_text(aes(label = round(Correlation, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "white") +  # Add correlation values as text on bars
  labs(title = "Significant Correlations (Cutoff 0.65) 2019-06-06 (max 2019 conc, DCM depth 8.6)",
       x = "Correlation Value",
       y = "Variable Pairs") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5))  # Center the title


looking<- final_data0|>
  filter(Date %in% c("2019-06-06"))

####RandomForest Anually####

#not splitting dataset because not enough data

#"We constructed a RF of 1500 trees for each of the two response
#variables using 1% PAR depth (m), DOC concentration (mg L21),
#thermocline depth (m), metalimnion thickness (m),
#buoyancy frequency at the thermocline (s21),
#lake surface area (log10(km2)), and maximum depth (log10(m))
#as predictors included in each analysis."
#Leach Patterns and Drivers

#prepare data for random forest

library(randomForest)
library(missForest)
# Identify numeric columns
numeric_cols <- sapply(full_weekly_data, is.numeric)

# Define a function to remove rows with Inf/-Inf
remove_infinite <- function(df) {
  df[apply(df, 1, function(row) all(is.finite(row))), ]
}

# Create cleaned dataset
for_RF <- full_weekly_data |>
  na.omit() |>
  select(which(numeric_cols)) |>
  remove_infinite()

set.seed(123)  # Setting seed for reproducibility

####grid search####

# Load necessary library
library(caret)
library(future)
# Define training control
train_control <- trainControl(method = "cv", number = 5)
grid <- expand.grid(mtry = c(2, 3, 4, 20, 50))  # only tuning mtry here. num of variables being randomly sampled

results <- list()
for (nt in c(50, 100, 200, 500)) {
  model <- train(
    DCM_depth ~ .,
    data = for_RF,
    method = "rf",
    trControl = train_control,
    tuneGrid = grid,
    ntree = nt,
    max.depth = 10, 
    splitrule = "variance"
  )
  results[[as.character(nt)]] <- model
}

# Compare results
lapply(results, function(x) max(x$results$Rsquared))
print(model)

bestModel <- model$bestTune
print(bestModel)

#default values are giving us a better result. We only define parameter for ntree


# Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
non_numeric_columns <- sapply(full_weekly_data, function(x) !is.numeric(x) & !is.factor(x))
train_data_no_non_numeric <- full_weekly_data |>
  select(-which(non_numeric_columns)) 

# Replace Inf and NaN with NA in all numeric columns
train_data_no_non_numeric <- train_data_no_non_numeric %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

# Remove columns with more than 75% NA values
train_data_no_na <- train_data_no_non_numeric %>%
  select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA

# Remove remaining rows with any NA values
train_data_imputed_z <- train_data_no_na %>%
  na.omit()
# Removes any rows with NAs
  #for 2022 leave out waterlevel bc too many NAs
  #train_data_imputed_z <- train_data_imputed_z|>
  #  select(-WaterLevel_m)
  
  # Add the excluded non-numeric columns (e.g., Date) back to the imputed dataset
set.seed(123)  
for (tree_num in c(100, 200, 300, 500)) {
  for (node_size in c(2, 4, 6, 8)) {
    for (mt in c(3, 6, 9, 20, 50)) {
      model_rf <- randomForest(
        DCM_depth ~ ., 
        data = train_data_imputed_z,
        ntree = tree_num,
        mtry = mt,
        importance = TRUE
      )
      
      rsq_test <- mean(model_rf$rsq)
      mse_test <- mean(model_rf$mse)
      
      cat("Trees:", tree_num, "| Node size:", node_size, "| mtry:", mt,
          "| R-squared:", rsq_test, "| MSE:", mse_test, "\n\n")
    }
  }
}
#Trees: 200 | Node size: 2 | mtry: 50 | R-squared: 0.6796417 | MSE: 1.614218 

  ######grid search CV function
  #look at hyperparameters for RandomForest tune as many as 
  #n_estimators, max_depth, minimum samples split, max_leaf nodes, and min_samples leaf
  #change ntrees to 100 when playing with parameters
  
#now to clean up test data
# Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
  non_numeric_columns <- sapply(test_data, function(x) !is.numeric(x) & !is.factor(x))
  test_data_no_non_numeric <- test_data %>%
    select(-which(non_numeric_columns)) 
  # Replace Inf and NaN with NA in all numeric columns
  
# Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
non_numeric_columns <- sapply(test_data, function(x) !is.numeric(x) & !is.factor(x))
test_data_no_non_numeric <- test_data %>%
  select(-which(non_numeric_columns)) 
# Replace Inf and NaN with NA in all numeric columns
test_data_no_non_numeric <- test_data_no_non_numeric %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
# Remove columns with more than 75% NA values
test_data_no_na <- test_data_no_non_numeric %>%
  select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA
# Remove remaining rows with any NA values
test_data_imputed_z <- test_data_no_na %>%
  na.omit() %>%
  select(
    -c("Week", "max_conc", "totals_mean", "totals_med",
       "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
       "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
       "SMn_mgL_max_val", "SMn_mgL_min_val")
  ) %>%
  select(-matches("_(min_val|max_val|range)$"))
  
  test_model_rf <- randomForest(DCM_depth ~ ., data = test_data_imputed_z, ntree = 500, importance = TRUE)
  importance(test_model_rf)
  
  #visualize
  importance_df <- as.data.frame(importance(test_model_rf))
  importance_df <- rownames_to_column(importance_df, var = "Variable") # Convert row names to a column
  
  filtered_importance_df <- importance_df %>%
    filter(!is.na(`%IncMSE`), `%IncMSE` > 0) %>%  # Filter for valid and positive %IncMSE
    filter(
      !Variable %in% c("Week", "max_conc", "totals_mean", "totals_med",
                       "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
                       "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
                       "SMn_mgL_max_val", "SMn_mgL_min_val") &
        !str_detect(Variable, "_(min_val|max_val|range)$")
    )

rsq_test<- mean((model_rf$rsq))
mse_test<- mean((model_rf$mse))
  
#write.csv(Final_RF_frame, "Final_RF_frame.csv")  

 # #run VIF (to figure out variance inflation factor)
 #  library(caret)
 #  
 #  
 #  # Build the model
 #  model1 <- model_rf
 #  # Make predictions
 #  predictions <- model1 %>% predict(train_data_imputed_z)
 #  # Model performance
 #  data.frame(
 #    RMSE = RMSE(predictions, train_data_imputed_z$DCM_depth),
 #    R2 = R2(predictions, train_data_imputed_z$DCM_depth)
 #  )
 #  
 #  car::vif(model1)
 
   
#for all years
#  filtered_importance_df <- filtered_importance_df |>
#    filter(!Variable %in% c("peak.top", "pH", "min_pH_depth",  "min_Cond_uScm_depth", "max_Cond_uScm_depth", "peak.magnitude", "peak.bottom", "secchi_PZ", "PAR_PZ", "max_Cond_uScm_depth", "DayOfYear", "DOY", "min_Cond_uScm_dept", "min_CH4_umolL_depth", "max_CH4_umolL_depth"))
  
  # Create the plot
  ggplot(filtered_importance_df, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
    geom_point(color = "blue", size = 3) +
    labs(
      title = "Variable Importance based on % IncMSE 2014-2023",
      x = "% IncMSE",
      y = "Variables"
    ) +
    theme_minimal()
  
  
  ####SHAP####
  library(fastshap)
  X = data.matrix(select(test_data_imputed_z, -DCM_depth))

  shap_values = fastshap::explain(test_model_rf, X=X, nsim=100, pred_wrapper=function(x,newdata){predict(x,newdata)})
  dim(shap_values)
  head(shap_values)
  
  preds = predict(test_model_rf, X)
  base_value = mean(preds)
  base_value
  cat(
    "shap+base_value:\t",sum(shap_values[1,]) + base_value,
    "\n     prediction:\t", preds[1]
  )

  as_tibble(X) %>% rownames_to_column('row_id') %>%
    pivot_longer(names_to='var', values_to='value', -row_id) -> vars
  as_tibble(shap_values) %>% rownames_to_column('row_id') %>%
    pivot_longer(names_to='var', values_to='shap', -row_id) -> shaps
  df = inner_join(vars, shaps, by=c('row_id', 'var'))
  head(df)
  
  # Check structure of relevant columns
  str(df)
  
  # Optional: Convert to atomic vectors just in case
  df <- df %>%
    mutate(
      shap = as.numeric(shap),
      value = as.character(value),  # or as.numeric if needed
      var = as.character(var)
    )
  
  # Now re-run your plot
  df %>%
    filter(row_id == 1) %>%
    ggplot(aes(x = shap, y = fct_reorder(paste0(var, "=", value), shap), fill = factor(sign(shap)))) +
    geom_col() +
    guides(fill = 'none') +
    labs(y = "", title = "SHAP values for X[1,]")
  
  group_by(df, var) %>% mutate(nv=scale(value)) %>%
    ggplot(aes(x=shap, y=var, color=nv)) +
    geom_quasirandom(groupOnX = FALSE,dodge.width = 0.3) +
    scale_color_viridis_c(option = 'H', limits=c(-3, 3), oob=scales::oob_squish) +
    labs(title='distribution of shap values for all samples', y='',color='z-scaled values')
  
  library(dplyr)
  library(ggplot2)
  library(ggbeeswarm)  # for geom_quasirandom
  library(viridis)
  
  df %>%
    mutate(value = as.numeric(value)) %>%  # Ensure 'value' is numeric
    group_by(var) %>%
    mutate(nv = scale(value)) %>%
    ggplot(aes(x = shap, y = var, color = nv)) +
    geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
    scale_color_viridis_c(option = 'H', limits = c(-3, 3), oob = scales::oob_squish) +
    labs(title = 'Distribution of SHAP values for all samples', y = '', color = 'z-scaled values')
  
  group_by(df, var) %>% 
    summarize(mean=mean(abs(shap))) %>%
    ggplot(aes(x=mean, y=fct_reorder(var, mean))) + 
    geom_col() +
    labs(x='mean(|shap value|)', title='mean absolute shap for all samples', y="")
  
  group_by(df, var, sign=factor(sign(shap))) %>%
    summarize(mean=mean(shap)) %>%
    ggplot(aes(x=mean, y=fct_reorder(var, mean), fill=sign)) + 
    geom_col() +
    labs(x='mean(shap value)', title='mean shap for all samples', y="")
 
  #this not showing 
  filter(df, var=='PZ') %>%
    ggplot(aes(x=value, y=shap)) + geom_point() +
    geom_smooth() +
    labs(title='interaction shap vs PZ', x='DCM_Depth')
  
  ggplot(aes(x=value, y=shap)) + geom_point() + 
    geom_smooth() +
    labs(title='interaction shap vs rm', x='rm')
  
  ####2016####
  
  RF_frame_2016 <- Final_RF_frame|>
    filter(Year == 2016)
  
  set.seed(123)  # Setting seed for reproducibility
  
  # Splitting data into training (70%) and testing (30%)
  index <- sample(1:nrow(RF_frame_2016), size = 0.7 * nrow(RF_frame_2016))  # 70% training data
  train_data2016 <- RF_frame_2016[index, ]
  test_data2016 <- RF_frame_2016[-index, ]
  
  #should run model on test dataset , currently mine is running on training. 
  #training and test RMSE, MAE and Rsquare value important to show. 
  #GLM-AED
  
  # Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
  non_numeric_columns <- sapply(train_data2016, function(x) !is.numeric(x) & !is.factor(x))
  train_data_no_non_numeric2016 <- train_data2016 %>%
    select(-which(non_numeric_columns)) 
  
  # Replace Inf and NaN with NA in all numeric columns
  train_data_no_non_numeric2016 <- train_data_no_non_numeric2016 %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
  # Remove columns with more than 75% NA values
  train_data_no_na2016 <- train_data_no_non_numeric2016 %>%
    select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA
  # Remove remaining rows with any NA values
  train_data_imputed_z2016 <- train_data_no_na2016 %>%
    na.omit()
  # Removes any rows with NAs
  #for 2022 leave out waterlevel bc too many NAs
  #train_data_imputed_z <- train_data_imputed_z|>
  #  select(-WaterLevel_m)
  
  # Add the excluded non-numeric columns (e.g., Date) back to the imputed dataset
  train_model_rf2016 <- randomForest(DCM_depth ~ ., data = train_data_imputed_z2016, ntree = 500, importance = TRUE)
  ######grid search CV function
  #look at hyperparameters for RandomForest tune as many as 
  #n_estimators, max_depth, minimum samples split, max_leaf nodes, and min_samples leaf
  #change ntrees to 100 when playing with parameters
  
  #now to clean up test data
  # Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
  non_numeric_columns <- sapply(test_data, function(x) !is.numeric(x) & !is.factor(x))
  test_data_no_non_numeric2016 <- test_data2016 %>%
    select(-which(non_numeric_columns)) 
  # Replace Inf and NaN with NA in all numeric columns
  
  # Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
  non_numeric_columns <- sapply(test_data2016, function(x) !is.numeric(x) & !is.factor(x))
  test_data_no_non_numeric2016 <- test_data2016 %>%
    select(-which(non_numeric_columns)) 
  # Replace Inf and NaN with NA in all numeric columns
  test_data_no_non_numeric2016 <- test_data_no_non_numeric2016 %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
  # Remove columns with more than 75% NA values
  test_data_no_na2016 <- test_data_no_non_numeric2016 %>%
    select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA
  # Remove remaining rows with any NA values
  test_data_imputed_z2016 <- test_data_no_na2016 %>%
    na.omit() %>%
    select(
      -c("Week", "max_conc", "totals_mean", "totals_med",
         "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
         "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
         "SMn_mgL_max_val", "SMn_mgL_min_val")
    ) %>%
    select(-matches("_(min_val|max_val|range)$"))
  
  test_model_rf2016 <- randomForest(DCM_depth ~ ., data = test_data_imputed_z2016, ntree = 500, importance = TRUE)
  importance(test_model_rf2016)
  
  #visualize
  importance_df2016 <- as.data.frame(importance(test_model_rf2016))
  importance_df2016 <- rownames_to_column(importance_df)|> # Convert row names to a column
  select(-rowname)
  
  
  filtered_importance_df2016 <- importance_df2016 %>%
    filter(!is.na(`%IncMSE`), `%IncMSE` > 0) %>%  # Filter for valid and positive %IncMSE
    filter(
      !Variable %in% c("Week", "max_conc", "totals_mean", "totals_med",
                       "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
                       "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
                       "SMn_mgL_max_val", "SMn_mgL_min_val", "Year", "buoyancy_freq") &
        !str_detect(Variable, "_(min_val|max_val|range)$")
    )
  
#rsq_test<- mean((test_model_rf2016$rsq))
#mse_test<- mean((test_model_rf2016$mse))
  
  #write.csv(Final_RF_frame, "Final_RF_frame.csv")  

  # Create the plot
  ggplot(filtered_importance_df2016, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
    geom_point(color = "blue", size = 3) +
    labs(
      title = "Variable Importance based on % IncMSE 2016",
      x = "% IncMSE",
      y = "Variables"
    ) +
    theme_minimal()
  
  
  ####SHAP####
  library(fastshap)
  X = data.matrix(select(test_data_imputed_z2016, -DCM_depth))
  
  shap_values = fastshap::explain(test_model_rf2016, X=X, nsim=100, pred_wrapper=function(x,newdata){predict(x,newdata)})
  dim(shap_values)
  head(shap_values)
  
  preds = predict(test_model_rf2016, X)
  base_value = mean(preds)
  base_value
  cat(
    "shap+base_value:\t",sum(shap_values[1,]) + base_value,
    "\n     prediction:\t", preds[1]
  )
  
  as_tibble(X) %>% rownames_to_column('row_id') %>%
    pivot_longer(names_to='var', values_to='value', -row_id) -> vars
  as_tibble(shap_values) %>% rownames_to_column('row_id') %>%
    pivot_longer(names_to='var', values_to='shap', -row_id) -> shaps
  df = inner_join(vars, shaps, by=c('row_id', 'var'))
  head(df)
  
  # Check structure of relevant columns
  str(df)
  # Optional: Convert to atomic vectors just in case
  df <- df %>%
    mutate(
      shap = as.numeric(shap),
      value = as.character(value),  # or as.numeric if needed
      var = as.character(var)
    )
  # Now re-run your plot
  df %>%
    filter(row_id == 1) %>%
    ggplot(aes(x = shap, y = fct_reorder(paste0(var, "=", value), shap), fill = factor(sign(shap)))) +
    geom_col() +
    guides(fill = 'none') +
    labs(y = "", title = "SHAP values for bloom depth 2016")
  
  group_by(df, var) %>% mutate(nv=scale(value)) %>%
    ggplot(aes(x=shap, y=var, color=nv)) +
    geom_quasirandom(groupOnX = FALSE,dodge.width = 0.3) +
    scale_color_viridis_c(option = 'H', limits=c(-3, 3), oob=scales::oob_squish) +
    labs(title='distribution of shap values for all samples', y='',color='z-scaled values')
  
  library(dplyr)
  library(ggplot2)
  library(ggbeeswarm)  # for geom_quasirandom
  library(viridis)
  
  df %>%
    mutate(value = as.numeric(value)) %>%  # Ensure 'value' is numeric
    group_by(var) %>%
    mutate(nv = scale(value)) %>%
    ggplot(aes(x = shap, y = var, color = nv)) +
    geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
    scale_color_viridis_c(option = 'H', limits = c(-3, 3), oob = scales::oob_squish) +
    labs(title = 'Distribution of SHAP values for all samples', y = '', color = 'z-scaled values')
  
  group_by(df, var) %>% 
    summarize(mean=mean(abs(shap))) %>%
    ggplot(aes(x=mean, y=fct_reorder(var, mean))) + 
    geom_col() +
    labs(x='mean(|shap value|)', title='mean absolute shap for all samples', y="")
  
  group_by(df, var, sign=factor(sign(shap))) %>%
    summarize(mean=mean(shap)) %>%
    ggplot(aes(x=mean, y=fct_reorder(var, mean), fill=sign)) + 
    geom_col() +
    labs(x='mean(shap value)', title='mean shap for all samples', y="")
  
  #this not showing 
  filter(df, var=='PZ') %>%
    ggplot(aes(x=value, y=shap)) + geom_point() +
    geom_smooth() +
    labs(title='interaction shap vs PZ', x='DCM_Depth')
  
  ggplot(aes(x=value, y=shap)) + geom_point() + 
    geom_smooth() +
    labs(title='interaction shap vs rm', x='rm')
  

####2022####
  RF_frame_2022 <- Final_RF_frame |>
    filter(Year == 2022)
  
  set.seed(123)  # Setting seed for reproducibility
  
  # Splitting data into training (70%) and testing (30%)
  index <- sample(1:nrow(RF_frame_2022), size = 0.7 * nrow(RF_frame_2022))
  train_data2022 <- RF_frame_2022[index, ]
  test_data2022 <- RF_frame_2022[-index, ]
  
  # Remove non-numeric columns
  non_numeric_columns <- sapply(train_data2022, function(x) !is.numeric(x) & !is.factor(x))
  train_data_no_non_numeric2022 <- train_data2022 %>%
    select(-which(non_numeric_columns))
  
  # Replace Inf and NaN with NA
  train_data_no_non_numeric2022 <- train_data_no_non_numeric2022 %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
  
  # Remove columns with more than 75% NA
  train_data_no_na2022 <- train_data_no_non_numeric2022 %>%
    select(where(~ mean(is.na(.)) <= 0.25))
  
  # Remove remaining rows with any NA
  train_data_imputed_z2022 <- train_data_no_na2022 %>%
    na.omit()
  
  # Model
  train_model_rf2022 <- randomForest(DCM_depth ~ ., data = train_data_imputed_z2022, ntree = 500, importance = TRUE)
  
  # Clean test data
  non_numeric_columns <- sapply(test_data2022, function(x) !is.numeric(x) & !is.factor(x))
  test_data_no_non_numeric2022 <- test_data2022 %>%
    select(-which(non_numeric_columns))
  
  test_data_no_non_numeric2022 <- test_data_no_non_numeric2022 %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
  
  test_data_no_na2022 <- test_data_no_non_numeric2022 %>%
    select(where(~ mean(is.na(.)) <= 0.25))
  
  test_data_imputed_z2022 <- test_data_no_na2022 %>%
    na.omit() %>%
    select(
      -c("Week", "max_conc", "totals_mean", "totals_med",
         "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
         "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
         "SMn_mgL_max_val", "SMn_mgL_min_val")
    ) %>%
    select(-matches("_(min_val|max_val|range)$"))
  
  test_model_rf2022 <- randomForest(DCM_depth ~ ., data = test_data_imputed_z2022, ntree = 500, importance = TRUE)
  
  importance_df2022 <- as.data.frame(importance(test_model_rf2022))
  importance_df2022 <- rownames_to_column(importance_df2022, var = "Variable")
  
  filtered_importance_df2022 <- importance_df2022 %>%
    filter(!is.na(`%IncMSE`), `%IncMSE` > 0) %>%
    filter(
      !Variable %in% c("Week", "max_conc", "totals_mean", "totals_med",
                       "SFe_mgL_max_val", "SFe_mgL_min_val", "SFe_mgL_range",
                       "TFe_mgL_max_val", "TFe_mgL_min_val", "TFe_mgL_range",
                       "SMn_mgL_max_val", "SMn_mgL_min_val", "Year", "buoyancy_freq") &
        !str_detect(Variable, "_(min_val|max_val|range)$")
    )
  
  ggplot(filtered_importance_df2022, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
    geom_point(color = "blue", size = 3) +
    labs(
      title = "Variable Importance based on % IncMSE 2022",
      x = "% IncMSE",
      y = "Variables"
    ) +
    theme_minimal()
  
  #### SHAP ####
  library(fastshap)
  X = data.matrix(select(test_data_imputed_z2022, -DCM_depth))
  
  shap_values = fastshap::explain(test_model_rf2022, X = X, nsim = 100, pred_wrapper = function(x, newdata) { predict(x, newdata) })
  dim(shap_values)
  head(shap_values)
  
  preds = predict(test_model_rf2022, X)
  base_value = mean(preds)
  base_value
  cat(
    "shap+base_value:\t", sum(shap_values[1, ]) + base_value,
    "\n     prediction:\t", preds[1]
  )
  
  as_tibble(X) %>% rownames_to_column('row_id') %>%
    pivot_longer(names_to = 'var', values_to = 'value', -row_id) -> vars
  as_tibble(shap_values) %>% rownames_to_column('row_id') %>%
    pivot_longer(names_to = 'var', values_to = 'shap', -row_id) -> shaps
  df = inner_join(vars, shaps, by = c('row_id', 'var'))
  
  df <- df %>%
    mutate(
      shap = as.numeric(shap),
      value = as.character(value),
      var = as.character(var)
    )
  
  df %>%
    filter(row_id == 1) %>%
    ggplot(aes(x = shap, y = fct_reorder(paste0(var, "=", value), shap), fill = factor(sign(shap)))) +
    geom_col() +
    guides(fill = 'none') +
    labs(y = "", title = "SHAP values for bloom depth 2022")
  
  group_by(df, var) %>% mutate(nv = scale(value)) %>%
    ggplot(aes(x = shap, y = var, color = nv)) +
    geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
    scale_color_viridis_c(option = 'H', limits = c(-3, 3), oob = scales::oob_squish) +
    labs(title = 'Distribution of SHAP values for all samples', y = '', color = 'z-scaled values')
  
  df %>%
    mutate(value = as.numeric(value)) %>%
    group_by(var) %>%
    mutate(nv = scale(value)) %>%
    ggplot(aes(x = shap, y = var, color = nv)) +
    geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
    scale_color_viridis_c(option = 'H', limits = c(-3, 3), oob = scales::oob_squish) +
    labs(title = 'Distribution of SHAP values for all samples', y = '', color = 'z-scaled values')
  
  group_by(df, var) %>%
    summarize(mean = mean(abs(shap))) %>%
    ggplot(aes(x = mean, y = fct_reorder(var, mean))) +
    geom_col() +
    labs(x = 'mean(|shap value|)', title = 'mean absolute shap for all samples', y = "")
  
  group_by(df, var, sign = factor(sign(shap))) %>%
    summarize(mean = mean(shap)) %>%
    ggplot(aes(x = mean, y = fct_reorder(var, mean), fill = sign)) +
    geom_col() +
    labs(x = 'mean(shap value)', title = 'mean shap for all samples', y = "")
  
  filter(df, var == 'PZ') %>%
    ggplot(aes(x = value, y = shap)) +
    geom_point() +
    geom_smooth() +
    labs(title = 'Interaction SHAP vs PZ', x = 'DCM_Depth')
  
  
  
  
  
  
  
  
  
  
  ####making predictions####
  # Predict Totals_DCM_depth for test data
  test_predictions <- predict(model_rf, newdata = train_data_imputed_z)
  
  rmse <- sqrt(mean((test_predictions - train_data_imputed_z$DCM_depth)^2, na.rm = TRUE))
  print(paste("RMSE:", rmse))
  
  # Plot predicted vs. actual values
  ggplot(data = NULL, aes(x = train_data_imputed_z$DCM_depth, y = test_predictions)) +
    geom_point(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Predicted vs Actual Totals_DCM_depth",
      x = "Actual Totals_DCM_depth",
      y = "Predicted Totals_DCM_depth"
    ) +
    theme_minimal()
  
  
  
  
  
 ####RF totals magnitude####
#####make dataframe#####
  
  # Create a vector of variable names that need to be summarized
  depth_variables <- c("Temp_C", "np_ratio", "SFe_mgL", "TFe_mgL", 
                       "SMn_mgL", "SCa_mgL", "TCa_mgL", 
                       "TCu_mgL", "SBa_mgL", "TBa_mgL", 
                       "CO2_umolL", "CH4_umolL", "DO_mgL", 
                       "DOsat_percent", "Cond_uScm", "ORP_mV", 
                       "pH", "TN_ugL", "TP_ugL", 
                       "NH4_ugL", "NO3NO2_ugL", "SRP_ugL", 
                       "DOC_mgL", "DIC_mgL", "DC_mgL")
  
  DCM_depths <- list()
  
  # Process to obtain the concentrations at Totals_DCM_depth for each variable
  DCM_final_conc <- final_data0 |>
    mutate(Date = as.Date(Date)) |>
    filter(month(Date) >= 4, month(Date) < 10) |>
    group_by(Date) |>
    mutate(DCM_buoyancy_freq = if_else(DCM == TRUE, buoyancy_freq, NA_real_)) |>
    fill(DCM_buoyancy_freq, .direction = "updown") |>
    summarise(
      DCM_buoyancy_freq = mean(DCM_buoyancy_freq, na.rm = TRUE),
      Totals_DCM_depth = mean(Totals_DCM_depth, na.rm = TRUE),
      Totals_DCM_conc = mean(Totals_DCM_conc, na.rm = TRUE),
      peak.top = mean(peak.top, na.rm = TRUE),
      peak.bottom = mean(peak.bottom, na.rm = TRUE),
      peak.width = mean(peak.width, na.rm = TRUE),
      peak.magnitude = mean(peak.magnitude, na.rm = TRUE),
      secchi_PZ = mean(secchi_PZ, na.rm = TRUE),
      PAR_PZ = mean(PAR_PZ, na.rm = TRUE),
      PZ = mean(PZ, na.rm = TRUE),
      Zeu = mean(Zeu, na.rm = TRUE),
      thermocline_depth = mean(thermocline_depth, na.rm = TRUE),
      WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE),
      .groups = "drop"  # Ungroup to prevent grouping issues in following steps
    ) |>
    filter(Totals_DCM_conc > 20)
  
  # Loop through depth_variables to join the concentration at Totals_DCM_depth
  for (var in depth_variables) {
    DCM_final_conc <- DCM_final_conc |>
      left_join(
        final_data0 |>
          filter(month(Date) >= 4, month(Date) < 10) |>
          select(Date, Depth_m, !!sym(var)) |>
          rename(Concentration = !!sym(var)) |>
          group_by(Date) |>
          filter(Depth_m == DCM_final_conc$Totals_DCM_depth[match(Date, DCM_final_conc$Date)]) |>
          summarise(!!paste0(var, "_at_DCM") := mean(Concentration, na.rm = TRUE)),
        by = "Date"
      )
  }
  
  
  DCM_final_conc|>
    select(-peak.top, -peak.bottom, -peak.width, )
  
  
  
  #####run RF#####
  library(randomForest)
  library(missForest)
  
  #trying within a year
  yearDCM_final <- DCM_final_conc |>
    # filter(year(Date) == 2023) |>
    mutate(DOY = yday(Date)) |>
    select(where(~ mean(is.na(.)) < 0.5))
  
  
  # select(-DCM_buoyancy_freq)#just for 2021
  
  # List of columns to apply the interpolation to (excluding Date and DOY)
  cols_to_interpolate <- c()
  
  # Loop through each column name in yearDCM_final
  for (col in colnames(yearDCM_final)) {
    # Check if the column has at least 3 non-NA observations
    if (sum(!is.na(yearDCM_final[[col]])) >= 3) {
      cols_to_interpolate <- c(cols_to_interpolate, col)  # Add column to list if condition is met
    }
  }
  
  # If you want to exclude specific columns (e.g., Date and DOY):
  cols_to_exclude <- c("Date", "DOY")
  
  # Loop through and filter out the excluded columns
  cols_to_interpolate <- cols_to_interpolate[!cols_to_interpolate %in% cols_to_exclude]
  
  library(pracma)
  
  #this is not appropriate
  # Loop through each column and apply pchip interpolation
  yearDCM_final <- yearDCM_final[order(yearDCM_final$DOY), ]
  
  for (col in cols_to_interpolate) {
    # Identify rows with non-NA values for the current column
    non_na_rows <- !is.na(yearDCM_final[[col]])
    
    # Perform PCHIP interpolation only on non-NA values
    yearDCM_final[[col]][!non_na_rows] <- pracma::pchip(
      yearDCM_final$DOY[non_na_rows],           # DOY values where the column is not NA
      yearDCM_final[[col]][non_na_rows],        # Column values where not NA
      yearDCM_final$DOY[!non_na_rows]           # DOY values where the column is NA
    )
  }
  # 
  
  set.seed(123) # Setting seed for reproducibility
  index <- sample(1:nrow(yearDCM_final), size = 0.7 * nrow(yearDCM_final))  # 70% training data
  train_data <- yearDCM_final[index, ]
  test_data <- yearDCM_final[-index, ]
  non_numeric_columns <- sapply(train_data, function(x) !is.numeric(x) & !is.factor(x))
  train_data_no_non_numeric <- train_data %>% select(-which(non_numeric_columns))
  
  # Apply na.roughfix() to impute missing values in numeric and factor columns
  train_data_imputed <- na.roughfix(train_data_no_non_numeric)
  
  #z-transform
  train_data_imputed_z <- train_data_imputed %>%
    mutate(across(everything(), ~ scale(.)))
  
  #when running RF for all years remove buoyancy freq
  train_data_imputed_z<- train_data_imputed_z|>
    select(-DCM_buoyancy_freq)
  
  train_data_imputed_z <- train_data_imputed_z %>%
    na.omit()  # Removes any rows with NAs
  
  #for 2022 leave out waterlevel bc too many NAs
  #train_data_imputed_z <- train_data_imputed_z|>
  #  select(-WaterLevel_m)
  
  # Add the excluded non-numeric columns (e.g., Date) back to the imputed dataset
  model_rf <- randomForest(Totals_DCM_conc ~ ., data = train_data_imputed_z, ntree = 500, importance = TRUE)
  
  importance(model_rf)
  
  #visualize
  importance_df <- as.data.frame(importance(model_rf))
  importance_df <- rownames_to_column(importance_df, var = "Variable") # Convert row names to a column
  
  filtered_importance_df <- importance_df %>%
    filter(!is.na(`%IncMSE`), `%IncMSE` > 0)# Filter for positive %IncMSE values
  
  #for all years
  filtered_importance_df <- filtered_importance_df |>
    filter(!Variable %in% c("peak.top", "pH_at_DCM", "min_pH_depth",  "min_Cond_uScm_depth", "max_Cond_uScm_depth", "peak.magnitude", "peak.bottom", "secchi_PZ", "PAR_PZ", "max_Cond_uScm_depth", "DayOfYear", "DOY", "min_Cond_uScm_dept", "min_CH4_umolL_depth", "max_CH4_umolL_depth"))
  
  
  
  # Create the plot
  ggplot(filtered_importance_df, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
    geom_point(color = "blue", size = 3) +
    labs(
      title = "Variable Importance based on % IncMSE 2014-2023",
      x = "% IncMSE",
      y = "Variables"
    ) +
    theme_minimal()
 
  


    


