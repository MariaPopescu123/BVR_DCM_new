Heavy use of ChatGPT (building off of the script that we worked together on already "Depth_analysis")
because I needed help
horrible rsquared value

# -------------------------------
# 1. Prep data for RandomForest
# -------------------------------
library(randomForest)
library(missForest)
library(caret)
library(future)
library(here)
library(dplyr)
library(ggplot2)
library(tibble)

full_weekly_data <- read.csv(here("CSVs", "full_weekly_data.csv"))|>
  filter(year(Date) != 2022)

depth_analysis <- full_weekly_data |>
  select(
    Date, DCM_depth, PZ, WaterLevel_m,
    depth_TFe_mgL_min, depth_TFe_mgL_max, schmidt_stability,
    avg_airtemp_lagged, Precip_avg_lagged, PAR_umolm2s_Weekly_Avg_lagged,
    depth_np_ratio_min
  )

# Remove non-numeric columns (excluding Date)
non_numeric_columns <- sapply(depth_analysis, function(x) !is.numeric(x) & !is.factor(x))
final_no_non_numeric <- depth_analysis |>
  select(-which(non_numeric_columns))

# Replace Inf and NaN with NA
final_no_non_numeric <- final_no_non_numeric %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

# Remove columns with > 75% NA
final_data_no_na <- final_no_non_numeric |>
  select(where(~ mean(is.na(.)) <= 0.25))

# Remove rows with any remaining NA
RF_depth_analysis <- final_data_no_na |>
  na.omit()

# -------------------------------
# 2. Split into 90% train / 10% test
# -------------------------------
set.seed(123)
train_index <- 1:floor(0.7 * nrow(RF_depth_analysis))
train_data <- RF_depth_analysis[train_index, ]
test_data  <- RF_depth_analysis[-train_index, ]

# Remove Date for modeling
train_model_data <- train_data
test_model_data  <- test_data

# -------------------------------
# 3. Grid search / tuning on training set
# -------------------------------
# Optional: parallel processing
plan(multisession)

results <- list()
i <- 1

for (tree_num in c(100, 200, 300, 500)) {
  for (node_size in c(2, 4, 6, 8)) {
    for (mt in c(3,4,5,6,7,8)) {
      
      model_rf <- randomForest(
        DCM_depth ~ ., 
        data = train_model_data,
        ntree = tree_num,
        mtry = mt,
        nodesize = node_size,
        importance = TRUE
      )
      
      preds <- predict(model_rf, train_model_data)
      obs <- train_model_data$DCM_depth
      rsq_train <- 1 - sum((obs - preds)^2) / sum((obs - mean(obs))^2)
      mse_train <- mean(model_rf$mse)
      
      # Store results
      results[[i]] <- data.frame(
        Trees = tree_num,
        Node_size = node_size,
        mtry = mt,
        R_squared = rsq_train,
        MSE = mse_train
      )
      i <- i + 1
    }
  }
}

# Combine results
depth_RF_tuning_scores <- do.call(rbind, results) |> arrange(desc(R_squared))
print(depth_RF_tuning_scores)

# -------------------------------
# 4. Train final RandomForest on training set using best params
# -------------------------------
best_params <- depth_RF_tuning_scores[1, ]

final_model_rf <- randomForest(
  DCM_depth ~ .,
  data = train_model_data,
  ntree = best_params$Trees,
  mtry = best_params$mtry,
  nodesize = best_params$Node_size,
  importance = TRUE
)

# -------------------------------
# 5. Evaluate on test set
# -------------------------------
pred_test <- predict(final_model_rf, newdata = test_model_data)
obs_test <- test_model_data$DCM_depth

rsq_test <- 1 - sum((obs_test - pred_test)^2) / sum((obs_test - mean(obs_test))^2)
rmse_test <- sqrt(mean((obs_test - pred_test)^2))
mae_test  <- mean(abs(obs_test - pred_test))

cat("Test set R²:", rsq_test, "\n")
cat("Test set RMSE:", rmse_test, "\n")
cat("Test set MAE:", mae_test, "\n")

# -------------------------------
# 6. Variable importance visualization
# -------------------------------
importance_df <- as.data.frame(importance(final_model_rf))
importance_df <- rownames_to_column(importance_df, var = "Variable")

filtered_importance_df <- importance_df |>
  filter(!is.na(`%IncMSE`), `%IncMSE` > 0)

var_importance_depth_plot <- ggplot(filtered_importance_df, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Variable Importance for Depth Prediction 2014–2023",
    x = "% IncMSE",
    y = "Variables"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 25, hjust = .5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 5)
  )

# Save plot
ggsave(here("Figs", "var_importance_depth_plot.png"), plot = var_importance_depth_plot, width = 10, height = 4, dpi = 600)

print(var_importance_depth_plot)
