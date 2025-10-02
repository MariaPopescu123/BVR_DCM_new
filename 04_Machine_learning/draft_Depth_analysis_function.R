#writing a function that will

#1. run a test random forest for a specific year
#2. select the best parameters and then run an actual random forest
#3. print out how many observations
#4. visualize variable importance
#5. visualize SHAP values


library(randomForest)
library(missForest)
library(caret)
library(future)
library(here)
library(fastshap)


var_importance_shap_plots <- function(dataframe, XYear){

depth_analysis <-  read.csv("CSVs/depth_analysis_revised.csv")

ys_depth_analysis <- depth_analysis|>
  filter(year(Date) == XYear)

# Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
non_numeric_columns <- sapply(ys_depth_analysis, function(x) !is.numeric(x) & !is.factor(x))
final_no_non_numeric <- ys_depth_analysis |>
  select(-which(non_numeric_columns)) 

# Replace Inf and NaN with NA in all numeric columns
final_no_non_numeric <- final_no_non_numeric %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

# Remove columns with more than 75% NA values
final_data_no_na <- final_no_non_numeric %>%
  select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA

# Remove remaining rows with any NA values
RF_depth_analysis <- final_data_no_na %>%
  na.omit()

#write.csv(RF_depth_analysis, here("CSVs", "RF_depth_analysis.csv"), row.names = FALSE)


#check to see how much is actually going into the analysis

test<-depth_analysis |>
  select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA

RF_frame_w_dates <- test %>%
  na.omit()

RF_frame_w_dates %>%
  count(year(Date)) %>%
  print() 


set.seed(123)

# Initialize empty list to store results
results <- list()

# Counter for indexing
i <- 1

for (tree_num in c(100, 200, 300, 500)) {
  for (node_size in c(2, 4, 6, 8)) {
    for (mt in c(3, 6, 9, 10, 20)) {
      
      model_rf <- randomForest(
        DCM_depth ~ ., 
        data = RF_depth_analysis,
        ntree = tree_num,
        mtry = mt,
        nodesize = node_size,
        importance = TRUE
      )
      
      preds <- predict(model_rf, RF_depth_analysis)
      obs <- RF_depth_analysis$DCM_depth
      rsq_test <- 1 - sum((obs - preds)^2) / sum((obs - mean(obs))^2)
      mse_test <- mean(model_rf$mse)
      
      # Store results
      results[[i]] <- data.frame(
        Trees = tree_num,
        `Node size` = node_size,
        mtry = mt,
        `R-squared` = rsq_test,
        MSE = mse_test
      )
      
      i <- i + 1
    }
  }
}

# Combine into a single data frame
depth_RF_tuning_scores <- do.call(rbind, results)


depth_RF_tuning_scores <- depth_RF_tuning_scores |>
  arrange(desc(`R.squared`))

print(depth_RF_tuning_scores)

#this gives the best score


# Now to run the actual model


test_model_rf <- randomForest(DCM_depth ~ .,
                              data = RF_depth_analysis,
                              ntree = 100,
                              node_size = 2,
                              mtry = 10,
                              importance = TRUE)

importance(test_model_rf)


#Prep to visualize

importance_df <- as.data.frame(importance(test_model_rf))
importance_df <- rownames_to_column(importance_df, var = "Variable") # Convert row names to a column

filtered_importance_df <- importance_df %>%
  filter(!is.na(`%IncMSE`), `%IncMSE` > 0)# Filter for valid and positive %IncMSE

rsq_test<- mean((model_rf$rsq))
mse_test<- mean((model_rf$mse))


# Create the plot
ggplot(filtered_importance_df, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = paste0("Variable Importance based on % IncMSE ", XYear),
    x = "% IncMSE",
    y = "Variables"
  ) +
  theme_minimal()

ggsave(
  filename = paste0("Figs/", XYear, "_depth_var_importance.png"),
  plot = plot_dist,
  width = 10,
  height = 4,
  dpi = 600,
  bg = "white"
)

#SHAP
#SHAP: SHAP values (SHapley Additive exPlanations) are a way to explain machine learning model predictions by showing how much each feature contributes to a particular prediction.
X = data.matrix(select(RF_depth_analysis, -DCM_depth))

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

# Now run plot
df %>%
  filter(row_id == 1) %>%
  ggplot(aes(x = shap, y = fct_reorder(paste0(var, "=", value), shap), fill = factor(sign(shap)))) +
  geom_col() +
  guides(fill = 'none') +
  labs(y = "", title = "SHAP values for X[1,]")


library(ggbeeswarm)
df %>%
  mutate(value = as.numeric(value)) %>%
  group_by(var) %>%
  mutate(nv = scale(value)) %>%
  ggplot(aes(x = shap, y = var, color = nv)) +
  geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
  scale_color_viridis_c(option = 'H', limits = c(-3, 3), oob = scales::oob_squish) +
  labs(
    title = paste('Distribution of SHAP values for all samples', XYear),
    y = '',
    color = 'z-scaled values'
  )

group_by(df, var) %>% 
  summarize(mean=mean(abs(shap))) %>%
  ggplot(aes(x=mean, y=fct_reorder(var, mean))) + 
  geom_col() +
  labs(x='mean(|shap value|)', title='mean absolute shap for all samples', y="")

group_by(df, var, sign=factor(sign(shap))) %>%
  summarize(mean=mean(shap)) %>%
  ggplot(aes(x=mean, y=fct_reorder(var, mean), fill=sign)) + 
  geom_col() +
  labs(x='mean(shap value)', title='mean shap for all samples 2023-2023', y="")


library(ggplot2)
library(dplyr)
library(scales)
library(here)

# Define the list of variables
vars_to_plot <- c('PZ', 'WaterLevel_m', 'thermocline_depth', 'schmidt_stability', 'depth_TFe_mgL_min')

# Loop and save each plot
for (v in vars_to_plot) {
  
  # Create plot
  p <- df %>%
    filter(var == v) %>%
    mutate(value = as.numeric(value)) %>%
    ggplot(aes(x = value, y = shap)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    labs(
      title = paste0('2013–2023 Interaction: SHAP vs ', v),
      x = v,
      y = "SHAP value"
    )
  
  # Print inline for RMarkdown rendering
  print(p)
  
  # Save to file
  ggsave(
    filename = here::here("Figs/xai_plots", paste0("shap_", v, ".png")),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}