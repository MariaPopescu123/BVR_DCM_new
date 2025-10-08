#writing a function that will

#before running the function, I want to be able to edit the dataframe

#1. run a test random forest for a specific year
#2. select the best parameters and then run a random forest
#3. print out how many observations
#4. visualize variable importance
#5. visualize SHAP values


library(randomForest)
library(missForest)
library(caret)
library(future)
library(here)
library(fastshap)
library(ggbeeswarm)

#xdataframe: either magnitude_analysis or depth_analysis
#XYear (range of dates you want to look at, if you want just one year then put same date for both)
#XYear2
#whichvars (this label helps name the file and title so that I can keep straight what I did differently)
#response_var (either DCM_depth or max_conc)
#save_dir = either "Depth" or "Magnitude"

var_importance_shap_plots <- function(Xdataframe, XYear, XYear2, whichvars, response_var, save_dir){
  
  whichvars <- whichvars
  prep_analysis <-  Xdataframe
  XYear <- XYear
  XYear2 <- XYear2
  response_var <- response_var
  save_dir <- save_dir
  
  ys_prep_analysis <- prep_analysis|>
    filter(year(Date) >= XYear, year(Date) <= XYear2)
  
  # Remove non-numeric columns (excluding Date, Depth_m, Year, etc.)
  non_numeric_columns <- sapply(ys_prep_analysis, function(x) !is.numeric(x) & !is.factor(x))
  final_no_non_numeric <- ys_prep_analysis |>
    select(-which(non_numeric_columns)) 
  
  # Replace Inf and NaN with NA in all numeric columns
  final_no_non_numeric <- final_no_non_numeric %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
  
  # Remove columns with more than 75% NA values
  final_data_no_na <- final_no_non_numeric %>%
    select(where(~ mean(is.na(.)) <= 0.25))  # Keep columns with ≤ 25% NA
  
  # Remove remaining rows with any NA values
  RF_prep_analysis <- final_data_no_na %>%
    na.omit()
  
  #write.csv(RF_prep_analysis, here("CSVs", "RF_prep_analysis.csv"), row.names = FALSE)
  
  
  ####check to see how much is actually going into the analysis####
  
  obs_per_year <- prep_analysis %>%
    semi_join(ys_prep_analysis,by = names(prep_analysis)[3:7]) %>%  # keep only rows also in ys_prep_analysis
    mutate(Date = as.Date(Date)) %>%
    count(Year = year(Date)) %>%
    arrange(Year)
  
  print(obs_per_year)
  
  set.seed(123)
  
  # Initialize empty list to store results
  results <- list()
  
  # Counter for indexing
  i <- 1
  
  rf_formula <- as.formula(paste(response_var, "~ ."))
  
  for (tree_num in c(100, 200, 300, 500)) {
    for (node_size in c(2, 4, 6, 8)) {
      for (mt in seq(3, (ncol(RF_prep_analysis) / 2), by = 1)) {
        
        model_rf <- randomForest(
          rf_formula,
          data = RF_prep_analysis,
          ntree = tree_num,
          mtry = mt,
          nodesize = node_size,
          importance = TRUE
        )
        
        
        preds <- predict(model_rf, RF_prep_analysis)
        obs <- RF_prep_analysis$response_var
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
  RF_tuning_scores <- do.call(rbind, results)
  
  
  RF_tuning_scores <- RF_tuning_scores |>
    arrange(desc(`R.squared`))
  
  print(RF_tuning_scores)
  
  #this gives the best score
  
  
  # Now to run the actual model
  
  best_params <- RF_tuning_scores[1, ]
  
  test_model_rf <- randomForest(rf_formula,
                                data = RF_prep_analysis,
                                ntree = best_params$Trees,
                                nodesize = best_params$Node.size, 
                                mtry = best_params$mtry,
                                importance = TRUE)
  
  importance(test_model_rf)
  
  
  #Prep to visualize
  
  importance_df <- as.data.frame(importance(test_model_rf))
  importance_df <- rownames_to_column(importance_df, var = "Variable") # Convert row names to a column
  
  filtered_importance_df <- importance_df %>%
    filter(!is.na(`%IncMSE`), `%IncMSE` > 0)# Filter for valid and positive %IncMSE
  
  rsq_test<- mean((model_rf$rsq))
  mse_test<- mean((model_rf$mse))
  
  ####variable importance plot####
  # Create the plot
  this <- ggplot(filtered_importance_df, aes(x = `%IncMSE`, y = reorder(Variable, `%IncMSE`))) +
    geom_point(color = "blue", size = 3) +
    labs(
      title = paste0(save_dir," Variable Importance based on % IncMSE \n",
                     XYear, "-", XYear2," ", whichvars),
      x = "% IncMSE",
      y = "Variables"
    ) +
    theme_classic(base_size = 10)
  
  ggsave(
    filename = here::here("Figs", "MachineLearning", save_dir,
                          paste0(XYear, "-", XYear2, "_", whichvars, "_",save_dir,"_var_importance.png")),
    plot = this ,
    width = 10,
    height = 4,
    dpi = 600,
    bg = "white"
  )
  
  #SHAP
  #SHAP: SHAP values (SHapley Additive exPlanations) are a way to explain machine learning model predictions by showing how much each feature contributes to a particular prediction.
  X = data.matrix(select(RF_prep_analysis, -response_var))
  
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
  
  # Convert to atomic vectors just in case
  df <- df %>%
    mutate(
      shap = as.numeric(shap),
      value = as.character(value),  # or as.numeric if needed
      var = as.character(var)
    )
  
  ####SHAP plot####
  
  p <- df %>%
    mutate(value = as.numeric(value)) %>%
    group_by(var) %>%
    mutate(
      nv = scale(value),
      mean_abs_shap = mean(abs(shap), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    ggplot(aes(
      x = shap,
      y = fct_reorder(var, mean_abs_shap, .desc = FALSE),  # reverse order
      color = nv
    )) +
    geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
    scale_color_viridis_c(
      option = 'H',
      limits = c(-3, 3),
      oob = scales::oob_squish
    ) +
    labs(
      title = paste(save_dir, 'Distribution of SHAP values\n', XYear, "-", XYear2," ",whichvars),
      y = '',
      color = 'z-scaled values'
    ) +
    theme_classic(base_size = 10) +   # white background
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Now save the plot
  ggsave(
    filename = here::here("Figs", "MachineLearning", save_dir,
                          paste0(XYear, "-", XYear2, "_", whichvars, "_SHAP.png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  ####combined plots####
  combined_plot <- this + p + plot_layout(ncol = 2) +
    plot_annotation(
      title = paste0("Variable Importance and SHAP Summary: ", response_var, " (", XYear, "-", XYear2, ") ", whichvars)
    )
  
  ggsave(
    filename = here::here("Figs", "MachineLearning", save_dir,
                          paste0(XYear, "-", XYear2, "_", whichvars, "_Combined.png")),
    plot = combined_plot,
    width = 12, height = 5, dpi = 600, bg = "white"
  )
  
return(combined_plot)
  # 
  # group_by(df, var) %>% 
  #   summarize(mean=mean(abs(shap))) %>%
  #   ggplot(aes(x=mean, y=fct_reorder(var, mean))) + 
  #   geom_col() +
  #   labs(x='mean(|shap value|)', title='mean absolute shap for all samples', y="")
  
  # group_by(df, var, sign=factor(sign(shap))) %>%
  #   summarize(mean=mean(shap)) %>%
  #   ggplot(aes(x=mean, y=fct_reorder(var, mean), fill=sign)) + 
  #   geom_col() +
  #   labs(x='mean(shap value)', title=paste('mean shap',XYear,'-',XYear2, y=""))
  
  
  # library(ggplot2)
  # library(dplyr)
  # library(scales)
  # library(here)
  # 
  # # Define the list of variables
  # vars_to_plot <- c('PZ', 'WaterLevel_m', 'thermocline_depth', 'schmidt_stability', 'depth_TFe_mgL_min')
  # 
  # # Loop and save each plot
  # for (v in vars_to_plot) {
  # 
  #   # Create plot
  #   p <- df %>%
  #     filter(var == v) %>%
  #     mutate(value = as.numeric(value)) %>%
  #     ggplot(aes(x = value, y = shap)) +
  #     geom_point() +
  #     geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  #     scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  #     labs(
  #       title = paste0('2013–2023 Interaction: SHAP vs ', v),
  #       x = v,
  #       y = "SHAP value"
  #     )
  # 
  #   # Print inline for RMarkdown rendering
  #   print(p)
  # 
  #   # Save to file
  # ggsave(
  #   filename = here::here("Figs/xai_plots", paste0("shap_", v, ".png")),
  #   plot = p,
  #   width = 6,
  #   height = 4,
  #   dpi = 300
  # )
}
