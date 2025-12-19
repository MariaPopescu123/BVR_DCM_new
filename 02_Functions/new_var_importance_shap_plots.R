library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggbeeswarm)
library(tibble)
library(randomForest)
library(fastshap)
library(patchwork)
library(here)

# Put this near the top of the function (after library calls is fine)
axis_only_theme <- theme_classic(base_size = 10) +
  theme(
    panel.border = element_blank(),     # no full border
    axis.line    = element_line(color = "black", linewidth = 0.4),
    axis.ticks   = element_line(color = "black")
  )

# Xdataframe: data.frame with Date column (Date or POSIXt) + predictors + response
# XYear, XYear2: inclusive year bounds (e.g., 2014, 2019)
# whichvars: label for titles/files
# response_var: name of response column as a string (e.g., "DCM_depth" or "max_conc")
# save_dir: subfolder under Figs/MachineLearning/ for saving (e.g., "Depth" or "Magnitude")
# variable_labels: named character vector mapping internal names -> pretty labels used in plots
var_importance_shap_plots <- function(Xdataframe, XYear, XYear2, whichvars, response_var, save_dir,
                                      variable_labels = NULL) {
  # helper: safe pretty labels
  pretty_lab <- function(v) {
    if (is.null(variable_labels)) return(v)
    out <- unname(variable_labels[as.character(v)])
    out[is.na(out)] <- v[is.na(out)]
    out
  }
  # pretty response label if available
  response_label <- pretty_lab(response_var)
  
  # ---- Filter by year window; keep Year & row id so we can tally after cleaning ----
  df <- Xdataframe %>%
    mutate(Date = as.Date(Date),
           Year = year(Date),
           .row_id = seq_len(n())) %>%
    filter(Year >= XYear, Year <= XYear2)
  
  # ---- Quick counts (pre-clean) ----
  obs_total_before <- nrow(df)
  obs_per_year_before <- df %>%
    count(Year, name = "n") %>%
    arrange(Year)
  
  cat("\n Observations before cleaning:\n")
  print(obs_per_year_before)
  cat("Total before cleaning:", obs_total_before, "\n")
  
  # ---- Build modeling frame: response + numeric/factor predictors; drop helpers ----
  keep_cols <- df %>%
    select(-Date, -Year, -.row_id) %>%
    select(where(~ is.numeric(.x) || is.factor(.x))) %>%
    names()
  
  if (!response_var %in% names(df)) stop("response_var not found in dataframe.")
  
  model_df <- df %>%
    select(.row_id, Year, all_of(response_var), any_of(setdiff(keep_cols, response_var)))
  
  # ---- Replace Inf/NaN → NA ----
  model_df <- model_df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  
  # ---- Drop columns with >25% NA ----
  resp_and_keys <- c(".row_id", "Year", response_var)
  na_frac <- sapply(model_df, function(x) mean(is.na(x)))
  drop_cols <- names(na_frac)[na_frac > 0.25]
  drop_cols <- setdiff(drop_cols, resp_and_keys)
  model_df <- model_df %>% select(-any_of(drop_cols))
  
  # ---- Omit remaining NA rows ----
  model_df <- model_df %>% tidyr::drop_na()
  
  # ---- Summary after cleaning ----
  obs_total_after <- nrow(model_df)
  obs_per_year_after <- model_df %>%
    count(Year, name = "n") %>%
    arrange(Year)
  
  cat("\n observations after cleaning:\n")
  print(obs_per_year_after)
  cat("Total after cleaning:", obs_total_after, "\n")
  
  # ---- Predictor summary ----
  pred_cols <- setdiff(names(model_df), c(".row_id", "Year", response_var))
  if (length(pred_cols) < 2L) stop("Not enough predictors after cleaning.")
  
  message(sprintf("\nFinal model summary → Total observations: %d | Predictors: %d\n",
                  nrow(model_df), length(pred_cols)))
  
  # ---- RF tuning ----
  set.seed(123)
  rf_formula <- as.formula(paste(response_var, "~ ."))
  rf_df <- model_df %>% select(all_of(response_var), all_of(pred_cols))
  
  grid_trees <- c(100, 200, 300, 500)
  grid_nodes <- c(2, 4, 6, 8)
  max_mtry  <- max(1, floor(length(pred_cols) / 2))
  grid_mtry <- unique(pmin(seq(1, max_mtry, by = 1), length(pred_cols)))
  
  results <- list()
  idx <- 1
  y_obs <- rf_df[[response_var]]
  
  for (nt in grid_trees) {
    for (ns in grid_nodes) {
      for (mt in grid_mtry) {
        fit <- randomForest(
          formula    = rf_formula,
          data       = rf_df,
          ntree      = nt,
          mtry       = mt,
          nodesize   = ns,
          importance = TRUE
        )
        preds <- predict(fit, rf_df)
        rsq   <- 1 - sum((y_obs - preds)^2) / sum((y_obs - mean(y_obs))^2)
        mse   <- mean((y_obs - preds)^2)
        
        results[[idx]] <- data.frame(
          Trees = nt,
          NodeSize = ns,
          mtry = mt,
          R2 = rsq,
          MSE = mse
        )
        idx <- idx + 1
      }
    }
  }
  
  RF_tuning_scores <- dplyr::bind_rows(results) %>%
    arrange(desc(R2), MSE)
  print(RF_tuning_scores)
  
  best <- RF_tuning_scores[1, ]
  
  # ---- Final model ----
  final_rf <- randomForest(
    formula    = rf_formula,
    data       = rf_df,
    ntree      = best$Trees,
    mtry       = best$mtry,
    nodesize   = best$NodeSize,
    importance = TRUE
  )
  
  preds_final <- predict(final_rf, rf_df)
  r2_final <- 1 - sum((y_obs - preds_final)^2) / sum((y_obs - mean(y_obs))^2)
  meta_subtitle <- sprintf("%s | R²=%.3f | n=%d | ntree=%d | mtry=%d | nodesize=%d",
                           response_label, r2_final, nrow(rf_df), best$Trees, best$mtry, best$NodeSize)
  
  # ---- Variable importance (%IncMSE) ----
  imp_df <- as.data.frame(importance(final_rf)) %>%
    rownames_to_column("Variable") %>%
    filter(!is.na(`%IncMSE`), `%IncMSE` > 0)
  
  ordered_vars   <- imp_df %>% arrange(desc(`%IncMSE`)) %>% pull(Variable)
  reversed_vars  <- rev(ordered_vars)
  
  p_imp <- ggplot(
    imp_df,
    aes(x = `%IncMSE`, y = factor(Variable, levels = reversed_vars))
  ) +
    geom_point(size = 3) +
    labs(
      title = paste0(save_dir, " Variable Importance (%IncMSE) ", XYear, "-", XYear2),
      subtitle = meta_subtitle,
      x = "% IncMSE",
      y = "Variables"
    ) +
    scale_y_discrete(labels = pretty_lab) +
    axis_only_theme +                                # << use axis-only styling
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9)
    )
  
  # ---- SHAP ----
  X_shap <- rf_df %>% select(all_of(pred_cols))
  
  shap_values <- fastshap::explain(
    object       = final_rf,
    X            = X_shap,
    nsim         = 100,
    pred_wrapper = function(object, newdata) predict(object, newdata = newdata)
  )
  
  X_shap_num <- X_shap %>%
    mutate(across(!where(is.numeric), ~ as.numeric(as.factor(.x))))
  
  vars_long <- as_tibble(X_shap_num) %>%
    rownames_to_column("row_id") %>%
    pivot_longer(-row_id, names_to = "var", values_to = "value_num")
  
  shaps_long <- as_tibble(shap_values) %>%
    rownames_to_column("row_id") %>%
    pivot_longer(-row_id, names_to = "var", values_to = "shap")
  
  df_shap <- inner_join(vars_long, shaps_long, by = c("row_id", "var")) %>%
    mutate(shap = as.numeric(shap))
  
  df_shap_f <- df_shap %>%
    filter(var %in% ordered_vars) %>%
    mutate(var = factor(var, levels = reversed_vars))
  
  p_shap <- df_shap_f %>%
    group_by(var) %>%
    mutate(nv = scale(value_num)) %>%
    ungroup() %>%
    ggplot(aes(x = shap, y = var, color = nv)) +
    geom_quasirandom(groupOnX = FALSE, dodge.width = 0.3) +
    scale_color_viridis_c(option = "H", limits = c(-3, 3), oob = scales::oob_squish) +
    labs(
      title  = paste0(save_dir, " SHAP value distribution ",
                      XYear, "-", XYear2, "  ", whichvars),
      subtitle = response_label,
      y = "",
      color = "z-scaled\nvalues"
    ) +
    scale_y_discrete(labels = pretty_lab) +
    axis_only_theme +                                # << use axis-only styling
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  # ---- Combined & save ----
  combined_plot <- p_imp + p_shap + plot_layout(ncol = 2)
  
  ggsave(
    filename = here::here("Figs", "MachineLearning", save_dir,
                          paste0(XYear, "-", XYear2, "_", whichvars, "_Combined.png")),
    plot = combined_plot,
    width = 12, height = 5, dpi = 600, bg = "white"
  )
  
  #added this so I can make SHAP interaction plots 
  vars_long_raw <- as_tibble(X_shap) %>%
    rownames_to_column("row_id") %>%
    pivot_longer(-row_id, names_to = "var", values_to = "value_raw")
  
  df_shap <- df_shap %>%
    left_join(vars_long_raw, by = c("row_id","var"))
  
  return(list(
    plots = list(
      importance = p_imp,
      shap       = p_shap,
      combined   = combined_plot
    ),
    var_order = ordered_vars,
    model_details = list(
      response     = response_var,
      response_label = response_label,
      years        = c(XYear, XYear2),
      whichvars    = whichvars,
      n            = nrow(rf_df),
      r2           = unname(r2_final),
      best_params  = list(
        ntree    = best$Trees,
        mtry     = best$mtry,
        nodesize = best$NodeSize
      )
    ),
    counts = list(
      before = list(
        total  = obs_total_before,
        by_year = obs_per_year_before
      ),
      after = list(
        total  = obs_total_after,
        by_year = obs_per_year_after
      )
    ),
    tuning_scores = RF_tuning_scores,
    importance_table = imp_df, 
    shap_long = df_shap,        
    shap_long_filtered = df_shap_f
  ))
}
