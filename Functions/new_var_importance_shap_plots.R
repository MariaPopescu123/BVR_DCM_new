#RF variable importance (%IncMSE) and SHAP value plots for a given response variable.

axis_only_theme <- theme_classic(base_size = 12) +
  theme(
    panel.border = element_blank(),
    axis.line    = element_line(color = "black", linewidth = 0.4),
    axis.ticks   = element_line(color = "black")
  )

var_importance_shap_plots <- function(Xdataframe,
                                      XYear = NULL,
                                      XYear2 = NULL,
                                      years = NULL,
                                      whichvars,
                                      response_var,
                                      save_dir,
                                      variable_labels = NULL) {
  pretty_lab <- function(v) {
    if (is.null(variable_labels)) return(v)
    out <- unname(variable_labels[as.character(v)])
    out[is.na(out)] <- v[is.na(out)]
    out
  }
  response_label <- pretty_lab(response_var)
  
  df <- Xdataframe %>%
    mutate(
      Date   = as.Date(Date),
      Year   = year(Date),
      .row_id = seq_len(n())
    )
  
  if (!is.null(years)) {
    years <- sort(unique(years))
    df <- df %>% filter(Year %in% years)
    XYear  <- min(years)
    XYear2 <- max(years)
  } else {
    if (is.null(XYear) || is.null(XYear2)) {
      stop("Provide either `years = c(YYYY, YYYY)` OR both `XYear` and `XYear2`.")
    }
    df <- df %>% filter(Year >= XYear, Year <= XYear2)
  }
  
  obs_total_before <- nrow(df)
  obs_per_year_before <- df %>%
    count(Year, name = "n") %>%
    arrange(Year)
  
  cat("\n Observations before cleaning:\n")
  print(obs_per_year_before)
  cat("Total before cleaning:", obs_total_before, "\n")
  
  keep_cols <- df %>%
    select(-Date, -Year, -.row_id) %>%
    select(where(~ is.numeric(.x) || is.factor(.x))) %>%
    names()
  
  if (!response_var %in% names(df)) stop("response_var not found in dataframe.")
  
  model_df <- df %>%
    select(.row_id, Year, all_of(response_var), any_of(setdiff(keep_cols, response_var)))
  
  model_df <- model_df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  
  #drop columns with >25% NA
  resp_and_keys <- c(".row_id", "Year", response_var)
  na_frac <- sapply(model_df, function(x) mean(is.na(x)))
  drop_cols <- names(na_frac)[na_frac > 0.25]
  drop_cols <- setdiff(drop_cols, resp_and_keys)
  model_df <- model_df %>% select(-any_of(drop_cols))
  
  model_df <- model_df %>% tidyr::drop_na()
  
  obs_total_after <- nrow(model_df)
  obs_per_year_after <- model_df %>%
    count(Year, name = "n") %>%
    arrange(Year)
  
  cat("\n observations after cleaning:\n")
  print(obs_per_year_after)
  cat("Total after cleaning:", obs_total_after, "\n")
  
  pred_cols <- setdiff(names(model_df), c(".row_id", "Year", response_var))
  if (length(pred_cols) < 2L) stop("Not enough predictors after cleaning.")
  
  message(sprintf("\nFinal model summary → Total observations: %d | Predictors: %d\n",
                  nrow(model_df), length(pred_cols)))
  
  #grid search for RF hyperparameters
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
        rsq <- fit$rsq[length(fit$rsq)]
        mse <- fit$mse[length(fit$mse)]

        results[[idx]] <- data.frame(
          Trees = nt,
          NodeSize = ns,
          mtry = mt,
          OOB_R2 = rsq,
          OOB_MSE = mse,
          OOB_RMSE = sqrt(mse)
        )
        idx <- idx + 1
      }
    }
  }
  
  RF_tuning_scores <- dplyr::bind_rows(results) %>%
    arrange(desc(OOB_R2), OOB_MSE)
  print(RF_tuning_scores)
  
  best <- RF_tuning_scores[1, ]
  
  final_rf <- randomForest(
    formula    = rf_formula,
    data       = rf_df,
    ntree      = best$Trees,
    mtry       = best$mtry,
    nodesize   = best$NodeSize,
    importance = TRUE
  )
  
  r2_oob   <- final_rf$rsq[length(final_rf$rsq)]
  mse_oob  <- final_rf$mse[length(final_rf$mse)]
  rmse_oob <- sqrt(mse_oob)
  meta_subtitle <- sprintf("%s | OOB R²=%.3f | OOB RMSE=%.2f | n=%d | ntree=%d | mtry=%d | nodesize=%d",
                           response_label, r2_oob, rmse_oob, nrow(rf_df), best$Trees, best$mtry, best$NodeSize)
  
  #%IncMSE importance plot
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
    geom_text(
      aes(label = sprintf("%.2f", `%IncMSE`)),
      hjust = -0.5,
      size = 4.0
    ) +
    labs(
      title = paste0(save_dir, " Variable Importance (%IncMSE) ", XYear, "-", XYear2),
      subtitle = meta_subtitle,
      x = "% IncMSE",
      y = "Variables"
    ) +
    scale_y_discrete(labels = pretty_lab) +
    axis_only_theme +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )+
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))
  
  
  
  
  
  
  

  
  
  #SHAP values
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
    axis_only_theme +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  combined_plot <- p_imp + p_shap + plot_layout(ncol = 2)
  
  ggsave(
    filename = here::here("Figs", "MachineLearning", save_dir,
                          paste0(XYear, "-", XYear2, "_", whichvars, "_Combined.png")),
    plot = combined_plot,
    width = 12, height = 5, dpi = 600, bg = "white"
  )
  
  # added this so I can make SHAP interaction plots 
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
      response       = response_var,
      response_label = response_label,
      years          = if (!is.null(years)) years else c(XYear, XYear2),
      whichvars      = whichvars,
      n              = nrow(rf_df),
      oob_r2         = unname(r2_oob),
      oob_mse        = unname(mse_oob),
      oob_rmse       = unname(rmse_oob),
      best_params    = list(
        ntree    = best$Trees,
        mtry     = best$mtry,
        nodesize = best$NodeSize
      )
    ),
    counts = list(
      before = list(
        total   = obs_total_before,
        by_year = obs_per_year_before
      ),
      after = list(
        total   = obs_total_after,
        by_year = obs_per_year_after
      )
    ),
    tuning_scores = RF_tuning_scores,
    importance_table = imp_df,
    shap_long = df_shap,
    shap_long_filtered = df_shap_f
  ))
}

