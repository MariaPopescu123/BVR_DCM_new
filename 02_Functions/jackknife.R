#trying to make the plot that visualizes the #incMSE for each variable within each year



#Example use of function
#-------------------------------------
res <- jackknife_incMSE_heatmap(
  Xdataframe     = selected_depth_analysis,
  year_min       = 2015,
  year_max       = 2024,
  response_var   = "DCM_depth",
  whichvars_label= "Selected variables",
  save_path      = here::here("Figs","MachineLearning","Depth","Jackknife_Heatmap.png")
)
# View the plot
res$plot
# See the summarized table (Year, Variable, mean_incMSE, sd_incMSE used for the tiles/labels)
res$summary %>% as.data.frame() %>% head()



#function
#--------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(tibble)
  library(randomForest)
  library(scales)
})

# Jackknife %IncMSE heatmap across years
# -----------------------------------------------------------
# Xdataframe : data.frame with a Date column and predictors + response
# year_min, year_max : inclusive year range (e.g., 2015, 2024)
# response_var : name of response column (character)
# whichvars_label : label for titles
# tune_grid : list with ntree, mtry candidates, nodesize candidates (optional)
# save_path : optional file path to ggsave the plot (PNG/PDF based on extension)
#
# Returns: list(plot = ggplot object, summary = data.frame of mean/sd per Year/Variable)
jackknife_incMSE_heatmap <- function(
    Xdataframe,
    year_min,
    year_max,
    response_var,
    whichvars_label = "",
    tune_grid = list(
      ntree    = c(300),         # keep reasonable for speed
      mtry     = NULL,           # if NULL, will use 2:floor(p/2) capped at p
      nodesize = c(2, 5)         # small nodesizes work well with RF
    ),
    save_path = NULL
) {
  
  # ---------- 0) Prep & cleaning ----------
  df0 <- Xdataframe %>%
    mutate(Date = as.Date(.data$Date),
           Year = year(.data$Date)) %>%
    filter(.data$Year >= year_min, .data$Year <= year_max)
  
  if (!response_var %in% names(df0)) {
    stop("response_var not found in dataframe.")
  }
  
  # Keep numeric/factor predictors (drop Date)
  keep_cols <- df0 %>%
    select(-Date) %>%
    select(where(~ is.numeric(.x) || is.factor(.x))) %>%
    names()
  
  # Build modeling frame: Year, response, predictors
  model_df <- df0 %>%
    select(Year, all_of(response_var), any_of(setdiff(keep_cols, response_var))) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  
  # Drop columns with >25% NA (keep Year and response)
  na_frac <- sapply(model_df, function(x) mean(is.na(x)))
  drop_cols <- setdiff(names(na_frac)[na_frac > 0.25], c("Year", response_var))
  model_df <- model_df %>% select(-any_of(drop_cols))
  
  # Drop any remaining NA rows
  model_df <- tidyr::drop_na(model_df)
  
  # Early exit if not enough predictors
  pred_cols_all <- setdiff(names(model_df), c("Year", response_var))
  if (length(pred_cols_all) < 2L) stop("Not enough predictors after cleaning.")
  
  # Count n per year (used in x-axis labels)
  n_per_year <- model_df %>%
    count(Year, name = "n") %>%
    arrange(Year)
  
  # ---------- 1) Per-year jackknife over observations ----------
  all_imp_long <- list()
  
  for (yy in sort(unique(model_df$Year))) {
    df_y <- model_df %>% filter(Year == yy) %>% select(-Year)
    
    # Skip years with too few rows
    if (nrow(df_y) < 5) next
    
    # Predictors for this year
    pred_cols <- setdiff(names(df_y), response_var)
    p <- length(pred_cols)
    y_obs <- df_y[[response_var]]
    
    # Build tuning grid (per year)
    mtry_grid <- if (is.null(tune_grid$mtry)) {
      unique(pmin(seq(2, max(2, floor(p/2))), p))
    } else {
      unique(pmin(unlist(tune_grid$mtry), p))
    }
    
    grid <- expand.grid(
      ntree    = unlist(tune_grid$ntree),
      mtry     = mtry_grid,
      nodesize = unlist(tune_grid$nodesize)
    )
    
    # Tune once on full year data
    best <- NULL
    best_score <- -Inf
    
    for (gi in seq_len(nrow(grid))) {
      g <- grid[gi, ]
      fit <- randomForest(
        as.formula(paste(response_var, "~ .")),
        data       = df_y,
        ntree      = g$ntree,
        mtry       = g$mtry,
        nodesize   = g$nodesize,
        importance = TRUE
      )
      preds <- predict(fit, df_y)
      rsq   <- 1 - sum((y_obs - preds)^2) / sum((y_obs - mean(y_obs))^2)
      
      if (rsq > best_score) {
        best_score <- rsq
        best <- g
      }
    }
    
    # Jackknife: drop each observation once
    imp_stack <- list()
    for (i in seq_len(nrow(df_y))) {
      fit_i <- randomForest(
        as.formula(paste(response_var, "~ .")),
        data       = df_y[-i, , drop = FALSE],
        ntree      = best$ntree,
        mtry       = best$mtry,
        nodesize   = best$nodesize,
        importance = TRUE
      )
      imp <- as.data.frame(importance(fit_i))
      imp$Variable <- rownames(imp)
      
      imp_long <- imp %>%
        as_tibble() %>%
        select(Variable, `%IncMSE`) %>%
        mutate(Year = yy, jack_idx = i)
      
      imp_stack[[i]] <- imp_long
    }
    
    all_imp_long[[as.character(yy)]] <- bind_rows(imp_stack)
  }
  
  if (length(all_imp_long) == 0) stop("No years had enough observations after cleaning.")
  
  imp_long <- bind_rows(all_imp_long) %>%
    filter(!is.na(`%IncMSE`))
  
  # ---------- 2) Summarize mean ± sd per (Year, Variable) ----------
  imp_summary <- imp_long %>%
    group_by(Year, Variable) %>%
    summarise(
      mean_incMSE = mean(`%IncMSE`, na.rm = TRUE),
      sd_incMSE   = sd(`%IncMSE`,   na.rm = TRUE),
      .groups = "drop"
    )
  
  # Determine a consistent variable order by overall mean across years
  var_order <- imp_summary %>%
    group_by(Variable) %>%
    summarise(overall_mean = mean(mean_incMSE, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(overall_mean)) %>%
    pull(Variable)
  
  # y order: most important at top
  imp_summary <- imp_summary %>%
    mutate(Variable = factor(Variable, levels = rev(var_order)))
  
  # Year labels with n
  year_lab <- n_per_year %>%
    mutate(Year_label = paste0(Year, "\n(n=", n, ")")) %>%
    select(Year, Year_label)
  
  plot_df <- imp_summary %>%
    left_join(year_lab, by = "Year") %>%
    mutate(Year_label = factor(Year_label, levels = unique(year_lab$Year_label)))
  
  # ---------- 3) Heatmap with text "mean±sd" ----------
  heat <- ggplot(plot_df, aes(x = Year_label, y = Variable, fill = mean_incMSE)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = sprintf("%.1f±%.1f", mean_incMSE, sd_incMSE)), size = 2.8) +
    scale_fill_viridis_c(name = "Mean %IncMSE", option = "H") +
    labs(
      title = paste0("Jackknife %IncMSE (", year_min, "–", year_max, ")"),
      subtitle = whichvars_label,
      x = "Year (n after cleaning)",
      y = "Variables (ordered by overall mean %IncMSE)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(lineheight = 0.9),
      plot.title = element_text(face = "bold")
    )
  
  # Save if requested
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = heat, width = 12, height = 8, dpi = 400, bg = "white")
  }
  
  invisible(list(plot = heat, summary = plot_df))
}
