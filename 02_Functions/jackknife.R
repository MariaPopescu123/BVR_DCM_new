#trying to make the plot that visualizes the #incMSE for each variable within each year



#Example use of function
# #-------------------------------------
res <- jackknife_incMSE_heatmap(
  Xdataframe     = final_depth_analysis,
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

jackknife_incMSE_heatmap <- function(
    Xdataframe,
    year_min,
    year_max,
    response_var,
    whichvars_label = "",
    tune_grid = list(
      ntree    = c(300),
      mtry     = NULL,           # if NULL, will use 2:floor(p/2) capped at p
      nodesize = c(2, 5)
    ),
    save_path = NULL
) {
  
  # ---------- 0) Prep & cleaning ----------
  df0 <- Xdataframe %>%
    mutate(Date = as.Date(.data$Date),
           Year = lubridate::year(.data$Date)) %>%
    dplyr::filter(.data$Year >= year_min, .data$Year <= year_max)
  
  if (!response_var %in% names(df0)) stop("response_var not found in dataframe.")
  
  # Keep numeric/factor predictors (drop Date)
  keep_cols <- df0 %>%
    dplyr::select(-Date) %>%
    dplyr::select(where(~ is.numeric(.x) || is.factor(.x))) %>%
    names()
  
  # Build modeling frame: Year, response, predictors
  model_df <- df0 %>%
    dplyr::select(Year, dplyr::all_of(response_var), dplyr::any_of(setdiff(keep_cols, response_var))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  
  # Drop columns with >25% NA (keep Year and response)
  na_frac <- sapply(model_df, function(x) mean(is.na(x)))
  drop_cols <- setdiff(names(na_frac)[na_frac > 0.25], c("Year", response_var))
  model_df <- model_df %>% dplyr::select(-dplyr::any_of(drop_cols))
  
  # Drop any remaining NA rows
  model_df <- tidyr::drop_na(model_df)
  
  # Early exit if not enough predictors
  pred_cols_all <- setdiff(names(model_df), c("Year", response_var))
  if (length(pred_cols_all) < 2L) stop("Not enough predictors after cleaning.")
  
  # Count n per year (used in x-axis labels)
  n_per_year <- model_df %>%
    dplyr::count(Year, name = "n") %>%
    dplyr::arrange(Year)
  
  # helper: build tuning grid given p
  .mtry_grid <- function(p) {
    if (is.null(tune_grid$mtry)) {
      unique(pmin(seq(2, max(2, floor(p/2))), p))
    } else {
      unique(pmin(unlist(tune_grid$mtry), p))
    }
  }
  
  # helper: tune RF once on provided data; returns best row (ntree/mtry/nodesize)
  .tune_once <- function(df_fit) {
    pred_cols <- setdiff(names(df_fit), response_var)
    p <- length(pred_cols)
    y_obs <- df_fit[[response_var]]
    
    grid <- expand.grid(
      ntree    = unlist(tune_grid$ntree),
      mtry     = .mtry_grid(p),
      nodesize = unlist(tune_grid$nodesize)
    )
    
    best <- NULL
    best_score <- -Inf
    
    for (gi in seq_len(nrow(grid))) {
      g <- grid[gi, ]
      fit <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data       = df_fit,
        ntree      = g$ntree,
        mtry       = g$mtry,
        nodesize   = g$nodesize,
        importance = TRUE
      )
      preds <- predict(fit, df_fit)
      rsq   <- 1 - sum((y_obs - preds)^2) / sum((y_obs - mean(y_obs))^2)
      if (rsq > best_score) { best_score <- rsq; best <- g }
    }
    best
  }
  
  # helper: extract %IncMSE safely and return tibble(Variable, `%IncMSE`)
  .imp_df <- function(fit) {
    imp <- as.data.frame(randomForest::importance(fit))
    imp$Variable <- rownames(imp)
    tibble::as_tibble(imp) %>% dplyr::select(Variable, `%IncMSE`)
  }
  
  all_imp_long <- list()
  
  # ---------- 1) Per-year jackknife (with graceful fallback for small n) ----------
  for (yy in sort(unique(model_df$Year))) {
    df_y <- model_df %>% dplyr::filter(Year == yy) %>% dplyr::select(-Year)
    n_y  <- nrow(df_y)
    if (n_y == 0) next
    
    # Tune once on full year's data
    best <- .tune_once(df_y)
    
    if (n_y < 5) {
      # Fallback: single fit (no jackknife) so the year still appears
      fit <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data       = df_y,
        ntree      = best$ntree,
        mtry       = best$mtry,
        nodesize   = best$nodesize,
        importance = TRUE
      )
      imp_long <- .imp_df(fit) %>% dplyr::mutate(Year = yy, jack_idx = 1L)
      all_imp_long[[as.character(yy)]] <- imp_long
      next
    }
    
    # Jackknife: drop each observation once
    imp_stack <- vector("list", n_y)
    for (i in seq_len(n_y)) {
      fit_i <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data       = df_y[-i, , drop = FALSE],
        ntree      = best$ntree,
        mtry       = best$mtry,
        nodesize   = best$nodesize,
        importance = TRUE
      )
      imp_stack[[i]] <- .imp_df(fit_i) %>% dplyr::mutate(Year = yy, jack_idx = i)
    }
    all_imp_long[[as.character(yy)]] <- dplyr::bind_rows(imp_stack)
  }
  
  # ---------- 1b) Add pooled "All years" jackknife as a final column ----------
  df_all <- model_df %>% dplyr::select(-Year)
  n_all  <- nrow(df_all)
  if (n_all < 2L) stop("Not enough rows overall for pooled analysis.")
  
  best_all <- .tune_once(df_all)
  
  if (n_all >= 5) {
    imp_stack_all <- vector("list", n_all)
    for (i in seq_len(n_all)) {
      fit_i <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data       = df_all[-i, , drop = FALSE],
        ntree      = best_all$ntree,
        mtry       = best_all$mtry,
        nodesize   = best_all$nodesize,
        importance = TRUE
      )
      imp_stack_all[[i]] <- .imp_df(fit_i) %>% dplyr::mutate(Year = 9999L, jack_idx = i)
    }
    all_imp_long[["ALL"]] <- dplyr::bind_rows(imp_stack_all)
  } else {
    # Fallback single fit if overall n is very small
    fit_all <- randomForest::randomForest(
      as.formula(paste(response_var, "~ .")),
      data       = df_all,
      ntree      = best_all$ntree,
      mtry       = best_all$mtry,
      nodesize   = best_all$nodesize,
      importance = TRUE
    )
    all_imp_long[["ALL"]] <- .imp_df(fit_all) %>% dplyr::mutate(Year = 9999L, jack_idx = 1L)
  }
  
  if (length(all_imp_long) == 0) stop("No years had enough observations after cleaning.")
  
  imp_long <- dplyr::bind_rows(all_imp_long) %>% dplyr::filter(!is.na(`%IncMSE`))
  
  # ---------- 2) Summarize mean ± sd per (Year, Variable) ----------
  imp_summary <- imp_long %>%
    dplyr::group_by(Year, Variable) %>%
    dplyr::summarise(
      mean_incMSE = mean(`%IncMSE`, na.rm = TRUE),
      sd_incMSE   = stats::sd(`%IncMSE`,   na.rm = TRUE),
      .groups = "drop"
    )
  
  # Determine a consistent variable order by overall mean across years
  var_order <- imp_summary %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(overall_mean = mean(mean_incMSE, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(overall_mean)) %>%
    dplyr::pull(Variable)
  
  # y order: most important at top
  imp_summary <- imp_summary %>%
    dplyr::mutate(Variable = factor(Variable, levels = rev(var_order)))
  
  # Year labels with n + pooled "All"
  n_all_row <- tibble::tibble(Year = 9999L, n = n_all)
  n_per_year2 <- dplyr::bind_rows(n_per_year, n_all_row)
  
  year_lab <- n_per_year2 %>%
    dplyr::mutate(Year_label = dplyr::if_else(Year == 9999L,
                                              paste0("All\n(n=", n, ")"),
                                              paste0(Year, "\n(n=", n, ")"))) %>%
    dplyr::select(Year, Year_label) %>%
    dplyr::arrange(Year)  # 9999 -> last
  
  plot_df <- imp_summary %>%
    dplyr::left_join(year_lab, by = "Year") %>%
    dplyr::mutate(Year_label = factor(Year_label, levels = unique(year_lab$Year_label)))
  
  # ---------- 3) Heatmap with text "mean±sd" ----------
  heat <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Year_label, y = Variable, fill = mean_incMSE)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f±%.1f", mean_incMSE, sd_incMSE)), size = 2.8) +
    viridis::scale_fill_viridis(name = "Mean %IncMSE", option = "H") +
    ggplot2::labs(
      title = paste0("Jackknife %IncMSE (", year_min, "–", year_max, " + All)"),
      subtitle = whichvars_label,
      x = "Year (n after cleaning)",
      y = "Variables (ordered by overall mean %IncMSE)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(lineheight = 0.9),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(filename = save_path, plot = heat, width = 12, height = 8, dpi = 400, bg = "white")
  }
  
  invisible(list(plot = heat, summary = plot_df))
}
