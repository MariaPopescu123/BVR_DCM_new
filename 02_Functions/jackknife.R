# trying to make the plot that visualizes the %IncMSE for each variable within each year

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(tibble)
  library(randomForest)
  library(scales)
  library(forcats)
})

jackknife_incMSE_heatmap <- function(
    Xdataframe, year_min, year_max,
    var_order = NULL, response_var,
    whichvars_label = "", save_path = NULL,
    seed_base = 20240601,
    variable_labels = NULL
) {
  
  # ---------- 0) Prep & cleaning ----------
  df0 <- Xdataframe %>%
    mutate(Date = as.Date(.data$Date),
           Year = lubridate::year(.data$Date)) %>%
    arrange(Year, Date) %>%
    filter(.data$Year >= year_min, .data$Year <= year_max)
  
  if (!response_var %in% names(df0)) stop("response_var not found in dataframe.")
  
  keep_cols <- df0 %>%
    select(-Date) %>%
    select(where(~ is.numeric(.x) || is.factor(.x))) %>%
    names()
  
  model_df <- df0 %>%
    select(Year, dplyr::all_of(response_var), dplyr::any_of(setdiff(keep_cols, response_var))) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  
  na_frac <- sapply(model_df, function(x) mean(is.na(x)))
  drop_cols <- setdiff(names(na_frac)[na_frac > 0.25], c("Year", response_var))
  model_df <- model_df %>% select(-any_of(drop_cols)) %>% tidyr::drop_na()
  
  pred_cols_all <- setdiff(names(model_df), c("Year", response_var))
  if (length(pred_cols_all) < 2L) stop("Not enough predictors after cleaning.")
  
  n_per_year <- model_df %>% count(Year, name = "n") %>% arrange(Year)
  
  # ---------- helpers ----------
  .tune_once <- function(df_fit, seed) {
    set.seed(seed)
    pred_cols <- setdiff(names(df_fit), response_var)
    p <- length(pred_cols)
    y_obs <- df_fit[[response_var]]
    
    grid_trees <- c(100, 200, 300, 500)
    grid_nodes <- c(2, 4, 6, 8)
    grid_mtry  <- unique(pmin(seq(1, max(1, floor(p / 2)), by = 1), p))
    
    results <- vector("list", length(grid_trees) * length(grid_nodes) * length(grid_mtry))
    idx <- 1
    for (nt in grid_trees) {
      for (ns in grid_nodes) {
        for (mt in grid_mtry) {
          set.seed(seed + nt*1e6 + ns*1e3 + mt)
          fit <- randomForest::randomForest(
            as.formula(paste(response_var, "~ .")),
            data = df_fit, ntree = nt, mtry = mt, nodesize = ns, importance = TRUE
          )
          preds <- predict(fit, df_fit)
          rsq <- 1 - sum((y_obs - preds)^2) / sum((y_obs - mean(y_obs))^2)
          mse <- mean((y_obs - preds)^2)
          results[[idx]] <- data.frame(Trees = nt, NodeSize = ns, mtry = mt, R2 = rsq, MSE = mse)
          idx <- idx + 1
        }
      }
    }
    best <- dplyr::bind_rows(results) %>%
      arrange(desc(R2), MSE) %>% slice(1)
    tibble(ntree = best$Trees, mtry = best$mtry, nodesize = best$NodeSize)
  }
  
  .imp_df <- function(fit) {
    imp <- as.data.frame(randomForest::importance(fit))
    imp$Variable <- rownames(imp)
    imp <- imp[!is.na(imp$Variable) & imp$Variable != "" & imp$Variable != "NA", , drop = FALSE]
    as_tibble(imp) %>% select(Variable, `%IncMSE`)
  }
  
  all_imp_long <- list()
  
  # ---------- 1) Per-year jackknife ----------
  for (yy in sort(unique(model_df$Year))) {
    df_y <- model_df %>% filter(Year == yy) %>% select(-Year)
    n_y  <- nrow(df_y); if (n_y == 0) next
    
    best <- .tune_once(df_y, seed = seed_base + yy)
    
    if (n_y < 5) {
      set.seed(seed_base + yy*1000)
      fit <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data = df_y, ntree = best$ntree, mtry = best$mtry, nodesize = best$nodesize, importance = TRUE
      )
      all_imp_long[[as.character(yy)]] <- .imp_df(fit) %>% mutate(Year = yy, jack_idx = 1L)
      next
    }
    
    imp_stack <- vector("list", n_y)
    for (i in seq_len(n_y)) {
      set.seed(seed_base + yy*1000 + i)
      fit_i <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data = df_y[-i, , drop = FALSE],
        ntree = best$ntree, mtry = best$mtry, nodesize = best$nodesize, importance = TRUE
      )
      imp_stack[[i]] <- .imp_df(fit_i) %>% mutate(Year = yy, jack_idx = i)
    }
    all_imp_long[[as.character(yy)]] <- bind_rows(imp_stack)
  }
  
  # ---------- 1b) Pooled "All years" jackknife ----------
  df_all <- model_df %>% select(-Year)
  n_all  <- nrow(df_all)
  stopifnot(n_all >= 2L)
  
  best_all <- .tune_once(df_all, seed = seed_base + 9999)
  
  if (n_all >= 5) {
    imp_stack_all <- vector("list", n_all)
    for (i in seq_len(n_all)) {
      set.seed(seed_base + 9999*1000 + i)
      fit_i <- randomForest::randomForest(
        as.formula(paste(response_var, "~ .")),
        data = df_all[-i, , drop = FALSE],
        ntree = best_all$ntree, mtry = best_all$mtry, nodesize = best_all$nodesize, importance = TRUE
      )
      imp_stack_all[[i]] <- .imp_df(fit_i) %>% mutate(Year = 9999L, jack_idx = i)
    }
    all_imp_long[["ALL"]] <- bind_rows(imp_stack_all)
  } else {
    set.seed(seed_base + 9999*1000)
    fit_all <- randomForest::randomForest(
      as.formula(paste(response_var, "~ .")),
      data = df_all, ntree = best_all$ntree, mtry = best_all$mtry, nodesize = best_all$nodesize, importance = TRUE
    )
    all_imp_long[["ALL"]] <- .imp_df(fit_all) %>% mutate(Year = 9999L, jack_idx = 1L)
  }
  
  if (length(all_imp_long) == 0) stop("No years had enough observations after cleaning.")
  
  imp_long <- bind_rows(all_imp_long) %>% filter(!is.na(`%IncMSE`))
  
  # ---------- 2) Summarize mean ± sd per (Year, Variable) ----------
  imp_summary <- imp_long %>%
    group_by(Year, Variable) %>%
    summarise(
      mean_incMSE = mean(`%IncMSE`, na.rm = TRUE),
      sd_incMSE   = stats::sd(`%IncMSE`,   na.rm = TRUE),
      .groups = "drop"
    )
  
  if (is.null(var_order)) {
    var_order <- imp_summary %>%
      group_by(Variable) %>%
      summarise(overall_mean = mean(mean_incMSE, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(overall_mean)) %>%
      pull(Variable)
  }
  
  vars_present <- unique(imp_summary$Variable)
  missing <- setdiff(vars_present, var_order)
  if (length(missing)) {
    message("Adding to var_order (not previously listed): ",
            paste(missing, collapse = ", "))
    var_order <- c(var_order, missing)
  }
  
  imp_summary <- imp_summary %>%
    mutate(Variable = forcats::fct_relevel(Variable, rev(var_order)))
  
  # Year labels with n + pooled "All"
  n_all_row <- tibble(Year = 9999L, n = n_all)
  n_per_year2 <- bind_rows(n_per_year, n_all_row)
  
  year_lab <- n_per_year2 %>%
    mutate(
      Year_label = if_else(
        Year == 9999L, paste0("All\n(n=", n, ")"),
        paste0(Year, "\n(n=", n, ")")
      )
    ) %>%
    select(Year, Year_label)
  
  year_levels <- c(
    year_lab$Year_label[year_lab$Year == 9999L],
    year_lab$Year_label[year_lab$Year != 9999L][order(year_lab$Year[year_lab$Year != 9999L])]
  )
  
  plot_df <- imp_summary %>%
    left_join(year_lab, by = "Year") %>%
    mutate(Year_label = factor(Year_label, levels = year_levels))
  
  # Safe labeller: map internal names -> pretty labels if provided
  y_lab_fun <- function(v) {
    if (is.null(variable_labels)) return(v)
    out <- unname(variable_labels[as.character(v)])
    out[is.na(out)] <- v[is.na(out)]
    out
  }
  
  # ---------- 3) Heatmap ----------
  heat <- ggplot(plot_df, aes(x = Year_label, y = Variable, fill = mean_incMSE)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(
      aes(label = sprintf("%.1f±%.1f", mean_incMSE, sd_incMSE),
          color = mean_incMSE < 2),
      size = 2.8
    ) +
    viridis::scale_fill_viridis(
      name = "Mean %IncMSE",
      option = "H",
      limits = c(0, 10),          # clamp range to 0–10
      oob = scales::squish,       # values >10 stay at top color
      breaks = c(0, 10),
      labels = c("0", "≥10")
    ) +
    scale_color_manual(
      values = c("TRUE" = "white", "FALSE" = "black"),
      guide = "none"
    ) +
    scale_y_discrete(labels = y_lab_fun) +
    labs(
      title = paste0("Jackknife %IncMSE (", year_min, "–", year_max, " + All)"),
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
  
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = heat, width = 12, height = 8, dpi = 400, bg = "white")
  }
  
  invisible(list(plot = heat, summary = plot_df))
}
