#function for summarizing variables 

#example of use 
#Date must be edited to be just Date
#variables <- c("SFe_mgL", "TFe_mgL", "SMn_mgL")
#summary_output <- summarize_data_by_week(metalsdf, variables)

library(dplyr)
library(lubridate)
library(rlang)
library(purrr)

weekly_sum_variables <- function(df, variables) {
  
  # return NA if too many missing
  max_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else max(x)
  }
  min_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else min(x)
  }
  
  # prep data
  df_prepped <- df |>
    mutate(
      Week = lubridate::week(Date),
      Year = lubridate::year(Date)
    ) |>
    select(Date, Depth_m, Week, Year, DCM_depth, all_of(variables))
  
  # valid Week-Year combos with depth range > 4 m
  valid_weeks <- df_prepped |>
    group_by(Year, Week, Date) |>
    summarise(
      depth_range = max(Depth_m, na.rm = TRUE) - min(Depth_m, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(depth_range > 4) |>
    distinct(Year, Week)
  
  # filter + group 
  df_filtered <- df_prepped |>
    semi_join(valid_weeks, by = c("Year", "Week")) |>
    group_by(Year, Week)
  
  # ---- helper: scalar value at (or nearest to) DCM depth ----
  .value_at_dcm <- function(.df, var_name) {
    var_sym <- sym(var_name)
    
    dcm_vals <- .df$DCM_depth[!is.na(.df$DCM_depth)]
    if (length(dcm_vals) == 0) return(NA_real_)  # no DCM_depth for this week
    dcm <- dcm_vals[1]
    
    dsub <- .df |>
      select(Depth_m, !!var_sym) |>
      filter(!is.na(Depth_m), !is.na(!!var_sym)) |>
      arrange(Depth_m)
    
    if (nrow(dsub) == 0) return(NA_real_)  # var is all NA this week
    
    max_depth <- max(dsub$Depth_m)
    
    # if DCM deeper than deepest sampled: take deepest sampled value (force 1 row)
    if (dcm >= max_depth) {
      return(dsub |> filter(Depth_m == max_depth) |> slice(1) |> pull(!!var_sym))
    }
    
    # else take closest depth (force scalar)
    idx <- which.min(abs(dsub$Depth_m - dcm))
    dsub |> slice(idx) |> pull(!!var_sym)
  }
  
  # helper: depth at max/min, return NA if var all NA
  .depth_at_extreme <- function(.df, var_name, which = c("max", "min")) {
    which <- match.arg(which)
    var_sym <- sym(var_name)
    
    tmp <- .df |>
      select(Depth_m, !!var_sym) |>
      filter(!is.na(Depth_m), !is.na(!!var_sym))
    
    if (nrow(tmp) == 0) return(NA_real_)  # var all NA this week
    
    tmp <- if (which == "max") {
      tmp |> arrange(desc(!!var_sym), Depth_m)
    } else {
      tmp |> arrange(!!var_sym, Depth_m)
    }
    
    tmp |> slice(1) |> pull(Depth_m)      # always 1 value
  }
  
  # Step 3: summarise per variable
  summary_list <- lapply(variables, function(var) {
    var_sym <- sym(var)
    
    df_filtered |>
      summarise(
        !!paste0("depth_", var, "_max") := .depth_at_extreme(cur_data(), var, "max"),
        !!paste0("depth_", var, "_min") := .depth_at_extreme(cur_data(), var, "min"),
        !!paste0(var, "_max_val")       := max_na(!!var_sym),
        !!paste0(var, "_min_val")       := min_na(!!var_sym),
        !!paste0(var, "_at_DCM")        := .value_at_dcm(cur_data(), var),
        .groups = "drop"
      )
  })
  
  # combine
  combined_summary <- reduce(summary_list, left_join, by = c("Year", "Week"))
  
  # add earliest date for each Week-Year
  week_dates <- df_filtered |>
    summarise(Date = min(Date), .groups = "drop")
  
  combined_summary |>
    left_join(week_dates, by = c("Year", "Week")) |>
    select(Date, everything())
}
