#Interpolates variables across depth and time on a daily grid.
#Data should be QC'd and flags removed before calling.
#
#Output: one row per (Date, Depth_m) for every calendar day in
#2014-01-01 .. 2024-12-31, with each variable interpolated first along
#depth (within the cast on that Date) then along time (across days,
#per Year and Depth_m, x = DOY).

interpolate_variable <- function(data, variable_list) {

  start_date <- as.Date("2014-01-01")
  end_date   <- as.Date("2024-12-31")

  Depth_fake <- seq(0, 13, by = 0.1)

  expanded_dates <- expand_grid(
    Date    = seq.Date(from = start_date, to = end_date, by = "day"),
    Depth_m = Depth_fake
  ) |>
    mutate(
      Year = year(Date),
      Week = week(Date),
      DOY  = yday(Date)
    )

  data <- data %>%
    filter(Reservoir == "BVR", Site == 50) %>%
    { if (!"Date" %in% names(.)) mutate(., Date = as_date(DateTime)) else . } %>%
    mutate(
      Week = week(Date),
      Year = year(Date),
      DOY  = yday(Date)
    )

  interpolated_results <- list()

  for (var in variable_list) {
    #collapse duplicate (Date, Depth_m) measurements within the source data
    var_daily <- data |>
      select(Depth_m, Year, Week, DOY, Date, all_of(var)) |>
      group_by(Year, DOY, Week, Date, Depth_m) |>
      summarise(!!sym(var) := mean(.data[[var]], na.rm = TRUE), .groups = "drop")

    var_depth_rounded <- var_daily |>
      mutate(Depth_m = round(Depth_m, digits = 1)) |>
      group_by(Date, Depth_m) |>
      summarise(!!sym(var) := mean(.data[[var]], na.rm = TRUE), .groups = "drop")

    var_interpolated <- expanded_dates %>%
      left_join(var_depth_rounded, by = c("Date", "Depth_m")) %>%

      #depth-axis interpolation within each profile (per Date)
      group_by(Date) %>%
      mutate(
        first_valid_depth = ifelse(all(is.na(.data[[var]])), NA_real_,
                                   min(Depth_m[!is.na(.data[[var]])], na.rm = TRUE)),
        last_valid_depth  = ifelse(all(is.na(.data[[var]])), NA_real_,
                                   max(Depth_m[!is.na(.data[[var]])], na.rm = TRUE)),
        Value_interp_depth = ifelse(
          Depth_m >= first_valid_depth & Depth_m <= last_valid_depth,
          zoo::na.approx(.data[[var]], x = Depth_m, na.rm = FALSE),
          NA_real_
        )
      ) %>%
      ungroup() %>%

      #time-axis interpolation across days within each Year/Depth (x = DOY)
      group_by(Year, Depth_m) %>%
      mutate(
        first_valid_DOY = ifelse(all(is.na(Value_interp_depth)), NA_real_,
                                 min(DOY[!is.na(Value_interp_depth)], na.rm = TRUE)),
        last_valid_DOY  = ifelse(all(is.na(Value_interp_depth)), NA_real_,
                                 max(DOY[!is.na(Value_interp_depth)], na.rm = TRUE)),
        Value_interp_time = ifelse(
          DOY >= first_valid_DOY & DOY <= last_valid_DOY,
          zoo::na.approx(Value_interp_depth, x = DOY, na.rm = FALSE),
          NA_real_
        )
      ) %>%
      ungroup() %>%

      mutate(interp_var = coalesce(Value_interp_depth, Value_interp_time)) %>%

      select(-matches(var), -first_valid_depth, -last_valid_depth, -Value_interp_depth,
             -first_valid_DOY, -last_valid_DOY, -Value_interp_time) %>%
      rename(!!sym(var) := interp_var)

    interpolated_results[[var]] <- var_interpolated
  }

  final_result <- plyr::join_all(interpolated_results,
                                 by = c("Week", "DOY", "Depth_m", "Year", "Date"))
  return(final_result)
}
