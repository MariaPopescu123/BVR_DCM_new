#Summarizes variables by Date: max/min values, depths of extremes, and value at DCM.
#Example: date_sum_variables(metals_df, c("SFe_mgL", "TFe_mgL"))
#
#Input must contain Date, Depth_m, DCM_depth, and the variables. One row per
#(Date, Depth_m); typically the daily-interpolated profile output from
#interpolate_variable() filtered/joined to phyto Dates.

date_sum_variables <- function(df, variables) {

  max_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else max(x)
  }
  min_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else min(x)
  }

  df_filtered <- df |>
    select(Date, Depth_m, DCM_depth, all_of(variables)) |>
    group_by(Date)

  .value_at_dcm <- function(.df, var_name) {
    var_sym <- sym(var_name)

    dcm_vals <- .df$DCM_depth[!is.na(.df$DCM_depth)]
    if (length(dcm_vals) == 0) return(NA_real_)
    dcm <- dcm_vals[1]

    dsub <- .df |>
      select(Depth_m, !!var_sym) |>
      filter(!is.na(Depth_m), !is.na(!!var_sym)) |>
      arrange(Depth_m)

    if (nrow(dsub) == 0) return(NA_real_)

    max_depth <- max(dsub$Depth_m)

    if (dcm >= max_depth) {
      return(dsub |> filter(Depth_m == max_depth) |> slice(1) |> pull(!!var_sym))
    }

    idx <- which.min(abs(dsub$Depth_m - dcm))
    dsub |> slice(idx) |> pull(!!var_sym)
  }

  .depth_at_extreme <- function(.df, var_name, which = c("max", "min")) {
    which <- match.arg(which)
    var_sym <- sym(var_name)

    tmp <- .df |>
      select(Depth_m, !!var_sym) |>
      filter(!is.na(Depth_m), !is.na(!!var_sym))

    if (nrow(tmp) == 0) return(NA_real_)

    tmp <- if (which == "max") {
      tmp |> arrange(desc(!!var_sym), Depth_m)
    } else {
      tmp |> arrange(!!var_sym, Depth_m)
    }

    tmp |> slice(1) |> pull(Depth_m)
  }

  summary_list <- lapply(variables, function(var) {
    var_sym <- sym(var)

    df_filtered |>
      summarise(
        !!paste0("depth_", var, "_max") := .depth_at_extreme(pick(everything()), var, "max"),
        !!paste0("depth_", var, "_min") := .depth_at_extreme(pick(everything()), var, "min"),
        !!paste0(var, "_max_val")       := max_na(!!var_sym),
        !!paste0(var, "_min_val")       := min_na(!!var_sym),
        !!paste0(var, "_at_DCM")        := .value_at_dcm(pick(everything()), var),
        .groups = "drop"
      )
  })

  reduce(summary_list, left_join, by = "Date")
}
