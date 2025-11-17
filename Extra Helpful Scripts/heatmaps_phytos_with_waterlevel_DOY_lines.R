phytos <- read.csv("CSVs/phytos.csv")


# 1) Make water_level unique per Week-Year and keep only WL column
wl_weekly <- water_level %>%
  group_by(Year, Week) %>%
  summarise(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE), .groups = "drop")

# 2) Join and mask below-WL values; keep numeric type with NA_real_
phytos <- phytos %>%
  left_join(wl_weekly, by = c("Week", "Year")) %>%
  mutate(
    TotalConc_ugL = ifelse(
      Depth_m > WaterLevel_m, NA_real_, TotalConc_ugL
    )
  ) %>%
  filter(!is.na(TotalConc_ugL))


pacman::p_load(
  tidyverse, lubridate, akima, reshape2,
  gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
  reader, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes,
  patchwork  # <- use patchwork to collect a single legend
)

#-----Function-----

global_max <- max(phytos$TotalConc_ugL, na.rm = TRUE)

flora_heatmap <- function(
    fp_data,
    year,
    site,
    z,
    unitz,
    global_max,
    show_legend = TRUE,
    wl_col = "WaterLevel_m"
) {
  # build a year/site subset even if z is missing
  base <- fp_data %>%
    mutate(Date = as.Date(Date), DOY = lubridate::yday(Date)) %>%
    dplyr::filter(lubridate::year(Date) == year, Site == site)
  
  # WL series (computed from base, not from z-filtered data)
  wl_daily <- {
    if (nrow(base)) {
      tmp <- base %>%
        dplyr::select(DOY, !!rlang::sym(wl_col)) %>%
        dplyr::rename(WL = !!rlang::sym(wl_col)) %>%
        dplyr::filter(is.finite(DOY), is.finite(WL)) %>%
        dplyr::group_by(DOY) %>% dplyr::summarise(WL = mean(WL), .groups = "drop")
      if (nrow(tmp)) {
        a <- approx(x = tmp$DOY, y = tmp$WL, xout = 1:366, rule = 2)
        tibble::tibble(DOY = a$x, WL = a$y)
      } else tibble::tibble(DOY = numeric(), WL = numeric())
    } else tibble::tibble(DOY = numeric(), WL = numeric())
  }
  
  # pick depth extent = max(measured depth, water level), rounded up to 0.5 m
  depth_data_max <- if (nrow(base)) max(base$Depth_m, na.rm = TRUE) else NA_real_
  depth_wl_max   <- if (nrow(wl_daily)) max(wl_daily$WL, na.rm = TRUE) else NA_real_
  ymax_needed    <- max(depth_data_max, depth_wl_max, na.rm = TRUE)
  if (!is.finite(ymax_needed)) ymax_needed <- 10
  ymax_plot <- ceiling(ymax_needed * 2) / 2  # nice round up to 0.5 m
  
  # build interpolation only if we have z values
  have_z <- nrow(base) && any(is.finite(base[[z]]))
  if (have_z) {
    io <- akima::interp(
      x = base$DOY, y = base$Depth_m, z = base[[z]],
      duplicate = "mean", nx = 200, ny = 200
    )
    interp <- expand.grid(x = io$x, y = io$y)
    interp$z <- as.vector(io$z)
  } else {
    interp <- tibble::tibble(x = numeric(), y = numeric(), z = numeric())
  }
  
  fig_title <- paste0("BVR ", year)
  
  p <- ggplot() +
    # heatmap (if any)
    geom_raster(data = interp, aes(x = x, y = y, fill = z)) +
    # gray below the water level for the full y-range
    geom_ribbon(
      data = wl_daily, inherit.aes = FALSE,
      aes(x = DOY, ymin = WL, ymax = ymax_plot),
      fill = "grey70"
    ) +
    # WL line
    geom_line(
      data = wl_daily, inherit.aes = FALSE,
      aes(x = DOY, y = WL), color = "black", linewidth = 0.8
    ) +
    # Vertical lines as explicit segments drawn on top
    geom_segment(
      data = data.frame(x = c(133, 285)),
      inherit.aes = FALSE,
      aes(x = x, xend = x, y = 0, yend = ymax_plot),
      linetype = "dashed",
      color = "black",
      linewidth = 0.8,            # a touch thicker helps in PNGs
      lineend = "butt"
    ) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 366, by = 30),
      labels = function(x) format(as.Date(x - 1, origin = paste0(year, "-01-01")), "%b")
    ) +
    scale_fill_gradientn(
      colours = c(colorRamps::blue2green2red(60), rep("black", 12)),
      values  = if (have_z)
        scales::rescale(c(min(interp$z, na.rm = TRUE), 40, 80, 110, global_max))
      else NULL,
      limits  = if (have_z) c(min(interp$z, na.rm = TRUE), global_max) else NULL,
      oob     = scales::squish,
      na.value = "grey70"
    ) +
    labs(x = "Day of year", y = "Depth (m)", title = fig_title, fill = unitz) +
    coord_cartesian(ylim = c(ymax_plot, 0), expand = FALSE, clip = "on") +
    theme_bw() +
    guides(fill = guide_colorbar(
      barwidth = .5, barheight = 15,
      ticks.colour = "black", frame.colour = "black",
      breaks = c(0, 20, 40, 60, 80, 100, 150, 200, 500, 1000),
      labels = c("0","20","40","60","80","100","150","200","500","1000")
    )) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.5, "cm"),
      panel.background = element_rect(fill = "grey90", colour = NA), # gray inside
      plot.background  = element_rect(fill = "white",  colour = NA)  # white outside
    )
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!have_z) p <- p + annotate("text", x = 20, y = 0.5*ymax_plot, label = "(no data)", hjust = 0)
  
  p
}




#### build all plots (legend ON so patchwork can collect one) ####
# Build all plots but only the last one keeps its legend
plots <- list(
  flora_heatmap(phytos, 2015, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2016, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2017, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2018, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2019, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2020, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2021, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2022, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2023, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2024, 50, "TotalConc_ugL", "ug/L", global_max_val, TRUE)   # only this keeps legend
)

# Combine with patchwork, collect only that one legend
library(patchwork)

final_with_legend <-
  (plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] +
     plots[[6]] + plots[[7]] + plots[[8]] + plots[[9]] + plots[[10]]) +
  plot_layout(ncol = 5, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "right"))

print(final_with_legend)

ggsave(
  filename = "Figs/Phytos_viz/DOYBVR_totalphytos_heatmapsFINAL_waterlevel_adjusted.png",
  plot = final_with_legend,
  width = 20, height = 7, dpi = 300, bg = "white",
  device = ragg::agg_png   # or device = "cairo-png"
)

#---only framing the timeframe we are looking at, water level y-adjusted-----####

global_max_val <- phytos %>%
  filter(Site == 50, Year >= 2015, Year <= 2024) %>%
  summarise(max_val = max(TotalConc_ugL, na.rm = TRUE)) %>%
  pull(max_val)

flora_heatmap <- function(
    fp_data,
    year,
    site,
    z,
    unitz,
    global_max,
    show_legend = TRUE,
    wl_col = "WaterLevel_m"
) {
  # DOY window you care about
  doy_min <- 133
  doy_max <- 285
  
  # build a year/site subset even if z is missing
  base <- fp_data %>%
    mutate(Date = as.Date(Date), DOY = lubridate::yday(Date)) %>%
    dplyr::filter(
      lubridate::year(Date) == year,
      Site == site,
      DOY >= doy_min,
      DOY <= doy_max
    )
  
  # WL series (computed from base, not from z-filtered data)
  wl_daily <- {
    if (nrow(base)) {
      tmp <- base %>%
        dplyr::select(DOY, !!rlang::sym(wl_col)) %>%
        dplyr::rename(WL = !!rlang::sym(wl_col)) %>%
        dplyr::filter(is.finite(DOY), is.finite(WL)) %>%
        dplyr::group_by(DOY) %>%
        dplyr::summarise(WL = mean(WL), .groups = "drop")
      if (nrow(tmp)) {
        # interpolate only over the DOY window of interest
        a <- approx(x = tmp$DOY, y = tmp$WL,
                    xout = seq(doy_min, doy_max),
                    rule = 2)
        tibble::tibble(DOY = a$x, WL = a$y)
      } else tibble::tibble(DOY = numeric(), WL = numeric())
    } else tibble::tibble(DOY = numeric(), WL = numeric())
  }
  
  # Fixed depth extent for all plots
  ymax_plot <- 12.5
  
  # build interpolation only if we have z values
  have_z <- nrow(base) && any(is.finite(base[[z]]))
  if (have_z) {
    # clean data for interp, just in case
    base_clean <- base %>%
      dplyr::filter(
        is.finite(DOY),
        is.finite(Depth_m),
        is.finite(.data[[z]])
      )
    
    have_z <- nrow(base_clean) && any(is.finite(base_clean[[z]]))
    
    if (have_z) {
      io <- akima::interp(
        x = base_clean$DOY,
        y = base_clean$Depth_m,
        z = base_clean[[z]],
        duplicate = "mean", nx = 200, ny = 200
      )
      interp <- expand.grid(x = io$x, y = io$y)
      interp$z <- as.vector(io$z)
    } else {
      interp <- tibble::tibble(x = numeric(), y = numeric(), z = numeric())
    }
  } else {
    interp <- tibble::tibble(x = numeric(), y = numeric(), z = numeric())
  }
  
  fig_title <- paste0("BVR ", year)
  
  p <- ggplot() +
    # heatmap (if any)
    geom_raster(data = interp, aes(x = x, y = y, fill = z)) +
    # gray below the water level for the full y-range
    geom_ribbon(
      data = wl_daily, inherit.aes = FALSE,
      aes(x = DOY, ymin = WL, ymax = ymax_plot),
      fill = "grey70"
    ) +
    # WL line
    geom_line(
      data = wl_daily, inherit.aes = FALSE,
      aes(x = DOY, y = WL), color = "black", linewidth = 0.8
    ) +

    scale_y_reverse(
      expand = c(0, 0),
      limits = c(ymax_plot, 0)  # fixed 12.5 → 0 for all panels
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(doy_min, doy_max),                 # only show 133–285
      breaks = seq(doy_min, doy_max, by = 30),
      labels = function(x) format(
        as.Date(x - 1, origin = paste0(year, "-01-01")),
        "%b"
      )
    ) +
    scale_fill_gradientn(
      colours = c(colorRamps::blue2green2red(60), rep("black", 12)),
      values  = if (have_z)
        scales::rescale(c(0, 40, 80, 110, global_max)) else NULL,
      limits  = if (have_z) c(0, global_max) else NULL,
      oob     = scales::squish,
      na.value = "grey70"
    ) +
    labs(x = "Day of year", y = "Depth (m)", title = fig_title, fill = unitz) +
    coord_cartesian(
      xlim = c(doy_min, doy_max),
      ylim = c(ymax_plot, 0),
      expand = FALSE,
      clip = "on"
    ) +
    theme_bw() +
    guides(fill = guide_colorbar(
      barwidth = .5, barheight = 15,
      ticks.colour = "black", frame.colour = "black",
      breaks = c(0, 20, 40, 60, 80, 100, 150, 200, 500, 1000),
      labels = c("0","20","40","60","80","100","150","200","500","1000")
    )) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.5, "cm"),
      panel.background = element_rect(fill = "grey90", colour = NA),
      plot.background  = element_rect(fill = "white",  colour = NA)
    )
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!have_z) p <- p + annotate("text", x = doy_min + 5, y = 0.5 * ymax_plot,
                                 label = "(no data)", hjust = 0)
  
  p
}


#### build all plots (legend ON so patchwork can collect one) ####
# Build all plots but only the last one keeps its legend
plots <- list(
  flora_heatmap(phytos, 2015, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2016, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2017, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2018, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2019, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2020, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2021, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2022, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2023, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos, 2024, 50, "TotalConc_ugL", "ug/L", global_max_val, TRUE)   # only this keeps legend
)

# Combine with patchwork, collect only that one legend
library(patchwork)

final_with_legend <-
  (plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] +
     plots[[6]] + plots[[7]] + plots[[8]] + plots[[9]] + plots[[10]]) +
  plot_layout(ncol = 5, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "right"))

print(final_with_legend)

ggsave(
  filename = "Figs/Phytos_viz/nov17_plot.png",
  plot = final_with_legend,
  width = 20, height = 7, dpi = 300, bg = "white",
  device = ragg::agg_png   # or device = "cairo-png"
)

