#Maria Popescu

# Alternate phytoplankton heatmap for presentations
# Same as the heatmap in 03_Phytos_heatmaps_profiles.R, but:
#  - y-axis (depth) only reaches 10 m for all panels
#  - water level component removed (no gray-below-WL ribbon, no WL line)
#
# Inputs:
# - DCM_metrics_filtered (created in 02_Phytos_dataframe.R)
#
# Outputs:
# - Figs/Phytos_viz/alternate_heatmap.png
#
# Dependencies:
# - Run after 02_Phytos_dataframe.R in the same session.

required_objects <- c("DCM_metrics_filtered")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Missing required objects for 02_alternate_phyto_heatmap.R: ",
       paste(missing_objects, collapse = ", "),
       ". Run 02_Phytos_dataframe.R first.")
}

# packages loaded in 01_DataDownload.R

phytos_heatmaps <- DCM_metrics_filtered %>%
  filter(!is.na(TotalConc_ugL))

# shared color scale anchor
global_max_val <- DCM_metrics_filtered %>%
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
    show_legend = TRUE
) {
  doy_min <- 133
  doy_max <- 286

  base <- fp_data %>%
    mutate(Date = as.Date(Date), DOY = lubridate::yday(Date)) %>%
    filter(lubridate::year(Date) == year,
           Site == site,
           DOY >= doy_min,
           DOY <= doy_max)

  # y-axis capped at 10 m
  ymax_plot <- 10

  have_z <- nrow(base) && any(is.finite(base[[z]]))
  if (have_z) {
    base_clean <- base %>%
      filter(is.finite(DOY), is.finite(Depth_m), is.finite(.data[[z]]))
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
      interp <- tibble(x = numeric(), y = numeric(), z = numeric())
    }
  } else {
    interp <- tibble(x = numeric(), y = numeric(), z = numeric())
  }

  fig_title <- as.character(year)

  p <- ggplot() +
    geom_raster(data = interp, aes(x = x, y = y, fill = z)) +
    scale_y_reverse(
      expand = c(0, 0),
      limits = c(ymax_plot, 0)
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(doy_min, doy_max),
      breaks = seq(doy_min, doy_max, by = 30)
    ) +
    scale_fill_gradientn(
      colours = c(colorRamps::blue2green2red(60), rep("black", 12)),
      values = if (have_z) scales::rescale(c(0, 40, 80, 110, global_max)) else NULL,
      limits = if (have_z) c(0, global_max) else NULL,
      oob = scales::squish,
      na.value = "grey70",
      guide = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0,
        barwidth = unit(7, "cm"),
        barheight = unit(0.5, "cm")
      )
    ) +
    labs(x = "Day of Year", y = "DCM Depth (m)", title = fig_title, fill = unitz) +
    coord_cartesian(
      xlim = c(doy_min, doy_max),
      ylim = c(ymax_plot, 0),
      expand = FALSE,
      clip = "on"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key.size = unit(0.7, "cm"),
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      panel.background = element_rect(fill = "grey90", colour = NA),
      plot.background  = element_rect(fill = "white",  colour = NA)
    )

  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!have_z) p <- p + annotate("text", x = doy_min + 5, y = 0.5 * ymax_plot,
                                 label = "(no data)", hjust = 0, size = 5)

  p
}

plots <- list(
  flora_heatmap(phytos_heatmaps, 2015, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2016, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2017, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2018, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2019, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2020, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2021, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2022, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2023, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE),
  flora_heatmap(phytos_heatmaps, 2024, 50, "TotalConc_ugL", "ug/L", global_max_val, TRUE)
)

final_with_legend <-
  (plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] +
     plots[[6]] + plots[[7]] + plots[[8]] + plots[[9]] + plots[[10]]) +
  plot_layout(ncol = 5, guides = "collect", axis_titles = "collect") +
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "right",
    legend.box.just = "right",
    legend.margin = ggplot2::margin(t = 4, r = 10, b = 4, l = 4)
  ))

ggsave(
  filename = "Figs/Phytos_viz/alternate_heatmap.png",
  plot = final_with_legend,
  width = 13.33, height = 5.96, units = "in",
  dpi = 900, bg = "white",
  device = ragg::agg_png
)
