#this script:
#1) Makes heatmaps for phytoplankton 2014-2024
#2) Makes figures displaying the profiles for the day of maximum
#phytoplankton concentration of each year

pacman::p_load(
  tidyverse, lubridate, akima, reshape2,
  gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
  reader, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes,
  patchwork  # <- use patchwork to collect a single legend
)

pacman::p_load(
  tidyverse, lubridate, akima, reshape2,
  gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
  reader, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes,
  patchwork  
)


# 1) Make water_level unique per Week-Year and keep only WL column
wl_weekly <- water_level %>%
  group_by(Year, Week) %>%
  summarise(WaterLevel_m = mean(WaterLevel_m, na.rm = TRUE), .groups = "drop")

# 2) Join and mask below-WL values; keep numeric type with NA_real_
phytos_heatmaps <- DCM_metrics_filtered %>%
  left_join(wl_weekly, by = c("Week", "Year")) %>%
  mutate(
    TotalConc_ugL = ifelse(
      Depth_m > WaterLevel_m, NA_real_, TotalConc_ugL
    )
  ) %>%
  filter(!is.na(TotalConc_ugL))


# 3) heatmap only framing the timeframe
#we are looking at displaying water level and y-adjusted####

global_max_val <- DCM_metrics_filtered %>%
  filter(Site == 50, Year >= 2015, Year <= 2024) %>%
  summarise(max_val = max(TotalConc_ugL, na.rm = TRUE)) %>%
  pull(max_val)

#heatmaps for phytoplankton concentration visualization across time and depth
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
    filter(lubridate::year(Date) == year,
           Site == site,
           DOY >= doy_min,
           DOY <= doy_max)
  
  # WL series
  wl_daily <- if (nrow(base)) {
    tmp <- base %>%
      select(DOY, !!rlang::sym(wl_col)) %>%
      rename(WL = !!rlang::sym(wl_col)) %>%
      filter(is.finite(DOY), is.finite(WL)) %>%
      group_by(DOY) %>%
      summarise(WL = mean(WL), .groups = "drop")
    
    a <- approx(x = tmp$DOY, y = tmp$WL,
                xout = seq(doy_min, doy_max), rule = 2)
    tibble(DOY = a$x, WL = a$y)
  } else tibble(DOY = numeric(), WL = numeric())
  
  # fixed depth extent
  ymax_plot <- 12.5
  
  # interpolate z if available
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
  
  fig_title <- as.character(year)  # only the year, no "BVR"
  
  p <- ggplot() +
    # heatmap
    geom_raster(data = interp, aes(x = x, y = y, fill = z)) +
    # gray below water level
    geom_ribbon(data = wl_daily, inherit.aes = FALSE,
                aes(x = DOY, ymin = WL, ymax = ymax_plot),
                fill = "grey70") +
    # water level line
    geom_line(data = wl_daily, inherit.aes = FALSE,
              aes(x = DOY, y = WL), color = "black", linewidth = 0.8) +
    
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
      na.value = "grey70"
    ) +
    labs(x = "Day of Year", y = "Depth (m)", title = fig_title, fill = unitz) +
    coord_cartesian(
      xlim = c(doy_min, doy_max),
      ylim = c(ymax_plot, 0),
      expand = FALSE,
      clip = "on"
    ) +
    theme_bw(base_size = 14) +  # all text slightly bigger
    theme(
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.key.size = unit(0.6, "cm"),
      panel.background = element_rect(fill = "grey90", colour = NA),
      plot.background  = element_rect(fill = "white",  colour = NA)
    )
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!have_z) p <- p + annotate("text", x = doy_min + 5, y = 0.5 * ymax_plot,
                                 label = "(no data)", hjust = 0, size = 5)
  
  p
}

#### build all plots (legend ON so patchwork can collect one) ####
# Build all plots but only the last one keeps its legend
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
  flora_heatmap(phytos_heatmaps, 2024, 50, "TotalConc_ugL", "ug/L", global_max_val, TRUE)   # only this keeps legend
)

# Combine with patchwork, collect only that one legend

final_with_legend <-
  (plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] +
     plots[[6]] + plots[[7]] + plots[[8]] + plots[[9]] + plots[[10]]) +
  plot_layout(ncol = 5, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "right"))

print(final_with_legend)

ggsave(
  filename = "Figs/Phytos_viz/final_phytos_heatmap_plot.png",
  plot = final_with_legend,
  width = 20, height = 7, dpi = 300, bg = "white",
  device = ragg::agg_png   
)




#Profile Casts####
#to show the profiles for the day where there was the maximum phytoplankton concentration
#each year. Produces Figure XXX
#---------------------------------------------
# Clean FacetID: remove "BVR" from facet labels
# Filter phytos for the years of interest
phytos <- phytos %>%
  filter(Year > 2014, Year < 2025)

# Casts table
casts <- phytos %>%
  mutate(Date = date(DateTime)) %>%
  select(Reservoir, Date, CastID, Site) %>%
  distinct()

# Max cast(s) per year
max_phytos_annual <- phytos %>%
  group_by(Year) %>%
  filter(TotalConc_ugL == max(TotalConc_ugL, na.rm = TRUE)) %>%
  ungroup()

# Unique CastIDs for those max casts
max_cast_ids <- unique(max_phytos_annual$CastID)

# Filter casts to only max CastIDs (Site 50, Reservoir BVR)
sample_dat <- casts %>%
  filter(Reservoir == "BVR",
         Site == 50,
         CastID %in% max_cast_ids) %>%
  mutate(Date = as.Date(Date))

# Build plot_dat
plot_dat <- phytos %>%
  mutate(Date = date(DateTime)) %>%
  filter(month(Date) != 1) %>%
  group_by(Reservoir, Date, Site, CastID) %>%
  # Create FacetID here
  mutate(FacetID = paste(Reservoir, Date, sep = " ")) %>%
  ungroup() %>%
  semi_join(sample_dat, by = c("Reservoir", "Date", "CastID")) %>%
  select(
    CastID, FacetID,
    GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL,
    TotalConc_ugL, Depth_m
  ) %>%
  pivot_longer(
    cols = GreenAlgae_ugL:TotalConc_ugL,
    names_to = "var",
    values_to = "ugL"
  )

# Now remove "BVR" from facet labels
plot_dat <- plot_dat %>%
  mutate(FacetID = gsub("^BVR\\s+", "", FacetID))

# Build the plot
plot_casts <- ggplot(plot_dat, aes(x = ugL, y = Depth_m, group = var)) +
  geom_path(aes(color = var, size = var)) +
  scale_y_reverse() +
  facet_wrap(
    facets = vars(FacetID),
    nrow = 2,
    ncol = 5
  ) +
  xlab("Micrograms per liter") +
  ylab("Depth (m)") +
  scale_color_manual(
    name = "Variable",
    values = c(
      "GreenAlgae_ugL" = "green3",
      "Bluegreens_ugL" = "blue",
      "BrownAlgae_ugL" = "orange",
      "MixedAlgae_ugL" = "red",
      "TotalConc_ugL" = "black"
    )
  ) +
  scale_size_manual(
    values = c(
      "GreenAlgae_ugL" = 0.5,
      "Bluegreens_ugL" = 0.5,
      "BrownAlgae_ugL" = 0.5,
      "MixedAlgae_ugL" = 0.5,
      "TotalConc_ugL" = 0.8
    ),
    guide = "none"
  ) +
  theme_minimal(base_size = 16) +  # bigger text
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 15, face = "bold"),  # facet labels
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

plot_casts

ggsave("Figs/Phytos_viz/FP_casts_2025.png",
       plot = plot_casts,
       width = 12, height = 7, units = "in",
       dpi = 300)  # high-res
