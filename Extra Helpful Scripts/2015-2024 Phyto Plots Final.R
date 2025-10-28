phytos <- read.csv("CSVs/phytos.csv")

pacman::p_load(
  tidyverse, lubridate, akima, reshape2,
  gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
  reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes
)

#### prep chl max line (unchanged) ####
chlorophyll_data <- phytos %>%
  mutate(
    Date = as.Date(Date),
    DayOfYear = yday(Date)
  ) %>%
  select(Date, Depth_m, TotalConc_ugL, DayOfYear) %>%  # change TotalConc_ugL to chl var if needed
  group_by(Date) %>%
  slice(which.max(TotalConc_ugL)) %>%
  ungroup() %>%
  mutate(
    Reservoir = "BVR",
    DateTime = Date
  ) %>%
  filter(
    DayOfYear > 133, DayOfYear < 285,
    TotalConc_ugL > 20
  ) %>%
  filter(!(month(Date) == 8 & year(Date) == 2017 & TotalConc_ugL < 35))


#### heatmap function with global legend support ####
flora_heatmap <- function(
    fp_data,
    year,
    site,
    z,
    unitz,
    global_max,
    show_legend = FALSE
) {
  
  # 1. make sure Date is Date, derive DOY, filter to year/site
  df <- fp_data %>%
    mutate(
      Date = as.Date(Date),
      DOY = yday(Date)
    ) %>%
    filter(year(Date) == year,
           Site == site)  # assumes fp_data has a column "Site"
  
  # safety check: if no data for that year/site, return blank plot
  if (nrow(df) == 0) {
    empty_plot <- ggplot() +
      theme_void() +
      ggtitle(paste0("BVR ", year, "\n(no data)"))
    if (!show_legend) {
      empty_plot <- empty_plot + theme(legend.position = "none")
    }
    return(empty_plot)
  }
  
  # 2. interpolate onto a grid for raster plotting
  #    akima::interp wants numeric x,y,z vectors with no NAs
  interp_obj <- akima::interp(
    x = df$DOY,
    y = df$Depth_m,
    z = df[[z]],
    duplicate = "mean",
    nx = 200,
    ny = 200
  )
  
  # convert interp_obj (a list with x,y,z matrix) into long data frame
  interp <- expand.grid(
    x = interp_obj$x,
    y = interp_obj$y
  )
  interp$z <- as.vector(interp_obj$z)
  
  # 3. figure title
  fig_title <- paste0("BVR ", year)
  
  # 4. build plot
  p1 <- ggplot(interp, aes(x = x, y = y)) +
    geom_raster(aes(fill = z)) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 366, by = 30),
      labels = function(x) format(as.Date(x - 1,
                                          origin = paste0(year, "-01-01")),
                                  "%b")
    ) +
    scale_fill_gradientn(
      colours = c(
        blue2green2red(60),
        rep("black", 12)
      ),
      values = scales::rescale(
        c(
          min(interp$z, na.rm = TRUE),
          40, 80, 110,
          global_max
        )
      ),
      limits = c(min(interp$z, na.rm = TRUE), global_max),
      oob = scales::squish
    ) +
    labs(
      x = "Day of year",
      y = "Depth (m)",
      title = fig_title,
      fill = unitz
    ) +
    theme_bw() +
    guides(
      fill = guide_colorbar(
        barwidth = .5,
        barheight = 15,
        ticks.colour = "black",
        frame.colour = "black",
        breaks = c(0, 20, 40, 60, 80, 100, 150, 200, 500, 1000),
        labels = c("0", "20", "40", "60", "80", "100", "150", "200", "500", "1000")
      )
    ) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.5, "cm")
    )
  
  # 5. hide / show legend
  if (!show_legend) {
    p1 <- p1 + theme(legend.position = "none")
  } else {
    p1 <- p1 + theme(legend.position = "right")
  }
  
  return(p1)
}


#### build all plots ####
library(cowplot)

# shared colorbar max across all panels
global_max_val <- max(phytos$TotalConc_ugL, na.rm = TRUE)

b2014 <- flora_heatmap(
  fp_data = phytos, year = 2014, site = 50,
  z = "TotalConc_ugL", unitz = "ug/L",
  global_max = global_max_val,
  show_legend = FALSE
)

b2015 <- flora_heatmap(phytos, 2015, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2016 <- flora_heatmap(phytos, 2016, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2017 <- flora_heatmap(phytos, 2017, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2018 <- flora_heatmap(phytos, 2018, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2019 <- flora_heatmap(phytos, 2019, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2020 <- flora_heatmap(phytos, 2020, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2021 <- flora_heatmap(phytos, 2021, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2022 <- flora_heatmap(phytos, 2022, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2023 <- flora_heatmap(phytos, 2023, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)
b2024 <- flora_heatmap(phytos, 2024, 50, "TotalConc_ugL", "ug/L", global_max_val, FALSE)

# one plot WITH legend so we can extract it
b_legend_src <- flora_heatmap(
  fp_data = phytos, year = 2024, site = 50,
  z = "TotalConc_ugL", unitz = "ug/L",
  global_max = global_max_val,
  show_legend = TRUE
)

shared_legend <- get_legend(b_legend_src)

# panel grid of all years you want to show
panel_grid <- plot_grid(
  b2015, b2016, b2017, b2018, b2019,
  b2020, b2021, b2022, b2023, b2024,
  ncol = 5,
  align = "hv"
)

# stick the legend to the right
final_with_legend <- plot_grid(
  panel_grid,
  shared_legend,
  ncol = 2,
  rel_widths = c(1, 0.07)
)

print(final_with_legend)

ggsave(
  "Figs/Phytos_viz/BVR_totalphytos_heatmapsFINAL.png",
  final_with_legend,
  width = 20,
  height = 7,
  dpi = 300
)

