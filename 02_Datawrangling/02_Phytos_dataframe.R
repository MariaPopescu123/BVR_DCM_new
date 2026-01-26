#prepping phyto dataframe


#adding columns with total_conc max and the depth at which it occurs
phytos <- phytos_df %>% 
  filter(Reservoir == "BVR", Site == 50)%>%
  mutate(Date  = as_date(DateTime)) |> 
  filter((hour(DateTime) >= 8), (hour(DateTime) <= 18))|>
  filter(!(CastID == 592))|> #filter out weird drop in 2017
  filter(!(CastID == 395))|> #weird drop in 2016
  filter(!CastID %in% c(467, 814, 856, 920, 1149, 788)) |> #make sure to remove these for heatmaps too
  #filter(!CastID == 793)|> #this was here because in the version that is currently published (9/30/2025) this cast is blank, but in the version that is in staging as of 9/30/2025, this cast is fixed 
  #filter(Flag_TotalConc_ugL != 2,Flag_TotalConc_ugL != 3)|> #2 is instrument malfunction and #3 is low transmission value
  mutate(Week = week(Date))|>
  mutate(Year = year(Date))|>
  mutate(DOY = yday(Date))|>
  filter(year(Date) >2014)

write.csv(phytos, "CSVs/phytos.csv", row.names = FALSE)
phytos <- read.csv("CSVs/phytos.csv")

####flora instrument data availability####
# 1. Build plotting data
plot_dat <- phytos %>%
  filter(!is.na(TotalConc_ugL)) %>%
  mutate(
    Year = year(Date),
    DayOfYear = yday(Date)
  ) %>%
  select(Date, Year, Week, DayOfYear, TotalConc_ugL, Depth_m)

# 2. For each Year, get the row with the max concentration
max_totals_per_year <- plot_dat %>%
  group_by(Year) %>%
  slice(which.max(TotalConc_ugL)) %>%
  ungroup() %>%
  mutate(
    label_text = paste0(
      "Max: ",
      round(TotalConc_ugL, 1), " µg/L at ",
      Depth_m, " m"
    ),
    label_x = 211  # fixed DayOfYear position for text
  )

# 3. Make the plot
phytosplot <- ggplot(plot_dat, aes(x = DayOfYear, y = as.factor(Year), group = Year)) +
  geom_line() +
  geom_point(size = 1) +
  # keep the red point at the true max position for visual reference
  geom_point(
    data = max_totals_per_year,
    aes(x = DayOfYear, y = as.factor(Year)),
    color = "red",
    size = 3
  ) +
  # add the label at DOY = 211 (not at the point location)
  geom_text(
    data = max_totals_per_year,
    aes(
      x = label_x,
      y = as.factor(Year),
      label = label_text
    ),
    vjust = 2,      # push text just below the line
    hjust = 0.5,
    size = 3,
    color = "black"
  ) +
  theme_bw() +
  labs(
    x = "Day of Year",
    y = "Year",
    title = "Fluoroprobe Data Availability 2015-2024"
  ) +
  scale_x_continuous(
    breaks = seq(1, 365, by = 30),
    limits = c(1, 365)
  ) +
  geom_vline(xintercept = c(133, 286), linetype = "dashed", color = "red") +
  theme(
    panel.grid.minor = element_blank()
  )

#print(phytosplot)

ggsave(
  "Figs/Data_availability/TotalPhytos2025pub.png",
  phytosplot,
  width = 8,
  height = 6,
  dpi = 400
)
#see that the data is dispersed at random intervals
####choosing casts and calculating peaks####
#1. Look at every cast for every year and remove casts that do not make sense
#2. Calculate peak metrics for each cast (peak depth, width, and magnitude)
#3. Visually check all casts and each metric to make sure it makes sense
#4. Average the peak metrics together (if appropriate)
#5. New data frame with one set of peak metrics for each week that we have data


#plot each one for cast selection 

# Make sure the Figs directory exists
if (!dir.exists("Figs")) {
  dir.create("Figs")
}

# Prepare your data with FacetID
DCM_metrics <- phytos |>
  select(
    Reservoir,
    Site,
    Date,
    Week,
    CastID,
    Depth_m,
    TotalConc_ugL
    # GreenAlgae_ugL,
    #Bluegreens_ugL,
    # BrownAlgae_ugL,
    # MixedAlgae_ugL
  ) |>
  group_by(Reservoir, Date, Site) |>
  mutate(FacetID = paste(CastID, Reservoir, Site, Date, "Week", Week, sep = " ")) |>
  ungroup()


# Get unique years in the dataset
years <- unique(year(DCM_metrics$Date))

types <- c("TotalConc_ugL"
           #,
          # "GreenAlgae_ugL",
           #"Bluegreens_ugL"
          # "BrownAlgae_ugL",
          # "MixedAlgae_ugL"
          )
# Loop over each type
for (type in types) {
  
  # Loop over each year
  for (yr in years) {
    
    # Filter data for the year
    test <- DCM_metrics |>
      filter(year(Date) == yr)
    
    # Skip if there's no data
    if (nrow(test) == 0) next
    
    # Create plot
    plot_casts <- ggplot(test, aes_string(x = type, y = "Depth_m")) +
      geom_path() +
      scale_y_reverse() +
      theme_bw() +
      facet_wrap(vars(FacetID)) +
      xlab("micrograms per liter") +
      ylab("Depth (m)") +
      ggtitle(paste(yr, type, "raw casts"))
    
    # Create directory if it doesn't exist
    dir.create("Figs/raw_flora_casts", recursive = TRUE, showWarnings = FALSE)
    
    # Save plot
    ggsave(
      filename = paste0("Figs/raw_flora_casts/", type, "_", yr, "_raw_casts.png"),
      plot = plot_casts,
      width = 12,
      height = 10,
      dpi = 300
    )
  }
}

#notes on casts
#casts to remove: 467, 814, 856, 920, 1149 

DCM_metrics_filtered <- DCM_metrics |>
  filter(!CastID %in% c(467, 814, 856, 920, 1149, 788, 933)) |> 
  mutate(
    CastID = case_when(
      CastID %in% c(485, 486) ~ 484,    # combine 485 + 486 into 484
      CastID %in% c(492, 493, 494) ~ 492,    # or 493 – pick ONE target
      CastID %in% c(499, 500, 501) ~ 499,    # or 500 – pick ONE target
      CastID %in% c(603, 604, 605) ~ 603,    # or 604 – pick ONE target
      TRUE ~ CastID
    )
  ) |>
  mutate(DOY = yday(Date), Year = year(Date)) 
  

####Peak.depth and max_conc####
# Define pigment variables to loop over
pigment_vars <- c("TotalConc_ugL") #if you want add these "GreenAlgae_ugL", "Bluegreens_ugL", "BrownAlgae_ugL", "MixedAlgae_ugL"

# Start from your filtered data
DCM_metrics_depth <- DCM_metrics_filtered |> group_by(CastID)

# Loop over each pigment variable
for (var in pigment_vars) {
  # Create new column names for max concentration and DCM depth
  max_col <- paste0(var, "_max_conc")
  dcm_col <- paste0(var, "_DCM_depth")
  
  # Dynamically create columns
  DCM_metrics_depth <- DCM_metrics_depth |>
    mutate(
      !!sym(max_col) := max(.data[[var]], na.rm = TRUE),
      !!sym(dcm_col) := ifelse(.data[[var]] == max(.data[[var]], na.rm = TRUE), Depth_m, NA_real_)
    ) |>
    fill(!!sym(max_col), .direction = "downup") |>
    fill(!!sym(dcm_col), .direction = "downup")
}
# Ungroup after loop
DCM_metrics_depth <- DCM_metrics_depth |> ungroup()

# final_DCM_metrics<- final_DCM_metrics|>
#   #select(-peak.top, -peak.bottom, -peak.width)|>#can remove this once have peak calculations figured out
#   group_by(CastID)|>
#   mutate(Q3 = quantile(TotalConc_ugL, 0.75)) #25% of data falls above this value 
# 
#-----max_conc check-----------####

# Pigments to visualize (columns that have *_DCM_depth already computed)
pigment_vars <- c("TotalConc_ugL") #, "GreenAlgae_ugL", "Bluegreens_ugL", "BrownAlgae_ugL", "MixedAlgae_ugL"

# Ensure output dir exists
dir.create("Figs/raw_flora_casts", recursive = TRUE, showWarnings = FALSE)

for (var in pigment_vars) {
  dcm_col <- paste0(var, "_DCM_depth")
  
  for (yr in years) {
    # Filter to this year
    test <- DCM_metrics_depth |>
      filter(year(Date) == yr)
    
    if (nrow(test) == 0) next
    
    # Defensive: skip if DCM column is missing
    if (!dcm_col %in% names(test)) next
    
    depth_max <- max(test$Depth_m, na.rm = TRUE)
    
    plot_casts <- ggplot(test, aes(x = .data[[var]], y = Depth_m, group = CastID)) +
      geom_path() +
      # light grid every meter
      geom_hline(yintercept = seq(0, depth_max, by = 1),
                 color = "lightblue", linetype = "dotted", linewidth = 0.3) +
      # DCM depth for this pigment
      geom_hline(aes(yintercept = .data[[dcm_col]]), color = "red") +
      # label the DCM depth on the right edge of each facet
      geom_text(
        aes(label = round(.data[[dcm_col]], 1), x = Inf, y = .data[[dcm_col]]),
        color = "black", hjust = 1.1, size = 3
      ) +
      scale_y_reverse(breaks = seq(0, depth_max, by = 1)) +
      theme_bw() +
      facet_wrap(vars(CastID)) +
      xlab("micrograms per liter") +
      ylab("Depth (m)") +
      ggtitle(paste(yr, "-", var, "raw casts"))
    
    ggsave(
      filename = paste0("Figs/raw_flora_casts/", var, "_", yr, "_raw_casts.png"),
      plot = plot_casts,
      width = 12,
      height = 10,
      dpi = 300
    )
  }
}

#
final_DCM_metrics<- DCM_metrics_depth|>
  mutate(max_conc = TotalConc_ugL_max_conc, 
         DCM_depth = TotalConc_ugL_DCM_depth)|>
  filter(!(CastID == 1087))|> #no real peak
  filter(!(CastID == 1150))|> #incomplete cast
  filter(Year <2025) 



####boxplots depth of DCM####
#need to use raw data for this to work 

#filtering so that max_conc is greater than 20 because otherwise we can't call it a deep chlorophyll maxima 
boxplot_Data <- final_DCM_metrics |>
  mutate(
    DayOfYear = yday(Date),
    Year = year(Date),
    Month = month(Date)
  ) |>
  filter(DayOfYear > 133, DayOfYear < 286) |>
  select(CastID, Date, Year, Month, DCM_depth, max_conc) |>
  # Remove exact duplicates BEFORE grouping
  distinct() |>
  # Reduce grouping to Date–CastID level only
  group_by(CastID, Date) |>
  filter(max_conc >= 20) |>
  filter(Year > 2014)|>
  summarise(
    Year = first(Year),
    Month = first(Month),
    DCM_depth = mean(DCM_depth, na.rm = TRUE),
    max_conc = mean(max_conc, na.rm = TRUE),
    .groups = "drop"
  ) 

label_data <- boxplot_Data %>%
  group_by(Year) %>%
  summarise(n = n())  # Calculate the number of data points per year

# Plot with labels for the number of data points
last_plot <- ggplot(boxplot_Data, aes(x = factor(Year), y = DCM_depth)) +
  geom_boxplot(outlier.shape = NA) +   # ⬅️ this line changed
  geom_point(
    aes(color = max_conc),
    position = position_jitter(width = 0.2),
    size = 2
  ) +
  scale_color_gradientn(
    colours = c("blue","cyan", "green","yellow", "red", "red3"),
    values = scales::rescale(c(0,40, 75, 100, 200, 380)),
    na.value = "gray",
    limits = c(20, 380), 
    breaks = c(20, 100, 200, 300, 380)
  ) +
  scale_y_reverse(name = "DCM Depth (inverted)") +
  ggtitle("DCM Depths") +
  ylim(10, 0) +
  labs(x = "Year", y = "DCM Depth", color = "totals ugL") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(
    data = label_data,
    aes(x = factor(Year), y = 0.5, label = paste0("n = ", n)),
    vjust = -0.5
  )
print(last_plot)

ggsave(
  filename = "DCM_depths_annual_boxplot_greaterthan20.png",
  plot = last_plot(),
  path = "Figs/Phytos_viz",
  width = 10,   # width in inches
  height = 5,   # height in inches
  dpi = 300     # optional: high resolution
)

####boxplots magnitude of DCM####
#visualizing just one box per year

boxplot_Data <- final_DCM_metrics |>
  filter(max_conc > 20) |>
  mutate(Date = as.Date(Date))|>
  filter(year(Date)>2014)|>
  mutate(DayOfYear = yday(Date))|>
  filter(DayOfYear>133, DayOfYear<286) |>
  mutate(Year = year(Date), Month = month(Date))|>
  group_by(Year, CastID)|>
  summarise(max_conc = mean(max_conc))
# Count points per year (after your filters/summary)
label_data <- boxplot_Data |>
  dplyr::group_by(Year) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

ggplot(boxplot_Data, aes(x = factor(Year), y = max_conc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = max_conc),
             position = position_jitter(width = 0.2), size = 2) +
  scale_color_gradientn(
    colours = c("blue","cyan","green","yellow","red","red3"),
    values = scales::rescale(c(0, 40, 75, 100, 200, 380)),
    na.value = "gray",
    limits = c(20, 380),
    breaks = c(20, 100, 200, 300, 380)
  ) +
  ggtitle("Peak Magnitudes") +
  ylim(0, 385) +
  labs(x = "Year", y = "Peak Magnitude", color = "Total (µg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Top-of-column labels (per-year)
  geom_text(
    data = label_data,
    aes(x = factor(Year), y = Inf, label = paste0("n = ", n)),
    vjust = 2.0,           # nudge down from the top
    size = 3.2,
    inherit.aes = FALSE
  )
ggsave(
  filename = "Peak_Magnitidues_2015_2024_boxplot_20.png",
  plot = last_plot(),
  path = "Figs/Phytos_viz",
  width = 10,   # width in inches
  height = 5,   # height in inches
  dpi = 300     # optional: high resolution
)

#For heatmaps go to "Extra Helpful Scripts/DCMFCRheatmaps"


#frame that will be added to for RF analysis at the end
#one measurement for each week that we have data available for 
final_phytos <- final_DCM_metrics|>
  group_by(Year,Week)|>
  summarise(
    DCM_depth = mean(DCM_depth, na.rm = TRUE),
    max_conc = mean(max_conc, na.rm = TRUE),
    .groups = "drop"
  )

#everything else will be joined to this dataframe
frame_weeks <- final_phytos|>
  distinct(Year, Week)

write.csv(frame_weeks, "CSVs/frame_weeks.csv", row.names = FALSE)
write.csv(final_phytos, "CSVs/final_phytos.csv", row.names = FALSE)
#metrics for each variable that needs to be calculated 
#depth_var_max
#depth_var_min
#var_max_val
#var_min_val
#var_mean
#var_range (max-main)
#-----Kruskal Wallis Test------
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

sig_grid_upper_fn <- function(data, response_col, title_label, year_min = 2015) {
  df <- data %>%
    mutate(Year_num = as.integer(as.character(Year))) %>%
    filter(!is.na(Year_num), Year_num >= year_min) %>%
    mutate(Year = factor(Year_num, levels = sort(unique(Year_num)))) %>%
    select(-Year_num)
  
  # Kruskal–Wallis
  fml <- as.formula(paste(response_col, "~ Year"))
  kw <- kruskal.test(fml, data = df)
  
  # Pairwise Wilcoxon (BH)
  resp_vec <- df[[response_col]]
  pw <- pairwise.wilcox.test(resp_vec, df$Year, p.adjust.method = "BH", exact = FALSE)
  
  # Full symmetric adjusted-p matrix
  years <- levels(df$Year)
  Pfull <- matrix(NA_real_, nrow = length(years), ncol = length(years),
                  dimnames = list(years, years))
  m <- pw$p.value
  if (!is.null(m)) {
    for (i in rownames(m)) for (j in colnames(m)) {
      Pfull[i, j] <- m[i, j]
      Pfull[j, i] <- m[i, j]
    }
  }
  diag(Pfull) <- NA_real_
  
  # Long + upper triangle
  p_long <- as.data.frame(Pfull) %>%
    tibble::rownames_to_column("Year1") %>%
    tidyr::pivot_longer(-Year1, names_to = "Year2", values_to = "p_adj") %>%
    dplyr::mutate(
      Year1_num = as.integer(as.character(Year1)),
      Year2_num = as.integer(as.character(Year2))
    ) %>%
    dplyr::filter(Year1_num < Year2_num) %>%
    dplyr::mutate(
      sig_bin = dplyr::case_when(
        is.na(p_adj)  ~ NA_character_,
        p_adj < 0.001 ~ "< 0.001",
        p_adj < 0.01  ~ "< 0.01",
        p_adj < 0.05  ~ "< 0.05",
        TRUE          ~ "\u2265 0.05"
      ),
      sig_bin = factor(sig_bin, levels = c("< 0.001", "< 0.01", "< 0.05", "\u2265 0.05")),
      # Stars only when significant; otherwise ""
      stars = dplyr::case_when(
        is.na(p_adj)  ~ "",
        p_adj < 0.001 ~ "***",
        p_adj < 0.01  ~ "**",
        p_adj < 0.05  ~ "*",
        TRUE          ~ ""   # no "ns"
      ),
      # Always show number when not NA; append stars only if significant
      label_txt = dplyr::case_when(
        is.na(p_adj) ~ "",
        p_adj < 0.05 ~ paste0(formatC(p_adj, format = "f", digits = 3), "\n", stars),
        TRUE         ~ formatC(p_adj, format = "f", digits = 3)
      )
    )
  
  sig_pal <- c(
    "< 0.001" = "#08306b",
    "< 0.01"  = "#2171b5",
    "< 0.05"  = "#6baed6",
    "\u2265 0.05" = "#e0e0e0"
  )
  
  ggplot2::ggplot(p_long, ggplot2::aes(x = Year1, y = Year2, fill = sig_bin)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = label_txt), size = 3.2, lineheight = 0.9) +
    ggplot2::scale_fill_manual(values = sig_pal, na.value = "white", drop = FALSE, name = "Adj. p (BH)") +
    ggplot2::coord_equal() +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title = title_label,
      subtitle = paste0("Kruskal - Wallis p = ", signif(kw$p.value, 3)),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(face = "bold")
    )
}


# ---------- Run for both responses ----------
# (If needed, coerce Year in your source once)
final_phytos <- final_phytos %>% mutate(Year = as.integer(as.character(Year)))

final_phytos_over20 <- final_phytos|>
  filter(max_conc >20)

p_depth <- sig_grid_upper_fn(final_phytos_over20, "DCM_depth",
                             "Pairwise Differences by Year - DCM Depth")
p_mag   <- sig_grid_upper_fn(final_phytos_over20, "max_conc",
                             "Pairwise Differences by Year - Max Phytoplankton")
# Stack
sig_both <- p_depth / p_mag + plot_annotation(
  title = "Year-wise Pairwise Significance (BH-adjusted)",
  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
)

sig_both

ggsave("Figs/Significance_UpperTriangle_DCM_and_MaxConc_over20_just_for_DCM_depth.png",
       sig_both, width = 10, height = 12, dpi = 600, bg = "white")

final_phytos_over20 %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    median = median(max_conc, na.rm = TRUE),
    IQR = IQR(max_conc, na.rm = TRUE),
    mean = mean(max_conc, na.rm = TRUE),
    sd = sd(max_conc, na.rm = TRUE)
  )


# Create the summary data
summary_df <- final_phytos_over20 %>%
  group_by(Year) %>%
  summarise(
    median = median(max_conc, na.rm = TRUE),
    q25 = quantile(max_conc, 0.25, na.rm = TRUE),
    q75 = quantile(max_conc, 0.75, na.rm = TRUE),
    mean = mean(max_conc, na.rm = TRUE),
    se = sd(max_conc, na.rm = TRUE) / sqrt(n())
  )

# Plot with legend
p <- ggplot(summary_df, aes(x = factor(Year))) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = "IQR", group = 1), alpha = 0.4) +
  geom_line(aes(y = median, color = "Median", group = 1), linewidth = 1.2) +
  geom_point(aes(y = median, color = "Median"), size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = "Mean ± SE", group = 1),
                width = 0.15, alpha = 0.6) +
  geom_point(aes(y = mean, color = "Mean ± SE"), shape = 21, fill = "white", size = 2) +
  scale_color_manual(name = "Statistics", values = c(
    "Median" = "blue",
    "Mean ± SE" = "red"
  )) +
  scale_fill_manual(name = "Statistics", values = c("IQR" = "grey80")) +
  labs(
    y = "Max phytoplankton concentration",
    x = "Year",
    title = "Median and Mean ± SE by Year"
  ) +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

# Save the plot to a file
ggsave("Figs/Phytos_viz/phytoplankton_summary.png", plot = p, width = 8, height = 5, dpi = 300)
