#Phytoplankton metrics and visualization
#published 2025
#current_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/9/f246b36c591a888cc70ebc87a5abbcb7")

#THIS IS IN STAGING RIGHT NOW: 
current_df <- read_csv("https://pasta-s.lternet.edu/package/data/eml/edi/1304/1/f246b36c591a888cc70ebc87a5abbcb7")

#published 2024
#current_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/8/0359840d24028e6522f8998bd41b544e")

#check July 6 2023

#adding columns with total_conc max and the depth at which it occurs
FCRphytos <- current_df %>% 
  filter(Reservoir == "FCR", Site == 50)%>%
  mutate(Date  = as_date(DateTime)) |> 
  filter((hour(DateTime) >= 8), (hour(DateTime) <= 18))|>
    #filter(!CastID == 793)|> #this was here because in the version that is currently published (9/30/2025) this cast is blank, but in the version that is in staging as of 9/30/2025, this cast is fixed 
  #filter(Flag_TotalConc_ugL != 2,Flag_TotalConc_ugL != 3)|> #2 is instrument malfunction and #3 is low transmission value
  mutate(Week = week(Date))|>
  mutate(Year = year(Date))|>
  mutate(DOY = yday(Date))|>
  filter(year(Date) >2014)

####flora instrument data availability####
# 1. Build plotting data
plot_dat <- FCRphytos %>%
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
DCM_metrics <- FCRphytos |>
  select(
    Reservoir,
    Site,
    Date,
    Week,
    CastID,
    Depth_m,
    TotalConc_ugL,
    # GreenAlgae_ugL,
    # Bluegreens_ugL,
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
           # "Bluegreens_ugL",
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
    dir.create("Figs/FCR/raw_flora_casts", recursive = TRUE, showWarnings = FALSE)
    
    # Save plot
    ggsave(
      filename = paste0("Figs/FCR/raw_flora_casts/", type, "_", yr, "_raw_casts.png"),
      plot = plot_casts,
      width = 12,
      height = 10,
      dpi = 300
    )
  }
}
