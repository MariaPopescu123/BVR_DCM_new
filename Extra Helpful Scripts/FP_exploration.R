# L1 FP data exploration 
# Maria Popescu adapated from original Author: Mary Lofton
# Date: 08JUL24

#flora data all years

library(dplyr)
library(lubridate)
library(tidyr)

phytos <- phytos|>
  filter(Year > 2014, Year <2025)

# casts table (as you had it)
casts <- phytos %>%
  mutate(Date = date(DateTime)) %>%
  select(Reservoir, Date, CastID, Site) %>%
  distinct()

# 1) Max cast(s) per year
max_phytos_annual <- phytos %>%
  group_by(Year) %>%
  filter(TotalConc_ugL == max(TotalConc_ugL, na.rm = TRUE)) %>%
  ungroup()

# 2) Get the unique CastIDs for those max casts
max_cast_ids <- unique(max_phytos_annual$CastID)

# 3) Filter casts to only those CastIDs
sample_dat <- casts %>%
  filter(
    Reservoir == "BVR",
    Site == 50,
    CastID %in% max_cast_ids
  ) %>%
  mutate(Date = as.Date(Date))

# 4) Build plot_dat, keeping only rows whose CastID/Date/Reservoir are in sample_dat
plot_dat <- phytos %>%
  mutate(Date = date(DateTime)) %>%
  filter(month(Date) != 1) %>%
  group_by(Reservoir, Date, Site, CastID) %>%
  mutate(FacetID = paste(Reservoir, Date, sep = " ")) %>%
  ungroup() %>%
  semi_join(sample_dat, by = c("Reservoir", "Date", "CastID")) %>%  # <- key line
  select(
    CastID, FacetID,
    GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL,
    TotalConc_ugL, Depth_m
    # SampleDepth   # add here later if/when you create it
  ) %>%
  pivot_longer(
    cols = GreenAlgae_ugL:TotalConc_ugL,
    names_to = "var",
    values_to = "ugL"
  )
plot_casts <- ggplot(plot_dat, aes(x = ugL, y = Depth_m, group = var)) +
  geom_path(aes(color = var, size = var)) +
  scale_y_reverse() +
  theme_bw() +
  facet_wrap(
    facets = vars(FacetID),
    nrow   = 2,
    ncol   = 5
  ) +
  xlab("micrograms per liter") +
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
  scale_linetype_discrete(na.translate = FALSE)

plot_casts

ggsave("Figs/Phytos_viz/FP_casts_2025.png", device = "png",
       height = 6, width = 10.5, units = "in")  # you will probably want to change the dimensions










