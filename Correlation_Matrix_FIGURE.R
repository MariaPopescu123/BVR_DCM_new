library(ggplot2)
library(dplyr)
library(tidyr)

full_weekly_data <- read.csv("CSVs/full_weekly_data.csv")

#print(colnames(full_weekly_data_clean))
depth_analysis <- full_weekly_data |>
  select(-ends_with("max_val"), -ends_with("min_val"), -ends_with("range"), 
         -max_conc, -totals_mean, -totals_med, -N_at_DCM, -Week, -Date.x, -Date.x.x, -Date.y.y)

magnitude_analysis <- full_weekly_data|>
  select(-starts_with("depth_"), -Week, -totals_mean, -totals_med, -Date.x, -Date.y.y, -Date.x.x)



# Correlation matrix (all numeric columns from 2:end)
cor_matrix <- cor(depth_analysis[, 2:ncol(depth_analysis)], 
                  method = "spearman", 
                  use = "pairwise.complete.obs")

# Convert to long format
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)

# Set factor levels to preserve order
var_names <- colnames(cor_matrix)
cor_long$Var1 <- factor(cor_long$Var1, levels = var_names)  # x-axis
cor_long$Var2 <- factor(cor_long$Var2, levels = var_names)  # y-axis

# Number of variables
nvars <- length(var_names)

# Split into upper and lower triangles
cor_upper <- cor_long %>% filter(as.numeric(Var1) <= as.numeric(Var2))
cor_lower <- cor_long %>% filter(as.numeric(Var1) >= as.numeric(Var2))

# Build the plot
p <- ggplot() +
  # Dashed grid lines
  geom_vline(xintercept = seq(0.5, nvars + 0.5, by = 1), 
             linetype = "dashed", color = "black", size = 0.3) +
  geom_hline(yintercept = seq(0.5, nvars + 0.5, by = 1), 
             linetype = "dashed", color = "black", size = 0.3) +
  # Gray lower triangle
  geom_tile(data = cor_upper, aes(x = Var1, y = Var2), 
            fill = "grey90", color = "white") +
  # Correlation upper triangle
  geom_point(data = cor_lower, 
             aes(x = Var1, y = Var2, fill = Correlation, size = abs(Correlation)), 
             shape = 22, color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  scale_size(range = c(1, 14), guide = "none") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_discrete(position = "top") +   # x-axis labels on top
  scale_y_discrete(limits = rev(var_names)) +  # y-axis from top to bottom
  labs(title = "Correlation Matrix Heatmap")

# Save the plot
ggsave("Figs/correlation_matrix_heatmap_upper.png", plot = p, 
       width = 20, height = 20, units = "in", dpi = 300, bg = "white")

