library(ggplot2)
library(dplyr)
library(tidyr)

# Correlation matrix
cor_matrix <- cor(depth_analysis[, 2:42], 
                  method = "spearman", 
                  use = "pairwise.complete.obs")

# Convert to long format
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)

# Plot using square points
ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation, size = abs(Correlation))) +
  geom_point(shape = 22, color = "white") +  # filled squares
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  scale_size(range = c(1, 10), guide = "none") +  # Bigger squares for stronger correlations
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank()) +
  labs(title = "Correlation Matrix Heatmap")

