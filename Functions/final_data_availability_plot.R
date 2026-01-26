#function for plotting data availability
#dataframe must have Date column (not)

library(ggplot2)
library(dplyr)
library(patchwork)  # For combining plots

# Define the function to handle multiple variables
library(ggplot2)
library(dplyr)
library(patchwork)
library(lubridate)

final_data_availability_plot <- function(dataframe, variables) {
  plot_list <- lapply(variables, function(var) {
    var_sym <- sym(var)
    
    plot_dat <- dataframe %>%
      filter(!is.na(!!var_sym)) %>%
      mutate(Year = year(Date), DayOfYear = yday(Date)) %>%
      select(Date, Year, DayOfYear, !!var_sym)
    
    max_values_per_year <- plot_dat %>%
      group_by(Year) %>%
      slice(which.max(!!var_sym)) %>%
      ungroup()
    
    # ✅ FIXED: ensure all '+' signs are in the right place
    p <- ggplot(plot_dat, aes(x = DayOfYear, y = as.factor(Year), group = Year)) +
      geom_line() +
      geom_point(shape = 18) +
      geom_point(
        data = max_values_per_year,
        aes(x = DayOfYear, y = as.factor(Year)),
        color = "red", size = 2
      ) +
      geom_text(
        data = max_values_per_year,
        aes(
          x = 301,
          y = as.factor(Year),
          label = paste0("Max: ", round(!!var_sym, 2))
        ),
        vjust = 1.5, hjust = 0.5,
        color = "black", size = 3
      ) +
      geom_vline(xintercept = 133, linetype = "dashed", color = "red") +   # ✅ moved here
      geom_vline(xintercept = 286, linetype = "dashed", color = "red") +   # ✅ also here
      theme_bw() +
      labs(x = "Day of Year", y = "Year", title = var) +
      scale_x_continuous(breaks = seq(1, 365, by = 30), limits = c(91, 331)) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
    
    return(p)
  })
  
  # Combine all plots and add one global title
  final_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title = "Data Availability",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
      )
    )
  
  return(final_plot)
}

