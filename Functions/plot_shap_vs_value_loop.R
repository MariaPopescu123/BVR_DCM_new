plot_shap_vs_value_loop <- function(shap_df,
                                    vars_to_plot,
                                    out_dir,
                                    prefix = "shap_vs",
                                    analysis_label = "Depth Analysis",
                                    var_labels = NULL,
                                    width = 6,
                                    height = 4,
                                    dpi = 300,
                                    panel_ncol = 3,
                                    panel_width = NULL,
                                    panel_height = NULL) {
  stopifnot(is.data.frame(shap_df))
  
  required_cols <- c("var", "value_num", "shap")
  missing_cols <- setdiff(required_cols, names(shap_df))
  if (length(missing_cols) > 0) {
    stop("shap_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  safe_name <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)
  
  pretty_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) {
      unname(var_labels[[v]])
    } else {
      v
    }
  }
  
  plot_list <- list()
  panel_idx <- 0
  
  for (v in vars_to_plot) {
    df_v <- shap_df %>%
      dplyr::filter(var == v) %>%
      dplyr::mutate(
        value_num = as.numeric(value_num),
        shap      = as.numeric(shap)
      ) %>%
      dplyr::filter(!is.na(value_num), !is.na(shap)) %>%
      dplyr::mutate(
        value_z = as.numeric(scale(value_num))
      )
    
    if (nrow(df_v) == 0) {
      message("Skipping ", v, " (no rows found in shap_df).")
      next
    }
    
    panel_idx <- panel_idx + 1
    panel_letter <- LETTERS[panel_idx]
    
    v_pretty <- pretty_label(v)
    title_txt <- paste0(analysis_label, ": SHAP vs ", v_pretty)
    
    p <- ggplot2::ggplot(df_v, ggplot2::aes(x = value_num, y = shap, color = value_z)) +
      ggplot2::geom_point(alpha = 0.75, size = 1.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::scale_color_viridis_c(
        option = "H",
        limits = c(-3, 3),
        oob = scales::oob_squish
      ) +
      ggplot2::labs(
        tag = panel_letter,
        title = title_txt,
        x = v_pretty,
        y = "SHAP value",
        color = "z-scaled\nvalue"
      ) +
      ggplot2::theme_classic(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 11),
        plot.tag = ggplot2::element_text(size = 14, face = "bold"),
        plot.tag.position = c(0.02, 0.98),
        legend.position = "right"
      )
    
    plot_list[[v]] <- p
  }
  
  panel_plot <- NULL
  if (length(plot_list) > 0) {
    
    n_panels <- length(plot_list)
    panel_ncol <- max(1, panel_ncol)
    n_rows <- ceiling(n_panels / panel_ncol)
    
    if (is.null(panel_width))  panel_width  <- width  * panel_ncol
    if (is.null(panel_height)) panel_height <- height * n_rows
    
    # Hide legends on all panels except the last one
    plot_list_collected <- lapply(plot_list, function(p) {
      p + ggplot2::theme(legend.position = "none")
    })
    
    # Keep legend on the last plot for collection
    plot_list_collected[[length(plot_list_collected)]] <- 
      plot_list[[length(plot_list)]] + 
      ggplot2::theme(legend.position = "right")
    
    panel_plot <- patchwork::wrap_plots(plot_list_collected, ncol = panel_ncol) +
      patchwork::plot_layout(guides = "collect")
    
    panel_file <- paste0(prefix, "_", safe_name(analysis_label),
                         "_PANEL_", panel_ncol, "col_", n_panels, "plots.png")
    
    ggplot2::ggsave(
      filename = file.path(out_dir, panel_file),
      plot = panel_plot,
      width = panel_width,
      height = panel_height,
      dpi = dpi,
      bg = "white"
    )
  }
  
  return(list(
    individual_plots = plot_list,
    panel_plot = panel_plot
  ))
}
