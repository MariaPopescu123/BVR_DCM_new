plot_shap_vs_value_loop <- function(shap_df,
                                    vars_to_plot,
                                    out_dir,
                                    prefix = "shap_vs",
                                    analysis_label = "Depth Analysis",
                                    var_labels = NULL,
                                    width = 6,
                                    height = 4,
                                    dpi = 300,
                                    panel_ncol = 2,
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
  
  # helper to lookup pretty labels
  pretty_label <- function(v) {
    if (!is.null(var_labels) && v %in% names(var_labels)) {
      unname(var_labels[[v]])
    } else {
      v
    }
  }
  
  plot_list <- list()
  
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
        title = title_txt,
        x = v_pretty,
        y = "SHAP value",
        color = "z-scaled\nvalue"
      ) +
      ggplot2::theme_classic(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 11),
        legend.position = "right"
      )
    
    plot_list[[v]] <- p
    
    file_name <- paste0(prefix, "_", safe_name(v), ".png")
    ggplot2::ggsave(
      filename = file.path(out_dir, file_name),
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }
  
  # Build and save an N-panel figure (supports 10+ panels) with one legend
  panel_plot <- NULL
  if (length(plot_list) > 0) {
    
    n_panels <- length(plot_list)
    panel_ncol <- max(1, panel_ncol)
    n_rows <- ceiling(n_panels / panel_ncol)
    
    # auto-size: scale panel dimensions from single-plot size
    if (is.null(panel_width))  panel_width  <- width  * panel_ncol
    if (is.null(panel_height)) panel_height <- height * n_rows
    
    panel_plot <- patchwork::wrap_plots(plot_list, ncol = panel_ncol) +
      patchwork::plot_layout(guides = "collect") +
      ggplot2::theme(legend.position = "right")
    
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
