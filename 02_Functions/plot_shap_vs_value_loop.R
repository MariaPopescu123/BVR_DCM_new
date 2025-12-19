plot_shap_vs_value_loop <- function(shap_df,
                                    vars_to_plot,
                                    out_dir,
                                    prefix = "shap_vs",
                                    years_label = NULL,
                                    width = 6,
                                    height = 4,
                                    dpi = 300) {
  stopifnot(is.data.frame(shap_df))
  required_cols <- c("var", "value_num", "shap")
  missing_cols <- setdiff(required_cols, names(shap_df))
  if (length(missing_cols) > 0) {
    stop("shap_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # helper to make safe filenames
  safe_name <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)
  
  for (v in vars_to_plot) {
    df_v <- shap_df %>%
      dplyr::filter(var == v) %>%
      dplyr::mutate(value_num = as.numeric(value_num),
                    shap = as.numeric(shap))
    
    if (nrow(df_v) == 0) {
      message("Skipping ", v, " (no rows found in shap_df).")
      next
    }
    
    title_txt <- if (!is.null(years_label)) {
      paste0(years_label, " Interaction: SHAP vs ", v)
    } else {
      paste0("SHAP vs ", v)
    }
    
    p <- ggplot2::ggplot(df_v, ggplot2::aes(x = value_num, y = shap)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::labs(title = title_txt, x = v, y = "SHAP value")
    
    print(p)
    
    file_name <- paste0(
      prefix, "_",
      if (!is.null(years_label)) paste0(safe_name(years_label), "_") else "",
      safe_name(v),
      ".png"
    )
    
    ggplot2::ggsave(
      filename = file.path(out_dir, file_name),
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }
  
  invisible(TRUE)
}
