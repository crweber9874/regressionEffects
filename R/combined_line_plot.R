create_combined_sunflower <- function(plot_data,
                                      predictions = NULL,
                                      me_data = NULL,
                                      main_title,
                                      subtitle = NULL,
                                      caption = NULL,
                                      moderator = "vote_trump",
                                      moderator_levels = c(0, 1),
                                      group_labels = c("Group 0", "Group 1"),
                                      category_var = "category",
                                      category_levels = c("1", "2", "3", "4", "5"),
                                      category_labels = c("Strongly Disagree", "Disagree",
                                                          "Neither Agree nor Disagree",
                                                          "Agree", "Strongly Agree"),
                                      x_var = "prepost",
                                      join_vars = c("prepost", "category"),
                                      focal_var = "prepost",
                                      focal_contrast = c(0, 1),
                                      colors = c("1" = "lightgrey", "2" = "grey",
                                                 "3" = "darkgrey", "4" = "darkslategrey",
                                                 "5" = "black"),
                                      me_y_var = "mean_effect",
                                      me_x_var = "category",
                                      me_ci_lower = NULL,
                                      me_ci_upper = NULL,
                                      sunflower_params = list(
                                        sunflower_size = 0.2,
                                        sunflower_alpha = 0.25,
                                        errorbar_width = 0.1,
                                        errorbar_linewidth = 0.8,
                                        line_linewidth = 1,
                                        dodge_width = 0.5,
                                        sunflower_density = 50,
                                        sunflower_aspect_ratio = 5,
                                        y_limits = c(0, 0.5),
                                        y_breaks = seq(0, 0.5, 0.1)
                                      ),
                                      marginal_params = list(
                                        point_size = 3,
                                        point_alpha = 0.8,
                                        connector_lines = FALSE,
                                        show_errorbar = TRUE,
                                        errorbar_width = 0.2,
                                        reference_line = 0,
                                        y_limits = c(-0.3, 0.3),
                                        dodge_width = 0.6
                                      ),
                                      layout_widths = c(0.9, 1),
                                      layout_heights = c(1, 1),
                                      save_path = NULL,
                                      width = 8,
                                      height = 6) {

  require(dplyr)
  require(patchwork)

  if (is.null(me_data)) {
    if (is.null(predictions)) {
      stop("Either predictions or me_data must be provided")
    }
    me_data <- calculate_marginal_effect(
      predictions,
      focal_var = focal_var,
      focal_contrast = focal_contrast,
      moderator = moderator
    )
  }

  pred_summary_group1 <- plot_data$plotting_data %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[1])

  observed_counts_group1 <- plot_data$observed_counts %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[1])

  pred_summary_group2 <- plot_data$plotting_data %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[2])

  observed_counts_group2 <- plot_data$observed_counts %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[2])

  group1_sunflower <- do.call(plotSunflower, c(
    list(
      pred_summary = pred_summary_group1,
      observed_counts = observed_counts_group1,
      x_var = x_var,
      join_vars = join_vars,
      colors = colors,
      category_labels = category_labels,
      title = group_labels[1],
      y_label = "Predicted Probability",
      x_label = "",
      legend_title = "Response\nCategory"
    ),
    sunflower_params
  ))

  group2_sunflower <- do.call(plotSunflower, c(
    list(
      pred_summary = pred_summary_group2,
      observed_counts = observed_counts_group2,
      x_var = x_var,
      join_vars = join_vars,
      colors = colors,
      category_labels = category_labels,
      title = group_labels[2],
      y_label = "Predicted Probability",
      x_label = "",
      legend_title = "Response\nCategory"
    ),
    sunflower_params
  ))

  group1_margins <- do.call(plotDots, c(
    list(
      plot_dat = me_data %>% dplyr::filter(.data[[moderator]] == moderator_levels[1]),
      y_var = me_y_var,
      x_var = me_x_var,
      color_var = NULL,
      facet_var = NULL,
      category_levels = category_levels,
      category_labels = category_labels,
      ci_lower = me_ci_lower,
      ci_upper = me_ci_upper,
      title = "",
      y_label = "",
      x_label = ""
    ),
    marginal_params
  ))

  group2_margins <- do.call(plotDots, c(
    list(
      plot_dat = me_data %>% dplyr::filter(.data[[moderator]] == moderator_levels[2]),
      y_var = me_y_var,
      x_var = me_x_var,
      color_var = NULL,
      facet_var = NULL,
      category_levels = category_levels,
      category_labels = category_labels,
      ci_lower = me_ci_lower,
      ci_upper = me_ci_upper,
      title = "",
      y_label = "",
      x_label = ""
    ),
    marginal_params
  ))

  group1_no_legend <- group1_sunflower +
    ggplot2::theme(legend.position = "none")

  combined_plot <- group1_no_legend + group1_margins +
    group2_sunflower + group2_margins +
    patchwork::plot_layout(widths = layout_widths,
                           heights = layout_heights) +
    patchwork::plot_annotation(
      title = main_title,
      subtitle = subtitle,
      caption = caption,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.caption = ggplot2::element_text(size = 9, hjust = 0)
      )
    )

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined_plot, width = width, height = height)
  }

  return(combined_plot)
}
