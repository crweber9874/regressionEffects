#' Create Combined Sunflower and Marginal Effects Plot
#'
#' Creates a 2x2 panel plot with sunflower plots and marginal effects for two
#' groups defined by a moderator variable.
#'
#' @param plot_data A list containing `plotting_data` and `observed_counts`
#' @param predictions Predictions object for calculating marginal effects
#' @param main_title Main title for the combined plot
#' @param moderator Name of the moderator variable (as string). Default: "vote_trump"
#' @param moderator_levels Vector of two values indicating which levels to plot.
#'   Default: c(0, 1)
#' @param group_labels Character vector of length 2 with labels for each group.
#'   Default: c("Group 0", "Group 1")
#' @param x_var Name of the x-axis variable (unlabeled). Default: "prepost"
#' @param x_var_label Name of the x-axis label variable. Default: "prepost_label"
#' @param x_labels Labels for x-axis categories. Default: c("Pre-Election", "Post-Election")
#' @param join_vars Character vector of variable names for joining. Default: c("prepost", "category")
#' @param focal_var Focal variable for marginal effects. Default: "prepost"
#' @param focal_contrast Contrast values for focal variable. Default: c(0, 1)
#' @param sunflower_params List of parameters for sunflower plots
#' @param marginal_params List of parameters for marginal effect plots
#' @param layout_widths Width ratios for patchwork layout. Default: c(0.9, 1)
#' @param layout_heights Height ratios for patchwork layout. Default: c(1, 1)
#' @param save_path Optional file path to save the plot
#' @param width Plot width for saving. Default: 6
#' @param height Plot height for saving. Default: 5
#'
#' @return A patchwork object with the combined plot
#'
#' @examples
#' \dontrun{
#' # Biden vs Trump voters
#' create_combined_plot(
#'   plot_data = plot_recount,
#'   predictions = predictions_recount,
#'   main_title = "Election Recounts",
#'   moderator = "vote_trump",
#'   moderator_levels = c(0, 1),
#'   group_labels = c("Biden Voters", "Trump Voters"),
#'   x_var = "prepost",
#'   x_var_label = "prepost_label",
#'   join_vars = c("prepost", "vote_trump", "category"),
#'   save_path = "recounts.png"
#' )
#'
#' # By party ID
#' create_combined_plot(
#'   plot_data = plot_party,
#'   predictions = predictions_party,
#'   main_title = "By Party",
#'   moderator = "party_id",
#'   moderator_levels = c(1, 2),
#'   group_labels = c("Democrats", "Republicans"),
#'   x_var = "time",
#'   x_var_label = "time_label",
#'   join_vars = c("time", "party_id", "category")
#' )
#' }
#'
#' @export
create_combined_plot <- function(plot_data,
                                 predictions,
                                 main_title,
                                 moderator = "vote_trump",
                                 moderator_levels = c(0, 1),
                                 group_labels = c("Group 0", "Group 1"),
                                 x_var = "prepost",
                                 x_var_label = "prepost_label",
                                 x_labels = c("Pre-Election", "Post-Election"),
                                 join_vars = c("prepost", "category"),
                                 focal_var = "prepost",
                                 focal_contrast = c(0, 1),
                                 sunflower_params = list(
                                   sunflower_alpha = 0.25,
                                   dodge_width = 0.65,
                                   sunflower_density = 60,
                                   sunflower_aspect_ratio = 7,
                                   sunflower_size = 0.01,
                                   errorbar_width = 0.05
                                 ),
                                 marginal_params = list(
                                   point_size = 3,
                                   point_alpha = 0.5
                                 ),
                                 layout_widths = c(0.9, 1),
                                 layout_heights = c(1, 1),
                                 save_path = NULL,
                                 width = 6,
                                 height = 5) {

  # Prepare data with labels
  plot_dat_labeled <- plot_data$plotting_data %>%
    dplyr::mutate(
      !!x_var_label := factor(.data[[x_var]],
                              levels = c(0, 1),
                              labels = x_labels)
    )

  # Create sunflower plot for first group
  group1_sunflower <- do.call(plotSunflower, c(
    list(
      pred_summary = plot_dat_labeled %>%
        dplyr::filter(.data[[moderator]] == moderator_levels[1]),
      observed_counts = plot_data$observed_counts,
      x_var = x_var_label,
      join_vars = join_vars,
      title = "",
      subtitle = group_labels[1],
      y_label = "Probability",
      x_label = "",
      legend_title = ""
    ),
    sunflower_params
  ))

  # Create sunflower plot for second group
  group2_sunflower <- do.call(plotSunflower, c(
    list(
      pred_summary = plot_dat_labeled %>%
        dplyr::filter(.data[[moderator]] == moderator_levels[2]),
      observed_counts = plot_data$observed_counts,
      x_var = x_var_label,
      join_vars = join_vars,
      title = "",
      subtitle = group_labels[2],
      y_label = "Probability",
      x_label = "",
      legend_title = ""
    ),
    sunflower_params
  ))

  # Calculate marginal effects
  me <- calculate_marginal_effect(
    predictions,
    focal_var = focal_var,
    focal_contrast = focal_contrast,
    moderator = moderator
  )

  # Create marginal effects plot for first group
  group1_margins <- do.call(plotMarginalEffects, c(
    list(
      me_data = me %>% dplyr::filter(.data[[moderator]] == moderator_levels[1]),
      title = "",
      y_label = "",
      x_label = ""
    ),
    marginal_params
  ))

  # Create marginal effects plot for second group
  group2_margins <- do.call(plotMarginalEffects, c(
    list(
      me_data = me %>% dplyr::filter(.data[[moderator]] == moderator_levels[2]),
      title = "",
      y_label = "",
      x_label = "Marginal Effect"
    ),
    marginal_params
  ))

  # Remove legend from first sunflower plot
  group1_no_legend <- group1_sunflower + ggplot2::theme(legend.position = "none")

  # Combine with patchwork
  combined_plot <- group1_no_legend + group1_margins +
    group2_sunflower + group2_margins +
    patchwork::plot_layout(widths = layout_widths,
                           heights = layout_heights) +
    patchwork::plot_annotation(title = main_title)

  # Save if path provided
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined_plot, width = width, height = height)
  }

  return(combined_plot)
}
