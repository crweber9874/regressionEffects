#' @title ggPoint_categorical
#'
#' @description Generate Point Estimates Plot for Categorical/Ordinal Dependent Variables
#' This function creates a `ggplot2` visualization of point estimates and credible intervals,
#' typically representing "winner/loser" effects, combined with a rug plot showing
#' the raw distribution of the dependent variable in each faceted column. It is designed for
#' ordinal or nominbal dependent variables.
#'
#' @param plot_dat Dataframe containing posterior predictions with `mean`, `lower`, `upper`
#' bounds for estimates, and columns for `.category`, `prepost`, and the faceting variable.
#' @param raw_data_for_rug Dataframe containing the original, raw data. This is used
#' to calculate the distribution for the rug plot. It must contain the columns specified
#' by `dv_col_name` and `facet_col_name`. The plot shows the marginal distributionals across
#' pre-post treatment.
#' @param dv_col_name Character string. The name of the column in `raw_data_for_rug` that
#' represents the raw dependent variable (e.g., `"burn_flag"`). This variable should
#' contain numeric or factor levels (e.g., "1" to "5") that map to `.category`.
#' @param facet_col_name Character string. The name of the column in both `plot_dat` and
#' `raw_data_for_rug` used for faceting (e.g., `"presvote_trump_2020"`).
#' @param color_col_name Character string. The name of the column in `plot_dat` used for
#' coloring points and lines (e.g., `"prepost"`).
#' @param category_levels Character vector. The underlying levels of the `.category` variable
#' (e.g., `c("1", "2", "3", "4", "5")`). Used for factor reordering.
#' @param category_labels Character vector. The labels corresponding to `category_levels`
#' (e.g., `c("Strongly Disagree", ..., "Strongly Agree")`).
#' @param facet_levels Character vector. The underlying levels of the `facet_col_name`
#' (e.g., `c("0", "1")`). Used for factor reordering.
#' @param facet_labels Character vector. The labels corresponding to `facet_levels`
#' (e.g., `c("Trump Voter", "Biden Voter")`).
#' @param color_levels Character vector. The underlying levels of the `color_col_name`
#' (e.g., `c("pre", "post")`). Used for factor reordering.
#' @param color_labels Character vector. The labels corresponding to `color_levels`
#' (e.g., `c("Pre-Survey", "Post-Survey")`).
#' @param color_values Character vector. The hexadecimal color codes for the levels of
#' `color_col_name` (e.g., `c("grey", "black")`).
#' @param point_size Numeric. Size of the `geom_point` markers. Defaults to `3`.
#' @param dodge_width Numeric. Horizontal dodging width for `geom_point` and `geom_linerange`
#' to separate overlaid groups. Defaults to `0.6`.
#' @param linerange_width Numeric. Line width for `geom_linerange`. Defaults to `1.25`.
#' @param title Character string. Main title of the plot. Defaults to `"Point estimates"`.
#' @param y_axis_title Character string. Label for the Y-axis (which becomes the X-axis
#' after `coord_flip()`). Defaults to `"Posterior Mean Estimate"`.
#' @param x_axis_title Character string. Label for the X-axis (which becomes the Y-axis
#' after `coord_flip()`). Defaults to `""` (empty string).
#' @param rug_jitter_height Numeric. Amount of vertical jitter for the rug plot points.
#' Defaults to `0.1`.
#' @param rug_side Character string. Side for the rug plot (`"b"`, `"t"`, `"l"`, `"r"`).
#' Defaults to `"b"` (bottom, which is left after `coord_flip()`).
#' @param rug_alpha Numeric. Transparency (alpha) of the rug plot marks. Defaults to `0.1`.
#' @param rug_size Numeric. Size (thickness) of the rug plot marks. Defaults to `0.3`.
#' @param rug_length Unit object. Length of the rug plot marks. Defaults to `unit(0.05, "npc")`.
#' @param rug_color Character string. Color of the rug plot marks. Defaults to `"lightgrey"`.
#' @param ... Additional arguments passed to `ggplot2::facet_wrap()`.
#' @return A `ggplot` object.
#' @export
ggPoint_categories <- function(plot_dat,
                                raw_data_for_rug,
                                dv_col_name = "burn_flag",
                                facet_col_name = "presvote_trump_2020",
                                color_col_name = "prepost",
                                category_levels = c("1", "2", "3", "4", "5"),
                                category_labels = c("Strongly Disagree", "Disagree",
                                                    "Neither Agree nor Disagree",
                                                    "Agree", "Strongly Agree"),
                                facet_levels = c("0", "1"),
                                facet_labels = c("Trump Voter", "Biden Voter"),
                                color_levels = c("pre", "post"), # color levels
                                color_values = c("grey", "black"),
                                color_labels = c("Pre-Survey", "Post-Survey"),
                                point_size = 3,
                                dodge_width = 0.6,
                                linerange_width = 1.25,
                                title = "Point estimates",
                                y_axis_title = "Posterior Mean Estimate",
                                x_axis_title = "",
                                rug_jitter_height = 0.1,
                                rug_side = "b",
                                rug_alpha = 0.1,
                                rug_size = 0.3,
                                rug_length = grid::unit(0.05, "npc"), # units::unit
                                rug_color = "lightgrey",
                                y_axis_limits = c(0,1),
                                ...) {

  # --- Input Validation (optional but good practice) ---
  if (!all(c("mean", "lower", "upper", ".category", "prepost") %in% names(plot_dat))) {
    stop("`plot_dat` must contain 'mean', 'lower', 'upper', '.category', and 'prepost' columns.")
  }
  if (!all(c(dv_col_name, facet_col_name) %in% names(raw_data_for_rug))) {
    stop(paste0("`raw_data_for_rug` must contain '", dv_col_name, "' and '", facet_col_name, "' columns."))
  }

  # --- Data Preparation for Main Plot ---

  plot_dat_prepared <- plot_dat %>%
    dplyr::mutate( # dplyr::mutate
      .category = factor(.category, levels = category_levels, labels = category_labels),
      # Use dynamic column name for prepost
      !!rlang::sym(color_col_name) := factor(.data[[color_col_name]], levels = color_levels, labels = color_labels), # rlang::sym
      # Use dynamic column name for facet_col_name
      !!rlang::sym(facet_col_name) := factor(.data[[facet_col_name]], levels = facet_levels, labels = facet_labels) # rlang::sym
    )

  # --- Data Preparation for Rug Plot (side_plot) ---

  side_plot_prepared <- raw_data_for_rug %>%
    dplyr::select(!!rlang::sym(dv_col_name), !!rlang::sym(facet_col_name)) %>% # dplyr::select, rlang::sym
    dplyr::mutate( # dplyr::mutate
      .category = jitter(as.numeric(.data[[dv_col_name]]), amount = rug_jitter_height * 5), # Jitter more broadly for actual categories
      !!rlang::sym(facet_col_name) := factor(.data[[facet_col_name]], levels = facet_levels, labels = facet_labels) # rlang::sym
    ) %>%
    tidyr::drop_na(.category, !!rlang::sym(facet_col_name)) # tidyr::drop_na, rlang::sym


  plot <- ggplot2::ggplot(plot_dat_prepared,
                       ggplot2::aes(x = .category, y = mean, ymin = lower, ymax = upper, colour = .data[[color_col_name]])) + # ggplot2::aes
    ggplot2::facet_wrap(~.data[[facet_col_name]], nrow = 2, ...) + # ggplot2::facet_wrap

    ggplot2::geom_point(size = point_size, position = ggplot2::position_dodge(width = dodge_width)) + # ggplot2::geom_point, ggplot2::position_dodge
    ggplot2::geom_linerange(ggplot2::aes(x=.category, ymax=mean, ymin=0), linewidth = linerange_width, position = ggplot2::position_dodge(width = dodge_width)) + # ggplot2::geom_linerange, ggplot2::aes

    ggplot2::geom_rug(data = side_plot_prepared,
                      ggplot2::aes(x = .category, y = 0,
                                   colour = NULL,
                                   ymin = NULL, ymax = NULL, group = NULL),
                      inherit.aes = FALSE,
                      position = ggplot2::position_jitter(width = rug_jitter_height, seed = 123),
                      sides = rug_side,
                      linewidth = rug_size,
                      alpha = rug_alpha,
                      color = rug_color,
                      length = rug_length) +

    ggplot2::coord_flip() +

    ggplot2::ggtitle(title) +
    ggplot2::scale_y_continuous(y_axis_title, limits = y_axis_limits) +
    ggplot2::scale_x_discrete(x_axis_title) +

    ggplot2::scale_colour_manual(
      name = "",
      values = color_values,
      labels = color_labels
    ) +

    ggplot2::theme(legend.position = "bottom",
                   panel.grid = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.text.x = ggplot2::element_text(angle = 0),
                   plot.title = ggplot2::element_text(color = "black", size = 12))

  return(plot)
}


