#' Plot bootstrap results for exploratory assessment
#'
#' Creates an exploratory plot of the bootstrap distribution, confidence
#' intervals, original estimate, and mean bootstrap estimate. The plot is
#' faceted by trait value to facilitate comparison between trait values.
#' It is intended for visual inspection of the bootstrap results rather
#' than for formal statistical inference.
#'
#' @param bootstrap_replicates A data frame containing the individual
#'   bootstrap replicates, as returned by [boot_to_dataframe()].
#' @param bootstrap_intervals A data frame containing the bootstrap
#'   confidence intervals and summary statistics, as returned by
#'   [bootci_to_dataframe()] or a data frame with the corresponding
#'   columns `trait_value`, `est_original`, `mean_boot`, `int_type`,
#'   `ll`, and `ul`.
#' @param path Path to save the plot using [ggplot2::ggsave()].
#'   Defaults to `NA` (do not save plot).
#' @param ggsave_args A named list of additional arguments passed to
#'   [ggplot2::ggsave()]. These arguments override the default figure
#'   dimensions when `save = TRUE`.
#'
#' @return A `ggplot` object showing the bootstrap distributions,
#'   confidence intervals, and estimates. If `save = TRUE`, the plot is
#'   also saved using [ggplot2::ggsave()].
#'
plot_bootstrap_results <- function(
  bootstrap_replicates,
  bootstrap_intervals,
  path = NA,
  ggsave_args = list()
) {
  require("ggplot2")
  require("dplyr")
  require("tidyr")

  # Prepare the original and mean bootstrap estimates for plotting.
  estimates_df <- bootstrap_intervals %>%
    distinct(
      trait_value,
      estimate = est_original,
      `bootstrap estimate` = mean_boot
    ) %>%
    pivot_longer(
      cols = c("estimate", "bootstrap estimate"),
      names_to = "Legend",
      values_to = "value"
    ) %>%
    mutate(
      Legend = factor(
        Legend,
        levels = c("estimate", "bootstrap estimate"),
        ordered = TRUE
      )
    )

  p <- ggplot(
    data = bootstrap_replicates,
    aes(x = trait_value)
  ) +
    # Show the distribution of bootstrap replicates.
    geom_violin(aes(y = rep_boot)) +

    # Add bootstrap confidence intervals.
    geom_errorbar(
      data = bootstrap_intervals,
      aes(
        ymin = ll,
        ymax = ul,
        colour = int_type
      ),
      position = position_dodge(0.8),
      linewidth = 0.8
    ) +

    # Add the original and mean bootstrap estimates.
    geom_point(
      data = estimates_df,
      aes(y = value, shape = Legend),
      colour = "black",
      position = position_dodge(0.2),
      size = 4
    ) +

    # Show each trait value in a separate panel.
    facet_wrap(~trait_value, scales = "free") +

    # Set labels and legend position.
    labs(
      y = "Bootstrap replicates",
      x = "",
      shape = "Legend:",
      colour = "Interval type:"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  if (!is.na(path)) {
    # Choose default dimensions based on the number of trait values
    n_trait_values <- length(unique(bootstrap_replicates$trait_value))

    if (n_trait_values <= 2) {
      default_width <- 7
      default_height <- 4.5
    } else if (n_trait_values <= 4) {
      default_width <- 8
      default_height <- 6
    } else if (n_trait_values <= 6) {
      default_width <- 10
      default_height <- 6
    } else if (n_trait_values <= 9) {
      default_width <- 10
      default_height <- 8
    } else {
      default_width <- 12
      default_height <- ceiling(n_trait_values / 3) * 2.5
    }

    # Combine default dimensions with user-supplied ggsave arguments
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    file <- file.path(
      path,
      paste0(
        "bootstrap_results_",
        unique(bootstrap_replicates$trait),
        ".png"
      )
    )

    save_args <- c(
      list(
        filename = file,
        plot = p,
        width = default_width,
        height = default_height,
        units = "in"
      ),
      ggsave_args
    )

    do.call(ggsave, save_args)
  }

  return(p)
}
