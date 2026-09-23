#' Plot Red List Index changes
#'
#' Creates plots of changes in the Red List Index and their bootstrap
#' confidence intervals for different trait values. Effect classifications
#' are shown based on the confidence intervals using the specified effect
#' classification threshold. A separate plot is created for each requested
#' bootstrap interval type.
#'
#' @param effects_df A data frame containing the RLI changes and bootstrap
#'   confidence intervals. Must contain the columns `trait`, `trait_value`,
#'   `n_spec`, `int_type`, `est_original`, `ll`, and `ul`.
#' @param interval_type A character vector specifying the bootstrap
#'   interval types to plot. Defaults to `"bca"`.
#' @param path Path to save the plots using [ggplot2::ggsave()].
#'   Defaults to `NA` (do not save plots).
#' @param ggsave_args A named list of additional arguments passed to
#'   [ggplot2::ggsave()].
#'
#' @return A named list of `ggplot` objects, with one plot for each
#'   interval type specified in `interval_type`. If `path` is provided,
#'   the plots are also saved as PNG files.
#'
plot_rli_change_results <- function(
  effects_df,
  interval_type = "bca",
  path = NA,
  ggsave_args = list()
) {
  require("ggplot2")
  require("dplyr")
  require("rlang")

  out <- vector(mode = "list", length = length(interval_type))

  for (i in seq_along(interval_type)) {
    interval <- interval_type[i]

    plot_data <- effects_df %>%
      filter_out(.data$int_type != interval) %>%
      mutate(
        trait_value = paste0(
          .data$trait_value,
          " (n = ",
          .data$n_spec,
          ")"
        )
      )

    p <- ggplot(
      plot_data,
      aes(
        y = .data$est_original,
        x = reorder(
          .data$trait_value,
          -.data$est_original
        ),
        ymin = .data$ll,
        ymax = .data$ul
      )
    ) +

      geom_errorbar(
        linewidth = 1.5,
        colour = "darkgrey"
      ) +

      effectclass::stat_effect(
        aes(
          ymin = .data$ll,
          ymax = .data$ul
        ),
        reference = 0,
        threshold = 0.02,
        size = 10
      ) +

      coord_flip() +
      labs(
        x = unique(plot_data$trait),
        y = "Change in RLI between 2025 and 2010",
        title = unique(plot_data$trait),
        subtitle = paste(interval, "bootstrap intervals"),
        colour = "Trend"
      ) +
      theme(
        legend.position = "right"
      )

    out[[i]] <- p

    if (!is.na(path)) {
      dir.create(
        path,
        showWarnings = FALSE,
        recursive = TRUE
      )

      file <- file.path(
        path,
        paste0(
          "rli_change_",
          unique(effects_df$trait),
          "_",
          interval,
          ".png"
        )
      )

      save_args <- c(
        list(
          filename = file,
          plot = p
        ),
        ggsave_args
      )

      do.call(ggsave, save_args)
    }
  }

  names(out) <- interval_type

  return(out)
}
