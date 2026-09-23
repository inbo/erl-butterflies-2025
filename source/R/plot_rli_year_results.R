#' Plot Red List Index results
#'
#' Creates plots of Red List Index estimates and bootstrap confidence
#' intervals for different trait values over time. Effect classifications
#' are added to the end of each RLI trajectory. A separate plot is created
#' for each requested bootstrap interval type.
#'
#' @param x A data frame containing the RLI estimates and bootstrap
#'   confidence intervals. Must contain the columns `trait`, `trait_value`,
#'   `Year`, `est_original`, `int_type`, `ll`, and `ul`.
#' @param effects_df A data frame containing the effect classifications
#'   for each trait value and bootstrap interval type. Must contain the
#'   columns `trait_value`, `int_type`, `effect_code`, and `effect`.
#' @param interval_type A character vector specifying the bootstrap
#'   interval types to plot. Defaults to `"bca"`.
#' @param path Path to save the plots using [ggplot2::ggsave()].
#'   Defaults to `NA` (do not save plots).
#' @param ggsave_args A named list of additional arguments passed to
#'   [ggplot2::ggsave()]. These arguments override the default figure
#'   dimensions.
#'
#' @return A named list of `ggplot` objects, with one plot for each
#'   interval type specified in `interval_type`. If `path` is provided,
#'   the plots are also saved as PNG files.
#'
plot_rli_year_results <- function(
  x,
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

    plot_data <- x %>%
      filter_out(.data$int_type != interval) %>%
      left_join(
        effects_df %>%
          dplyr::filter(.data$int_type == interval) %>%
          distinct(.data$trait_value, .data$effect_code, .data$effect),
        by = join_by("trait_value"),
        relationship = "many-to-many"
      ) %>%
      mutate(
        trait_value = paste0(.data$trait_value, " (n = ", .data$n_spec, ")")
      )

    # Set position adjustment for estimates and confidence intervals.
    pd <- position_dodge(width = 1.5)

    # Create the RLI plot.
    p <- ggplot(
      plot_data,
      aes(
        x = .data$Year,
        y = .data$est_original,
        group = .data$trait_value,
        colour = .data$trait_value
      )
    ) +

      # Add RLI estimates.
      geom_point(
        size = 3,
        position = pd
      ) +

      # Add confidence intervals.
      geom_errorbar(
        aes(
          ymin = .data$ll,
          ymax = .data$ul,
          colour = .data$trait_value,
          group = .data$trait_value
        ),
        width = 2,
        linewidth = 1,
        position = pd
      ) +

      # Connect RLI estimates between years.
      geom_line(
        linewidth = 1,
        position = pd
      ) +

      # Add effect classifications at the end of each line.
      ggrepel::geom_label_repel(
        data = plot_data %>%
          dplyr::filter(.data$Year == max(plot_data$Year)),
        aes(
          x = .data$Year + 2,
          y = .data$est_original,
          label = as.character(.data$effect_code),
          fill = .data$trait_value
        ),
        colour = "white",
        box.padding = 0,
        label.padding = unit(0.2, "lines"),
        label.r = 0.5,
        direction = "y",
        hjust = "center",
        vjust = "center",
        segment.color = NA,
        show.legend = FALSE
      ) +

      labs(
        x = "Year",
        y = "Red List Index",
        colour = unique(plot_data$trait),
        title = unique(plot_data$trait),
        subtitle = paste(interval, "bootstrap intervals")
      ) +

      # Set y-axis limits.
      ylim(NA, 1) +

      # Set x-axis limits and breaks.
      scale_x_continuous(
        breaks = c(2010, 2025),
        expand = expansion(mult = c(0.05, 0.1))
      )

    out[[i]] <- p

    # Save the plot if a path was provided.
    if (!is.na(path)) {
      # Create the output directory if it does not exist.
      dir.create(
        path,
        showWarnings = FALSE,
        recursive = TRUE
      )

      # Construct the output filename.
      file <- file.path(
        path,
        paste0(
          "rli_",
          unique(x$trait),
          "_",
          interval,
          ".png"
        )
      )

      # Combine defaults with user-supplied ggsave arguments.
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
