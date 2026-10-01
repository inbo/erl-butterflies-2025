# This script creates the figure of the change in RLI for the paper
# The Red List Index of European butterflies: Extinction risk is trait-dependent

## Load packages
library(targets)
library(tidyverse)
library(INBOtheme)

theme_set(theme_inbo(transparent = TRUE))

## Globals
devices <- c("png", "jpg", "pdf", "svg") # Devices to export
# Function to save figures using multiple devices
source(file.path("source", "R", "save_figure.R"))
# Function to read data from a targets pipeline
source(file.path("source", "R", "tar_read_rli.R"))
# Functions to order trait values
source(file.path("source", "R", "order_trait_values.R"))
# Effect classification
reference <- 0
threshold <- 0.02


## Load data
traits <- c(
  "Overall",
  "GlobalDistribution",
  "RangeSize",
  "SpeciesTemperatureIndex",
  "TemperatureRange",
  "BiotopePreference",
  "Specialisation",
  "HostPlantType",
  "Voltinism",
  "OverwinteringStage",
  "Wingspan",
  "Elevation"
)
rli_change_effects_raw <- lapply(traits, function(t) {
  # Create file name
  path <- "output/tables"
  file <- paste0("rli_change_results_", t, ".csv")

  # Get CSV file with results
  if (file.exists(file.path(path, file))) {
    read_csv(
      file.path(path, file),
      show_col_types = FALSE
    )
  } else {
    zen4R::download_zenodo(
      "10.5281/zenodo.22940460",
      path = path,
      files = list(file)
    )
    read_csv(
      file.path(path, file),
      show_col_types = FALSE
    )
  }
})
names(rli_change_effects_raw) <- traits

## Data preparation
rli_change_effects <- lapply(rli_change_effects_raw, function(x) {
  # Order trait values
  trait <- unique(x$trait)
  df <- order_trait_values(x, trait)

  # Calculate labels
  df %>%
    dplyr::filter(.data$int_type == "bca") %>%
    mutate(
      trait_clean = stringr::str_to_sentence(
        stringr::str_replace_all(
          as.character(.data$trait_value),
          "([a-z])([A-Z])",
          "\\1 \\2"
        )
      ),
      trait_label = paste0(
        .data$trait_clean,
        " (n = ",
        .data$n_spec,
        ")"
      )
    ) %>%
    mutate(
      trait_label = factor(
        .data$trait_label,
        levels = .data$trait_label[
          order(as.integer(.data$trait_value))
        ],
        ordered = TRUE
      )
    )
})

## Create figures
plots <- lapply(
  seq_along(rli_change_effects),
  function(i) {
    # Get dataframe
    x <- rli_change_effects[[i]]

    # Get axis label size
    if (length(unique(x$trait_value)) > 4) {
      label_size <- 5
    } else if (length(unique(x$trait_value)) < 4) {
      label_size <- 7
    } else {
      label_size <- 6
    }

    x %>%
      ggplot(
        aes(
          y = .data$est_original,
          x = .data$trait_label,
          ymin = .data$ll,
          ymax = .data$ul
        )
      ) +
      geom_hline(
        yintercept = reference,
        linewidth = 0.5
      ) +
      geom_hline(
        yintercept = c(-1, 1) * threshold,
        linetype = "dotdash",
        linewidth = 0.5
      ) +
      effectclass::stat_effect(
        reference = 0,
        threshold = 0.02,
        size = 4,
        ref_line = "none"
      ) +
      coord_flip() +
      labs(
        x = "",
        y = if (i %in% (length(traits) - 1):length(traits)) {
          "Difference in Red List Index\nbetween 2025 and 2010"
        } else {
          ""
        },
        title = stringr::str_to_sentence(
          stringr::str_replace_all(
            unique(x$trait),
            "([a-z])([A-Z])",
            "\\1 \\2"
          )
        )
      ) +
      scale_y_continuous(
        limits = c(-0.31, 0.05),
        breaks = seq(-0.5, 0.05, by = 0.05),
        labels = scales::label_number(accuracy = 0.01)
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",

        # Text
        plot.title = element_text(size = 11),
        axis.title.x = element_text(
          size = 9,
          margin = margin(t = 10)
        ),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = label_size),

        # Spacing
        plot.margin = margin(0, 0, 0, 0)
      )
  }
)
plots

# Extract the legend from the plot
p <- bind_rows(rli_change_effects_raw) %>%
  ggplot() +
  effectclass::stat_effect(
    aes(
      y = .data$est_original,
      x = .data$trait_value,
      ymin = .data$ll,
      ymax = .data$ul
    ),
    reference = 0,
    threshold = 0.02,
    show.legend = TRUE
  ) +
  labs(colour = "") +
  theme(legend.position = "bottom")
legend <- cowplot::get_legend(p)


# Remove legends from individual plots
plots <- lapply(
  plots,
  \(x) x + theme(legend.position = "none")
)

# Grid of plots
figure <- cowplot::plot_grid(
  plotlist = plots,
  labels = paste0(
    LETTERS[seq_along(plots)],
    "."
  ),
  ncol = 2,
  align = "hv",
  label_size = 12,
  label_fontface = "bold",
  label_x = 0.015,
  label_y = 0.99,
  hjust = 0,
  vjust = 1
) +
  theme(
    plot.margin = margin(
      t = 15,
      r = 10,
      b = 10,
      l = 10
    )
  )

# Put the legend above the grid
figure_final <- cowplot::plot_grid(
  figure,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
) +
  theme(
    plot.margin = margin(
      t = 0,
      r = 10,
      b = 15,
      l = 10
    )
  )

save_figure(
  figure_final,
  file_name = "rli_change_effects",
  path = "output/figures/paper",
  devices = "jpg", #devices,
  width = 210,
  height = 297,
  units = "mm"
)
