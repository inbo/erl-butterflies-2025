## Load packages
library(targets)
library(tidyverse)
library(INBOtheme)

theme_set(theme_inbo(transparent = TRUE))
conflicted::conflicts_prefer(dplyr::filter)

## Globals
devices <- c("png", "tiff", "eps") # Devices to export
# Function to save figures using multiple devices
source(file.path("source", "R", "save_figure.R"))
# Function to read data from a targets pipeline
source(file.path("source", "R", "tar_read_rli.R"))

reference <- 0
threshold <- 0.02


## Load data
traits <- c(
  "BiotopePreference",
  "Elevation",
  "GlobalDistribution",
  "HostPlantType",
  "OverwinteringStage",
  "RangeSize",
  "Specialisation",
  "SpeciesTemperatureIndex",
  "Voltinism",
  "Wingspan",
  "Overall"
)
rli_change_effects <- lapply(traits, function(x) {
  df <- tar_read_rli(
    name = "rli_change_effects",
    trait = x
  )
  df %>%
    filter(.data$int_type == "bca") %>%
    mutate(
      trait_label = paste0(
        .data$trait_value,
        " (n = ",
        .data$n_spec,
        ")"
      )
    )

})
names(rli_change_effects) <- traits

## Create figures
plots <- lapply(rli_change_effects, function(x) {
  x %>%
    ggplot(
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
    geom_hline(
      yintercept = reference,
      linetype = "longdash",
      colour = "black",
      linewidth = 0.5
    ) +
    geom_hline(
      yintercept = c(-1, 1) * threshold,
      linetype = "dotdash",
      linewidth = 0.5
    ) +
    geom_errorbar(
      linewidth = 0.8,
      colour = "darkgrey"
    ) +
    effectclass::stat_effect(
      aes(
        ymin = .data$ll,
        ymax = .data$ul
      ),
      reference = 0,
      threshold = 0.02,
      size = 5,
      ref_line = "none"
    ) +
    coord_flip() +
    labs(
      x = "",
      y = "Difference in Red List Index\nbetween 2025 and 2010",
      title = stringr::str_to_sentence(
        stringr::str_replace_all(
          unique(x$trait),
          "([a-z])([A-Z])",
          "\\1 \\2"
        )
      )
    ) +
    scale_x_discrete(
      labels = \(x) stringr::str_wrap(x, width = 20)
    ) +
    scale_y_continuous(
      limits = c(-0.35, 0.05),
      breaks = seq(-0.3, 0.05, by = 0.05),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",

      # Text
      plot.title = element_text(size = 11, face = "bold"),
      axis.title.x = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),

      # Spacing
      plot.margin = margin(5, 5, 5, 5),
      panel.grid.minor = element_blank()
    )
})

figure <- cowplot::plot_grid(
  plotlist = plots[-length(plots)],
  labels = paste0(
    LETTERS[seq_along(plots[-length(plots)])],
    "."
  ),
  ncol = 2,
  align = "hv",
  label_size = 10,
  label_fontface = "bold",
  label_x = 0.015,
  label_y = 0.99,
  hjust = 0,
  vjust = 1
)

figure_test <- cowplot::ggdraw(figure) +
  theme(
    plot.margin = margin(
      t = 15,
      r = 10,
      b = 10,
      l = 10
    )
  )

save_figure(
  figure_test,
  file_name = "rli_change_effects",
  path = "output/figures",
  devices = "jpg",
  width = 210,
  height = 297,
  units = "mm"
)
