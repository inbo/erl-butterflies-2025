# This script creates the figure of the change in RLI for the paper
# The Red List Index of European butterflies: Extinction risk is trait-dependent

## Load packages
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

###############################################################################
##
## Overall plots
##
###############################################################################
overall_trait_data <- tar_read_rli(
  "single_trait_data_joined",
  trait = "Overall"
)

plot_trait_data <- overall_trait_data %>%
  mutate(
    rl_desc = factor(
      case_when(
        RLC == "DD" ~ "Data Deficient",
        RLC == "LC" ~ "Least Concern",
        RLC == "NT" ~ "Near Threatened",
        RLC == "VU" ~ "Vulnerable",
        RLC == "EN" ~ "Endangered",
        RLC == "CR" ~ "Critically Endangered",
        RLC == "RE" ~ "Regionally Extinct",
        RLC == "EX" ~ "Globally Extinct"
      ),
      levels = c(
        "Globally Extinct",
        "Regionally Extinct",
        "Critically Endangered",
        "Endangered",
        "Vulnerable",
        "Near Threatened",
        "Least Concern",
        "Data Deficient"
      )
    )
  ) %>%
  mutate(year_num = as.integer(gsub("y", "", Year))) %>%
  count(rl_desc, year_num) %>%
  mutate(n_year = sum(n), .by = "year_num") %>%
  mutate(prop = 100 * (n / n_year))

p_bar_overall <- plot_trait_data %>%
  ggplot(aes(x = factor(year_num), y = prop, fill = rl_desc)) +
  geom_col(position = "fill", colour = "grey") +
  labs(x = "", y = "Proportion", fill = "Red List Category") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Globally Extinct" = "black",
      "Regionally Extinct" = "darkgrey",
      "Critically Endangered" = "darkred",
      "Endangered" = "orange",
      "Vulnerable" = "yellow",
      "Near Threatened" = "lightyellow",
      "Least Concern" = "darkgreen",
      "Data Deficient" = "steelblue"
    )
  )

save_figure(
  p_bar_overall,
  file_name = "erl_cat_proportions",
  path = "output/figures/paper",
  devices = devices,
  width = 8,
  height = 5
)

x <- tar_read_rli(
  "rli_df",
  trait = "Overall"
)
effects_df <- tar_read_rli(
  "rli_change_effects",
  trait = "Overall"
)
interval <- "bca"

# Get trait and order trait values
trait_char <- unique(x$trait)
x <- order_trait_values(
  x,
  trait = trait_char
)
effects_df <- order_trait_values(
  effects_df,
  trait = trait_char
)
plot_overall_data <- x %>%
  filter_out(int_type != interval) %>%
  left_join(
    effects_df %>%
      dplyr::filter(int_type == interval) %>%
      distinct(trait_value, effect_code, effect),
    by = join_by("trait_value"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    label = paste0(trait_value, " (n = ", n_spec, ")")
  ) %>%
  mutate(
    label = factor(
      label,
      levels = unique(label[
        order(as.integer(trait_value))
      ])
    )
  )
label_data <- plot_data %>%
  summarise(
    x = mean(Year),
    y = mean(est_original),
    effect_code = unique(effect_code)
  )

# Create the RLI plot
p_year_overall <- ggplot(
  plot_overall_data,
  aes(
    x = Year,
    y = est_original
  )
) +
  # Connect RLI estimates between years.
  geom_line(
    linewidth = 1,
    colour = "darkgrey",
    linetype = "dashed"
  ) +

  # Add RLI estimates.
  geom_point(
    size = 3.5
  ) +

  # Add confidence intervals.
  geom_errorbar(
    aes(
      ymin = ll,
      ymax = ul
    ),
    width = 1.5,
    linewidth = 1
  ) +

  # Add effect classifications at the end of each line.
  geom_label(
    data = label_data,
    aes(
      x = x,
      y = y + 0.02,
      label = as.character(effect_code)
    ),
    colour = "white",
    fill = "#6D0000",
    label.padding = unit(0.4, "lines"),
    label.r = unit(0.7, "lines"),
    hjust = "center",
    vjust = "center",
    show.legend = FALSE
  ) +

  labs(
    x = "",
    y = "Red List Index"
  ) +

  # Set y-axis limits.
  ylim(NA, 1) +

  # Set x-axis limits and breaks.
  scale_x_continuous(
    breaks = c(2010, 2025),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  theme(panel.grid.minor.x = element_blank())

save_figure(
  p_year_overall,
  file_name = "rli_year_overall",
  path = "output/figures/paper",
  devices = devices,
  width = 5,
  height = 4
)

# Grid of plots
figure_overall <- cowplot::plot_grid(
  plotlist = list(p_bar_overall, p_year_overall),
  labels = c("A.", "B."),
  ncol = 2,
  align = "h",
  label_size = 12,
  label_fontface = "bold",
  rel_widths = c(1.7, 1),
  hjust = 0,
  vjust = 1
) +
  theme(
    plot.margin = margin(
      t = 10,
      r = 0,
      b = 0,
      l = 10
    )
  )

save_figure(
  figure_overall,
  file_name = "erl_overall",
  path = "output/figures/paper",
  devices = devices,
  width = 8,
  height = 4
)

###############################################################################
##
## Difference in the mean Red List Index between 2010 and 2025 per trait
##
###############################################################################
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
      "10.5281/zenodo.23164000",
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
        axis.text.y = element_text(size = 7),

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
  theme(legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size = 9))
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
  devices = devices,
  width = 210,
  height = 297,
  units = "mm"
)
