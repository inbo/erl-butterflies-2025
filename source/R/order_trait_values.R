#' Order trait values for RLI effect plots
#'
#' Orders the `trait_value` column according to predefined meaningful
#' categories for selected traits. For traits without a predefined order,
#' values are ordered by decreasing estimated RLI change (`est_original`).
#'
#' @param x A data frame containing at least the columns `trait_value` and
#'   `est_original`.
#' @param trait A character string specifying the trait. The following traits
#'   have a predefined order: `"Elevation"`, `"OverwinteringStage"`,
#'   `"RangeSize"`, `"Specialisation"`, `"SpeciesTemperatureIndex"`, and
#'   `"Wingspan"`.
#'
#' @return The input data frame with `trait_value` reordered. For traits with
#'   a predefined order, `trait_value` is an ordered factor. For other traits,
#'   `trait_value` is reordered according to decreasing `est_original`.
#'
order_trait_values <- function(x, trait) {
  require("dplyr")
  require("rlang")

  if (trait == "Elevation") {
    x %>%
      mutate(
        trait_value = factor(
          .data$trait_value,
          levels = c(
            "Lowland",
            "Intermediate",
            "Upland"
          ),
          ordered = TRUE
        )
      )
  } else if (trait == "OverwinteringStage") {
    x %>%
      mutate(
        trait_value = factor(
          .data$trait_value,
          levels = c(
            "Egg",
            "Caterpillar",
            "Pupa",
            "Adult"
          ),
          ordered = TRUE
        )
      )
  } else if (trait == "RangeSize") {
    x %>%
      mutate(
        trait_value = factor(
          .data$trait_value,
          levels = c(
            "VerySmall",
            "Small",
            "Large",
            "VeryLarge"
          ),
          ordered = TRUE
        )
      )
  } else if (trait == "Specialisation") {
    x %>%
      mutate(
        trait_value = factor(
          .data$trait_value,
          levels = c(
            "Monophagous",
            "Oligophagous",
            "Polyphagous"
          ),
          ordered = TRUE
        )
      )
  } else if (trait == "SpeciesTemperatureIndex") {
    x %>%
      mutate(
        trait_value = factor(
          .data$trait_value,
          levels = c(
            "VeryCold",
            "Cold",
            "Warm",
            "VeryWarm"
          ),
          ordered = TRUE
        )
      )
  } else if (trait == "Wingspan") {
    x %>%
      mutate(
        trait_value = factor(
          .data$trait_value,
          levels = c(
            "VerySmall",
            "Small",
            "Large",
            "VeryLarge"
          ),
          ordered = TRUE
        )
      )
  } else {
    x %>%
      mutate(
        trait_value = reorder(
          .data$trait_value,
          -.data$est_original
        )
      )
  }
}
