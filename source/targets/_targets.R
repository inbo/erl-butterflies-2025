# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(dplyr)
library(tidyr)
library(readr)

# Set target options:
tar_option_set(
  packages = c("dplyr")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("./source/R")

# Replace the target list below with your own:
list(
  # Read raw data
  ## Trait data
  tar_target(
    name = traits_file,
    command = "./data/tblTrait.csv",
    format = "file"
  ),
  tar_target(
    name = traits_data,
    command = read_csv(traits_file, show_col_types = FALSE)
  ),
  ## Red list data
  tar_target(
    name = red_list_file,
    command = "./data/tblRLCEurope20102025.csv",
    format = "file"
  ),
  tar_target(
    name = red_list_data,
    command = read_csv(red_list_file, show_col_types = FALSE)
  ),

  # Prepare analysis datasets
  ## Prepare traits data
  tar_target(
    name = traits_data_filtered,
    command = traits_data %>%
      dplyr::filter(nYears >= 2) %>%
      select("Speciesname", "Trait", "TraitValue")
  ),
  ## Prepare red list data
  tar_target(
    name = red_list_data_filtered,
    command = red_list_data %>%
      filter(Year != "y1999") %>%
      filter(!is.na(RLC))
  ),

  # Map over every trait
  tar_map(
    values = list(
      trait_map = c(
        "BiotopePreference"
      )
    ),

    # Prepare branching over each trait dataset
    tar_target(
      name = single_trait_data,
      command = traits_data_filtered %>%
        dplyr::filter(Trait == trait_map)
    ),
    tar_group_by(
      name = single_trait_data_grouped,
      command = single_trait_data,
      TraitValue
    ),

    # Prepare analysis datasets
    tar_target(
      name = single_trait_data_joined,
      command = left_join(
        single_trait_data_grouped,
        red_list_data_filtered,
        by = "Speciesname"
      ),
      pattern = map(single_trait_data_grouped)
    ),
    tar_target(
      name = analysis_data_wide,
      command = single_trait_data_joined %>%
        dplyr::filter(
          TraitValue != "Range extends outside Palearctic and Holarctic"
        ) %>%
        dplyr::filter(!is.na(RLC)) %>%
        pivot_wider(
          id_cols = c(Speciesname, Trait, TraitValue),
          names_from = Year,
          values_from = RLC,
          names_prefix = "RLC_"
        ) %>%
        select("Speciesname", "Trait", "TraitValue", "RLC_y2010", "RLC_y2025"),
      pattern = map(single_trait_data_joined)
    ),
    tar_target(
      name = red_list_scores,
      command = c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5)
    ),

    # Calculate red list indices
    tar_target(
      name = rli_change_20102025,
      command = calculate_rli_change(
        analysis_data_wide,
        rli_scores = red_list_scores,
        max_score = 5
      ),
      pattern = map(analysis_data_wide)
    ),
    tar_target(
      name = rli_change_df,
      command = analysis_data_wide %>%
        distinct(Trait, TraitValue) %>%
        mutate(rli = rli_change_20102025),
      pattern = map(analysis_data_wide, rli_change_20102025)
    ),
    tar_target(
      name = rli_df,
      command = analysis_data_wide %>%
        distinct(Trait, TraitValue) %>%
        mutate(
          rli_2010 = calculate_rli(
            analysis_data_wide,
            rli_scores = red_list_scores,
            col = "RLC_y2010",
            max_score = 5
          ),
          rli_2025 = calculate_rli(
            analysis_data_wide,
            rli_scores = red_list_scores,
            col = "RLC_y2025",
            max_score = 5
          )
        ) %>%
        pivot_longer(
          cols = starts_with("rli_"),
          names_to = "year",
          names_prefix = "rli_",
          values_to = "rli"
        ),
      pattern = map(analysis_data_wide, rli_change_20102025)
    )
  )
)
