# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(dplyr)
library(tidyr)
library(readr)

# Set target options:
tar_option_set(
  packages = c("boot", "ggplot2", "dplyr", "tidyr")
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
      mutate(
        TraitValue = case_when(
          TraitValue == "VeryLow" ~ "Lowland",
          TraitValue == "Low" ~ "Lowland",
          TraitValue == "High" ~ "Upland",
          TraitValue == "VeryHigh" ~ "Upland",
          TraitValue == "PartialBivoltine" ~ "Bivoltine",
          TRUE ~ TraitValue
        )
      ) %>%
      distinct(Speciesname, Trait, TraitValue)
  ),
  ## Add overall trait
  tar_target(
    name = traits_data_full,
    command = traits_data_filtered %>%
      bind_rows(
        traits_data_filtered %>%
          mutate(
            Trait = "Overall",
            TraitValue = "All"
          ) %>%
          distinct()
      )
  ),
  ## Prepare red list data
  tar_target(
    name = red_list_data_filtered,
    command = red_list_data %>%
      dplyr::filter(Year != "y1999") %>%
      dplyr::filter(!is.na(RLC))
  ),

  # Map over every trait
  tar_map(
    values = list(
      trait_map = c(
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
    ),

    # Prepare grouping over each trait dataset
    tar_target(
      name = single_trait_data,
      command = traits_data_full %>%
        dplyr::filter(Trait == trait_map) %>%
        dplyr::filter(
          TraitValue != "Range extends outside the Palaearctic and Holarctic"
        )
    ),
    tar_group_by(
      name = single_trait_data_grouped,
      command = single_trait_data,
      TraitValue
    ),

    # Prepare analysis datasets
    ## Join trait and red list datasets
    tar_target(
      name = single_trait_data_joined,
      command = left_join(
        single_trait_data_grouped,
        red_list_data_filtered,
        by = "Speciesname"
      ),
      pattern = map(single_trait_data_grouped)
    ),
    ## Clean and pivot data
    tar_target(
      name = analysis_data_wide,
      command = single_trait_data_joined %>%
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
    ## Define red list scores
    tar_target(
      name = red_list_scores,
      command = c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5)
    ),

    # Calculate bootstrap confidence intervals
    ## Bootstrapping RLI 2010
    tar_target(
      name = rli_2010_boot,
      command = bootstrap_rli(
        x = analysis_data_wide,
        f = calculate_rli,
        rli_scores = red_list_scores,
        col = "RLC_y2010",
        max_score = 5,
        bootstrap_samples = 10000,
        seed = 123
      ),
      pattern = map(analysis_data_wide),
      iteration = "list"
    ),
    tar_target(
      name = rli_2010_boot_df,
      command = boot_to_dataframe(
        rli_2010_boot
      ),
      pattern = map(rli_2010_boot)
    ),

    ## Bootstrapping RLI 2025
    tar_target(
      name = rli_2025_boot,
      command = bootstrap_rli(
        x = analysis_data_wide,
        f = calculate_rli,
        rli_scores = red_list_scores,
        col = "RLC_y2025",
        max_score = 5,
        bootstrap_samples = 1000,
        seed = 123
      ),
      pattern = map(analysis_data_wide),
      iteration = "list"
    ),
    tar_target(
      name = rli_2025_boot_df,
      command = boot_to_dataframe(
        rli_2025_boot
      ),
      pattern = map(rli_2025_boot)
    ),

    ## Bootstrapping change in RLI
    tar_target(
      name = rli_change_boot,
      command = bootstrap_rli(
        x = analysis_data_wide,
        f = calculate_rli_change,
        rli_scores = red_list_scores,
        max_score = 5,
        bootstrap_samples = 1000,
        seed = 123
      ),
      pattern = map(analysis_data_wide),
      iteration = "list"
    ),
    tar_target(
      name = rli_change_boot_df,
      command = boot_to_dataframe(
        rli_change_boot
      ),
      pattern = map(rli_change_boot)
    ),

    ## Confidence interval calculation RLI 2010
    tar_target(
      name = rli_2010_boot_ci,
      command = boot::boot.ci(
        boot.out = rli_2010_boot,
        conf = 0.95,
        type = c("norm", "perc", "bca")
      ),
      pattern = map(rli_2010_boot),
      iteration = "list"
    ),
    tar_target(
      name = rli_2010_df,
      command = bootci_to_dataframe(
        boot_obj = rli_2010_boot,
        bootci_obj = rli_2010_boot_ci
      ) %>%
        mutate(Year = 2010),
      pattern = map(rli_2010_boot, rli_2010_boot_ci)
    ),

    ## Confidence interval calculation RLI 2025
    tar_target(
      name = rli_2025_boot_ci,
      command = boot::boot.ci(
        boot.out = rli_2025_boot,
        conf = 0.95,
        type = c("norm", "perc", "bca")
      ),
      pattern = map(rli_2025_boot),
      iteration = "list"
    ),
    tar_target(
      name = rli_2025_df,
      command = bootci_to_dataframe(
        boot_obj = rli_2025_boot,
        bootci_obj = rli_2025_boot_ci
      ) %>%
        mutate(Year = 2025),
      pattern = map(rli_2025_boot, rli_2025_boot_ci)
    ),

    ## Combine RLI results
    tar_target(
      name = rli_df,
      command = bind_rows(
        rli_2010_df,
        rli_2025_df
      ),
      pattern = map(rli_2010_df, rli_2025_df)
    ),

    ## Confidence interval calculation change in RLI
    tar_target(
      name = rli_change_boot_ci,
      command = boot::boot.ci(
        boot.out = rli_change_boot,
        conf = 0.95,
        type = c("norm", "perc", "bca")
      ),
      pattern = map(rli_change_boot),
      iteration = "list"
    ),
    tar_target(
      name = rli_change_df,
      command = bootci_to_dataframe(
        boot_obj = rli_change_boot,
        bootci_obj = rli_change_boot_ci
      ),
      pattern = map(rli_change_boot, rli_change_boot_ci)
    ),

    # Visualise bootstrap results
    tar_target(
      name = plot_rli_change_boot,
      command = plot_bootstrap_results(
        bootstrap_replicates = rli_change_boot_df,
        bootstrap_intervals = rli_change_df,
        path = "./output/figures/bootstrap_results",
        ggsave_args = list(
          dpi = 150
        )
      )
    ),

    # Effect classification
    tar_target(
      name = rli_change_effects,
      command = add_effect_classification(
        rli_change_df,
        cl_columns = c("ll", "ul"),
        threshold = 0.02,
        reference = 0,
        coarse = FALSE
      )
    ),
    tar_target(
      name = rli_change_effects_path,
      command = write_effects_table(
        rli_change_effects,
        path = "./output/tables",
        digits = 6
      )
    ),

    # Visualisation
    ## RLI by year
    tar_target(
      name = vis_rli_year_results,
      command = plot_rli_year_results(
        rli_df,
        effects_df = rli_change_effects,
        interval_type = c("bca", "percent"),
        path = "./output/figures/rli_year_results",
        ggsave_args = list(
          dpi = 150,
          width = 6,
          height = 4,
          units = "in"
        )
      )
    ),
    ## RLI change
    tar_target(
      name = vis_rli_change_results,
      command = plot_rli_change_results(
        effects_df = rli_change_effects,
        interval_type = c("bca", "percent"),
        path = "./output/figures/rli_change_results",
        ggsave_args = list(
          dpi = 150,
          width = 8,
          height = 4,
          units = "in"
        )
      )
    )
  )
)
