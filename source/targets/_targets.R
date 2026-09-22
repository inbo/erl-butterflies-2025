# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(dplyr)
library(readr)

# Set target options:
tar_option_set(
  packages = c("dplyr"),
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  # Load data
  tar_target(
    name = traits_file,
    command = "./data/tblTrait.csv",
    format = "file"
  ),
  tar_target(
    name = red_list_file,
    command = "./data/tblRLCEurope20102025.csv",
    format = "file"
  ),
  tar_target(
    name = traits_data,
    command = read_csv(traits_file, show_col_types = FALSE)
  ),
  tar_target(
    name = red_list_data,
    command = read_csv(red_list_file, show_col_types = FALSE)
  )
)
