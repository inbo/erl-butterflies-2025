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
  tar_read_rli(
    name = "rli_change_effects",
    trait = x
  )
})
names(rli_change_effects) <- traits
