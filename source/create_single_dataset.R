library(tidyverse)

# Trait data
files <- list.files(
  "./data",
  pattern = "^tblTrait.+\\.csv$",
  full.names = TRUE
)

tbl_trait <- files %>%
  map_dfr(\(file) {
    read_delim(
      file,
      delim = ";",
      show_col_types = FALSE
    )
  }) %>%
  mutate(
    TraitValue = if_else(
      Trait == "RangeSize",
      EoOValue,
      TraitValue
    )
  ) %>%
  select(
    Speciesname = SpeciesnameFull,
    Trait,
    TraitValue
  )

write_csv(tbl_trait, "./data/tblTrait.csv")

# Red list data
tbl_rle <- read_delim(
  "./data/tblRLCEurope20102025_short.csv",
  delim = ";",
  show_col_types = FALSE
) %>%
  select(
    Speciesname,
    RLC,
    Year,
    GlobalRange
  )

write_csv(tbl_rle, "./data/tblRLCEurope20102025.csv")
