library(tidyverse)

files <- list.files(
  "./data",
  pattern = "^tblTrait.+\\.csv$",
  full.names = TRUE
)

tblTrait <- files %>%
  map_dfr(\(file) read_delim(
    file,
    delim = ";",
    show_col_types = FALSE
  )) %>%
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
    TraitValue,
    nYears
  )

write_csv(tblTrait, "./data/tblTrait.csv")
