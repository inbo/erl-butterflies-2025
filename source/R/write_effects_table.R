#' Write RLI change results to a CSV file
#'
#' Writes a data frame containing Red List Index change results to a CSV
#' file. Selected numerical result columns are rounded to the specified
#' number of decimal places. The output directory is created if it does
#' not already exist.
#'
#' @param x A data frame containing the RLI change results. The data frame
#'   must contain a `trait` column with a single unique trait.
#' @param path A character string giving the directory in which to save
#'   the CSV file. The directory is created if it does not exist.
#' @param digits An integer giving the number of decimal places to use
#'   when rounding the RLI estimates, bootstrap statistics, and confidence
#'   interval limits. Defaults to `3`.
#' @param ... Additional arguments passed to [readr::write_csv()].
#'
#' @return A character string giving the path to the written CSV file.
#'
write_effects_table <- function(
  x,
  path,
  digits = 3,
  ...
) {
  # Check that exactly one trait is present.
  trait <- unique(x$trait)

  if (length(trait) != 1) {
    stop("`x` must contain exactly one unique value in `trait`.")
  }

  # Round the RLI estimates, bootstrap statistics, and confidence
  # interval limits.
  round_cols <- c(
    "est_original",
    "mean_boot",
    "median_boot",
    "se_boot",
    "bias_boot",
    "ll",
    "ul"
  )

  x <- x %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(round_cols),
        \(x) round(x, digits = digits)
      )
    )

  # Create the output directory if it does not exist.
  dir.create(
    path,
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Construct the output filename from the trait name.
  file <- file.path(
    path,
    paste0(
      "rli_change_results_",
      trait,
      ".csv"
    )
  )

  # Write the results to a CSV file.
  readr::write_csv(
    x = x,
    file = file,
    ...
  )

  # Return the path to the written file.
  return(file)
}
