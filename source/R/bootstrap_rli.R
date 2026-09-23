#' Bootstrap a Red List Index statistic
#'
#' Performs non-parametric bootstrap resampling of a data frame and
#' calculates a Red List Index statistic for each bootstrap sample.
#' The statistic can be any function that accepts the resampled data
#' as its first argument, such as [calculate_rli()] or
#' [calculate_rli_change()].
#'
#' @param x A data frame containing the Red List assessments to be
#'   resampled.
#' @param f A function used to calculate the statistic of interest.
#'   The function must accept `x` as its first argument. For example,
#'   [calculate_rli()] or [calculate_rli_change()].
#' @param ... Additional arguments passed to `f`.
#' @param bootstrap_samples An integer giving the number of bootstrap
#'   samples to generate.
#' @param seed An optional integer used to initialise the random number
#'   generator. Defaults to `NA`, in which case no seed is set.
#'
#' @return An object of class `"boot"` containing the bootstrap
#'   replicates and the original statistic.
#'
#' @examples
#' rli_scores <- c(
#'   LC = 0,
#'   NT = 1,
#'   VU = 2,
#'   EN = 3,
#'   CR = 4,
#'   EX = 5
#' )
#'
#' data <- data.frame(
#'   RLC_y2010 = c("LC", "VU", "EN", "LC"),
#'   RLC_y2025 = c("LC", "NT", "EN", "VU")
#' )
#'
#' # Bootstrap the RLI for 2010
#' bootstrap_rli(
#'   x = data,
#'   f = calculate_rli,
#'   rli_scores = rli_scores,
#'   col = "RLC_y2010",
#'   bootstrap_samples = 1000,
#'   seed = 123
#' )
#'
#' # Bootstrap the change in RLI between 2010 and 2025
#' bootstrap_rli(
#'   x = data,
#'   f = calculate_rli_change,
#'   rli_scores = rli_scores,
#'   ref_col = "RLC_y2010",
#'   diff_col = "RLC_y2025",
#'   bootstrap_samples = 1000,
#'   seed = 123
#' )
#'
bootstrap_rli <- function(
  x,
  f,
  ...,
  bootstrap_samples,
  seed = NA
) {
  require("boot")

  # Set seed
  if (!is.na(seed)) {
    set.seed(seed)
  }

  # Define the function applied to each bootstrap sample
  bootstrap_function <- function(data, indices) {
    sample <- data[indices, ]
    f(sample, ...)
  }

  # Generate bootstrap samples
  boot <- boot::boot(
    data = x,
    statistic = bootstrap_function,
    R = bootstrap_samples
  )

  return(boot)
}
