#' Convert bootstrap confidence intervals to a data frame
#'
#' Extracts bootstrap confidence intervals from a `boot.ci` object and
#' returns them in a tidy data frame. The interval types are obtained
#' from the `type` argument stored in the object's call.
#'
#' @param x An object returned by [boot::boot.ci()].
#'
#' @return A data frame with one row per confidence interval type and
#'   the following columns:
#'   \describe{
#'     \item{est_original}{The original statistic before bootstrapping.}
#'     \item{int_type}{The type of bootstrap confidence interval.}
#'     \item{ll}{The lower confidence limit.}
#'     \item{ul}{The upper confidence limit.}
#'     \item{conf}{The confidence level used to calculate the interval.}
#'   }
#'
bootci_to_dataframe <- function(
  boot_obj,
  bootci_obj
) {
  # Get the requested confidence interval types from the original call
  patterns <- eval(bootci_obj$call$type)

  # Identify the elements containing the requested confidence intervals.
  # Some interval types have names with additional suffixes, so match
  # names based on their starting pattern.
  interval_types <- names(bootci_obj)[
    vapply(
      names(bootci_obj),
      \(x) any(startsWith(x, patterns)),
      logical(1)
    )
  ]

  # Get confidence interval info
  if (!is.null(bootci_obj)) {
    int_type <- interval_types
    conf <- bootci_obj$call$conf
    ll <- sapply(
      unname(bootci_obj[names(bootci_obj) %in% interval_types]),
      function(x) x[length(x) - 1]
    )
    ul <- sapply(
      unname(bootci_obj[names(bootci_obj) %in% interval_types]),
      function(x) x[length(x)]
    )
  } else {
    int_type <- conf <- ll <- ul <- NA
  }

  # Extract the original estimate and confidence limits for each interval
  # type and combine them into a data frame.
  data.frame(
    trait = unique(boot_obj$data$Trait),
    trait_value = unique(boot_obj$data$TraitValue),
    n_spec = nrow(boot_obj$data),
    est_original = boot_obj$t0,
    mean_boot = mean(boot_obj$t),
    median_boot = median(boot_obj$t),
    se_boot = sd(boot_obj$t),
    bias_boot = mean(boot_obj$t) - boot_obj$t0,
    int_type = int_type,
    conf = conf,
    ll = ll,
    ul = ul
  )
}
