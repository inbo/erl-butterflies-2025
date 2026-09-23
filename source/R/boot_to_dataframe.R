#' Convert bootstrap results to a data frame
#'
#' Converts an object returned by [boot::boot()] into a data frame
#' containing the individual bootstrap replicates and summary statistics.
#' Information on the trait and trait value is also included when these
#' columns are present in the bootstrap input data.
#'
#' @param boot_obj An object returned by [boot::boot()].
#'
#' @return A data frame with one row per bootstrap replicate and the
#'   following columns:
#'   \describe{
#'     \item{sample}{The sequential bootstrap sample number.}
#'     \item{rep_boot}{The statistic calculated for the bootstrap sample.}
#'     \item{trait}{The trait associated with the bootstrap sample.}
#'     \item{trait_value}{The trait value associated with the bootstrap
#'       sample.}
#'     \item{n_spec}{The number of observations in the original data.}
#'     \item{est_original}{The statistic calculated from the original
#'       data.}
#'     \item{mean_boot}{The mean of the bootstrap replicates.}
#'     \item{median_boot}{The median of the bootstrap replicates.}
#'     \item{se_boot}{The standard deviation of the bootstrap replicates,
#'       used as the bootstrap standard error.}
#'     \item{bias_boot}{The bootstrap bias, calculated as the mean of the
#'       bootstrap replicates minus the original estimate.}
#'   }
#'
boot_to_dataframe <- function(
  boot_obj
) {
  # Extract the bootstrap replicates and calculate summary statistics.
  boot_rep <- as.vector(boot_obj$t)

  data.frame(
    # Sequential identifier for each bootstrap sample
    sample = seq_len(boot_obj$R),

    # Statistic calculated for each bootstrap sample
    rep_boot = boot_rep,

    # Trait information from the original bootstrap data
    trait = unique(boot_obj$data$Trait),
    trait_value = unique(boot_obj$data$TraitValue),

    # Number of observations in the original data
    n_spec = nrow(boot_obj$data),

    # Estimate calculated from the original data
    est_original = boot_obj$t0,

    # Summary statistics of the bootstrap distribution
    mean_boot = mean(boot_rep),
    median_boot = median(boot_rep),
    se_boot = sd(boot_rep),
    bias_boot = mean(boot_rep) - boot_obj$t0
  )
}
