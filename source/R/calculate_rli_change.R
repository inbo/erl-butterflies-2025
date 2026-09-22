#' Calculate change in the Red List Index
#'
#' Calculates the change in the Red List Index between two Red List
#' assessment columns. The RLI is calculated separately for the reference
#' and comparison assessments, after which the reference RLI is subtracted
#' from the comparison RLI. Assessments without a corresponding Red List
#' score are excluded from each RLI calculation.
#'
#' @param x A data frame containing the Red List assessments.
#' @param rli_scores A named numeric vector containing the Red List score
#'   for each category. The names must correspond to the values in
#'   `ref_col` and `diff_col`.
#' @param ref_col A character string giving the name of the column in `x`
#'   containing the reference Red List categories. Defaults to
#'   `"RLC_y2010"`.
#' @param diff_col A character string giving the name of the column in `x`
#'   containing the comparison Red List categories. Defaults to
#'   `"RLC_y2025"`.
#' @param max_score A numeric value giving the maximum possible Red List
#'   score. Defaults to `5`.
#'
#' @return A numeric value giving the change in the Red List Index,
#'   calculated as the RLI of the comparison assessment minus the RLI of
#'   the reference assessment. Positive values indicate an increase in RLI,
#'   while negative values indicate a decrease.
#'
calculate_rli_change <- function(
  x,
  rli_scores,
  ref_col = "RLC_y2010",
  diff_col = "RLC_y2025",
  max_score = 5
) {
  # Calculate the RLI for the reference and comparison assessments
  rli_ref <- calculate_rli(
    x = x,
    rli_scores = rli_scores,
    col = ref_col,
    max_score = max_score
  )

  rli_diff <- calculate_rli(
    x = x,
    rli_scores = rli_scores,
    col = diff_col,
    max_score = max_score
  )

  # Calculate the change in RLI
  return(rli_diff - rli_ref)
}
