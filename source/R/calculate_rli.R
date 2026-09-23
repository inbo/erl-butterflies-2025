#' Calculate a Red List Index (RLI)
#'
#' Calculates the Red List Index from a data frame containing species
#' assessments and a named vector of Red List scores. The RLI is calculated
#' as 1 minus the mean Red List score divided by the maximum possible score.
#' Assessments without a corresponding Red List score are excluded from
#' the calculation.
#'
#' @param x A data frame containing the species assessments.
#' @param rli_scores A named numeric vector containing the Red List score
#'   for each category. The names must correspond to the values in `col`.
#' @param col A character string giving the name of the column in `x`
#'   containing the Red List categories.
#' @param max_score A numeric value giving the maximum possible Red List
#'   score. Defaults to `5`.
#'
#' @return A numeric value giving the Red List Index. An RLI of 1 indicates
#'   the best possible conservation status, whereas lower values indicate
#'   a greater degree of threat.
#'
calculate_rli <- function(
  x,
  rli_scores,
  col,
  max_score = 5
) {
  # Look up the RLI score for each assessment
  scores <- rli_scores[x[[col]]]
  scores <- scores[!is.na(scores)]

  # Calculate the RLI relative to the maximum possible score
  rli_val <- 1 - sum(scores) / (length(scores) * max_score)

  return(rli_val)
}
