#' Title
#'
#' @param object `indcp` object
#' @param agg Aggregation method. Default is `full`.
#' @param compute_var Logical. If `TRUE`, compute variance of the estimated.
#'
#' @return dataframe with the summary statistics
#' @export
#'
summary.indcp <- function(object, agg = "full", compute_var = FALSE) {
  if (compute_var) {

  } else {
    aggregate_mean(object, agg = agg)
  }
}
