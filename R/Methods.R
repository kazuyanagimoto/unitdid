#' Title
#'
#' @param object `indcp` object
#' @param agg Aggregation method. Default is `full`.
#' @param compute_var Logical. If `TRUE`, compute variance of the estimated.
#' @param ... Additional arguments to be passed to `aggregate_mean` or `aggregate_var`.
#'
#' @return dataframe with the summary statistics
#' @export
#'
summary.indcp <- function(object, agg = "full", compute_var = FALSE, ...) {
  if (compute_var) {
    aggregate_var(object, agg = agg)
  } else {
    aggregate_mean(object, agg = agg)
  }
}
