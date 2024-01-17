#' Aggregate the mean and variance of the estimated individual-level child penalties
#'
#' @param object `indcp` object
#' @param agg Aggregation method. Default is `full`.
#' @param compute_var Logical. If `TRUE`, compute variance of the estimated.
#' @param ... Additional arguments to be passed to `aggregate_mean` or `aggregate_var`.
#'
#' @return A `tibble` with the summary statistics
#' @examples
#' library(indcp)
#' mdl_base <- base_indcp |>
#'   indcp(yname = "y",
#'         iname = "id",
#'         tname = "year",
#'         bname = "byear",
#'         kname = "rel_time")
#' summary(mdl_base, agg = "cage)
#'
#' @export
#'
summary.indcp <- function(object, agg = "full", compute_var = FALSE, ...) {
  if (compute_var) {
    aggregate_var(object, agg = agg)
  } else {
    aggregate_mean(object, agg = agg)
  }
}
