#' Aggregate the mean and variance of the estimated individual-level child penalties
#'
#' @param object `indcp` object
#' @param ... `aggregate_indcp` arguments
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
#' summary(mdl_base, agg = "cage")
#'
#' @export
#'
summary.indcp <- function(object, ...) {

  aggregate_indcp(object, ...)
}
