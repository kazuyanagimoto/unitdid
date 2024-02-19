#' Aggregate the mean and variance of the estimated unit-level DiD effects
#'
#' @param object `unitdid` object
#' @param ... `aggregate_unitdid` arguments
#'
#' @return A `tibble` with the summary statistics
#' @examples
#' library(unitdid)
#' mdl_base <- base_heterocp |>
#'   unitdid(yname = "y",
#'         iname = "id",
#'         tname = "year",
#'         ename = "cyear",
#'         bname = "byear")
#' summary(mdl_base, agg = "event_age")
#'
#' @export
#'
summary.unitdid <- function(object, ...) {

  aggregate_unitdid(object, ...)
}