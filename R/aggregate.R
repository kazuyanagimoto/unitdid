#' Aggregate the mean and variance of the estimated individual-level child penalties
#'
#' @param object `indcp` object
#' @param agg Aggregation method. One of `c("full", "cage", "cage_byear")` and the default is `full`. The `cage` means the age at the treatment. The `cage_byear` means the age at the treatment and the birth year.
#' @param na.rm Logical. If `TRUE` and `agg = c("cage", "cage_byear")`, remove `NA` values for `sd_epsilon` aggregation. The default is `TRUE`.
#'
#' @return A `tibble` with the summary statistics
#' @export
#'
aggregate_indcp <- function(object, .by, na.rm = TRUE) {

  object$aggregated |>
    dplyr::summarize(zz000mean = stats::weighted.mean(zz000mean, w = zz000n, na.rm = TRUE),
                     zz000var = stats::weighted.mean(zz000var^2, w = zz000n, na.rm = TRUE),
                     n = sum(zz000n),
                     .by = .by)
  
}