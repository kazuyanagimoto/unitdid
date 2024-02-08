#' Aggregate the mean and variance of the estimated individual-level child penalties
#'
#' @param object `indcp` object
#' @param agg Aggregation method.
#' One of `c("full", "event", "event_age")` and the default is `full`.
#' If `by` is provided in the model,
#' all the options will separately aggregate by its group.
#' The `event` option aggregates by the group of the event timing.
#' The `event_age` option aggregates by the group of the age at the event timing.
#' `event_age` requires the `bname` to be provided in the model.
#' @param na.rm Logical. If `TRUE`, remove `NA` values for the aggregation. The default is `TRUE`.
#'
#' @return A `tibble` with the summary statistics
#' @export
#'
aggregate_indcp <- function(object, agg = "full", na.rm = TRUE) {

  by <- c(object$info$by, "zz000k")

  if (agg == "full") {
    # Skip
  } else if (agg == "event") {
    by <- c(by, object$info$ename)
  } else if (agg == "event_age") {
    if (is.null(object$info$bname)) {
      stop("You need to provide the bname in the model to aggregate by age at the event.")
    } else {
      by <- c(by, "event_age")
      object$aggregated <- object$aggregated |>
        dplyr::mutate(event_age = !!rlang::sym(object$info$ename) -
                        !!rlang::sym(object$info$bname))
    }
  } else {
    stop("The `agg` argument must be one of `c('full', 'event', 'event_age')`.")
  }

  if (object$info$compute_var) {
    result <- object$aggregated |>
      dplyr::summarize(mean = stats::weighted.mean(zz000mean, w = zz000n, na.rm = na.rm),
                       var = pmax(stats::weighted.mean(zz000var, w = zz000n, na.rm = na.rm), 0),
                       n = sum(zz000n),
                       .by = by)
  } else {
    result <- object$aggregated |>
      dplyr::summarize(mean = stats::weighted.mean(zz000mean, w = zz000n, na.rm = na.rm),
                       n = sum(zz000n),
                       .by = by)
  }

  result |>
    dplyr::rename("rel_time" = zz000k)
}
