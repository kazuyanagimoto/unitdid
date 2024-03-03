#' Aggregate the mean and variance of the estimated unit-level DiD effects
#'
#' @param object `unitdid` object
#' @param agg Aggregation method.
#' One of `c("full", "event", "event_age")` and the default is `full`.
#' If `by` is provided in the model,
#' all the options will separately aggregate by its group.
#' The `event` option aggregates by the group of the event timing.
#' The `event_age` option aggregates by the group of the age at the event timing.
#' `event_age` requires the `bname` to be provided in the model.
#' @param by A character vector of variables to aggregate separately by.
#' Default is inherited from the `unitdid` object but you can override it here.
#' You can estimate the unit-level DiD effects separately by `by` in `unitdid`
#' but you can also aggregate the estimates by (higher-level) `by` here.
#' @param normalized Logical. If `TRUE`, the function will normalize the aggregated mean and variance
#' by the mean of the imputed outcome variable. Default is inherited from the `unitdid` object.
#'
#' @return A `tibble` with the aggregated mean and variance of the estimated unit-level DiD effects
#' @export
#'
aggregate_unitdid <- function(object,
                              agg = "full",
                              by = NULL,
                              normalized = NULL) {

  if (is.null(normalized)) {
    normalized <- object$info$normalized
  }

  if (normalized) {

    object$aggregated <- object$aggregated |>
      dplyr::mutate(zz000t = !!rlang::sym(object$info$ename) + zz000k) |>
      dplyr::left_join(object$yhat_agg,
                       by = c(object$info$by_est, object$info$ename,
                              "zz000t" = object$info$tname)) |>
      dplyr::mutate(zz000mean = zz000mean / zz000yhat_agg)

    if (object$info$compute_var) {
      object$aggregated <- object$aggregated |>
        dplyr::mutate(zz000var = zz000var / zz000yhat_agg^2)
    }
  }

  # Higher Level Aggregation `by`
  if (is.null(by)) {
    by <- c(object$info$by, "zz000k")
  } else {
    by <- c(by, "zz000k")
  }

  # Aggregation Method
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

  # Aggregation
  if (object$info$compute_var) {
    result <- object$aggregated |>
      dplyr::summarize(mean = stats::weighted.mean(zz000mean, w = zz000w),
                       var = pmax(stats::weighted.mean(zz000var, w = zz000w), 0),
                       zz000w = sum(zz000w),
                       .by = by)
  } else {
    result <- object$aggregated |>
      dplyr::summarize(mean = stats::weighted.mean(zz000mean, w = zz000w),
                       zz000w = sum(zz000w),
                       .by = by)
  }

  # Export
  if (is.null(object$info$wname)) {
    result <- result |>
      dplyr::rename("n" = zz000w)
  } else {
    result <- result |>
      dplyr::rename("weight" = zz000w)
  }

  result |>
    dplyr::rename("rel_time" = zz000k)
}
