#' Aggregate the mean and variance of the estimated unit-level DiD effects
#'
#' @param object `unitdid` object
#' @param agg Aggregation method.
#' One of `c("full", "event", "event_age")` and the default is `full`.
#' If `by` is provided in the model,
#' all the options will separately aggregate by its group.
#' The `event` option aggregates by the group of the event timing.
#' The `event_age` option aggregates by the group of the age at the event time.
#' `event_age` requires the `bname` to be provided in the model.
#' @param na.rm Logical. If `TRUE`, remove `NA` values for the aggregation.
#' The default is `TRUE`.
#' @param by A character vector of variables to aggregate separately by.
#' Default is inherited from the `unitdid` object but you can override it here.
#' You can estimate the unit-level DiD effects separately by `by` in `unitdid`
#' but you can also aggregate the estimates by (higher-level) `by` here.
#' You can use "rel_time" as the highest level of aggregation.
#' @param normalized Logical. If `TRUE`, the function will normalize
#' the aggregated mean and variance by the mean of the imputed outcome variable.
#' Default is inherited from the `unitdid` object.
#' @param allow_negative_var Logical. If `FALSE`, the function will return
#' the estimated variance trimmed at zero. Default is `FALSE`.
#' @param only_full_horizon Logical. If TRUE, when you aggregate
#'   the unit-level treatment effect, only the event year (`ename`)
#'   with full horizon (`k_min:k_max`) will be included.
#'   This is recommended in the case that you do not want to change
#'   the composition of the event year (or age for the child penalties)
#'   for each estimated point in `k_min:k_max`. Default is TRUE.
#'
#' @return A `tibble` with the aggregated mean and variance of
#'    the estimated unit-level DiD effects
#' @export
#'
aggregate_unitdid <- function(object,
                              agg = "full",
                              na.rm = TRUE,
                              by = NULL,
                              normalized = NULL,
                              allow_negative_var = FALSE,
                              only_full_horizon = TRUE) {

  # Override the normalization option
  if (is.null(normalized)) {
    normalized <- object$info$normalized
  }

  # Higher Level Aggregation `by`
  if (is.null(by)) {
    by <- c(object$info$by, "zz000k")
  } else if (by == "rel_time") {
    by <- c("zz000k")
  }  else {
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
      by <- c(by, "zz000eage")
    }
  } else {
    stop("The `agg` argument must be one of `c('full', 'event', 'event_age')`.")
  }

  ## Aggregation
  df_unitdid <- get_unitdid(object, normalized = normalized, export = FALSE,
                            only_full_horizon = only_full_horizon)

  var_min <- ifelse(allow_negative_var, -Inf, 0)

  if (!is.null(object$info$bname)) {
    df_unitdid$zz000eage <- df_unitdid[[object$info$ename]] -
      df_unitdid[[object$info$bname]]
  }

  if (object$info$compute_varcov == "cov") {
    result <- df_unitdid |>
      dplyr::summarize(mean = stats::weighted.mean(zz000ytilde,
                                                  w = zz000w,
                                                  na.rm = na.rm),
                      zz000cov = stats::weighted.mean(zz000cov,
                                                      w = zz000w,
                                                      na.rm = na.rm),
                      zz000w = sum(zz000w),
                      .by = c(by, zz000l)) |>
      dplyr::mutate(zz000cov = ifelse(zz000k == zz000l,
                                      pmax(zz000cov, var_min), zz000cov)) |>
      dplyr::arrange(!!!rlang::syms(by), zz000l)
  } else {
    result <- df_unitdid |>
      dplyr::summarize(mean = stats::weighted.mean(zz000ytilde,
                                                  w = zz000w,
                                                  na.rm = na.rm),
                      zz000var = pmax(stats::weighted.mean(zz000var,
                                                            w = zz000w,
                                                            na.rm = na.rm),
                                      var_min),
                      zz000w = sum(zz000w),
                      .by = by) |>
      dplyr::arrange(!!!rlang::syms(by))
  }

  # Export
  result$rel_time <- result$zz000k
  if (object$info$compute_varcov == "cov") {
    result$rel_time2 <- result$zz000l
  }

  ## Rename for weights
  if (is.null(object$info$wname)) {
    result$n <- result$zz000w
  } else {
    result$weight <- result$zz000w
  }

  ## Rename for the variance
  if (object$info$compute_varcov == "var") {
    result$var <- result$zz000var
  } else if (object$info$compute_varcov == "cov") {
    result$cov <- result$zz000cov
  }

  ## Rename for agg="event_age"
  if (agg == "event_age") {
    result$event_age <- result$zz000eage
  }

  ## Drop the zz000 prefix
  result |>
    dplyr::select(-dplyr::starts_with("zz000"))
}

#' Get unit-level Difference-in-Differences estimates
#'
#' @param object `unitdid` object
#' @param normalized Logical. If `TRUE`, the function will normalize them
#' by the mean of the imputed outcome variable.
#' Default is inherited from the `unitdid` object.
#' @param export Logical. If `TRUE`, the function will not export the columns
#' with the `zz000` prefix, which are used in the internal computation.
#' @param only_full_horizon Logical. If TRUE, only the event year (`ename`)
#'   with full horizon (`k_min:k_max`) will be exported.
#'   This is recommended in the case that you do not want to change
#'   the composition of the event year (or age for the child penalties)
#'   for each estimated point in `k_min:k_max` for aggregation.
#'   Default is FALSE.
#' @return A dataframe with a new column of the unit-level DiD estimates
#' @export
#'
get_unitdid <- function(object, normalized = NULL, export = TRUE,
                        only_full_horizon = FALSE) {

  if (is.null(normalized)) {
    normalized <- object$info$normalized
  }

  if (normalized) {
    if (object$info$compute_varcov == "cov") {
      object$data <- object$data |>
        dplyr::mutate(zz000t = !!rlang::sym(object$info$ename) + zz000k,
                      zz000s = !!rlang::sym(object$info$ename) + zz000l) |>
        dplyr::left_join(object$yhat_agg,
                         by = c(object$info$by_est, object$info$ename,
                                "zz000t" = object$info$tname)) |>
        dplyr::left_join(object$yhat_agg,
                         by = c(object$info$by_est, object$info$ename,
                                "zz000s" = object$info$tname),
                         suffix = c("", "_s")) |>
        dplyr::mutate(zz000ytilde = zz000ytilde / zz000yhat_agg,
                      zz000cov = zz000cov / (zz000yhat_agg * zz000yhat_agg_s))

    } else {
      object$data <- object$data |>
        dplyr::mutate(zz000t = !!rlang::sym(object$info$ename) + zz000k) |>
        dplyr::left_join(object$yhat_agg,
                         by = c(object$info$by_est, object$info$ename,
                                "zz000t" = object$info$tname)) |>
        dplyr::mutate(zz000ytilde = zz000ytilde / zz000yhat_agg,
                      zz000var = zz000var / zz000yhat_agg^2)
    }
  }

  # Only full horizon
  if (only_full_horizon) {
    feasible_ek <- object$aggregated |>
      dplyr::summarize(zz000k_max = max(zz000k),
                       .by = c(object$info$by_est, object$info$ename)) |>
      dplyr::filter(zz000k_max == object$info$k_max) |>
      dplyr::select(-zz000k_max)

    object$data <- feasible_ek |>
      dplyr::left_join(object$data,
                       by = c(object$info$by_est, object$info$ename))
  }

  # Export
  df_export <- object$data

  if (export) {
    df_export[[object$info$ytildename]] <- df_export$zz000ytilde

    if (object$info$compute_varcov == "var") {
      df_export[[object$info$yvarname]] <- df_export$zz000var
    } else if (object$info$compute_varcov == "cov") {
      df_export[[object$info$kprimename]] <- df_export$zz000l
      df_export[[object$info$ycovname]] <- df_export$zz000cov
    }

    if (!is.null(object$info$wname)) {
      df_export[[object$info$wname]] <- df_export$zz000w
    }

    df_export <- df_export |>
      dplyr::select(-dplyr::starts_with("zz000"))
  }

  return(df_export)
}