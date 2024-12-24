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
#'   for each estimated point in `k_min:k_max`. Default is FALSE.
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
                              only_full_horizon = FALSE) {

  # CRAN Errors
  zz000ytilde = zz000w = zz000k = zz000l = NULL
  zz000var = zz000varraw = zz000varerr = NULL
  zz000cov = zz000covraw = zz000coverr = NULL

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
      stop("You need to provide the `bname` in the model to aggregate by age at the event.")
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
      dplyr::summarize(dplyr::across(c(zz000ytilde, zz000cov, zz000covraw, zz000coverr),
                              ~stats::weighted.mean(.x, w = zz000w, na.rm = na.rm)),
                       zz000w = sum(zz000w),
                       .by = c(by, zz000l)) |>
      dplyr::mutate(zz000cov = ifelse(zz000k == zz000l,
                                      pmax(zz000cov, var_min), zz000cov)) |>
      dplyr::arrange(!!!rlang::syms(by), zz000l)
  } else {
    result <- df_unitdid |>
      dplyr::summarize(dplyr::across(c(zz000ytilde, zz000var, zz000varraw, zz000varerr),
                              ~stats::weighted.mean(.x, w = zz000w, na.rm = na.rm)),
                       zz000var = pmax(zz000var, var_min),
                       zz000w = sum(zz000w),
                       .by = by) |>
      dplyr::arrange(!!!rlang::syms(by))
  }

  # Export
  result$rel_time <- result$zz000k
  result$mean <- result$zz000ytilde
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
    result$var_raw <- result$zz000varraw
    result$var_err <- result$zz000varerr
  } else if (object$info$compute_varcov == "cov") {
    result$cov <- result$zz000cov
    result$cov_raw <- result$zz000covraw
    result$cov_err <- result$zz000coverr
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

  # CRAN Errors
  zz000k = zz000l = zz000ytilde = zz000cov = zz000covraw = zz000coverr = NULL
  zz000yhat_agg = zz000yhat_agg_s = zz000var = zz000varraw = zz000varerr = NULL

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
                      zz000cov = zz000cov / (zz000yhat_agg * zz000yhat_agg_s),
                      zz000covraw = zz000covraw / (zz000yhat_agg * zz000yhat_agg_s),
                      zz000coverr = zz000coverr / (zz000yhat_agg * zz000yhat_agg_s))

    } else {
      object$data <- object$data |>
        dplyr::mutate(zz000t = !!rlang::sym(object$info$ename) + zz000k) |>
        dplyr::left_join(object$yhat_agg,
                         by = c(object$info$by_est, object$info$ename,
                                "zz000t" = object$info$tname)) |>
        dplyr::mutate(zz000ytilde = zz000ytilde / zz000yhat_agg,
                      zz000var = zz000var / zz000yhat_agg^2,
                      zz000varraw = zz000varraw / zz000yhat_agg^2,
                      zz000varerr = zz000varerr / zz000yhat_agg^2)
    }
  }

  # Only full horizon
  if (only_full_horizon) {

    object$data <- object$data |>
      dplyr::filter(dplyr::between(zz000k,
                                   object$info$k_min, object$info$k_max)) |>
      dplyr::group_by(!!rlang::sym(object$info$iname)) |>
      dplyr::filter(dplyr::n_distinct(zz000k) == object$info$k_max - object$info$k_min + 1) |>
      dplyr::ungroup()

  }

  # Export
  df_export <- object$data

  if (export) {
    df_export[[object$info$newnames$ytildename]] <- df_export$zz000ytilde

    if (object$info$compute_varcov == "var") {
      df_export[[object$info$newnames$yvarname]] <- df_export$zz000var
      df_export[[object$info$newnames$yvarrawname]] <- df_export$zz000varraw
      df_export[[object$info$newnames$yvarerrname]] <- df_export$zz000varerr
    } else if (object$info$compute_varcov == "cov") {
      df_export[[object$info$newnames$kprimename]] <- df_export$zz000l
      df_export[[object$info$newnames$ycovname]] <- df_export$zz000cov
      df_export[[object$info$newnames$ycovrawname]] <- df_export$zz000covraw
      df_export[[object$info$newnames$ycoverrname]] <- df_export$zz000coverr
    }

    if (!is.null(object$info$wname)) {
      df_export[[object$info$wname]] <- df_export$zz000w
    }

    df_export <- df_export |>
      dplyr::select(-dplyr::starts_with("zz000"))
  }

  return(df_export)
}