#' A function estimates unit-level difference-in-differences
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param iname Unit identifier
#' @param tname Time variable
#' @param ename Event timing variable
#' @param first_stage Formula for Y(0).
#'   Formula follows \code{\link[fixest:feols]{fixest::feols}}.
#'   If not specified, unit (`iname`) and time (`tname`) fixed effects
#'   will be used.
#' @param wname Optional. The name of the weight variable.
#' @param ytildename Optional. The name of the imputed outcome variable.
#'   If not provided, the function will use `paste0(yname, "_tilde")`.
#' @param yvarname Optional. The name of the unit-level variance of
#' the outcome variable. If not provided, the function will use
#' `paste0(yname, "_var")`.
#' @param ycovname Optional. The name of the unit-level covariance of
#' the outcome variable. If not provided, the function will use
#' `paste0(yname, "_cov")`.
#' @param kprimename Optional. The name of the relative time to treatment.
#' This is used for the second column name of the relative time of
#' the unit-level covariance estimation. Default is "kprime".
#' @param k_min Relative time to treatment at which treatment starts.
#'   Default is 0.
#' @param k_max Relative time to treatment at which treatment ends.
#'   Default is 5.
#' @param compute_varcov One of c("none", "var", "cov") and Default is "none".
#'  If "var", the function will estimate the unit-level variance of the outcome
#'  variable. If "cov", the function will estimate the unit-level covariance of
#'  the outcome variable for each pair within `k_min:k_max`.
#' @param by A character vector of variables to estimate separately by.
#'   Default is NULL.
#' @param bname Birth year variable. Default is NULL.
#'   Necessary to aggregate the estimates by age at event.
#' @param normalized Logical. If TRUE,
#'   the function will normalize the outcome variable scale. Default is FALSE.
#'
#' @return A `unitdid` class object.
#' @export
#'
unitdid <- function(data,
                    yname,
                    iname,
                    tname,
                    ename,
                    first_stage = NULL,
                    wname = NULL,
                    ytildename = NULL,
                    yvarname = NULL,
                    ycovname = NULL,
                    kprimename = "kprime",
                    k_min = 0,
                    k_max = 5,
                    compute_varcov = "none",
                    by = NULL,
                    bname = NULL,
                    normalized = FALSE) {

  by_est <- c(by, bname) |> unique()

  # Check the input
  ytildename <- ifelse(is.null(ytildename), paste0(yname, "_tilde"), ytildename)
  if (ytildename %in% colnames(data)) {
    stop("Please specify a different name in the data for `ytildename`")
  }
  yvarname <- ifelse(is.null(yvarname), paste0(yname, "_var"), yvarname)
  if (yvarname %in% colnames(data)) {
    stop("Please specify a different name in the data for `yvarname`")
  }
  ycovname <- ifelse(is.null(ycovname), paste0(yname, "_cov"), ycovname)
  if (ycovname %in% colnames(data)) {
    stop("Please specify a different name in the data for `ycovname`")
  }
  if (kprimename %in% colnames(data) && compute_varcov == "cov") {
    stop("Please specify a different name in the data for `kprimename`")
  }

  if (!(compute_varcov %in% c("none", "var", "cov"))) {
    stop("`compute_varcov` must be one of c('none', 'var', 'cov')")
  }

  # Sample Selection & Variable Creation
  t_min <- data[[tname]] |> min(na.rm = TRUE)
  data <- data[data[[ename]] + k_min > t_min &
                 data[[ename]] + k_max >= data[[tname]], ]
  data$zz000k <- data[[tname]] - data[[ename]]

  # Weights
  if (is.null(wname)) {
    data$zz000w <- 1
  } else {
    data$zz000w <- data[[wname]]
  }

  # Imputation
  data <- data |>
    dplyr::group_by(!!!rlang::syms(by_est)) %>%
    dplyr::do(prep_data(., yname, iname, tname, first_stage, k_min)) |>
    dplyr::ungroup()

  # Construct the object
  info <- list(yname = yname,
               iname = iname,
               tname = tname,
               ename = ename,
               wname = wname,
               k_min = k_min,
               k_max = k_max,
               ytildename = ytildename,
               yvarname = yvarname,
               ycovname = ycovname,
               kprimename = kprimename,
               compute_varcov = compute_varcov,
               by = by,
               bname = bname,
               by_est = by_est,
               normalized = normalized)
  object <- list(data = data, info = info)
  class(object) <- "unitdid"

  # Denominator of Normalization
  object$yhat_agg <- data |>
    dplyr::summarize(zz000yhat_agg = stats::weighted.mean(zz000yhat, w = zz000w),
                     .by = c(!!!rlang::syms(by_est), ename, tname))

  # Compute the projection
  object <- compute_projection(object)

  return(object)
}
