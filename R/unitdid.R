#' A function estimates unit-level difference-in-differences
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param iname Individual identifier
#' @param tname Time variable
#' @param ename Event timing variable
#' @param wname Optional. The name of the weight variable.
#' @param ytildename Optional. The name of the imputed outcome variable.
#' If not provided, the function will use `paste0(yname, "_tilde")`.
#' @param k_min Relative time to treatment at which treatment starts. Default is 0.
#' @param k_max Relative time to treatment at which treatment ends. Default is 5.
#' @param compute_var Logical. If TRUE, the function will compute the variance of the measurement errors and the variance of the individual-level child-penalties. Default is FALSE.
#' @param only_full_horizon Logical. If TRUE, when you aggregate the individual child penalties, only the cohorts with full horizon (`k_min:k_max`) will be included. Default is TRUE.
#' @param by A character vector of variables to estimate separately by. Default is NULL.
#' @param bname Birth year variable. Default is NULL.
#' Necessary to aggregate the estimates by age at event.
#' @param normalized Logical. If TRUE,
#' the function will normalize the outcome variable scale. Default is FALSE.
#'
#' @return A `unitdid` class object.
#' @export
#'
unitdid <- function(data,
                    yname,
                    iname,
                    tname,
                    ename,
                    wname = NULL,
                    ytildename = NULL,
                    k_min = 0,
                    k_max = 5,
                    compute_var = FALSE,
                    only_full_horizon = TRUE,
                    by = NULL,
                    bname = NULL,
                    normalized = FALSE) {

  by_est <- c(by, bname) |> unique()

  # Check the input
  ytildename <- ifelse(is.null(ytildename), paste0(yname, "_tilde"), ytildename)
  if (ytildename %in% colnames(data)) {
    stop("Please specify a different name in the data for `ytildename`")
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
    dplyr::do(prep_data(., yname, iname, tname, ename, k_min, normalized)) |>
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
               compute_var = compute_var,
               only_full_horizon = only_full_horizon,
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
