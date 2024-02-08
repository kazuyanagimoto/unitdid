#' A function estimates individual-level child penalties
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param iname Individual identifier
#' @param tname Time variable
#' @param bname Birth year variable
#' @param kname Relative time to treatment variable
#' @param aname Optional. Age at treatment variable. If not provided, it will be computed as t - b - k and added to the dataframe as `cage`.
#' @param k_min Relative time to treatment at which treatment starts. Default is 0.
#' @param k_max Relative time to treatment at which treatment ends. Default is 5.
#' @param compute_var_me Logical. If TRUE, the function will compute the variance of the measurement errors and the variance of the individual-level child-penalties. Default is FALSE.
#' @param only_full_horizon Logical. If TRUE, when you aggregate the individual child penalties, only the cohorts with full horizon (`k_min:k_max`) will be included. Default is TRUE.
#'
#' @return A `indcp` class object.
#' @export
#'
indcp <- function(data,
                  yname,
                  iname,
                  tname,
                  aname,
                  by,
                  k_min = 0,
                  k_max = 5,
                  compute_var = FALSE,
                  only_full_horizon = TRUE) {

  # Sample Selection & Variable Creation
  t_min <- data[[tname]] |> min()
  data <- data[data[[aname]] + k_min > t_min &
                 data[[aname]] + k_max >= data[[tname]], ]
  data$zz000k <- data[[tname]] - data[[aname]]

  # Imputation
  data <- data |>
    dplyr::group_by(!!rlang::sym(by)) %>%
    dplyr::do(prep_data(., yname, iname, tname, aname, k_min))

  # Construct the object
  info <- list(yname = yname,
               iname = iname,
               tname = tname,
               aname = aname,
               by = by,
               k_min = k_min,
               k_max = k_max,
               compute_var = compute_var,
               only_full_horizon = only_full_horizon)
  object <- list(data = data, info = info)
  class(object) <- "indcp"

  # Compute the projection
  object <- compute_projection(object)

  return(object)
}
