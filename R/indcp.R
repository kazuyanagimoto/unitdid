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
                  bname,
                  kname,
                  aname = "cage",
                  k_min = 0,
                  k_max = 5,
                  compute_var_me = FALSE,
                  only_full_horizon = TRUE) {

  object <- prep_data(data, yname, iname, tname, bname, kname, aname,
                      k_min, k_max, compute_var_me, only_full_horizon)

  object <- compute_projection(object)

  return(object)
}
