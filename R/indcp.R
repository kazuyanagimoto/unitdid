#' Title
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param bname Birth year variable
#' @param first_stage Fixed effects and other covariates to residualize in the first stage
#' @param treatment Indicator variable for the treatment
#' @param weights Optional. Variable name for regression weights
#'
#' @return dataframe with the residualized outcome variable
#' @export
#'
#' @examples
#' library(indcp)
#' est_indcp(yname = "y", bname = "byear", treatment = "is_treated",
#'          first_stage = ~ 0 | id + year, data = base_indcp)
est_indcp <- function(data, yname, bname, first_stage, treatment, weights = NULL) {

  byears <- unique(data[[bname]])

  result <- lapply(byears, function(b) est_indcp_byear(data, yname, bname, first_stage, treatment, weights, b))
  result <- do.call(rbind, result)

  return (result)
}

est_indcp_byear <- function(data, yname, bname, first_stage, treatment, weights, byear) {

  data <- data[data[[bname]] == byear, ]
  fixest::setFixest_fml(
    ..first_stage = first_stage
  )

  not_yet_treated <- data[data[[treatment]] == 0, ]
  if (is.null(weights)) {
    weights_vector <- NULL
  } else {
    weights_vector <- note_yet_treated[[weights]]
  }

  first_stage <- fixest::feols(
    fixest::xpd(~ 0 + ..first_stage, lhs = yname),
    data = not_yet_treated,
    weights = weights_vector,
    combine.quick = FALSE,
    warn = FALSE,
    notes = FALSE)

  data[[paste0(yname, "_hat")]] <- stats::predict(first_stage, newdata = data)
  data[[paste0(yname, "_tilde")]] <- data[[yname]] - data[[paste0(yname, "_hat")]]

  return (data[!is.na(data[paste0(yname, "_tilde")]), ])
}
