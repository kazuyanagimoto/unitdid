#' Title
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param iname Individual identifier
#' @param tname Time variable
#' @param bname Birth year variable
#' @param treatment Indicator variable for the treatment
#' @param weights Optional. Variable name for regression weights
#'
#' @return dataframe with the residualized outcome variable
#' @export
#'
#' @examples
#' library(indcp)
#' est_indcp(yname = "y", iname = "id", tname = "year", bname = "byear",
#'           treatment = "is_treated", data = base_indcp)
#'
est_indcp <- function(data,
                      yname,
                      iname,
                      tname,
                      bname,
                      treatment,
                      weights = NULL) {

  byears <- unique(data[[bname]])

  result <- lapply(byears, function(b) {
    est_indcp_byear(data, yname, iname, tname, bname, treatment, weights, b)
  })
  result <- do.call(rbind, result)

  return(result)
}

est_indcp_byear <- function(data,
                            yname,
                            iname,
                            tname,
                            bname,
                            treatment,
                            weights,
                            byear) {

  data <- data[data[[bname]] == byear, ]

  not_yet_treated <- data[data[[treatment]] == 0, ]
  if (is.null(weights)) {
    weights_vector <- NULL
  } else {
    weights_vector <- not_yet_treated[[weights]]
  }

  first_stage <- fixest::feols(
    stats::as.formula(paste0(yname, "~ 0 |", iname, " + ", tname)),
    data = not_yet_treated,
    weights = weights_vector,
    combine.quick = FALSE,
    warn = FALSE,
    notes = FALSE)

  data[[paste0(yname, "_hat")]] <- stats::predict(first_stage, newdata = data)
  data[[paste0(yname, "_tilde")]] <- data[[yname]] -
    data[[paste0(yname, "_hat")]]

  return(data[!is.na(data[paste0(yname, "_tilde")]), ])
}
