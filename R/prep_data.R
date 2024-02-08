prep_data <- function(data, yname, iname, tname, aname, k_min) {

  not_yet_treated <- data[data$zz000k < k_min, ]

  if (nrow(not_yet_treated) == 0 ||
        fixest:::cpp_isConstant(not_yet_treated[[yname]])) {
    return(data[FALSE, ])
  }

  first_stage <- fixest::feols(
    stats::as.formula(paste0(yname, "~ 0 |", iname, " + ", tname)),
    data = not_yet_treated,
    combine.quick = FALSE,
    warn = FALSE,
    notes = FALSE)

  data$zz000yhat <- stats::predict(first_stage, newdata = data)
  data$zz000ytilde <- data[[yname]] - data$zz000yhat

  return(data[!is.na(data$zz000ytilde), ])
}
