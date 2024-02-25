prep_data <- function(data, yname, iname, tname, ename, k_min, normalized) {

  not_yet_treated <- data[data$zz000k < k_min, ]

  if (nrow(not_yet_treated) == 0 ||
        fixest:::cpp_isConstant(not_yet_treated[[yname]])) {
    return(data[FALSE, ])
  }
  first_stage <- fixest::feols(
    stats::as.formula(paste0(yname, "~ 0 |", iname, " + ", tname)),
    data = not_yet_treated,
    weights = ~zz000w,
    warn = FALSE,
    notes = FALSE)

  data$zz000yhat <- stats::predict(first_stage, newdata = data)
  data$zz000ytilde <- data[[yname]] - data$zz000yhat

  data <- data |>
    dplyr::filter(!is.na(zz000yhat))

  return (data)
}
