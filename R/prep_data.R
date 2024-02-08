prep_data <- function(data, yname, iname, tname, ename, k_min, normalized) {

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

  data <- data |>
    dplyr::filter(!is.na(zz000yhat))

  if (normalized) {
    yhat_agg <- data |>
      dplyr::summarize(zz000yhat_agg = mean(zz000yhat), .by = c(ename, tname))

    data <- data |>
      dplyr::left_join(yhat_agg, by = c(ename, tname)) |>
      dplyr::mutate(zz000ytilde = zz000ytilde / zz000yhat_agg)
  }

  return (data)
}
