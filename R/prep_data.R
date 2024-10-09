prep_data <- function(data, yname, iname, tname, first_stage, k_min) {

  # CRAN Errors
  zz000ytilde = NULL

  # Sample Selection
  not_yet_treated <- data[data$zz000k < k_min, ]

  if (nrow(not_yet_treated) == 0 ||
        all(not_yet_treated[[yname]] == not_yet_treated[[yname]][1])) {
    return(data[FALSE, ])
  }

  # Extract the first stage formula
  if (is.null(first_stage)) {
    first_stage <- paste0("0 | ", iname, " + ", tname)
  } else if (inherits(first_stage, "formula")) {
    first_stage <- as.character(first_stage)[[2]]
  }

  formula <- stats::as.formula(paste0(yname, " ~ ", first_stage))

  # First stage
  first_stage <- fixest::feols(
    fml = formula,
    data = not_yet_treated,
    weights = ~zz000w,
    combine.quick = FALSE,
    warn = FALSE,
    notes = FALSE)

  data$zz000yhat <- stats::predict(first_stage, newdata = data)
  data$zz000ytilde <- data[[yname]] - data$zz000yhat

  data <- data |>
    dplyr::filter(!is.na(zz000ytilde))

  return(data)
}
