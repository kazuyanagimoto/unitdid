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
#'
#' @return A `indcp` class object. A `tibble` with the estimated individual-level child penalties (e.g., y_tilde).
#' @export
#'
indcp <- function(data,
                  yname,
                  iname,
                  tname,
                  bname,
                  kname,
                  aname = "cage",
                  k_min = 0) {

  byears <- unique(data[[bname]])

  df_indcp <- purrr::map(byears, ~indcp_byear(data, yname, iname,
                                  tname, bname, kname, k_min, .x)) |>
    purrr::list_rbind()

  # Return
  t_min <- df_indcp[[tname]] |> min()
  t_max <- df_indcp[[tname]] |> max()
  b_min <- df_indcp[[bname]] |> min()
  b_max <- df_indcp[[bname]] |> max()

  df_indcp[[aname]] <- df_indcp[[tname]] -
    df_indcp[[bname]] - df_indcp[[kname]]

  a_min <- df_indcp[[aname]] |> min()
  a_max <- df_indcp[[aname]] |> max()

  info <- list(yname = yname,
               iname = iname,
               tname = tname,
               bname = bname,
               kname = kname,
               aname = aname,
               t_min = t_min,
               t_max = t_max,
               b_min = b_min,
               b_max = b_max,
               k_min = k_min,
               a_min = a_min,
               a_max = a_max,
               ytildename = paste0(yname, "_tilde"))
  res <- list(df_indcp = df_indcp, info = info)

  class(res) <- "indcp"

  return(res)
}

indcp_byear <- function(data,
                        yname,
                        iname,
                        tname,
                        bname,
                        kname,
                        k_min,
                        byear) {

  data <- data[data[[bname]] == byear, ]

  not_yet_treated <- data[data[[kname]] < k_min, ]

  if (nrow(not_yet_treated) == 0) {
    return(tibble::tibble())
  }

  if (fixest:::cpp_isConstant(not_yet_treated[[yname]])) {

    data[[paste0(yname, "_hat")]] <- not_yet_treated[[yname]][1]
    data[[paste0(yname, "_tilde")]] <- data[[yname]] -
      data[[paste0(yname, "_hat")]]

  } else {

    first_stage <- fixest::feols(
      stats::as.formula(paste0(yname, "~ 0 |", iname, " + ", tname)),
      data = not_yet_treated,
      combine.quick = FALSE,
      warn = FALSE,
      notes = FALSE)

    data[[paste0(yname, "_hat")]] <- stats::predict(first_stage, newdata = data)

  }

  data[[paste0(yname, "_tilde")]] <- data[[yname]] -
    data[[paste0(yname, "_hat")]]

  return(data[!is.na(data[paste0(yname, "_tilde")]), ])
}
