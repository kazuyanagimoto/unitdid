prep_data <- function(data,
                      yname,
                      iname,
                      tname,
                      bname,
                      kname,
                      aname,
                      k_min,
                      k_max,
                      compute_var_me,
                      only_full_horizon) {

  b_min <- data[[bname]] |> min()
  b_max <- data[[bname]] |> max()

  df_indcp <- purrr::map(b_min:b_max,
                         ~prep_data_b(data, yname, iname,
                                      tname, bname, kname, aname, k_min, .x)) |>
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
               k_max = k_max,
               a_min = a_min,
               a_max = a_max,
               ytildename = paste0(yname, "_tilde"),
               compute_var_me = compute_var_me,
               only_full_horizon = only_full_horizon)
  object <- list(df_indcp = df_indcp, info = info)

  class(object) <- "indcp"

  return(object)
}

prep_data_b <- function(data,
                        yname,
                        iname,
                        tname,
                        bname,
                        kname,
                        aname,
                        k_min,
                        b) {

  data <- data[data[[bname]] == b, ]
  t_min <- data[[tname]] |> min()
  data <- data[data[[bname]] + data[[aname]] + k_min > t_min, ]

  not_yet_treated <- data[data[[kname]] < k_min, ]

  if (nrow(not_yet_treated) == 0 || fixest:::cpp_isConstant(not_yet_treated[[yname]])) {
    return(tibble::tibble())
  }

  first_stage <- fixest::feols(
    stats::as.formula(paste0(yname, "~ 0 |", iname, " + ", tname)),
    data = not_yet_treated,
    combine.quick = FALSE,
    warn = FALSE,
    notes = FALSE)

  data[[paste0(yname, "_hat")]] <- stats::predict(first_stage, newdata = data)

  data[[paste0(yname, "_tilde")]] <- data[[yname]] -
    data[[paste0(yname, "_hat")]]

  return(data[!is.na(data[paste0(yname, "_tilde")]), ])
}
