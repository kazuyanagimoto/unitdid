#' Title
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
#' @return dataframe with the residualized outcome variable
#' @export
#'
indcp <- function(data,
                  yname,
                  iname,
                  tname,
                  bname,
                  kname,
                  aname = NULL,
                  k_min = 0) {

  byears <- unique(data[[bname]])

  list_indcp <- lapply(byears, function(b) {
    indcp_byear(data, yname, iname, tname, bname, kname, k_min, b)
  })
  df_indcp <- do.call(rbind, list_indcp)

  # Return
  t_min <- data[[tname]] |> min()
  t_max <- data[[tname]] |> max()
  b_min <- data[[bname]] |> min()
  b_max <- data[[bname]] |> max()
  
  if (is.null(aname)) {
    df_indcp$cage <- df_indcp[[tname]] - df_indcp[[bname]] - df_indcp[[kname]]
    aname <- "cage"
  }

  a_min <- data[[aname]] |> min()
  a_max <- data[[aname]] |> max()

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
