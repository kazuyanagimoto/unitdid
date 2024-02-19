#' Get unit-level Difference-in-Differences estimates
#'
#' @param object `unitdid` object
#' @param normalized Logical. If `TRUE`, the function will normalize them by the mean of the imputed outcome variable.
#' Default is inherited from the `unitdid` object.
#'
#' @return A dataframe with a new column of the unit-level DiD estimates
#' @export
#'
get_unitdid <- function(object, normalized = NULL) {

  if (is.null(normalized)) {
    normalized <- object$info$normalized
  }

  if (normalized) {
    object$data <- object$data |>
      dplyr::mutate(zz000t = !!rlang::sym(object$info$ename) + zz000k) |>
      dplyr::left_join(object$yhat_agg,
                       by = c(object$info$by_est, object$info$ename,
                              "zz000t" = object$info$tname)) |>
      dplyr::mutate(zz000ytilde = zz000ytilde / zz000yhat_agg)
  }

  object$data |>
    dplyr::filter(dplyr::between(zz000k, object$info$k_min, object$info$k_max)) |>
    dplyr::rename(!!rlang::sym(object$info$ytildename) := zz000ytilde) |>
    dplyr::select(-dplyr::starts_with("zz000"))
}
