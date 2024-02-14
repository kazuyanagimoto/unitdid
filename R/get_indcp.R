#' Get individual-level child penalties
#'
#' @param object `indcp` object
#' @return A dataframe with estimated individual-level child penalties
#' @export
#'
get_indcp <- function(object, normalized = NULL) {

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
