#' Get individual-level child penalties
#'
#' @param object `indcp` object
#' @return A dataframe with estimated individual-level child penalties
#' @export
#'
get_indcp <- function(object) {

  object$data |>
    dplyr::filter(dplyr::between(zz000k, object$info$k_min, object$info$k_max)) |>
    dplyr::rename(!!rlang::sym(object$info$ytildename) := zz000ytilde) |>
    dplyr::select(-dplyr::starts_with("zz000"))
}
