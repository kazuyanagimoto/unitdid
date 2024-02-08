compute_projection <- function(object) {

  object$aggregated <- object$data |>
    dplyr::group_by(!!rlang::sym(object$info$by)) %>%
    dplyr::do(projection_group(., object$info$tname, object$info$aname,
                               object$info$k_min, object$info$k_max,
                               object$info$compute_var,
                               object$info$only_full_horizon)) |>
    dplyr::ungroup()

  return(object)
}

projection_group <- function(data, tname, aname, k_min, k_max,
                             compute_var, only_full_horizon) {

  aggregated <- data |>
    dplyr::filter(dplyr::between(zz000k, k_min, k_max)) |>
    dplyr::summarize(zz000mean = mean(zz000ytilde),
                     zz000vartr = stats::var(zz000ytilde),
                     zz000n = dplyr::n(),
                     .by = c(aname, "zz000k")) |>
    dplyr::arrange(!!rlang::sym(aname), zz000k)

  if (nrow(aggregated) == 0) {
    return(aggregated)
  }

  if (only_full_horizon) {
    ak_feasible <- aggregated |>
      dplyr::distinct(!!rlang::sym(aname), zz000k)

    ak_feasible <- ak_feasible |>
      dplyr::summarize(zz000k_max = max(zz000k), .by = aname) |>
      dplyr::filter(zz000k_max == k_max) |>
      dplyr::select(-zz000k_max) |>
      dplyr::left_join(ak_feasible, by = aname)

    if (nrow(ak_feasible) == 0) {
      return(aggregated[FALSE, ])
    }

    aggregated <- ak_feasible |>
      dplyr::left_join(aggregated, by = c(aname, "zz000k"))
  }

  if (compute_var) {
    sum_varcont <- ak_feasible |>
      dplyr::mutate(zz000varcont = purrr::map2_dbl(!!rlang::sym(aname), zz000k,
                                                ~ varcont_ak(data, tname, aname, .x, .y)))

    aggregated <- aggregated |>
      dplyr::left_join(sum_varcont, by = c(aname, "zz000k")) |>
      dplyr::mutate(zz000var = zz000vartr - zz000varcont)
  }

  return(aggregated)
}

varcont_ak <- function(data, tname, aname, a, k) {

  stats::var(data[data[[aname]] > a + k & data[[tname]] <= a + k, ]$zz000ytilde)

}