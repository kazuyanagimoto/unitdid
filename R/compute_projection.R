compute_projection <- function(object) {

  object$aggregated <- object$data |>
    dplyr::group_by(!!!rlang::syms(object$info$by_est)) %>%
    dplyr::do(projection_group(., object$info$iname, object$info$tname,
                               object$info$ename,
                               object$info$k_min, object$info$k_max,
                               object$info$compute_var,
                               object$info$only_full_horizon)) |>
    dplyr::ungroup()

  object$data <- object$data |> dplyr::ungroup()

  return(object)
}

projection_group <- function(data, iname, tname, ename, k_min, k_max,
                             compute_var, only_full_horizon) {

  aggregated <- data |>
    dplyr::filter(dplyr::between(zz000k, k_min, k_max)) |>
    dplyr::summarize(zz000mean = stats::weighted.mean(zz000ytilde, w = zz000w),
                     zz000vartr = weighted.var(zz000ytilde, w = zz000w),
                     zz000w = sum(zz000w),
                     .by = c(ename, "zz000k")) |>
    dplyr::arrange(!!rlang::sym(ename), zz000k)

  if (nrow(aggregated) == 0) {
    return(aggregated)
  }

  if (only_full_horizon) {
    ak_feasible <- aggregated |>
      dplyr::distinct(!!rlang::sym(ename), zz000k)

    ak_feasible <- ak_feasible |>
      dplyr::summarize(zz000k_max = max(zz000k), .by = ename) |>
      dplyr::filter(zz000k_max == k_max) |>
      dplyr::select(-zz000k_max) |>
      dplyr::left_join(ak_feasible, by = ename)

    if (nrow(ak_feasible) == 0) {
      return(aggregated[FALSE, ])
    }

    aggregated <- ak_feasible |>
      dplyr::left_join(aggregated, by = c(ename, "zz000k"))
  }

  if (compute_var) {
    sum_varcont <- ak_feasible |>
      dplyr::mutate(zz000varcont = purrr::map2_dbl(
          !!rlang::sym(ename), zz000k,
          ~ varcont_ek(data, iname, tname, ename, .x, .y)))

    aggregated <- aggregated |>
      dplyr::left_join(sum_varcont, by = c(ename, "zz000k")) |>
      dplyr::mutate(zz000var = zz000vartr - zz000varcont)
  }

  return(aggregated)
}

varcont_ek <- function(data, iname, tname, ename, e, k) {

  v_ek <- data |>
    dplyr::filter(!!rlang::sym(ename) > e + k,
                  !!rlang::sym(tname) < e + k) |>
    dplyr::summarize(v_ek = stats::weighted.mean(zz000ytilde, w = zz000w),
                     .by = !!rlang::sym(iname))

  if (nrow(v_ek) == 0) {
    return(NA_real_)
  }

  data |>
    dplyr::filter(!!rlang::sym(ename) > e + k,
                  !!rlang::sym(tname) == e + k) |>
    dplyr::left_join(v_ek, by = c(iname)) |>
    dplyr::mutate(zz000epsilonhat = zz000ytilde - v_ek) |>
    dplyr::pull() |>
    stats::var(na.rm = TRUE) # For unbalanced panels, na.rm = TRUE

}