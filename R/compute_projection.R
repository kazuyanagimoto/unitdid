compute_projection <- function(object) {

  object$aggregated <- object$data |>
    dplyr::group_by(!!!rlang::syms(object$info$by_est)) %>%
    dplyr::do(projection_group(., object$info$iname, object$info$tname,
                               object$info$ename,
                               object$info$k_min, object$info$k_max,
                               object$info$compute_var)) |>
    dplyr::ungroup()

  # Individual Level Variance
  if (object$info$compute_var) {
    object$data <- object$aggregated |>
      dplyr::select(object$info$by_est, object$info$ename, zz000k,
                    zz000mean, zz000varcont) |>
      dplyr::left_join(object$data,
                       by = c(object$info$by_est,
                              object$info$ename,
                              "zz000k")) |>
      dplyr::mutate(zz000var = (zz000ytilde - zz000mean)^2 - zz000varcont)
  } else {
    object$data <- object$data |>
      dplyr::mutate(zz000var = NA_real_) # For its convenience in the summary
  }

  return(object)
}

projection_group <- function(data, iname, tname, ename, k_min, k_max,
                             compute_var) {

  aggregated <- data |>
    dplyr::filter(dplyr::between(zz000k, k_min, k_max)) |>
    dplyr::summarize(zz000mean = stats::weighted.mean(zz000ytilde, w = zz000w),
                     zz000w = sum(zz000w),
                     .by = c(ename, "zz000k")) |>
    dplyr::arrange(!!rlang::sym(ename), zz000k)

  if (nrow(aggregated) == 0) {
    return(aggregated)
  }

  if (compute_var) {
    aggregated <- aggregated |>
      dplyr::mutate(zz000varcont = purrr::map2_dbl(
        !!rlang::sym(ename), zz000k,
        ~ varcont_ek(data, iname, tname, ename, .x, .y)))
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
    dplyr::summarize(zz000varcont = weighted.var(zz000epsilonhat,
                                                 w = zz000w,
                                                 na.rm = TRUE)) |>
    dplyr::pull(zz000varcont) # Note: for unbalanced panels, na.rm = TRUE
}