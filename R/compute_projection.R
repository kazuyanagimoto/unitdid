compute_projection <- function(object) {

  object$aggregated <- object$data |>
    dplyr::group_by(!!!rlang::syms(object$info$by_est)) %>%
    dplyr::do(projection_group(., object$info$iname, object$info$tname,
                               object$info$ename,
                               object$info$k_min, object$info$k_max,
                               object$info$compute_varcov)) |>
    dplyr::ungroup()

  # Individual Level Variance-Covariance
  object$data <- object$data |>
    dplyr::filter(dplyr::between(zz000k, object$info$k_min, object$info$k_max))

  if (object$info$compute_varcov == "var") {
    object$data <- object$aggregated |>
      dplyr::select(object$info$by_est, object$info$ename, zz000k,
                    zz000mean, zz000varerr) |>
      dplyr::left_join(object$data,
                       by = c(object$info$by_est,
                              object$info$ename,
                              "zz000k")) |>
      dplyr::mutate(zz000varraw = (zz000ytilde - zz000mean)^2,
                    zz000var = zz000varraw - zz000varerr)

  } else if (object$info$compute_varcov == "cov") {

    object$data <- object$aggregated |>
      dplyr::select(-zz000w) |>
      dplyr::left_join(object$data,
                       by = c(object$info$by_est,
                              object$info$ename,
                              "zz000k")) |>
      dplyr::left_join(object$data |>
                         dplyr::select(!!rlang::sym(object$info$iname),
                                       zz000l = zz000k,
                                       zz000ytilde_l = zz000ytilde),
                       by = c(object$info$iname, "zz000l")) |>
      dplyr::mutate(zz000covraw = (zz000ytilde - zz000mean) *
                      (zz000ytilde_l - zz000mean_l),
                    zz000cov = zz000covraw - zz000coverr)

  } else {
    object$data <- object$data |>
      dplyr::mutate(zz000var = NA_real_,
                    zz000varraw = NA_real_,
                    zz000varerr = NA_real_) # For its convenience in the summary
  }

  return(object)
}

projection_group <- function(data, iname, tname, ename, k_min, k_max,
                             compute_varcov) {

  aggregated <- data |>
    dplyr::filter(dplyr::between(zz000k, k_min, k_max)) |>
    dplyr::summarize(zz000mean = stats::weighted.mean(zz000ytilde, w = zz000w),
                     zz000w = sum(zz000w),
                     .by = c(ename, "zz000k")) |>
    dplyr::arrange(!!rlang::sym(ename), zz000k)

  if (nrow(aggregated) == 0) {
    return(aggregated)
  }

  if (compute_varcov == "var") {
    coverr <- coverr_group(data, iname, tname, ename, k_min, k_max,
                            compute_varcov)
    aggregated <- aggregated |>
      dplyr::left_join(coverr, by = c(ename, "zz000k"))
  }
  if (compute_varcov == "cov") {
    coverr <- coverr_group(data, iname, tname, ename, k_min, k_max,
                             compute_varcov)
    aggregated <- aggregated |>
      dplyr::left_join(coverr, by = c(ename, "zz000k")) |>
      dplyr::left_join(aggregated |>
                         dplyr::select(!!rlang::sym(ename),
                                       zz000l = zz000k,
                                       zz000mean_l = zz000mean),
                       by = c(ename, "zz000l"))
  }

  return(aggregated)
}

coverr_group <- function(data, iname, tname, ename, k_min, k_max,
                          compute_varcov) {

  feasible_ek <- data |>
    dplyr::filter(dplyr::between(zz000k, k_min, k_max)) |>
    dplyr::distinct(!!rlang::sym(ename), zz000k) |>
    dplyr::arrange(!!rlang::sym(ename), zz000k)

  altepsilon <- purrr::map2(feasible_ek[[ename]], feasible_ek$zz000k,
                            ~ altepsilon_ek(data, iname, tname, ename, k_min,
                                            .x, .y)) |>
    dplyr::bind_rows()

  if (compute_varcov == "var") {
    return(altepsilon |>
             dplyr::summarize(zz000varerr = weighted.var(zz000altepsilon,
                                                         w = zz000w,
                                                         na.rm = TRUE),
                              .by = c(ename, "zz000k")))
  }

  replicate(k_max - k_min + 1, feasible_ek, simplify = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::mutate(zz000l = rep(k_min:k_max, each = nrow(feasible_ek))) |>
    dplyr::filter(zz000k >= zz000l) |>
    dplyr::left_join(altepsilon, by = c(ename, "zz000k")) |>
    dplyr::left_join(altepsilon |>
                       dplyr::select(!!rlang::sym(iname),
                                     !!rlang::sym(ename),
                                     zz000l = zz000k,
                                     zz000altepsilon_l = zz000altepsilon),
                     by = c(iname, ename, "zz000l")) |>
    dplyr::summarize(zz000coverr = weighted.cov(zz000altepsilon,
                                                 zz000altepsilon_l,
                                                 w = zz000w,
                                                 na.rm = TRUE),
                     .by = c(ename, "zz000k", "zz000l"))
}

altepsilon_ek <- function(data, iname, tname, ename, k_min, e, k) {

  mean_epsilon <- data |>
    dplyr::filter(!!rlang::sym(ename) + k_min > e + k,
                  !!rlang::sym(tname) < e + k) |>
    dplyr::summarize(mean_epsilon = stats::weighted.mean(zz000ytilde,
                                                         w = zz000w),
                     .by = !!rlang::sym(iname))

  data |>
    dplyr::filter(!!rlang::sym(ename) + k_min > e + k,
                  !!rlang::sym(tname) == e + k) |>
    dplyr::left_join(mean_epsilon, by = c(iname)) |>
    dplyr::mutate(zz000altepsilon = zz000ytilde - mean_epsilon) |>
    dplyr::select(!!rlang::sym(iname), zz000altepsilon, zz000w) |>
    dplyr::mutate(!!rlang::sym(ename) := e, zz000k = k)
}
