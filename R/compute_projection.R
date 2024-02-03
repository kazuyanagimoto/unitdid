compute_projection <- function(object) {

  kname <- object$info$kname
  aname <- object$info$aname
  bname <- object$info$bname
  ytildename <- object$info$ytildename
  k_min <- object$info$k_min
  k_max <- object$info$k_max

  # Aggregated Data by cage_byear level
  object$aggregated <- object$df_indcp |>
    dplyr::filter(dplyr::between(!!rlang::sym(kname), k_min, k_max)) |>
    dplyr::summarize(!!paste0("mean_", ytildename) := mean(!!rlang::sym(ytildename)),
                      !!paste0("sd_", ytildename) := stats::sd(!!rlang::sym(ytildename)),
                      n = dplyr::n(),
                      .by = c(bname, aname, kname)) |>
    dplyr::arrange(!!rlang::sym(bname),
                   !!rlang::sym(aname),
                   !!rlang::sym(kname))

  # Choose only the cohorts with full horizon
  if (object$info$only_full_horizon) {
    object$aggregated <- object$aggregated |>
      dplyr::summarize(n_k = dplyr::n(),
                       .by = c(bname, aname)) |>
      dplyr::filter(!!rlang::sym("n_k") == k_max - k_min + 1) |>
      dplyr::select(-dplyr::any_of("n_k")) |>
      dplyr::left_join(object$aggregated, by = c(bname, aname))

      object$info$b_min <- object$aggregated[[bname]] |> min()
      object$info$b_max <- object$aggregated[[bname]] |> max()
      object$info$a_min <- object$aggregated[[aname]] |> min()
      object$info$a_max <- object$aggregated[[aname]] |> max()
  }

  # Compute Variance of epsilon
  if (object$info$compute_var_me) {
    var_epsilon <- purrr::map(object$info$b_min:object$info$b_max,
                              ~var_epsilon_b(object, .x, k_max = k_max)) |>
      purrr::list_rbind()

    object$aggregated <- object$aggregated |>
      dplyr::left_join(var_epsilon, by = c(bname, aname, kname)) |>
      dplyr::mutate(!!paste0("var_", ytildename, "_estimated")
                      := (!!rlang::sym(paste0("sd_", ytildename)))^2 - (!!rlang::sym("sd_epsilon"))^2,
                    !!paste0("sd_", ytildename, "_estimated")
                      := sqrt(dplyr::if_else(!!rlang::sym(paste0("var_", ytildename, "_estimated")) > 0,
                                            !!rlang::sym(paste0("var_", ytildename, "_estimated")), 0))) |>
      dplyr::select(-dplyr::any_of(paste0("var_", ytildename, "_estimated")))
  }

  return(object)
}

var_epsilon_b <- function(object, b, k_max) {

  k_min <- object$info$k_min
  a_min <- object$info$a_min
  a_max <- object$info$a_max
  t_min <- object$info$t_min
  t_max <- object$info$t_max
  b_min <- object$info$b_min

  a_start <- max(a_min, t_min - b - k_min + 1)
  a_end <- min(a_max, t_max - k_min - 1 - k_max - (b - b_min))

  if (a_start > a_end) {
    return(dplyr::tibble())
  }

  result <- purrr::map2(rep(a_start:a_end, each = k_max - k_min + 1),
                        rep(k_min:k_max, times = a_end - a_start + 1),
                        ~var_epsilon_ak(object, b, .x, .y)) |>
    purrr::list_rbind()

  return(result)
}

var_epsilon_ak <- function(object, b, a, k) {

  iname <- object$info$iname
  tname <- object$info$tname
  aname <- object$info$aname
  bname <- object$info$bname
  kname <- object$info$kname
  ytildename <- object$info$ytildename

  df_var <- object$df_indcp |>
    dplyr::filter(!!rlang::sym(bname) == b)

  epsilon_right <- df_var |>
    dplyr::filter(!!rlang::sym(aname) > a + k,
                  !!rlang::sym(tname) < b + a + k) |>
    dplyr::summarize(epsilon_right = mean(!!rlang::sym(ytildename)),
                     .by = !!rlang::sym(iname))

  sum_epsilon <- df_var |>
    dplyr::filter(!!rlang::sym(aname) > a + k,
                  !!rlang::sym(tname) == b + a + k) |>
    dplyr::left_join(epsilon_right, by = c(iname)) |>
    dplyr::mutate(epsilon_hat = !!rlang::sym(ytildename) - epsilon_right) |>
    dplyr::filter(!is.na(!!rlang::sym("epsilon_hat"))) |>
    dplyr::summarize(sd_epsilon = stats::sd(!!rlang::sym("epsilon_hat")),
                     n = dplyr::n())

  result <- dplyr::tibble(!!bname := b,
                          !!aname := a,
                          !!kname := k,
                          "sd_epsilon" = sum_epsilon$sd_epsilon)
  return(result)
}
