aggregate_mean <- function(object, agg = "full", k_max = 5) {

  data <- object$df_indcp
  kname <- object$info$kname
  aname <- object$info$aname
  bname <- object$info$bname
  ytildename <- object$info$ytildename
  b_min <- object$info$b_min
  k_min <- object$info$k_min
  a_max <- object$info$a_max

  a_end <- a_max - k_min - 1 - k_max

  data <- object$df_indcp |>
    dplyr::filter(dplyr::between(!!rlang::sym(kname), k_min, k_max),
                  !!rlang::sym(aname) <= a_end - (!!rlang::sym(bname) - b_min))

  if (agg == "full") {

    aggregated <- data |>
      dplyr::summarize(n = dplyr::n(),
                       mean = mean(!!rlang::sym(ytildename)),
                       .by = kname) |>
      dplyr::arrange(!!rlang::sym(kname))

    return(aggregated)

  } else if (agg == "cage") {

    aggregated <- data |>
      dplyr::summarize(n = dplyr::n(),
                       mean = mean(!!rlang::sym(ytildename)),
                       .by = c(aname, kname)) |>
      dplyr::arrange(!!rlang::sym(aname), !!rlang::sym(kname))

    return(aggregated)

  } else if (agg == "cage_byear") {

    aggregated <- data |>
      dplyr::summarize(n = dplyr::n(),
                       mean = mean(!!rlang::sym(ytildename)),
                       .by = c(bname, aname, kname)) |>
      dplyr::arrange(!!rlang::sym(bname),
                     !!rlang::sym(aname),
                     !!rlang::sym(kname))

    return(aggregated)

  } else {
    stop("agg must be one of 'full', 'age', or 'age_byear'")
  }
}

aggregate_var <- function(object, agg = "full", k_max = 5) {

  object$info$k_max <- k_max

  b_min <- object$info$b_min
  b_max <- object$info$b_max
  aname <- object$info$aname
  kname <- object$info$kname

  result <- purrr::map(b_min:b_max, ~var_b(object, .x)) |>
    purrr::list_rbind() |>
    dplyr::filter(n > 0)

  if (agg == "full") {

    sum_mean <- result |>
      dplyr::summarize("mean" = stats::weighted.mean(!!rlang::sym("mean"),
                                                     w = !!rlang::sym("n")),
                       "n_mean" = sum(!!rlang::sym("n")),
                       .by = kname) |>
      dplyr::arrange(!!rlang::sym(kname))

    sum_sd <- result |>
      dplyr::filter(!is.na(!!rlang::sym("sd"))) |>
      dplyr::summarize("sd" = sqrt(stats::weighted.mean((!!rlang::sym("sd"))^2,
                                                        w = !!rlang::sym("n"))),
                       "n_sd" = sum(!!rlang::sym("n")),
                       .by = kname) |>
      dplyr::arrange(!!rlang::sym(kname))


    sum_full <- sum_mean |>
      dplyr::left_join(sum_sd, by = kname)

    return(sum_full)

  } else if (agg == "cage") {

    sum_mean <- result |>
      dplyr::summarize("mean" = stats::weighted.mean(!!rlang::sym("mean"),
                                                     w = !!rlang::sym("n")),
                       "n_mean" = sum(!!rlang::sym("n")),
                       .by = c(aname, kname)) |>
      dplyr::arrange(!!rlang::sym(aname), !!rlang::sym(kname))

    sum_sd <- result |>
      dplyr::filter(!is.na(!!rlang::sym("sd"))) |>
      dplyr::summarize("sd" = sqrt(stats::weighted.mean((!!rlang::sym("sd"))^2,
                                                        w = !!rlang::sym("n"))),
                       "n_sd" = sum(!!rlang::sym("n")),
                       .by = c(aname, kname)) |>
      dplyr::arrange(!!rlang::sym(aname), !!rlang::sym(kname))


    sum_cage <- sum_mean |>
      dplyr::left_join(sum_sd, by = c(aname, kname))

    return(sum_cage)

  } else if (agg == "cage_byear") {

    return(result)

  }
}

var_b <- function(object, b) {

  k_min <- object$info$k_min
  k_max <- object$info$k_max
  a_min <- object$info$a_min
  a_max <- object$info$a_max
  t_min <- object$info$t_min
  b_min <- object$info$b_min

  a_start <- max(a_min, t_min - b - k_min + 1)
  a_end <- min(a_max, a_max - k_min - 1 - k_max - (b - b_min))

  if (a_start > a_end) {
    return(tibble::tibble())
  }

  result <- purrr::map2(rep(a_start:a_end, each = k_max - k_min + 1),
                        rep(k_min:k_max, times = a_end - a_start + 1),
                        ~var_ak(object, b, .x, .y)) |>
    purrr::list_rbind()

  return(result)
}

var_ak <- function(object, b, a, k) {

  iname <- object$info$iname
  tname <- object$info$tname
  aname <- object$info$aname
  bname <- object$info$bname
  kname <- object$info$kname
  ytildename <- object$info$ytildename

  data <- object$df_indcp |>
    dplyr::filter(!!rlang::sym(bname) == b)

  sum_ak <- data |>
    dplyr::filter(!!rlang::sym(aname) == a, !!rlang::sym(tname) == b + a + k) |>
    dplyr::summarize(n = dplyr::n(),
                     mean_ytilde = mean(!!rlang::sym(ytildename)),
                     var_ytilde = stats::var(!!rlang::sym(ytildename)))

  epsilon_right <- data |>
    dplyr::filter(!!rlang::sym(aname) > a + k,
                  !!rlang::sym(tname) < b + a + k) |>
    dplyr::summarize(epsilon_right = mean(!!rlang::sym(ytildename)),
                     .by = !!rlang::sym(iname))

  sum_epsilon <- data |>
    dplyr::filter(!!rlang::sym(aname) > a + k,
                  !!rlang::sym(tname) == b + a + k) |>
    dplyr::left_join(epsilon_right, by = c(iname)) |>
    dplyr::mutate(epsilon_hat = !!rlang::sym(ytildename) - epsilon_right) |>
    dplyr::filter(!is.na(epsilon_hat)) |>
    dplyr::summarize(n = n(),
                     var_epsilon = stats::var(epsilon_hat))

  result <- tibble::tibble(!!bname := b,
                           !!aname := a,
                           !!kname := k,
                           "var" = sum_ak$var_ytilde - sum_epsilon$var_epsilon,
                           "mean" = sum_ak$mean_ytilde,
                           "n" = sum_ak$n) |>
    dplyr::mutate("var" = dplyr::if_else(var > 0, var, 0),
                  "sd" = sqrt(var)) |>
    dplyr::select(-c("var"))

  return(result)
}
