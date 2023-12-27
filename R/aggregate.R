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

  data <- data[data[[aname]] <= a_end - (data[[bname]] - b_min) &
                 data[[kname]] >= k_min &
                 data[[kname]] <= k_max, ]

  if (agg == "full") {
    aggregated <- stats::aggregate(data[[ytildename]],
                            by = list(k = data[[kname]]),
                            FUN = function(x) c(n = length(x), tau = mean(x)))

    return(data.frame(k = aggregated$k,
                      n = aggregated$x[, 1],
                      tau = aggregated$x[, 2]))
  } else if (agg == "cage") {
    aggregated <- stats::aggregate(data[[ytildename]],
                            by = list(k = data[[kname]], cage = data[[aname]]),
                            FUN = function(x) c(n = length(x), tau = mean(x)))

    return(data.frame(k = aggregated$k,
                      cage = aggregated$cage,
                      n = aggregated$x[, 1],
                      tau = aggregated$x[, 2]))
  } else if (agg == "cage_byear") {
    aggregated <- stats::aggregate(data[[ytildename]],
                            by = list(k = data[[kname]],
                                      cage = data[[aname]],
                                      byear = data[[bname]]),
                            FUN = function(x) c(n = length(x), tau = mean(x)))

    return(data.frame(k = aggregated$k,
                      cage = aggregated$cage,
                      byear = aggregated$byear,
                      n = aggregated$x[, 1],
                      tau = aggregated$x[, 2]))
  } else {
    stop("agg must be one of 'full', 'age', or 'age_byear'")
  }
}

aggregate_var <- function(object, agg = "full", k_max = 5) {

  object$info$k_max <- k_max

  b_min <- object$info$b_min
  b_max <- object$info$b_max

  result <- lapply(b_min:b_max, function(b) {
    var_b(object, b)
  })
  result <- do.call(rbind, result)

  if (agg == "full") {

    result_list <- split(result, result$rel_time) |>
      lapply(function(df) {
        n <- sum(df$n)
        tau <- stats::weighted.mean(df$tau, w = df$n)
        sd <- sqrt(stats::weighted.mean(df$sd^2, w = df$n))
        rel_time <- df$rel_time[1]
        return(data.frame(rel_time = rel_time, n = n, tau = tau, sd = sd))
      })

    result_summary <- do.call(rbind, result_list)
    row.names(result_summary) <- NULL

    return(result_summary)

  } else if (agg == "cage") {

    result_list <- split(result, list(result$rel_time, result$cage)) |>
      lapply(function(df) {
        return(data.frame(cage = df$cage[1],
                          rel_time = df$rel_time[1],
                          tau = stats::weighted.mean(df$tau, w = df$n),
                          sd = sqrt(stats::weighted.mean(df$sd^2, w = df$n)),
                          n = sum(df$n)))
      })

    result_summary <- do.call(rbind, result_list)
    row.names(result_summary) <- NULL
    return(result_summary)

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

  result <- mapply(function(a, k) {
    var_ak(object, b, a, k)
  },
  rep(a_start:a_end, each = k_max - k_min + 1),
  rep(k_min:k_max, times = a_end - a_start + 1),
  SIMPLIFY = FALSE)

  result <- do.call(rbind, result)

  return(result)
}

var_ak <- function(object, b, a, k) {

  data <- object$df_indcp
  iname <- object$info$iname
  tname <- object$info$tname
  aname <- object$info$aname
  bname <- object$info$bname
  ytildename <- object$info$ytildename

  data <- data[data[[bname]] == b, ]
  n <- data[data[[aname]] == a & data[[tname]] == b + a + k, ] |> nrow()
  mean_ytilde <- data[data[[aname]] == a &
                        data[[tname]] == b + a + k, ][[ytildename]] |> mean()
  var_ytilde <- data[
    data[[aname]] == a & data[[tname]] == b + a + k, ytildename
  ][[ytildename]] |> stats::var()

  epsilon_right <- data[data[[aname]] > a + k & data[[tname]] < b + a + k, ] |>
    stats::aggregate(eval(stats::as.formula(paste0(ytildename, " ~ ", iname))),
                     data = _, FUN = mean)
  epsilon_right <- epsilon_right[[ytildename]]

  var_epsilon <- (data[data[[aname]] > a + k & data[[tname]] == b + a + k,
                  ][[ytildename]] - epsilon_right) |>
    stats::var()

  result <- data.frame(byear = b, cage = a, rel_time = k,
                       tau = mean_ytilde,
                       var = var_ytilde - var_epsilon,
                       n = n)

  result$sd <- ifelse(result$var < 0, 0, sqrt(result$var))
  result$var <- NULL

  return(result)
}
