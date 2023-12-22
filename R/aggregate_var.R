aggregate_var <- function(object, agg = "full", k_max = 5) {
  
}


agg_one <- function(data,
                    ytildename,
                    iname,
                    tname,
                    bname,
                    aname,
                    kname,
                    a_min = 25,
                    a_max = 34,
                    k_min = 0,
                    k_max = 5) {

  result <- agg_cage(data, ytildename, iname, tname, bname, aname, kname,
                     a_min, a_max, k_min, k_max)

  result_list <- lapply(
    split(result, result$rel_time),
    function(df) {
      n <- sum(df$n)
      tau <- stats::weighted.mean(df$tau, df$n)
      sd <- sqrt(stats::weighted.mean(df$sd^2, df$n))
      return(data.frame(n = n, tau = tau, sd = sd, rel_time = df$rel_time[1]))
    }
  )

  result_df <- do.call(rbind, result_list)
  return(result_df)
}

agg_cage <- function(data, ytildename, iname, tname, bname, aname, kname,
                     a_min, a_max, k_min, k_max) {

  t_min <- data[[tname]] |> min()
  t_max <- data[[tname]] |> max()
  b_min <- data[[bname]] |> min()
  b_max <- data[[bname]] |> max()
  a_min <- max(a_min, t_min - b_max - k_min + 1)
  a_max <- min(a_max, t_max - b_min - k_max)

  result <- lapply(b_min:b_max, function(b) {
    agg_cage_byear(data, ytildename, iname, tname, bname, aname, kname, b,
                   t_min, t_max, a_min, a_max, k_min, k_max)
  })

  result <- do.call(rbind, result)

  # Aggregate by Age at Treatment
  result_list <- lapply(
    split(result, interaction(result$rel_time, result$cage)),
    function(df) {
      n <- sum(df$n)
      tau <- stats::weighted.mean(df$tau, df$n)
      sd <- sqrt(stats::weighted.mean(df$sd^2, df$n))
      return(data.frame(n = n, tau = tau, sd = sd, rel_time = df$rel_time[1]))
    }
  )

  result_df <- do.call(rbind, result_list)

  split_names <- strsplit(row.names(result_df), "\\.")
  result_df$rel_time <- as.integer(sapply(split_names, "[", 1))
  result_df$cage <- as.integer(sapply(split_names, "[", 2))

  row.names(result_df) <- NULL

  return(result_df)
}

agg_cage_byear <- function(data, ytildename, iname, tname, bname, aname, kname,
                           b, t_min, t_max, a_min, a_max, k_min, k_max) {

  a_start <- max(a_min, t_min - b - k_min + 1)
  a_end <- min(a_max, t_max - b - k_max)

  data <- data[data[[bname]] == b, ]
  result <- mapply(function(a, k) {
    agg_cage_byear_ak(data,
                      ytildename, iname, tname, bname, aname, kname, b, a, k)
  },
  rep(a_start:a_end, each = k_max - k_min + 1),
  rep(k_min:k_max, times = a_end - a_start + 1),
  SIMPLIFY = FALSE)

  result <- do.call(rbind, result)

  return(result)
}

agg_cage_byear_ak <- function(data,
                              ytildename,
                              iname,
                              tname,
                              bname,
                              aname,
                              kname,
                              b,
                              a,
                              k) {

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
  result <- subset(result, select = -var)

  return(result)
}
