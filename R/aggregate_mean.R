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
    aggregated <- aggregate(data[[ytildename]],
                            by = list(k = data[[kname]]),
                            FUN = function(x) c(n = length(x), tau = mean(x)))

    return(data.frame(k = aggregated$k,
                      n = aggregated$x[, 1],
                      tau = aggregated$x[, 2]))
  } else if (agg == "cage") {
    aggregated <- aggregate(data[[ytildename]],
                            by = list(k = data[[kname]], cage = data[[aname]]),
                            FUN = function(x) c(n = length(x), tau = mean(x)))

    return(data.frame(k = aggregated$k,
                      cage = aggregated$cage,
                      n = aggregated$x[, 1],
                      tau = aggregated$x[, 2]))
  } else if (agg == "cage_byear") {
    aggregated <- aggregate(data[[ytildename]],
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