weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA)
  }

  w <- w * length(w) / sum(w)
  sum(w * (x - stats::weighted.mean(x, w))^2) / (sum(w) - 1)
}

weighted.cov <- function(x, y, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[!is.na(x) & !is.na(y)]
    x_tmp <- x[!is.na(x) & !is.na(y)]
    y <- y[!is.na(x) & !is.na(y)]
    x <- x_tmp
  }

  if (length(x)  == 0) {
    return(NA)
  }

  w <- w * length(w) / sum(w)
  sum(w * (x - stats::weighted.mean(x, w)) *
        (y - stats::weighted.mean(y, w))) / (sum(w) - 1)
}
