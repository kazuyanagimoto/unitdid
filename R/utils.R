weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  w <- w * length(w) / sum(w)
  sum(w * (x - stats::weighted.mean(x, w))^2) / (sum(w) - 1)
}