#' Aggregate the mean and variance of the estimated individual-level child penalties
#'
#' @param object `indcp` object
#' @param agg Aggregation method. One of `c("full", "cage", "cage_byear")` and the default is `full`. The `cage` means the age at the treatment. The `cage_byear` means the age at the treatment and the birth year.
#' @param na.rm Logical. If `TRUE` and `agg = c("cage", "cage_byear")`, remove `NA` values for `sd_epsilon` aggregation. The default is `TRUE`.
#'
#' @return A `tibble` with the summary statistics
#' @export
#'
aggregate_indcp <- function(object, agg = "full", na.rm = TRUE) {

  kname <- object$info$kname
  aname <- object$info$aname
  ytildename <- object$info$ytildename

  result <- dplyr::tibble()

  if (agg == "full") {

    result <- object$aggregated |>
      dplyr::summarize(!!paste0("mean_", ytildename) := stats::weighted.mean(!!rlang::sym(paste0("mean_", ytildename))),
                       dplyr::across(dplyr::starts_with("sd_"), ~sqrt(stats::weighted.mean(.x^2, w = !!rlang::sym("n"), na.rm = na.rm))),
                       n = sum(!!rlang::sym("n")),
                       .by = kname) |>
      dplyr::arrange(!!rlang::sym(kname))

  } else if (agg == "cage") {

    result <- object$aggregated |>
      dplyr::summarize(!!paste0("mean_", ytildename) := stats::weighted.mean(!!rlang::sym(paste0("mean_", ytildename))),
                       dplyr::across(dplyr::starts_with("sd_"), ~sqrt(stats::weighted.mean(.x^2, w = !!rlang::sym("n"), na.rm = na.rm))),
                       n = sum(!!rlang::sym("n")),
                       .by = c(aname, kname)) |>
      dplyr::arrange(!!rlang::sym(aname), !!rlang::sym(kname))

    return(result)

  } else if (agg == "cage_byear") {

    result <- object$aggregated

  } else {
    stop("agg must be one of 'full', 'cage', or 'cage_byear'")
  }

  return(result)
}