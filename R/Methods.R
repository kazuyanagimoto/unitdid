#' Aggregate the mean and variance of the estimated individual-level child penalties
#'
#' @param object `indcp` object
#' @param agg Aggregation method. One of `c("full", "cage", "cage_byear")` and the default is `full`. The `cage` means the age at the treatment. The `cage_byear` means the age at the treatment and the birth year.
#'
#' @return A `tibble` with the summary statistics
#' @examples
#' library(indcp)
#' mdl_base <- base_indcp |>
#'   indcp(yname = "y",
#'         iname = "id",
#'         tname = "year",
#'         bname = "byear",
#'         kname = "rel_time")
#' summary(mdl_base, agg = "cage")
#'
#' @export
#'
summary.indcp <- function(object, agg = "full") {

  kname <- object$info$kname
  aname <- object$info$aname
  ytildename <- object$info$ytildename

  if (agg == "full") {

    result <- object$aggregated |>
        dplyr::summarize(!!paste0("mean_", ytildename) := stats::weighted.mean(!!rlang::sym(paste0("mean_", ytildename))),
                        !!paste0("sd_", ytildename) := sqrt(stats::weighted.mean((!!rlang::sym(paste0("sd_", ytildename)))^2, w = !!rlang::sym("n"))),
                        n = sum(!!rlang::sym("n")),
                        .by = kname) |>
        dplyr::arrange(!!rlang::sym(kname))

    if (object$info$compute_var_me) {
      result_epsilon <- object$aggregated |>
        dplyr::filter(!is.na(!!rlang::sym("sd_epsilon"))) |>
        dplyr::summarize("sd_epsilon" := sqrt(stats::weighted.mean((!!rlang::sym("sd_epsilon"))^2, w = !!rlang::sym("n"))),
                        !!rlang::sym(paste0("sd_", ytildename, "_estimated")) := sqrt(stats::weighted.mean((!!rlang::sym(paste0("sd_", ytildename, "_estimated")))^2, w = !!rlang::sym("n"))),
                        n_var_estimate = sum(!!rlang::sym("n")),
                        .by = kname)

      result <- result |>
        dplyr::left_join(result_epsilon, by = kname)
    }

    return(result)

  } else if (agg == "cage") {

    result <- object$aggregated |>
        dplyr::summarize(!!paste0("mean_", ytildename) := stats::weighted.mean(!!rlang::sym(paste0("mean_", ytildename))),
                        !!paste0("sd_", ytildename) := sqrt(stats::weighted.mean((!!rlang::sym(paste0("sd_", ytildename)))^2, w = !!rlang::sym("n"))),
                        n = sum(!!rlang::sym("n")),
                        .by = c(aname, kname)) |>
        dplyr::arrange(!!rlang::sym(aname), !!rlang::sym(kname))

    if (object$info$compute_var_me) {
      result_epsilon <- object$aggregated |>
        dplyr::filter(!is.na(!!rlang::sym("sd_epsilon"))) |>
        dplyr::summarize("sd_epsilon" := sqrt(stats::weighted.mean((!!rlang::sym("sd_epsilon"))^2, w = !!rlang::sym("n"))),
                        !!rlang::sym(paste0("sd_", ytildename, "_estimated")) := sqrt(stats::weighted.mean((!!rlang::sym(paste0("sd_", ytildename, "_estimated")))^2, w = !!rlang::sym("n"))),
                        n_var_estimate = sum(!!rlang::sym("n")),
                        .by = c(aname, kname))

      result <- result |>
        dplyr::left_join(result_epsilon, by = c(aname, kname))
    }

    return(result)

  } else if (agg == "cage_byear") {

    return(object$aggregated)

  } else {
    stop("agg must be one of 'full', 'age', or 'age_byear'")
  }
}
