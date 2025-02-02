

<!-- README.md is generated from README.Rmd. Please edit that file -->

# [unitdid](https://kazuyanagimoto.com/unitdid/)

<!-- badges: start -->

<a href="https://kazuyanagimoto.r-universe.dev"><img src="https://kazuyanagimoto.r-universe.dev/badges/unitdid" class="img-fluid" alt="R-universe status badge"></a>
[![R-CMD-check](https://github.com/kazuyanagimoto/unitdid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kazuyanagimoto/unitdid/actions/workflows/R-CMD-check.yaml)
<a href = "https://github.com/kazuyanagimoto/unitdid/blob/main/LICENSE.md" target = "_blank"><img src="https://img.shields.io/badge/license-MIT-blue"></a>
[![Docs](https://img.shields.io/badge/docs-homepage-blue.svg)](https://kazuyanagimoto.com/unitdid/index.html)
<!-- badges: end -->

The `unitdid` package provides a set of functions for the analysis of
the unit-level event studies (ULES) ([Arkhangelsky, Yanagimoto, and
Zohar 2024](https://arxiv.org/abs/2403.19563)).

## Installation

You can install the development version of unitdid from R-universe with:

``` r
install.packages('unitdid',
                 repos = 'https://kazuyanagimoto.r-universe.dev')
```

## Example

This is a basic example with the simulated `base_heterocp` data set:

``` r
library(unitdid)
library(dplyr)
library(ggplot2)

base_heterocp |>
    head()
#> # A tibble: 6 × 5
#>      id  year byear cyear      y
#>   <int> <int> <int> <int>  <dbl>
#> 1     1  1999  1955  1985 -0.848
#> 2     1  2000  1955  1985  0.759
#> 3     1  2001  1955  1985 -1.03 
#> 4     1  2002  1955  1985  0.858
#> 5     1  2003  1955  1985 -0.866
#> 6     1  2004  1955  1985 -0.651
```

ULES are estimated by `unitdid()`:

``` r
mdl_base <- base_heterocp |>
    unitdid(
        yname = "y",
        iname = "id",
        tname = "year",
        ename = "cyear",
        bname = "byear"
    )

# Estimated ULES (y_tilde)
get_unitdid(mdl_base)
#> # A tibble: 32,257 × 6
#>       id  year byear cyear      y    y_tilde
#>    <int> <int> <int> <int>  <dbl>      <dbl>
#>  1   705  2000  1957  2000 0.138  -0.0287   
#>  2   997  2000  1958  2000 0.138   0.0849   
#>  3   998  2000  1958  2000 0.119  -0.104    
#>  4  1013  2000  1958  2000 0.115  -0.0000709
#>  5  1082  2000  1958  2000 0.0362  0.00549  
#>  6  1127  2000  1958  2000 0.386   0.125    
#>  7  1225  2001  1959  2001 0.158  -0.118    
#>  8  1228  2000  1959  2000 0.241  -0.0937   
#>  9  1228  2001  1959  2000 0.443   0.0226   
#> 10  1230  2000  1959  2000 0.143  -0.0266   
#> # ℹ 32,247 more rows
```

### Aggregation

They can be aggregated to the `full`, `event` (year at event
(treatment). Mainly for staggered DiD design), `event_age` (age at
event. Mainly for child penalties) levels:

``` r
summary(mdl_base) # default agg = "full"
#> # A tibble: 6 × 3
#>   rel_time    mean     n
#>      <int>   <dbl> <dbl>
#> 1        0 -0.0630  6332
#> 2        1 -0.187   5987
#> 3        2 -0.296   5593
#> 4        3 -0.308   5214
#> 5        4 -0.346   4774
#> 6        5 -0.349   4357
```

The `only_full_horizon` option restricts the summary to the units that
have the full horizon (`k_min`, … , `k_max`) for the estimates:

``` r
sum_eage <- summary(mdl_base, agg = "event_age", only_full_horizon = TRUE)

sum_eage |>
    filter(rel_time == 0) |>
    mutate(rel_time = -1, mean = 0) |>
    bind_rows(sum_eage) |>
    filter(between(event_age, 25, 34)) |>
    mutate(lbl_facet = paste0("Age ", event_age)) |>
    ggplot(aes(x = rel_time, y = mean)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = -1, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    facet_wrap(~lbl_facet, ncol = 5) +
    labs(x = "Relative Time to Event", y = "Aggregated ULES") +
    theme_minimal() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
    )
```

<img src="man/figures/README-agg_cage-1.svg" style="width:100.0%" />

### Variance of ULES

Since the ULES are estimated with measurement errors, the variance of
the `y_tilde` is not equal to the variance of the ULES.

The `compute_varcov = "var"` option of the `unitdid` estimates the
variance of the measurement errors and the variance of the ULES by
subtracting the variance of the measurement errors from the variance of
`y_tilde`

``` r
mdl_base <- base_heterocp |>
    unitdid(
        yname = "y",
        iname = "id",
        tname = "year",
        ename = "cyear",
        bname = "byear",
        compute_varcov = "var"
    )

sum_eage <- summary(mdl_base, agg = "event_age", only_full_horizon = TRUE)

sum_eage |>
    filter(rel_time == 0) |>
    mutate(rel_time = -1, var = 0) |>
    bind_rows(sum_eage) |>
    filter(between(event_age, 25, 34)) |>
    mutate(lbl_facet = paste0("Age ", event_age)) |>
    ggplot(aes(x = rel_time, y = sqrt(var))) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = -1, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    facet_wrap(~lbl_facet, ncol = 5) +
    labs(x = "Relative Time to Event", y = "S.D. of ULES") +
    theme_minimal() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
    )
```

<img src="man/figures/README-var_cage-1.svg" style="width:100.0%" />

## References

Arkhangelsky, Dmitry, Kazuharu Yanagimoto, and Tom Zohar. 2024.
“Flexible Analysis of Individual Heterogeneity in Event Studies:
Application to the Child Penalty.” arXiv.
<https://arxiv.org/abs/2403.19563>.
