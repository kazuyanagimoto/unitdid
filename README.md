
<!-- README.md is generated from README.Rmd. Please edit that file -->

# indcp

<!-- badges: start -->
<!-- badges: end -->

The `indcp` package provides a set of functions for the analysis of the
individual-level child penalties (Arkhangelsky, Yanagimoto, and Zohar,
2024)

## Installation

You can install the development version of indcp from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("kazuyanagimoto/indcp")
```

## Example

This is a basic example with the simulated `base_indcp` data set:

``` r
library(indcp)

base_indcp |>
  head()
#>   id year byear cage rel_time          y
#> 1  1 1999  1965   27        7 -0.5382475
#> 2  1 2000  1965   27        8 -0.0809379
#> 3  1 2001  1965   27        9 -0.2175191
#> 4  1 2002  1965   27       10 -0.5613995
#> 5  1 2003  1965   27       11 -0.4271110
#> 6  1 2004  1965   27       12 -0.4449481
```

Individual-level child penalties are estimated by `indcp()`:

``` r
mdl_base <- base_indcp |>
  indcp(yname = "y",
        iname = "id",
        tname = "year",
        bname = "byear",
        kname = "rel_time")

# Estimated individual-level child penalties (y_tilde)
mdl_base$df_indcp |>
  head()
#>   id year byear cage rel_time           y       y_hat      y_tilde
#> 1  3 1999  1965   41       -7  0.01747722  0.02659942 -0.009122202
#> 2  3 2000  1965   41       -6  0.16430370  0.17539746 -0.011093763
#> 3  3 2001  1965   41       -5  0.26554958  0.25778103  0.007768548
#> 4  3 2002  1965   41       -4 -0.08445270 -0.08140325 -0.003049458
#> 5  3 2003  1965   41       -3  0.20472857  0.19825995  0.006468619
#> 6  3 2004  1965   41       -2  0.21283326  0.20219996  0.010633293
```

### Aggregation (in the Mean)

They can be aggregated to the `full`, `cage` (age at childbirth), and
`cage_byear` (age at childbirth and birth year) levels:

``` r
summary(mdl_base) # default agg = "full"
#> # A tibble: 6 × 3
#>   rel_time     n    mean
#>      <int> <int>   <dbl>
#> 1        0  3319 -0.0318
#> 2        1  3319 -0.102 
#> 3        2  3319 -0.134 
#> 4        3  3319 -0.137 
#> 5        4  3319 -0.153 
#> 6        5  3319 -0.174
```

``` r
library(dplyr)
library(ggplot2)

sum_cage <- summary(mdl_base, agg = "cage")

sum_cage |>
  filter(rel_time == 0) |>
  mutate(rel_time = -1,
         mean = 0) |>
  bind_rows(sum_cage) |>
  filter(between(cage, 25, 34)) |>
  mutate(lbl_facet = paste0("Age ", cage)) |>
  ggplot(aes(x = rel_time, y = mean)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  facet_wrap(~lbl_facet, ncol = 5) +
  labs(x = "Time to First Childbirth", y = "Child Penalties on y") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
```

<img src="man/figures/README-agg_cage-1.svg" width="100%" />

### Variance of Individual-level Child Penalties

The `compute_var` option of the `summary` computes the variance of
individual-level child penalties. This can be computed from the (naive)
variance of the individual-level child penalties subtracted by the
estimated variance of the measurement errors.

``` r
sum_cage <- summary(mdl_base, agg = "cage", compute_var = TRUE)

sum_cage |>
  filter(rel_time == 0) |>
  mutate(rel_time = -1,
         sd = 0) |>
  bind_rows(sum_cage) |>
  filter(between(cage, 25, 34)) |>
  mutate(lbl_facet = paste0("Age ", cage)) |>
  ggplot(aes(x = rel_time, y = sd)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  facet_wrap(~lbl_facet, ncol = 5) +
  labs(x = "Time to First Childbirth", y = "S.D. of Child Penalties") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
```

<img src="man/figures/README-var_cage-1.svg" width="100%" />
