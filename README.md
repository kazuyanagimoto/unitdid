
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
library(dplyr)
library(ggplot2)

base_indcp |>
  head()
#> # A tibble: 6 × 6
#>      id  year byear  cage rel_time       y
#>   <int> <int> <int> <int>    <int>   <dbl>
#> 1     1  1999  1955    23       21  0.0426
#> 2     1  2000  1955    23       22 -0.868 
#> 3     1  2001  1955    23       23 -0.708 
#> 4     1  2002  1955    23       24  0.808 
#> 5     1  2003  1955    23       25 -1.43  
#> 6     1  2004  1955    23       26  0.766
```

Individual-level child penalties are estimated by `indcp()`:

``` r
mdl_base <- base_indcp |>
  filter(cage >= 25) |>
  indcp(yname = "y",
        iname = "id",
        tname = "year",
        bname = "byear",
        kname = "rel_time")

# Estimated individual-level child penalties (y_tilde)
mdl_base$df_indcp |>
  head()
#> # A tibble: 6 × 8
#>      id  year byear  cage rel_time       y   y_hat  y_tilde
#>   <int> <int> <int> <int>    <int>   <dbl>   <dbl>    <dbl>
#> 1  1015  1999  1957    44       -2  0.0326  0.0247  0.00788
#> 2  1015  2000  1957    44       -1  0.147   0.155  -0.00788
#> 3  1062  1999  1957    44       -2  0.0514  0.0723 -0.0209 
#> 4  1062  2000  1957    44       -1  0.224   0.203   0.0209 
#> 5  1078  1999  1957    43       -1 -0.0253 -0.0253  0      
#> 6  1078  2000  1957    43        0  0.0935  0.105  -0.0119
```

### Aggregation of Individual-level Child Penalties

They can be aggregated to the `full`, `cage` (age at childbirth), and
`cage_byear` (age at childbirth and birth year) levels:

``` r
summary(mdl_base) # default agg = "full"
#> # A tibble: 6 × 4
#>   rel_time mean_y_tilde sd_y_tilde     n
#>      <int>        <dbl>      <dbl> <int>
#> 1        0      -0.0569      0.151  5732
#> 2        1      -0.165       0.451  5732
#> 3        2      -0.247       0.480  5732
#> 4        3      -0.268       0.511  5732
#> 5        4      -0.291       0.524  5732
#> 6        5      -0.308       0.550  5732
```

``` r
sum_cage <- summary(mdl_base, agg = "cage")

sum_cage |>
  filter(rel_time == 0) |>
  mutate(rel_time = -1,
         mean_y_tilde = 0) |>
  bind_rows(sum_cage) |>
  filter(between(cage, 25, 34)) |>
  mutate(lbl_facet = paste0("Age ", cage)) |>
  ggplot(aes(x = rel_time, y = mean_y_tilde)) +
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

Since the individual-level child penalties are estimated with
measurement errors, the variance of the `y_tilde` is not equal to the
variance of the individual-level child penalties.

The `compute_var_me` option of the `indcp` estimates the variance of the
measurement errors and the variance of the individual-level child
penalties by subtracting the variance of the measurement errors from the
variance of `y_tilde`

``` r
mdl_base_var <- base_indcp |>
  filter(cage >= 25) |>
  indcp(yname = "y",
        iname = "id",
        tname = "year",
        bname = "byear",
        kname = "rel_time",
        compute_var_me = TRUE)

sum_cage_var <- summary(mdl_base_var, agg = "cage")

sum_cage_var |>
  filter(rel_time == 0) |>
  mutate(rel_time = -1,
         sd_y_tilde_estimated = 0) |>
  bind_rows(sum_cage_var) |>
  filter(between(cage, 25, 34)) |>
  mutate(lbl_facet = paste0("Age ", cage)) |>
  ggplot(aes(x = rel_time, y = sd_y_tilde_estimated)) +
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
