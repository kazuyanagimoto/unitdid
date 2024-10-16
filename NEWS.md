# News

## 0.0.6

New Features:

- Export `varraw`, `varerr`, `covraw`, and `coverr` for explicit variance and covariance adjustment
- Simplify the argument in `unitdid` for new column names (`newnames`)

Bug Fixes:

- `varerr` and `coverr` is not correctly estimated when `k_min` is not 0
- Formula for `varerr` and `coverr` was incorrect (v0.0.6.1)

## 0.0.5

New Features:

- Support for unit-level covariance estimation, which allows for more flexible aggregation using the covariance matrix
- Add `only_full_horizon` option for `get_unitdid()`

## 0.0.4

New Features:

- Support for unit-level variance estimation, which allows for more flexible aggregation of the variance estimation
- Add `allow_negative_var` option to allow for negative variance estimation, which used to be automatically trimmed at 0 (v0.0.4.1).

Bug Fixes:

- Measurement error (`zz000varcont` in the code) is not esimated with weights ([#11](https://github.com/kazuyanagimoto/unitdid/issues/11))


## 0.0.3

New Features:

- Support for `first_stage` argument for more parametric first stage estimation
- Support for `by` argument for aggregation

Bug Fixes:

- Bug when `data[[yname]]` includes `NA` values
- Bug when variance estimation includes `NA` values (v0.0.3.1)
- Bug when `by` does not have a higher category (v0.0.3.2)

## 0.0.2

New Features:

- Support for `wname` argument for weighted estimation

Bug Fixes:

- Normalization in `summary` when `normalized = FALSE` in the model
- Bug when `data[[tname]]` includes `NA` values

## 0.0.1

- Initial GitHub submission
