# Add power

Estimates the power of the replication study, given the original effect
size, the sample size of the replication study, and a one-tailed
("greater") test.

## Usage

``` r
add_replication_power(
  fred_data,
  es_original = "es_o",
  N_replication = "n_r",
  power_column = "power_r",
  alpha = 0.05
)
```

## Arguments

- fred_data:

  FReD dataset

- es_original:

  Character. Name of original effect size column.

- N_replication:

  Character. Name of replication sample size column.

- power_column:

  Character. Name of target column for power.

- alpha:

  Numeric. Significance level for the one-tailed test (default `0.05`).

## Value

Augmented FReD dataset with power column.

## Details

For `N_replication <= 2` (`df <= 0`), a correlation test cannot be
performed and power is returned as `NA`. For `2 < N_replication < 4`
(i.e. `n = 3`, where `df = 1` but Fisher's \\z\\ approximation requires
\\n \ge 4\\), power is floored at `alpha` whenever a valid original
effect size (`|r| < 1`) is available.
