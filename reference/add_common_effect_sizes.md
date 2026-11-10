# Add common effect size columns to FReD dataset

Converts original and replication effect sizes to common metric (r) and
adds them to the dataset.

## Usage

``` r
add_common_effect_sizes(
  fred_data,
  es_value_columns = c("es_value_o", "es_value_r"),
  es_type_columns = c("es_type_o", "es_type_r"),
  es_common_names = c("es_o", "es_r"),
  coalesce_values = TRUE,
  not_convertible_columns = c("es_not_convertible_o", "es_not_convertible_r")
)
```

## Arguments

- fred_data:

  FReD dataset

- es_value_columns:

  Character vector of column names for effect sizes

- es_type_columns:

  Character vector of column names for effect size types

- es_common_names:

  Names of columns where effect sizes should be saved. If
  coalesce_values is TRUE, existing values in these columns will be
  retained *if* no conversion is possible.

- coalesce_values:

  Logical. Should existing values in es_type_columns be retained?

- not_convertible_columns:

  Names of optional columns in which the dataset records why an effect
  must not be converted to r (e.g., a chi-squared(1) from a Wald,
  likelihood-ratio, model-fit or goodness-of-fit test rather than from a
  contingency table). A non-empty value sets the common effect size to
  `NA`. Missing columns are ignored.

## Value

FReD dataset with additional columns for common effect sizes, and
columns `<es_common_names>_na_reason` explaining why an effect has no
common effect size where a reason is known.
