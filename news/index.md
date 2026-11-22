# Changelog

## FReD (development version)

### New features

- Added **Reported Success** as a success criterion in
  [`assess_replication_outcome()`](https://forrt.org/fred/reference/assess_replication_outcome.md)
  (`criterion = "reported_success"`) and the FReD Explorer. It uses the
  outcome reported by the replication authors, as coded in the
  `reported_success` variable, and matches the criterion of the same
  name in the web-based Explorer. The default criterion is unchanged.
- [`assess_replication_outcome()`](https://forrt.org/fred/reference/assess_replication_outcome.md)
  can now optionally evaluate coded p-values from the dataset (with
  fallback to `p_from_r()`) for `"significance_r"` and
  `"homogeneity_significance"`; see
  [`?assess_replication_outcome`](https://forrt.org/fred/reference/assess_replication_outcome.md)
  for details
  ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5)).

### Bug fixes

- Fixed offline snapshot and
  [`read_fred()`](https://forrt.org/fred/reference/read_fred.md) type
  coercion so `pval_value_o`/`pval_value_r` are numeric and
  `es_value_o`/`es_value_r` stay character by default, recovering ~1,220
  APA test-statistic strings previously lost as `NA`
  ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5),
  [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- Fixed replication outcome bugs in
  [`assess_replication_outcome()`](https://forrt.org/fred/reference/assess_replication_outcome.md)
  and
  [`code_replication_outcomes()`](https://forrt.org/fred/reference/code_replication_outcomes.md)
  (reversal detection in `significance_agg`, unreachable
  `"inconsistent, opposite"` labels, `QEp` boundary and missing-data
  handling in `homogeneity_significance`, and `NA` p-value
  classification)
  ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5),
  [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- Fixed effect-size parsing and helper edge cases:
  [`align_effect_direction()`](https://forrt.org/fred/reference/align_effect_direction.md)
  preserves `es_r` when `es_o` is `0` or `NA`;
  [`convert_effect_sizes()`](https://forrt.org/fred/reference/convert_effect_sizes.md)
  supports decimal `df`, leading decimals, and `r-squared`/`f-square`
  aliases; `p_from_r()` and interval/power helpers guard invalid `n` and
  `|r| >= 1`
  ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5),
  [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- Fixed FReD Explorer filtering/plotting edge cases and prevented
  [`update_offline_data()`](https://forrt.org/fred/reference/update_offline_data.md)
  from silently saving stale snapshots on read errors
  ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5),
  [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- Chi-squared statistics are only converted to r when they can come from
  a 2 x 2 contingency table: chi2(1) with N and chi2 / N \<= 1. Other
  chi-squared values (df \> 1, no N, chi2 / N \> 1) return `NA` with a
  reason instead of impossible values such as r = 1.19
  (forrtproject/fred-data#66).
- [`add_common_effect_sizes()`](https://forrt.org/fred/reference/add_common_effect_sizes.md)
  honours dataset flags (`es_not_convertible_o`, `es_not_convertible_r`)
  for chi2(1) from Wald, likelihood-ratio, model-fit or goodness-of-fit
  tests, and records reasons in `es_o_na_reason` / `es_r_na_reason`.

## FReD 0.2.0

### Breaking Changes

- **New data source URL**: The package now uses a new data source with
  updated variable naming conventions
- **Variable naming convention**: All variable names now use `_o` suffix
  for original study variables and `_r` suffix for replication study
  variables (e.g., `es_original` → `es_o`, `n_replication` → `n_r`,
  `ref_original` → `ref_o`, `doi_replication` → `doi_r`)
- **[`run_annotator()`](https://forrt.org/fred/reference/run_annotator.md)
  deprecated**: The local annotator app has been removed.
  [`run_annotator()`](https://forrt.org/fred/reference/run_annotator.md)
  now opens the web version at forrt.org instead

### Notes

- If you have code that references old variable names, you will need to
  update it to use the new `_o`/`_r` suffixes

## FReD 0.1.0

### New features

- The Replication Database has merged with the FORRT Reversals &
  Replications project to become FReD
- The FReD R package now wraps the shiny apps, to facilitate
  installation and maintenance
- The Shiny apps now offer the choice between many methods to assess
  replication success, detailled in a vignette

### Enhancements

- Further expansion of the dataset
- The package now defaults to using offline data to speed up the apps,
  but prompts users to update data on package load after 30 days if a
  new release is available. To always use the latest data, run
  `use_FReD_offline(FALSE)`.

### Bug Fixes

- Change user defaults from options to environment variables, so that
  they are respected in background apps.
- Effect size conversions now retain the sign of the original effect
  size (and introduce it for odds ratios)
