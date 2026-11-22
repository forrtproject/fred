This version history contains noteworthy changes. For a full history of changes, see the [commit history](https://github.com/forrtproject/FReD/commits/main/)

# FReD (development version)

## New features
- Added **Reported Success** as a success criterion in `assess_replication_outcome()` (`criterion = "reported_success"`) and the FReD Explorer. It uses the outcome reported by the replication authors, as coded in the `reported_success` variable, and matches the criterion of the same name in the web-based Explorer. The default criterion is unchanged.
- `assess_replication_outcome()` now accepts optional, opt-in coded p-value arguments (`pval_value_o`, `pval_type_o`, `pval_tails_o`, `pval_value_r`, `pval_type_r`, `pval_tails_r`) for `"significance_r"` and `"homogeneity_significance"`, handling inequality labels (`<`, `<=`, `=`, `>`, `>=` and variants), doubling one-tailed p-values onto the two-tailed scale, and falling back to `p_from_r()` wherever coded p-values are missing or indeterminate. The FReD Explorer continues to use the default raw effect/sample-size calculation.

## Bug fixes
- Fixed offline snapshot and `read_fred()` / `clean_variables()` type coercion: regenerated `inst/extdata/snapshot/data.RDS` with numeric `pval_value_o`/`pval_value_r` (preventing lexicographic string comparisons on scientific notation like `"4.02E-22"`) and character `es_value_o`/`es_value_r` (recovering ~1,220 APA test-statistic strings previously coerced to `NA` because `retain_es_as_character = TRUE` was ignored). Offline and error-fallback reads now run the same normalization as online reads.
- `update_offline_data()` now reads the target data file directly without silently falling back to the old inbuilt snapshot, restores `FRED_OFFLINE` via `on.exit()`, and returns succeeded/failed items; `.github/workflows/update-data.yml` now passes the exact hashed file and aborts on update failure.
- Fixed `assess_replication_outcome()` statistical criteria bugs:
  - `significance_agg` now returns `ci_ub` from `compare_effectsizes()` and catches significant opposite-direction aggregated effects as `"failure (reversal)"` instead of checking the impossible condition `ci_lb > 0 & est < 0`.
  - `homogeneity_significance` now uses `QEp >= 0.05` (matching `homogeneity`), distinguishes opposite-direction pooled significance, returns `NA_character_` (not `"not coded"`) in `outcome_report` when inputs are missing, and no longer reports `"OS not significant"` when replication data are missing.
  - `significance_r` no longer classifies rows with missing original significance as `"failure"`; `consistency_pi` explicitly defaults to `NA_character_`; `small_telescopes` now handles unaligned negative `es_o`.
  - `assess_replication_outcome()` now validates that `criterion` is a single string and that input vector lengths are compatible (equal or length 1).
- Fixed `code_replication_outcomes()`: `"inconsistent, opposite"` is now checked before `"inconsistent, smaller"` (previously unreachable for positive `es_o`), and missing original/replication p-values no longer produce `"failed replication"`.
- Fixed `align_effect_direction()` so rows with `es_o == 0` or `is.na(es_o)` preserve `es_r` instead of multiplying by `0` or `NA`.
- Fixed `convert_effect_sizes()` and `add_common_effect_sizes()`: added `"r-squared"`, `"f-square"`, `"f-squared"` aliases, decimal df and leading-decimal support in `t`/`F`/`z` parsers, domain guards for invalid inputs, `NA_real_` return types, `±1` clamping to `±0.9999` for converted correlations, proper `quiet = TRUE` suppression, and fixed broken `all.equal()` column-length checks across `add_common_effect_sizes()`, `add_uncertainty()`, and `add_sampling_variances()`.
- Fixed `p_from_r()`, `compute_ci_r()`, `compute_power_r()`, `find_r_for_power()`, `calculate_prediction_interval()`, `add_replication_power()`, and `augment_for_zcurve()` to return typed `NA_real_` and guard invalid `n` and `|r| >= 1`.
- Fixed several FReD Explorer issues (`power_r` moderator column name, `NA`-safe filtering for `source` and `power_r`, single-row outcome updates, `na.rm = TRUE` on forest-plot x-axis limits, empty reference-checker input handling, and synced `replication_outcome_styles.R` colours).
- Chi-squared statistics are only converted to r when they can come from a 2 x 2 contingency table: chi2(1) with N and chi2 / N <= 1. Other chi-squared values (df > 1, no N, chi2 / N > 1) return `NA` with a reason instead of impossible values such as r = 1.19 (forrtproject/fred-data#66).
- `add_common_effect_sizes()` honours dataset flags (`es_not_convertible_o`, `es_not_convertible_r`) for chi2(1) from Wald, likelihood-ratio, model-fit or goodness-of-fit tests, and records reasons in `es_o_na_reason` / `es_r_na_reason`.

# FReD 0.2.0

## Breaking Changes
- **New data source URL**: The package now uses a new data source with updated variable naming conventions
- **Variable naming convention**: All variable names now use `_o` suffix for original study variables and `_r` suffix for replication study variables (e.g., `es_original` → `es_o`, `n_replication` → `n_r`, `ref_original` → `ref_o`, `doi_replication` → `doi_r`)
- **`run_annotator()` deprecated**: The local annotator app has been removed. `run_annotator()` now opens the web version at forrt.org instead

## Notes
- If you have code that references old variable names, you will need to update it to use the new `_o`/`_r` suffixes

# FReD 0.1.0

## New features
- The Replication Database has merged with the FORRT Reversals & Replications project to become FReD
- The FReD R package now wraps the shiny apps, to facilitate installation and maintenance
- The Shiny apps now offer the choice between many methods to assess replication success, detailled in a vignette

## Enhancements
- Further expansion of the dataset
- The package now defaults to using offline data to speed up the apps, but prompts users to update data on package load after 30 days if a new release is available. To always use the latest data, run `use_FReD_offline(FALSE)`.

## Bug Fixes
- Change user defaults from options to environment variables, so that they are respected in background apps.
- Effect size conversions now retain the sign of the original effect size (and introduce it for odds ratios)

