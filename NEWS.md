This version history contains noteworthy changes. For a full history of changes, see the [commit history](https://github.com/forrtproject/FReD/commits/main/)

# FReD (development version)

## Data source
- FReD now downloads its data from the FReD release on Zenodo (<https://doi.org/10.5281/zenodo.24461606>) instead of OSF. The latest version is resolved through the Zenodo API when the data is first downloaded, and the package reads its `fred.csv` file. `FRED_DATA_URL` and `FRED_DATA_FILE` still override the default and accept .csv or .xlsx files.
- `create_citation()` cites the FReD Zenodo record (latest version) instead of the FLoRA citation, and `get_dataset_changelog()` combines the `CHANGELOG.md` files of all Zenodo versions.

## New features
- Added **Reported Success** as a success criterion in `assess_replication_outcome()` (`criterion = "reported_success"`) and the FReD Explorer. It uses the outcome reported by the replication authors, as coded in the `reported_success` variable, and matches the criterion of the same name in the web-based Explorer. The default criterion is unchanged.
- `assess_replication_outcome()` can now optionally evaluate coded p-values from the dataset (with fallback to `p_from_r()`) for `"significance_r"` and `"homogeneity_significance"`; see `?assess_replication_outcome` for details ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5)).

## Bug fixes
- Fixed offline snapshot and `read_fred()` type coercion so `pval_value_o`/`pval_value_r` are numeric and `es_value_o`/`es_value_r` stay character by default, recovering ~1,220 APA test-statistic strings previously lost as `NA` ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5), [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- Fixed replication outcome bugs in `assess_replication_outcome()` and `code_replication_outcomes()` (reversal detection in `significance_agg`, unreachable `"inconsistent, opposite"` labels, `QEp` boundary and missing-data handling in `homogeneity_significance`, and `NA` p-value classification) ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5), [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- `add_replication_power()` now returns `NA` when `n_r <= 2` (`df <= 0`, where a correlation test cannot be run) and floors power at `alpha` (default `0.05`) when `2 < n_r < 4` with a valid `es_o`, instead of returning `0` for `n_r < 4`.
- Fixed effect-size parsing and helper edge cases: `align_effect_direction()` preserves `es_r` when `es_o` is `0` or `NA`; `convert_effect_sizes()` supports decimal `df`, leading decimals, and `r-squared`/`f-square` aliases; `p_from_r()` and interval/power helpers guard invalid `n` and `|r| >= 1` ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5), [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
- Fixed FReD Explorer filtering/plotting edge cases and prevented `update_offline_data()` from silently saving stale snapshots on read errors ([5e631e5](https://github.com/forrtproject/FReD/commit/5e631e5), [1e8c225](https://github.com/forrtproject/FReD/commit/1e8c225)).
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

