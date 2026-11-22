# Assess Replication Outcomes Based on Various Criteria

This function evaluates the outcomes of replication studies against the
original studies using various statistical criteria (see below, and
[`vignette("success_criteria")`](https://forrt.org/fred/articles/success_criteria.md)
for more details).

## Usage

``` r
assess_replication_outcome(
  es_o,
  n_o,
  es_r,
  n_r,
  criterion,
  reported_success = NULL,
  result = NULL,
  pval_value_o = NULL,
  pval_type_o = NULL,
  pval_value_r = NULL,
  pval_type_r = NULL,
  pval_tails_o = NULL,
  pval_tails_r = NULL
)
```

## Arguments

- es_o:

  Numeric. The effect size from the original study.

- n_o:

  Integer. The sample size of the original study.

- es_r:

  Numeric. The effect size from the replication study.

- n_r:

  Integer. The sample size of the replication study.

- criterion:

  Character. The criterion to use for assessing the replication outcome.
  Options include: "significance_r", "significance_agg",
  "consistency_ci", "consistency_pi", "homogeneity",
  "homogeneity_significance", "small_telescopes" and "reported_success".

- reported_success:

  Character. The outcome reported for each replication, as coded in the
  `reported_success` variable of the FReD dataset. Required when
  `criterion = "reported_success"`, and ignored otherwise.

- result:

  Character. Optional fallback for `criterion = "reported_success"`,
  used where `reported_success` is missing: the `result` variable
  returned by
  [`load_fred_data()`](https://forrt.org/fred/reference/load_fred_data.md).

- pval_value_o:

  Numeric (or numeric string). Optional coded p-values for the original
  study (the `pval_value_o` column of FReD). Opt-in: used by
  `"significance_r"` and `"homogeneity_significance"` when supplied,
  falling back to `p_from_r(es_o, n_o)` where missing or indeterminate.

- pval_type_o:

  Character. Optional inequality type labels for `pval_value_o` (e.g.,
  `"l (<)"`, `"e (=)"`, `"g (>)"`, `"ge (>=)"`, `"le (<=)"`). Defaults
  to exact equality (`"="`) when omitted.

- pval_value_r:

  Numeric (or numeric string). Optional coded p-values for the
  replication study (the `pval_value_r` column of FReD). Opt-in: used by
  `"significance_r"` when supplied, falling back to
  `p_from_r(es_r, n_r)` where missing or indeterminate.

- pval_type_r:

  Character. Optional inequality type labels for `pval_value_r`.
  Defaults to exact equality (`"="`) when omitted.

- pval_tails_o:

  Character or numeric. Optional tail specification for `pval_value_o`
  (the `pval_tails_o` column of FReD, e.g., `"1 (one)"` vs `"2 (two)"`).
  One-tailed p-values are doubled (`pmin(2 * p, 1)`) before comparing
  against two-tailed alpha = 0.05; omitted or missing values are treated
  as two-tailed.

- pval_tails_r:

  Character or numeric. Optional tail specification for `pval_value_r`
  (the `pval_tails_r` column of FReD). Handled identically to
  `pval_tails_o`.

## Value

A data frame with the outcome of the assessment based on the specified
criterion, with three columns: `outcome` (success, failure or 'OS not
significant'), `outcome_detailed` (a specific description of the
criterion) and `outcome_report` (usually the same as outcome, but
further broken out where there are distinct reasons for failure). For
`criterion = "reported_success"`, all three columns contain the reported
label.

## Details

The function assesses the replication outcome using one of several
criteria:

- significance_r:

  Evaluates the statistical significance of the replication effect (and
  whether its direction is consistent with the original effect). If the
  original study was not significant, this is highlighted as
  `"OS not significant"` (even if the replication effect size is
  missing, since the criterion is already meaningless). Coded p-values
  (`pval_value_o`/`pval_value_r`, with optional `pval_type_*` and
  `pval_tails_*`) are opt-in: when omitted (the default, used by the
  FReD Explorer), significance is computed solely from `es` and `n` via
  two-tailed `p_from_r()`. When supplied, coded p-values are evaluated
  first (with one-tailed values doubled to the two-tailed scale) and
  `p_from_r()` is used as a fallback wherever a coded p-value is missing
  or indeterminate at alpha = 0.05.

- significance_agg:

  Aggregates the effect sizes from the original and replication studies
  using a meta-analytic approach and assesses whether the combined
  effect is significantly different from zero.

- consistency_ci:

  Checks whether the *original* effect size falls within the confidence
  interval of the *replication* effect size, thus assessing consistency
  between the original and replication findings.

- consistency_pi:

  Evaluates whether the replication effect size falls within the
  prediction interval derived from the original study and the size of
  the replication sample. This accounts for the expected variability in
  replication results.

- homogeneity:

  Assesses whether the effects from the original and replication studies
  are homogeneous (i.e., consistent) using a heterogeneity test
  (Q-test).

- homogeneity_significance:

  Combines the assessment of homogeneity with the significance of the
  effect sizes. It checks whether the two effects are homogeneous and
  jointly significantly different from zero. Because this criterion
  requires comparing both studies, rows with missing replication data
  return `NA` rather than `"OS not significant"`.

- small_telescopes:

  Tests whether the replication effect size is larger than the effect
  size that would have given the original study a power of 33%. Derived
  from Simonsohn (2015), the idea here is that replications should only
  count as successful if they indicate that the original study provided
  evidence (rather than a lucky guess).

- reported_success:

  Uses the outcome as described by the replication authors instead of
  calculating one from effect sizes, so the effect size and sample size
  arguments do not affect it. Coded labels are returned unchanged (e.g.,
  "successful", "failed", "mixed"). Where no outcome is coded, `result`
  is used, and "Not calculable" if that is missing as well.

## Examples

``` r
es_o <- 0.3  # Effect size from the original study
n_o <- 100   # Sample size of the original study
es_r <- 0.25 # Effect size from the replication study
n_r <- 120   # Sample size of the replication study
assess_replication_outcome(es_o, n_o, es_r, n_r, "significance_r")
#>   outcome                  outcome_detailed outcome_report
#> 1 success replication effect is significant        success
assess_replication_outcome(es_o, n_o, es_r, n_r, "reported_success", reported_success = "mixed")
#>   outcome outcome_detailed outcome_report
#> 1   mixed            mixed          mixed
```
