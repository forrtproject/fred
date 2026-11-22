
#' Compare Effect Sizes Between Original and Replication Studies
#'
#' This function compares and combines effect sizes between an original and a replication study
#' using a meta-analytic approach. It calculates the combined effect sizes and evaluates heterogeneity between them.
#'
#' @param r1 Numeric vector. Correlation coefficients from the original study.
#' @param n1 Integer vector. Sample sizes of the original study.
#' @param r2 Numeric vector. Correlation coefficients from the replication study.
#' @param n2 Integer vector. Sample sizes of the replication study.
#'
#' @return A data frame containing:
#' \item{est}{Estimated effect sizes from the meta-analysis.}
#' \item{ci_lb}{Lower bounds of the confidence intervals for the estimated effect sizes.}
#' \item{ci_ub}{Upper bounds of the confidence intervals for the estimated effect sizes.}
#' \item{QEp}{p-values for the test of heterogeneity (Q-test) between effect sizes.}
#'
#' @examples
#' # Example usage:
#' r1 <- c(0.3, 0.4)
#' n1 <- c(100, 120)
#' r2 <- c(0.35, 0.45)
#' n2 <- c(110, 130)
#' compare_effectsizes(r1, n1, r2, n2)
#' @noRd

compare_effectsizes <- function(r1, n1, r2, n2) {
  if (length(r1) == 0L) {
    return(data.frame(est = numeric(0), ci_lb = numeric(0), ci_ub = numeric(0), QEp = numeric(0)))
  }

  process_pair <- function(r1, n1, r2, n2) {
    if (any(is.na(c(r1, n1, r2, n2))) || n1 <= 2 || n2 <= 2 || abs(r1) >= 1 || abs(r2) >= 1) {
      return(data.frame(est = NA_real_, ci_lb = NA_real_, ci_ub = NA_real_, QEp = NA_real_))
    }

    # Calculate effect sizes for each study
    r1_result <- metafor::escalc(ni = n1, ri = r1, measure = "COR")
    r2_result <- metafor::escalc(ni = n2, ri = r2, measure = "COR")

    # Meta-analysis on the two effect sizes
    res <- metafor::rma(yi = c(r1_result$yi, r2_result$yi),
                        vi = c(r1_result$vi, r2_result$vi),
                        method = "FE")

    return(data.frame(est = res$beta[1], ci_lb = res$ci.lb, ci_ub = res$ci.ub, QEp = res$QEp))
  }

  results <- mapply(process_pair, r1, n1, r2, n2, SIMPLIFY = FALSE)

  do.call(rbind, results)

}

#' Parse coded p-value inequality type strings into canonical operators
#'
#' Normalizes FReD `pval_type` labels (e.g., `"l (<)"`, `"e (=)"`, `"g (>)"`,
#' `"ge (>=)"`, `"le (<=)"`, `"<"`, `'"<"'`, `'"="'`, `"ge (>)"`, `"=<"`, `"=>"`,
#' `"\u2264"`, `"\u2265"`) to one of `"<"`, `"<="`, `"="`, `">"`, `">="`, or
#' `NA_character_` when unrecognised. Compound operators (`<=`, `=<`, `>=`, `=>`,
#' `le`, `ge`) and strict inequalities (`<`, `>`, `l`, `g`) are matched before
#' `=`/`e` so strings like `"ge (>=)"` and `"le (<=)"` are never misclassified as
#' equality.
#'
#' @param pval_type Character vector of p-value type labels.
#' @return Character vector of canonical operators (`"<"`, `"<="`, `"="`, `">"`, `">="`, or `NA_character_`).
#' @noRd
parse_pval_type <- function(pval_type) {
  s <- tolower(trimws(as.character(pval_type)))
  # Strip surrounding quotes (e.g., '"<"', '"="')
  s <- gsub('^["\']+|["\']+$', "", s)
  s <- trimws(s)

  out <- rep(NA_character_, length(s))
  missing_or_empty <- is.na(s) | s == ""
  out[missing_or_empty] <- "="

  non_empty <- !missing_or_empty
  if (any(non_empty)) {
    v <- s[non_empty]
    op <- dplyr::case_when(
      grepl("<=|=<|\u2264|^le(?:\\b|\\s|\\()", v, perl = TRUE) ~ "<=",
      grepl(">=|=>|\u2265|^ge(?:\\b|\\s|\\()", v, perl = TRUE) ~ ">=",
      grepl("<|^l(?:\\b|\\s|\\()", v, perl = TRUE) ~ "<",
      grepl(">|^g(?:\\b|\\s|\\()", v, perl = TRUE) ~ ">",
      grepl("=|^e(?:\\b|\\s|\\()", v, perl = TRUE) ~ "=",
      .default = NA_character_
    )
    out[non_empty] <- op
  }

  out
}

#' Detect one-tailed p-value labels
#'
#' @param pval_tails Character or numeric vector of tail specifications (e.g., `"1 (one)"`, `"1"`, `"2 (two)"`).
#' @return Logical vector (`TRUE` for one-tailed, `FALSE` otherwise).
#' @noRd
is_one_tailed <- function(pval_tails) {
  if (is.null(pval_tails)) return(logical(0))
  s <- tolower(trimws(as.character(pval_tails)))
  !is.na(s) & grepl("^(?:1(?:\\b|\\s|\\()|one\\b|one[- ]?(?:tailed|sided))", s, perl = TRUE)
}

#' Determine statistical significance (two-tailed p < alpha) from coded p-values and types
#'
#' Evaluates whether a coded p-value and inequality type definitively establish
#' two-tailed `p < alpha` (`TRUE`), `p >= alpha` (`FALSE`), or are indeterminate
#' (`NA`, e.g., `p < 0.10`, `p <= 0.05`, or `p > 0.01` at `alpha = 0.05`).
#' When `pval_tails` marks an entry as one-tailed (e.g., `"1"` or `"1 (one)"`),
#' the coded p-value is doubled (`pmin(2 * p, 1)`) before comparison against
#' two-tailed `alpha` so coded p-values use the same two-tailed threshold as
#' `p_from_r()`.
#'
#' @param pval_value Numeric (or character coercible to numeric) vector of coded p-values.
#' @param pval_type Optional character vector of p-value inequality types (defaults to exact `"="`).
#' @param pval_tails Optional character/numeric vector indicating 1- vs 2-tailed tests.
#' @param alpha Two-tailed significance threshold (default `0.05`).
#' @return Logical vector (`TRUE`, `FALSE`, or `NA`).
#' @noRd
is_p_significant <- function(pval_value, pval_type = NULL, pval_tails = NULL, alpha = 0.05) {
  val <- suppressWarnings(as.numeric(pval_value))
  val[is.nan(val) | is.infinite(val) | val < 0 | val > 1] <- NA_real_

  if (!is.null(pval_tails)) {
    one_tail <- is_one_tailed(pval_tails)
    val[one_tail & !is.na(val)] <- pmin(2 * val[one_tail & !is.na(val)], 1)
  }

  op <- if (is.null(pval_type)) rep("=", length(val)) else parse_pval_type(pval_type)

  out <- rep(NA, length(val))
  valid <- !is.na(val) & !is.na(op)
  if (any(valid)) {
    vv <- val[valid]
    ov <- op[valid]
    out[valid] <- dplyr::case_when(
      ov == "=" ~ vv < alpha,
      ov == "<" ~ ifelse(vv <= alpha, TRUE, NA),
      ov == "<=" ~ ifelse(vv < alpha, TRUE, NA),
      ov %in% c(">", ">=") ~ ifelse(vv >= alpha, FALSE, NA),
      .default = NA
    )
  }

  out
}

#' Resolve statistical significance using coded p-values with p_from_r fallback
#'
#' @param es Numeric vector of correlations.
#' @param n Numeric vector of sample sizes.
#' @param pval_value Optional numeric/character vector of coded p-values.
#' @param pval_type Optional character vector of coded p-value types.
#' @param pval_tails Optional character/numeric vector of coded p-value tails.
#' @param alpha Two-tailed significance threshold (default `0.05`).
#' @return Logical vector (`TRUE`, `FALSE`, or `NA`).
#' @noRd
resolve_significance <- function(es, n, pval_value = NULL, pval_type = NULL, pval_tails = NULL, alpha = 0.05) {
  p_calc <- p_from_r(r = es, N = n)
  sig_calc <- ifelse(is.na(p_calc), NA, p_calc < alpha)

  if (is.null(pval_value)) {
    return(sig_calc)
  }

  sig_coded <- is_p_significant(pval_value = pval_value, pval_type = pval_type, pval_tails = pval_tails, alpha = alpha)
  dplyr::coalesce(sig_coded, sig_calc)
}

#' Assess Replication Outcomes Based on Various Criteria
#'
#' This function evaluates the outcomes of replication studies against the original studies using
#' various statistical criteria (see below, and `vignette("success_criteria")` for more details).
#'
#' @param es_o Numeric. The effect size from the original study.
#' @param n_o Integer. The sample size of the original study.
#' @param es_r Numeric. The effect size from the replication study.
#' @param n_r Integer. The sample size of the replication study.
#' @param criterion Character. The criterion to use for assessing the replication outcome.
#'        Options include: "significance_r", "significance_agg", "consistency_ci", "consistency_pi",
#'        "homogeneity", "homogeneity_significance", "small_telescopes" and "reported_success".
#' @param reported_success Character. The outcome reported for each replication, as coded in the
#'        `reported_success` variable of the FReD dataset. Required when `criterion = "reported_success"`,
#'        and ignored otherwise.
#' @param result Character. Optional fallback for `criterion = "reported_success"`, used where
#'        `reported_success` is missing: the `result` variable returned by [load_fred_data()].
#' @param pval_value_o Numeric (or numeric string). Optional coded p-values for the original study
#'        (the `pval_value_o` column of FReD). Opt-in: used by `"significance_r"` and
#'        `"homogeneity_significance"` when supplied, falling back to `p_from_r(es_o, n_o)` where
#'        missing or indeterminate.
#' @param pval_type_o Character. Optional inequality type labels for `pval_value_o` (e.g., `"l (<)"`,
#'        `"e (=)"`, `"g (>)"`, `"ge (>=)"`, `"le (<=)"`). Defaults to exact equality (`"="`) when omitted.
#' @param pval_value_r Numeric (or numeric string). Optional coded p-values for the replication study
#'        (the `pval_value_r` column of FReD). Opt-in: used by `"significance_r"` when supplied,
#'        falling back to `p_from_r(es_r, n_r)` where missing or indeterminate.
#' @param pval_type_r Character. Optional inequality type labels for `pval_value_r`. Defaults to
#'        exact equality (`"="`) when omitted.
#' @param pval_tails_o Character or numeric. Optional tail specification for `pval_value_o`
#'        (the `pval_tails_o` column of FReD, e.g., `"1 (one)"` vs `"2 (two)"`). One-tailed p-values
#'        are doubled (`pmin(2 * p, 1)`) before comparing against two-tailed alpha = 0.05; omitted or
#'        missing values are treated as two-tailed.
#' @param pval_tails_r Character or numeric. Optional tail specification for `pval_value_r`
#'        (the `pval_tails_r` column of FReD). Handled identically to `pval_tails_o`.
#'
#' @details
#' The function assesses the replication outcome using one of several criteria:
#' \describe{
#'   \item{significance_r}{Evaluates the statistical significance of the replication
#'   effect (and whether its direction is consistent with the original effect). If
#'   the original study was not significant, this is highlighted as `"OS not significant"`
#'   (even if the replication effect size is missing, since the criterion is already meaningless).
#'   Coded p-values (`pval_value_o`/`pval_value_r`, with optional `pval_type_*` and `pval_tails_*`)
#'   are opt-in: when omitted (the default, used by the FReD Explorer), significance is computed
#'   solely from `es` and `n` via two-tailed `p_from_r()`. When supplied, coded p-values are
#'   evaluated first (with one-tailed values doubled to the two-tailed scale) and `p_from_r()` is
#'   used as a fallback wherever a coded p-value is missing or indeterminate at alpha = 0.05.}
#'   \item{significance_agg}{Aggregates the effect sizes from the original and replication studies using a meta-analytic approach and
#'   assesses whether the combined effect is significantly different from zero.}
#'   \item{consistency_ci}{Checks whether the *original* effect size falls within the confidence interval of the *replication* effect size,
#'   thus assessing consistency between the original and replication findings.}
#'   \item{consistency_pi}{Evaluates whether the replication effect size falls within the prediction interval derived from the original study and
#'   the size of the replication sample. This accounts for the expected variability in replication results.}
#'   \item{homogeneity}{Assesses whether the effects from the original and replication studies are homogeneous (i.e., consistent)
#'   using a heterogeneity test (Q-test).}
#'   \item{homogeneity_significance}{Combines the assessment of homogeneity with the significance of the effect sizes.
#'   It checks whether the two effects are homogeneous and jointly significantly different from zero. Because this criterion
#'   requires comparing both studies, rows with missing replication data return `NA` rather than `"OS not significant"`.}
#'   \item{small_telescopes}{Tests whether the replication effect size is larger than the effect size that would have given the
#'   original study a power of 33%. Derived from Simonsohn (2015), the idea here is that replications should only count as
#'   successful if they indicate that the original study provided evidence (rather than a lucky guess).}
#'   \item{reported_success}{Uses the outcome as described by the replication authors instead of calculating one from effect sizes,
#'   so the effect size and sample size arguments do not affect it. Coded labels are returned unchanged (e.g., "successful", "failed",
#'   "mixed"). Where no outcome is coded, `result` is used, and "Not calculable" if that is missing as well.}
#' }
#'
#' @return A data frame with the outcome of the assessment based on the specified criterion, with three columns:
#' `outcome` (success, failure or 'OS not significant'), `outcome_detailed` (a specific description of the criterion) and
#' `outcome_report` (usually the same as outcome, but further broken out where there are distinct reasons for failure).
#' For `criterion = "reported_success"`, all three columns contain the reported label.
#'
#' @examples
#' es_o <- 0.3  # Effect size from the original study
#' n_o <- 100   # Sample size of the original study
#' es_r <- 0.25 # Effect size from the replication study
#' n_r <- 120   # Sample size of the replication study
#' assess_replication_outcome(es_o, n_o, es_r, n_r, "significance_r")
#' assess_replication_outcome(es_o, n_o, es_r, n_r, "reported_success", reported_success = "mixed")
#'
#' @export

assess_replication_outcome <- function(es_o, n_o, es_r, n_r, criterion,
                                       reported_success = NULL, result = NULL,
                                       pval_value_o = NULL, pval_type_o = NULL,
                                       pval_value_r = NULL, pval_type_r = NULL,
                                       pval_tails_o = NULL, pval_tails_r = NULL) {

  # Check that relevant arguments are numeric and have compatible lengths (allowing length-1 recycling)
  checkmate::assert_numeric(es_o)
  checkmate::assert_numeric(n_o)
  checkmate::assert_numeric(es_r)
  checkmate::assert_numeric(n_r)
  checkmate::assert_string(criterion)

  lens <- c(length(es_o), length(n_o), length(es_r), length(n_r))
  n_rows <- if (any(lens == 0L)) 0L else max(lens)
  if (n_rows == 0L) {
    if (!all(lens %in% c(0L, 1L))) {
      stop("`es_o`, `n_o`, `es_r`, and `n_r` must have compatible lengths (all equal or length 1).", call. = FALSE)
    }
  } else if (!all(lens %in% c(n_rows, 1L))) {
    stop("`es_o`, `n_o`, `es_r`, and `n_r` must have compatible lengths (all equal or length 1).", call. = FALSE)
  }

  validate_pval_value <- function(x, name) {
    if (is.null(x)) return(NULL)
    if (!is.numeric(x) && !is.character(x) && !all(is.na(x))) {
      stop("`", name, "` must be numeric or character.", call. = FALSE)
    }
    if (length(x) != n_rows) {
      stop("`", name, "` must have length ", n_rows, " (got ", length(x), ").", call. = FALSE)
    }
    x
  }

  validate_pval_meta <- function(x, name) {
    if (is.null(x)) return(NULL)
    x_chr <- as.character(x)
    checkmate::assert_character(x_chr, len = n_rows, .var.name = name)
    x_chr
  }

  pval_value_o <- validate_pval_value(pval_value_o, "pval_value_o")
  pval_type_o <- validate_pval_meta(pval_type_o, "pval_type_o")
  pval_value_r <- validate_pval_value(pval_value_r, "pval_value_r")
  pval_type_r <- validate_pval_meta(pval_type_r, "pval_type_r")
  pval_tails_o <- validate_pval_meta(pval_tails_o, "pval_tails_o")
  pval_tails_r <- validate_pval_meta(pval_tails_r, "pval_tails_r")

  inputs <- data.frame(
    es_o = rep_len(es_o, n_rows),
    n_o = rep_len(n_o, n_rows),
    es_r = rep_len(es_r, n_rows),
    n_r = rep_len(n_r, n_rows)
  )


  assess_rep_significance <- function(df) {

    #LR: should sig test for replication effects here be one-tailed?
    #LR: should inconsistent direction be always included here? I'd say yes.
    os_sig <- resolve_significance(df$es_o, df$n_o, pval_value_o, pval_type_o, pval_tails_o)
    rep_sig <- resolve_significance(df$es_r, df$n_r, pval_value_r, pval_type_r, pval_tails_r)

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          !is.na(os_sig) & !os_sig ~ "original effect is not significant",
          !is.na(os_sig) & os_sig & !is.na(rep_sig) & !rep_sig ~ "replication effect is not significant",
          !is.na(os_sig) & os_sig & !is.na(rep_sig) & rep_sig &
            !is.na(.data$es_o) & !is.na(.data$es_r) & .data$es_o != 0 & .data$es_r != 0 &
            sign(.data$es_o) == sign(.data$es_r) ~ "replication effect is significant",
          !is.na(os_sig) & os_sig & !is.na(rep_sig) & rep_sig &
            !is.na(.data$es_o) & !is.na(.data$es_r) & .data$es_o != 0 & .data$es_r != 0 &
            sign(.data$es_o) != sign(.data$es_r) ~ "direction of effect is inconsistent",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          .data$outcome_detailed == "original effect is not significant" ~ "OS not significant",
          .data$outcome_detailed %in% c("replication effect is not significant", "direction of effect is inconsistent") ~ "failure",
          .data$outcome_detailed == "replication effect is significant" ~ "success",
          .default = NA_character_
        ),
        outcome_report = dplyr::case_when(
          .data$outcome_detailed == "direction of effect is inconsistent" ~ "failure (reversal)",
          .default = .data$outcome
        )
      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  assess_agg_significance <- function(df) {

    effect_comparison <- compare_effectsizes(df$es_o, df$n_o, df$es_r, df$n_r)
    same_dir <- ifelse(df$es_o < 0, effect_comparison$ci_ub < 0, effect_comparison$ci_lb > 0)
    opp_dir <- ifelse(df$es_o < 0, effect_comparison$ci_lb > 0, effect_comparison$ci_ub < 0)
    not_sig <- !is.na(effect_comparison$ci_lb) & !is.na(effect_comparison$ci_ub) &
      effect_comparison$ci_lb <= 0 & effect_comparison$ci_ub >= 0

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          same_dir ~ "aggregated effect is larger than 0 and in the same direction",
          opp_dir ~ "aggregated effect is larger than 0 but in the opposite direction",
          not_sig ~ "aggregated effect is not larger than 0",
          .default = NA_character_
        )) %>%
      dplyr::mutate(
        outcome = dplyr::case_when(
          .data$outcome_detailed == "aggregated effect is larger than 0 and in the same direction" ~ "success",
          .data$outcome_detailed %in% c("aggregated effect is larger than 0 but in the opposite direction", "aggregated effect is not larger than 0") ~ "failure",
          .default = NA_character_
        )) %>%
      dplyr::mutate(
        outcome_report = dplyr::case_when(
          .data$outcome_detailed == "aggregated effect is larger than 0 but in the opposite direction" ~ "failure (reversal)",
          .default = .data$outcome
        )) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")

  }


  assess_or_consistency_with_rep <- function(df) {

    # we already have CIs in our processed data - so no real need to calculate them again, but can do for clarity
    # LR: I switched to our own function as it is vectorised

    cis <- compute_ci_r(df$es_r, df$n_r)

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          .data$es_o > cis$lower & .data$es_o < cis$upper ~ "Original effect size is within replication's CI",
          .data$es_o <= cis$lower | .data$es_o >= cis$upper ~ "Original effect size is not within replication's CI",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          .data$outcome_detailed == "Original effect size is within replication's CI" ~ "success",
          .data$outcome_detailed == "Original effect size is not within replication's CI" ~ "failure",
          .default = NA_character_
        ),
        outcome_report = .data$outcome
      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  assess_rep_consistency_with_pi <- function(df) {
    #LR: note that the replication's N should be included here, as larger N there lead to narrower PIs
    pred_intervals <- calculate_prediction_interval(df$es_o, df$n_o, df$n_r)

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          .data$es_r > pred_intervals$lower & .data$es_r < pred_intervals$upper ~ "Replication effect size is within the prediction interval",
          .data$es_r <= pred_intervals$lower | .data$es_r >= pred_intervals$upper ~ "Replication effect size is not within the prediction interval",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          .data$outcome_detailed == "Replication effect size is within the prediction interval" ~ "success",
          .data$outcome_detailed == "Replication effect size is not within the prediction interval" ~ "failure",
          .default = NA_character_
        ),
        outcome_report = .data$outcome
      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  assess_homogeneity <- function(df) {
    effect_comparison <- compare_effectsizes(df$es_o, df$n_o, df$es_r, df$n_r)

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          effect_comparison$QEp < 0.05 ~ "effects are heterogeneous",
          effect_comparison$QEp >= 0.05 ~ "effects are not heterogeneous",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          .data$outcome_detailed == "effects are not heterogeneous" ~ "success",
          .data$outcome_detailed == "effects are heterogeneous" ~ "failure",
          .default = NA_character_
        ),
        outcome_report = .data$outcome
      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  assess_homogeneity_and_significance <- function(df) {

    # LR: should any replications were OS is not significant be shown as such? I am not sure about the success case - but
    # consistent non-significance cannot be coded as failure? Currently all non-significant OS are coded as "OS not significant"

    effect_comparison <- compare_effectsizes(df$es_o, df$n_o, df$es_r, df$n_r)
    os_sig <- resolve_significance(df$es_o, df$n_o, pval_value_o, pval_type_o, pval_tails_o)
    sig_same_dir <- ifelse(df$es_o < 0, effect_comparison$ci_ub < 0, effect_comparison$ci_lb > 0)
    sig_opp_dir <- ifelse(df$es_o < 0, effect_comparison$ci_lb > 0, effect_comparison$ci_ub < 0)
    not_sig <- !is.na(effect_comparison$ci_lb) & !is.na(effect_comparison$ci_ub) &
      effect_comparison$ci_lb <= 0 & effect_comparison$ci_ub >= 0

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          effect_comparison$QEp >= 0.05 & sig_same_dir ~ "effect sizes are homogeneous and jointly significantly above 0",
          effect_comparison$QEp >= 0.05 & sig_opp_dir ~ "effect sizes are homogeneous but jointly significant in opposite direction",
          effect_comparison$QEp >= 0.05 & not_sig ~ "effect sizes are homogeneous but not significant",
          effect_comparison$QEp < 0.05 & sig_same_dir ~ "effect sizes are not homogeneous but jointly significantly above 0",
          effect_comparison$QEp < 0.05 & sig_opp_dir ~ "effect sizes are not homogeneous and jointly significant in opposite direction",
          effect_comparison$QEp < 0.05 & not_sig ~ "effect sizes are not homogeneous and not significant",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          is.na(.data$outcome_detailed) ~ NA_character_,
          !is.na(os_sig) & !os_sig ~ "OS not significant",
          .data$outcome_detailed == "effect sizes are homogeneous and jointly significantly above 0" ~ "success",
          .data$outcome_detailed %in% c(
            "effect sizes are homogeneous but jointly significant in opposite direction",
            "effect sizes are not homogeneous but jointly significantly above 0",
            "effect sizes are not homogeneous and jointly significant in opposite direction",
            "effect sizes are not homogeneous and not significant",
            "effect sizes are homogeneous but not significant"
          ) ~ "failure",
          .default = NA_character_
        ),
        outcome_report = dplyr::case_when(
          is.na(.data$outcome) ~ NA_character_,
          .data$outcome == "OS not significant" ~ "OS not significant",
          .default = paste0(.data$outcome, " (", stringr::str_remove(.data$outcome_detailed, "effect sizes are "), ")")
        )
      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  assess_small_telescopes <- function(df) {
    df %>%
      dplyr::mutate(
        es_with_33power = find_r_for_power(.33, .data$n_o),
        es_r_aligned = ifelse(!is.na(.data$es_o) & .data$es_o < 0, -.data$es_r, .data$es_r),
        outcome_detailed = dplyr::case_when(
          .data$es_r_aligned >= .data$es_with_33power ~ "original study had >= 33% power to detect replication effect",
          .data$es_r_aligned < .data$es_with_33power ~ "original study had < 33% power to detect replication effect",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          .data$outcome_detailed == "original study had >= 33% power to detect replication effect" ~ "success",
          .data$outcome_detailed == "original study had < 33% power to detect replication effect" ~ "failure",
          .default = NA_character_
        ),
        outcome_report = .data$outcome
      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  # Mirrors getOutcome() in the fred-apps Explorer (shared/js/success-criteria.js)
  assess_reported_success <- function(df) {
    if (is.null(reported_success)) {
      stop("`reported_success` must be provided when criterion = \"reported_success\".", call. = FALSE)
    }

    coded_label <- function(x) {
      x <- as.character(x)
      checkmate::assert_character(x, len = nrow(df))
      x[x %in% ""] <- NA_character_
      x
    }

    fallback <- if (is.null(result)) rep(NA_character_, nrow(df)) else coded_label(result)
    label <- dplyr::coalesce(coded_label(reported_success), fallback, "Not calculable")

    data.frame(outcome = label, outcome_detailed = label, outcome_report = label)
  }

  switch(
    criterion,
    significance_r = assess_rep_significance(inputs),
    significance_agg = assess_agg_significance(inputs),
    consistency_ci = assess_or_consistency_with_rep(inputs),
    consistency_pi = assess_rep_consistency_with_pi(inputs),
    homogeneity = assess_homogeneity(inputs),
    homogeneity_significance = assess_homogeneity_and_significance(inputs),
    small_telescopes = assess_small_telescopes(inputs),
    reported_success = assess_reported_success(inputs),
    stop("Criterion not recognised")
  )
}

