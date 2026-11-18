
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
#' @return A list containing:
#' \item{est}{Estimated effect sizes from the meta-analysis.}
#' \item{ci_lb}{Lower bounds of the confidence intervals for the estimated effect sizes.}
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

  process_pair <- function(r1, n1, r2, n2) {
    if (any(is.na(c(r1, n1, r2, n2)))) return(data.frame(est = NA, ci_lb = NA, QEp = NA))

    # Calculate effect sizes for each study
    r1_result <- metafor::escalc(ni = n1, ri = r1, measure = "COR")
    r2_result <- metafor::escalc(ni = n2, ri = r2, measure = "COR")

    # Meta-analysis on the two effect sizes
    res <- metafor::rma(yi = c(r1_result$yi, r2_result$yi),
                        vi = c(r1_result$vi, r2_result$vi),
                        method = "FE")

    return(data.frame(est = res$beta[1], ci_lb = res$ci.lb, QEp = res$QEp))
  }

  results <- mapply(process_pair, r1, n1, r2, n2, SIMPLIFY = FALSE)

  do.call(rbind, results)

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
#'
#' @details
#' The function assesses the replication outcome using one of several criteria:
#' \describe{
#'   \item{significance_r}{Evaluates the statistical significance of the replication
#'   effect (and whether its direction is consistent with the original effect). If
#'   the original study was not significant, this is highlighted, as the criterion is meaningless}
#'   \item{significance_agg}{Aggregates the effect sizes from the original and replication studies using a meta-analytic approach and
#'   assesses whether the combined effect is significantly different from zero.}
#'   \item{consistency_ci}{Checks whether the *original* effect size falls within the confidence interval of the *replication* effect size,
#'   thus assessing consistency between the original and replication findings.}
#'   \item{consistency_pi}{Evaluates whether the replication effect size falls within the prediction interval derived from the original study and
#'   the size of the replication sample. This accounts for the expected variability in replication results.}
#'   \item{homogeneity}{Assesses whether the effects from the original and replication studies are homogeneous (i.e., consistent)
#'   using a heterogeneity test (Q-test).}
#'   \item{homogeneity_significance}{Combines the assessment of homogeneity with the significance of the effect sizes.
#'   It checks whether the two effects are homogeneous and jointly significantly different from zero.}
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

assess_replication_outcome <- function(es_o, n_o, es_r, n_r, criterion, reported_success = NULL, result = NULL) {

  # Check that relevant arguments are numeric and not missing
  checkmate::assert_numeric(es_o)
  checkmate::assert_numeric(n_o)
  checkmate::assert_numeric(es_r)
  checkmate::assert_numeric(n_r)
  checkmate::assert_character(criterion)

  inputs <- data.frame(
    es_o = es_o,
    n_o = n_o,
    es_r = es_r,
    n_r = n_r
  )


  assess_rep_significance <- function(df) {

    #LR: should sig test for replication effects here be one-tailed?
    #LR: should inconsistent direction be always included here? I'd say yes.
      df %>%
        dplyr::mutate(
          outcome_detailed = dplyr::case_when(
            p_from_r(r = .data$es_o, N = .data$n_o) >= 0.05 ~ "original effect is not significant",
            p_from_r(r = .data$es_r, N = .data$n_r) >= 0.05 ~ "replication effect is not significant",
            p_from_r(r = .data$es_r, N = .data$n_r) < 0.05 & sign(.data$es_o) == sign(.data$es_r) ~ "replication effect is significant",
            p_from_r(r = .data$es_r, N = .data$n_r) < 0.05 & sign(.data$es_o) != sign(.data$es_r) ~ "direction of effect is inconsistent",
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

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          effect_comparison$ci_lb > 0 & effect_comparison$est > 0 ~ "aggregated effect is larger than 0 and in the same direction",
          effect_comparison$ci_lb > 0 & effect_comparison$est < 0 ~ "aggregated effect is larger than 0 but in the opposite direction",
          effect_comparison$ci_lb <= 0 ~ "aggregated effect is not larger than 0",
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
        )      ) %>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")

  }


  assess_or_consistency_with_rep <- function(df) {

    # we already have CIs in our processed data - so no real need to calculate them again, but can do for clarity
    # LR: I switched to our own function as it is vectorised

    cis <- compute_ci_r(df$es_r, df$n_r)

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          es_o > cis$lower & es_o < cis$upper ~ "Original effect size is within replication's CI",
          es_o <= cis$lower | es_o >= cis$upper ~ "Original effect size is not within replication's CI",
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
          es_r > pred_intervals$lower & es_r < pred_intervals$upper ~ "Replication effect size is within the prediction interval",
          es_r <= pred_intervals$lower | es_r >= pred_intervals$upper ~ "Replication effect size is not within the prediction interval",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          .data$outcome_detailed == "Replication effect size is within the prediction interval" ~ "success",
          .data$outcome_detailed == "Replication effect size is not within the prediction interval" ~ "failure",
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

    df %>%
      dplyr::mutate(
        outcome_detailed = dplyr::case_when(
          effect_comparison$QEp > 0.05 & effect_comparison$ci_lb > 0 ~ "effect sizes are homogeneous and jointly significantly above 0",
          effect_comparison$QEp > 0.05 & effect_comparison$ci_lb <= 0 ~ "effect sizes are homogeneous but not significant",
          effect_comparison$QEp <= 0.05 & effect_comparison$ci_lb > 0 ~ "effect sizes are not homogeneous but jointly significantly above 0",
          effect_comparison$QEp <= 0.05 & effect_comparison$ci_lb <= 0 ~ "effect sizes are not homogeneous and not significant",
          .default = NA_character_
        ),
        outcome = dplyr::case_when(
          p_from_r(r = .data$es_o, N = .data$n_o) >= 0.05 ~ "OS not significant",
          .data$outcome_detailed == "effect sizes are homogeneous and jointly significantly above 0" ~ "success",
          .data$outcome_detailed == "effect sizes are not homogeneous but jointly significantly above 0" ~ "failure",
          .data$outcome_detailed == "effect sizes are not homogeneous and not significant" ~ "failure",
          .data$outcome_detailed == "effect sizes are homogeneous but not significant" ~ "failure",
          .default = NA_character_
        ),
        outcome_report = dplyr::case_when(
          p_from_r(r = .data$es_o, N = .data$n_o) >= 0.05 ~ "OS not significant",
          is.na(.data$outcome) ~ "not coded",
          .default = paste0(.data$outcome, " (", .data$outcome_detailed %>% stringr::str_remove("effect sizes are "), ")")
      )
      )%>%
      dplyr::select("outcome", "outcome_detailed", "outcome_report")
  }

  assess_small_telescopes <- function(df) {
    df %>%
      dplyr::mutate(
        es_with_33power = find_r_for_power(.33, .data$n_o),
        outcome_detailed = dplyr::case_when(
          df$es_r >= .data$es_with_33power ~ "original study had >= 33% power to detect replication effect",
          df$es_r < .data$es_with_33power ~ "original study had < 33% power to detect replication effect",
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

