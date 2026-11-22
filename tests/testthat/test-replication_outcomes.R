test_that("replication success assessed correctly", {
  expect_equal(2 * 2, 4)
})

es_agg <- compare_effectsizes(c(.1, .4), c(10, 100), c(.4, .1), c(100, 10))

test_that("replications can be aggregated", {
  expect_subset(names(es_agg), c("est", "ci_lb", "ci_ub", "QEp"))
  expect_true("ci_ub" %in% names(es_agg))
  expect_true(all(es_agg$ci_lb <= es_agg$est & es_agg$est <= es_agg$ci_ub))

  # Edge cases: invalid N, |r| >= 1, NA, empty input
  na_agg <- compare_effectsizes(c(0.5, 1.0, NA_real_), c(2, 50, 50), c(0.5, 0.5, 0.5), c(50, 50, 50))
  expect_true(all(is.na(na_agg$est)))
  expect_type(na_agg$est, "double")
  expect_equal(nrow(compare_effectsizes(numeric(0), numeric(0), numeric(0), numeric(0))), 0L)
})

test_that("significance_agg detects significant negative reversals and leaves es_o == 0 direction indeterminate", {
  # Strong negative replication overwhelms small positive original -> reversal
  res <- assess_replication_outcome(0.2, 30, -0.4, 200, "significance_agg")
  expect_equal(res$outcome, "failure")
  expect_equal(res$outcome_report, "failure (reversal)")
  expect_equal(res$outcome_detailed, "aggregated effect is significantly different from 0 but in the opposite direction")

  # Unaligned negative original with matching negative replication -> success
  res_neg <- assess_replication_outcome(-0.4, 50, -0.35, 100, "significance_agg")
  expect_equal(res_neg$outcome, "success")
  expect_equal(res_neg$outcome_report, "success")
  expect_equal(res_neg$outcome_detailed, "aggregated effect is significantly different from 0 and in the same direction")

  # Unaligned negative original with strong positive replication -> reversal
  res_neg_rev <- assess_replication_outcome(-0.2, 30, 0.4, 200, "significance_agg")
  expect_equal(res_neg_rev$outcome, "failure")
  expect_equal(res_neg_rev$outcome_report, "failure (reversal)")

  # Zero original effect has no direction: significant pooled effect stays indeterminate (NA)
  res_zero_sig <- assess_replication_outcome(0, 30, 0.4, 200, "significance_agg")
  expect_true(is.na(res_zero_sig$outcome))
  expect_true(is.na(res_zero_sig$outcome_detailed))
})

test_that("parse_pval_type and is_p_significant handle all FReD pval_type formats and tails", {
  types <- c("l (<)", "e (=)", "g (>)", "ge (>=)", "le (<=)", "<", '"<"', '"="', "ge (>)", "=<", "=>", "\u2264", "\u2265", NA, "")
  parsed <- parse_pval_type(types)
  expect_equal(parsed, c("<", "=", ">", ">=", "<=", "<", "<", "=", ">=", "<=", ">=", "<=", ">=", "=", "="))

  # At alpha = 0.05:
  # - p < 0.05 is significant (TRUE); p < 0.10 is indeterminate (NA)
  expect_equal(is_p_significant(c(0.05, 0.001, 0.10), c("l (<)", '"<"', "<")), c(TRUE, TRUE, NA))
  # - p = 0.05 is not significant (FALSE); p = 0.049 is significant (TRUE); scientific notation strings work
  expect_equal(is_p_significant(c("4.02E-22", "5.66E-15", "0.05", "0.049"), c("e (=)", '"="', "e (=)", NA)),
               c(TRUE, TRUE, FALSE, TRUE))
  # - p > 0.05 and p >= 0.05 (including "ge (>=)", "ge (>)", "=>", "≥") are not significant (FALSE); p > 0.01 is NA
  expect_equal(is_p_significant(c(0.05, 0.05, 0.05, 0.05, 0.05, 0.01, 0.01),
                                c("g (>)", "ge (>=)", "ge (>)", "=>", "\u2265", "g (>)", "ge (>=)")),
               c(FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA))
  # - p <= 0.01 is significant (TRUE); p <= 0.05 (including "=<", "≤") and p <= 0.25 are indeterminate (NA)
  expect_equal(is_p_significant(c(0.01, 0.05, 0.05, 0.05, 0.25),
                                c("le (<=)", "le (<=)", "=<", "\u2264", "le (<=)")),
               c(TRUE, NA, NA, NA, NA))
  # - One-tailed p-values are doubled onto the two-tailed scale
  expect_equal(
    is_p_significant(
      c(0.02, 0.03, 0.05, 0.03, 0.03),
      c("e (=)", "e (=)", "l (<)", "g (>)", "e (=)"),
      pval_tails = c("1 (one)", "1", "1 (one)", "1 (one)", "2 (two)")
    ),
    c(TRUE, FALSE, NA, FALSE, TRUE)
  )
  # - Out-of-range p-values return NA
  expect_equal(is_p_significant(c(-0.01, 1.2), c("e (=)", "e (=)")), c(NA, NA))

  # resolve_significance uses coded p-values first and falls back to p_from_r where NA/indeterminate
  expect_equal(
    resolve_significance(
      es = c(0.3, 0.3, 0.05, 0.3),
      n = c(100, 100, 100, 100),
      pval_value = c(0.03, 0.05, NA_real_, 0.10),
      pval_type = c("e (=)", "l (<)", NA_character_, "l (<)"),
      pval_tails = c("1 (one)", "1 (one)", NA_character_, "2 (two)")
    ),
    c(FALSE, TRUE, FALSE, TRUE)
  )
})

test_that("significance_r supports coded p-values with p_from_r fallback, tails, and NA OS handling", {
  # Coded p-values override p_from_r when present and determinate
  res <- assess_replication_outcome(
    es_o = c(0.3, 0.3, 0.3, 0.3, 0.3, NA_real_, 0.3),
    n_o  = c(100, 100, 100, 100, 100, NA_real_, 100),
    es_r = c(0.25, 0.25, -0.25, 0.25, 0.25, 0.01, 0.25),
    n_r  = c(100, 100, 100, 100, 100, 100, 100),
    criterion = "significance_r",
    pval_value_o = c("4.02E-22", 0.05, 0.001, 0.10, NA_real_, NA_real_, 0.001),
    pval_type_o  = c("e (=)", "ge (>=)", "l (<)", "l (<)", NA_character_, NA_character_, "e (=)"),
    pval_value_r = c("5.66E-15", 0.001, 0.01, 0.05, NA_real_, NA_real_, 0.03),
    pval_type_r  = c("e (=)", "l (<)", '"<"', "g (>)", NA_character_, NA_character_, "e (=)"),
    pval_tails_r = c("2 (two)", "2 (two)", "2 (two)", "2 (two)", NA_character_, NA_character_, "1 (one)")
  )

  # 1: both coded significant (scientific notation strings) & same sign -> success
  expect_equal(res$outcome[1], "success")
  # 2: OS coded ge (>=) 0.05 -> OS not significant (must check g|> before e|=)
  expect_equal(res$outcome[2], "OS not significant")
  # 3: both coded significant, opposite signs -> failure (reversal)
  expect_equal(res$outcome[3], "failure")
  expect_equal(res$outcome_report[3], "failure (reversal)")
  # 4: OS coded p < 0.10 (indeterminate -> falls back to p_from_r(0.3, 100) < 0.05 = sig),
  #    rep coded p > 0.05 -> not significant -> failure
  expect_equal(res$outcome[4], "failure")
  expect_equal(res$outcome_detailed[4], "replication effect is not significant")
  # 5: both coded NA -> falls back to p_from_r -> success
  expect_equal(res$outcome[5], "success")
  # 6: OS completely missing -> must stay NA, NOT falsely "failure"
  expect_true(is.na(res$outcome[6]))
  expect_true(is.na(res$outcome_detailed[6]))
  expect_true(is.na(res$outcome_report[6]))
  # 7: rep coded one-tailed p = 0.03 -> two-tailed 0.06 >= 0.05 -> failure
  expect_equal(res$outcome[7], "failure")
  expect_equal(res$outcome_detailed[7], "replication effect is not significant")
})

test_that("homogeneity_significance handles QEp boundary, opposite direction, missing es_r, and NA outcome_report", {
  # Missing es_r with non-significant OS should return NA (not "OS not significant" or "not coded")
  res_missing <- assess_replication_outcome(0.05, 30, NA_real_, NA_real_, "homogeneity_significance")
  expect_true(is.na(res_missing$outcome))
  expect_true(is.na(res_missing$outcome_detailed))
  expect_true(is.na(res_missing$outcome_report))

  # Missing both sides returns NA_character_ (not "not coded")
  res_na <- assess_replication_outcome(NA_real_, NA_real_, NA_real_, NA_real_, "homogeneity_significance")
  expect_true(is.na(res_na$outcome))
  expect_true(is.na(res_na$outcome_report))

  # Non-significant OS with complete replication pair still returns "OS not significant"
  res_os_ns <- assess_replication_outcome(0.05, 30, 0.04, 100, "homogeneity_significance")
  expect_equal(res_os_ns$outcome, "OS not significant")
  expect_equal(res_os_ns$outcome_report, "OS not significant")

  # Opposite-direction pooled significance is labelled distinctly (not "not significant")
  res_opp <- assess_replication_outcome(0.35, 40, -0.45, 250, "homogeneity_significance")
  expect_equal(res_opp$outcome, "failure")
  expect_match(res_opp$outcome_detailed, "opposite direction")
  expect_match(res_opp$outcome_report, "opposite direction")
})

test_that("consistency_pi, small_telescopes, and input validation handle edge cases cleanly", {
  expect_no_warning(
    res_pi <- assess_replication_outcome(c(0.5, NA_real_, 0.5), c(50, 50, 3), c(0.1, 0.1, 0.1), c(30, 30, 30), "consistency_pi")
  )
  expect_equal(res_pi$outcome, c("success", NA_character_, NA_character_))
  expect_equal(res_pi$outcome_report, c("success", NA_character_, NA_character_))

  expect_no_warning(
    res_st <- assess_replication_outcome(c(0.5, 0.5, -0.3), c(50, 3, 50), c(0.2, 0.2, -0.3), c(100, 100, 100), "small_telescopes")
  )
  expect_equal(res_st$outcome, c("success", NA_character_, "success"))

  # Scalar recycling works, mismatched non-1 lengths error, non-scalar criterion errors
  expect_equal(nrow(assess_replication_outcome(c(0.5, 0.4), 50, c(0.3, 0.2), 100, "significance_r")), 2L)
  expect_error(assess_replication_outcome(c(0.5, 0.4), c(50, 50, 50), c(0.3, 0.2), c(100, 100), "significance_r"), "compatible lengths")
  expect_error(assess_replication_outcome(0.5, 50, 0.3, 100, c("significance_r", "homogeneity")))
  expect_error(assess_replication_outcome(c(0.5, 0.4), c(50, 50), c(0.3, 0.2), c(100, 100), "significance_r", pval_value_o = 0.01), "length")
})

test_that("success assessments work", {
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "significance_r")$outcome, "success")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "significance_agg")$outcome, "success")
  expect_equal(assess_replication_outcome(.5, 50, -.5, 50, "significance_agg")$outcome, "failure")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "consistency_pi")$outcome, "failure")
  expect_equal(assess_replication_outcome(.5, 50, .1, 30, "consistency_pi")$outcome, "success")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "consistency_ci")$outcome, "failure")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "homogeneity")$outcome, "failure")
  expect_equal(assess_replication_outcome(.3, 50, 0, 40, "homogeneity")$outcome, "success")
  expect_equal(assess_replication_outcome(.3, 50, 0, 40, "homogeneity_significance")$outcome, "failure")
})

test_that("reported success returns coded labels and handles missing values", {
  labels <- c("successful", "failed", "mixed", "statistically successful but flawed",
              "uninformative", "descriptive only")
  res <- assess_replication_outcome(rep(.5, 6), rep(50, 6), rep(.1, 6), rep(100, 6),
                                    "reported_success", reported_success = labels)
  expect_equal(res$outcome, labels)
  expect_equal(res$outcome_detailed, labels)
  expect_equal(res$outcome_report, labels)

  res <- assess_replication_outcome(rep(NA_real_, 4), rep(NA_real_, 4), rep(NA_real_, 4), rep(NA_real_, 4),
                                    "reported_success",
                                    reported_success = c(NA, "", NA, "failed"),
                                    result = c("failed replication", "OS not significant", NA, "successful replication"))
  expect_equal(res$outcome, c("failed replication", "OS not significant", "Not calculable", "failed"))

  expect_error(assess_replication_outcome(.5, 50, .1, 100, "reported_success"), "reported_success")
  expect_error(assess_replication_outcome(c(.5, .5), c(50, 50), c(.1, .1), c(100, 100),
                                          "reported_success", reported_success = "failed"), "length")
})

test_that("other criteria ignore reported outcomes", {
  expect_equal(
    assess_replication_outcome(.5, 50, .1, 1000, "significance_r", reported_success = "failed"),
    assess_replication_outcome(.5, 50, .1, 1000, "significance_r")
  )
})

test_that("reported success matches the fred-apps Explorer on its records", {
  # Created by fixtures/make-explorer-reported-success.js from fred-apps commit 68223a8
  explorer <- read.csv(test_path("fixtures", "explorer-reported-success.csv"),
                       na.strings = "NA", stringsAsFactors = FALSE, colClasses = "character")
  k <- nrow(explorer)
  res <- assess_replication_outcome(rep(NA_real_, k), rep(NA_real_, k), rep(NA_real_, k), rep(NA_real_, k),
                                    "reported_success",
                                    reported_success = explorer$reported_success,
                                    result = explorer$result)

  expect_gt(sum(explorer$record != "synthetic"), 50)
  expect_equal(res$outcome, explorer$explorer_outcome)
  expect_equal(res$outcome_report, explorer$explorer_outcome_report)
})

test_that("reported success is available for every coded record in the package data", {
  fred <- suppressWarnings(suppressMessages(load_fred_data()))
  res <- assess_replication_outcome(fred$es_o, fred$n_o, fred$es_r, fred$n_r, "reported_success",
                                    reported_success = fred$reported_success, result = fred$result)
  coded <- !is.na(fred$reported_success) & fred$reported_success != ""

  expect_gt(sum(coded), 0)
  expect_equal(res$outcome[coded], fred$reported_success[coded])
})
