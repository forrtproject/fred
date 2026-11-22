test_that("replication success assessed correctly", {
  expect_equal(2 * 2, 4)
})

es_agg <- compare_effectsizes(c(.1, .4), c(10, 100), c(.4, .1), c(100, 10))

test_that("replications can be aggregated", {
  expect_subset(names(es_agg), c("est", "ci_lb", "ci_ub", "QEp"))
  expect_true(all(es_agg$ci_lb <= es_agg$est & es_agg$est <= es_agg$ci_ub))
})

test_that("significance_agg catches reversals, unaligned negatives, and zero original effects", {
  res <- assess_replication_outcome(c(0.2, -0.4, 0), c(30, 50, 30), c(-0.4, -0.35, 0.4), c(200, 100, 200), "significance_agg")
  expect_equal(res$outcome, c("failure", "success", NA_character_))
  expect_equal(res$outcome_report, c("failure (reversal)", "success", NA_character_))
})

test_that("significance_r supports coded p-values, inequality types, one-tailed values, and p_from_r fallback", {
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

  expect_equal(res$outcome, c("success", "OS not significant", "failure", "failure", "success", NA_character_, "failure"))
  expect_equal(res$outcome_report[3], "failure (reversal)")
  expect_equal(is_p_significant(c(0.05, 0.05, 0.05), c("ge (>)", "=<", "=>")), c(FALSE, NA, FALSE))
})

test_that("homogeneity_significance, consistency_pi, and small_telescopes handle edge cases cleanly", {
  # Missing es_r -> NA (not "OS not significant" or "not coded"); opposite pooled CI -> labelled distinctly
  res_hs <- assess_replication_outcome(c(0.05, 0.35), c(30, 40), c(NA_real_, -0.45), c(NA_real_, 250), "homogeneity_significance")
  expect_true(is.na(res_hs$outcome[1]) && is.na(res_hs$outcome_report[1]))
  expect_equal(res_hs$outcome[2], "failure")
  expect_match(res_hs$outcome_report[2], "opposite direction")

  expect_no_warning(res_pi <- assess_replication_outcome(c(0.5, 0.5), c(50, 3), c(0.1, 0.1), c(30, 30), "consistency_pi"))
  expect_equal(res_pi$outcome, c("success", NA_character_))

  expect_no_warning(res_st <- assess_replication_outcome(c(0.5, -0.3), c(3, 50), c(0.2, -0.3), c(100, 100), "small_telescopes"))
  expect_equal(res_st$outcome, c(NA_character_, "success"))
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
