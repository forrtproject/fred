test_that("replication success assessed correctly", {
  expect_equal(2 * 2, 4)
})

es_agg <- compare_effectsizes(c(.1, .4), c(10, 100), c(.4, .1), c(100, 10))

test_that("replications can be aggregated", {
  expect_subset(names(es_agg), c("est", "ci_lb", "QEp"))
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
