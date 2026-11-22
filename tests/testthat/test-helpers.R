test_that("power functions work", {
  expect_equal(.4, compute_power_r(find_r_for_power(.4, 100, .05), 100, .05), tolerance = 1e-5)
  # As small sample correction cannot be analytically reversed, we need to use a larger tolerance here - but 3 identical decimal digits are still ok.
  expect_equal(.4, compute_power_r(find_r_for_power(.4, 10, .05), 10, .05), tolerance = 1e-3)
})

test_that("helper functions guard invalid n <= 3, |r| >= 1, and NA inputs cleanly", {
  expect_no_warning(ci <- compute_ci_r(c(NA_real_, 1, -1.2, 0.5, 0.9999), c(50, 50, 50, 3, 10)))
  expect_true(all(is.na(ci$lower[1:4])))
  expect_true(all(is.na(ci$upper[1:4])))
  expect_true(is.finite(ci$lower[5]) && is.finite(ci$upper[5]))
  expect_type(ci$lower, "double")

  expect_no_warning(pw <- compute_power_r(c(NA_real_, 1, 0.5), c(50, 50, 3)))
  expect_true(all(is.na(pw)))
  expect_type(pw, "double")

  expect_no_warning(rf <- find_r_for_power(0.33, c(NA_real_, 2, 3, 50)))
  expect_true(all(is.na(rf[1:3])))
  expect_true(is.finite(rf[4]))

  expect_no_warning(pi <- calculate_prediction_interval(c(0.5, 1.0, 0.5, NA_real_), c(30, 30, 3, 30), c(30, 30, 30, 30)))
  expect_true(is.finite(pi$lower[1]) && is.finite(pi$upper[1]))
  expect_true(all(is.na(pi$lower[2:4])))
  expect_true(all(is.na(pi$upper[2:4])))
})
