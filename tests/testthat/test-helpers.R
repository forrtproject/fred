test_that("power functions work", {
  expect_equal(.4, compute_power_r(find_r_for_power(.4, 100, .05), 100, .05), tolerance = 1e-5)
  # As small sample correction cannot be analytically reversed, we need to use a larger tolerance here - but 3 identical decimal digits are still ok.
  expect_equal(.4, compute_power_r(find_r_for_power(.4, 10, .05), 10, .05), tolerance = 1e-3)
})

test_that("helper functions guard invalid n <= 3 and |r| >= 1 without warnings", {
  expect_no_warning({
    ci <- compute_ci_r(c(1, 0.5), c(50, 3))
    pw <- compute_power_r(c(1, 0.5), c(50, 3))
    rf <- find_r_for_power(0.33, c(3, 50))
    pi <- calculate_prediction_interval(c(1, 0.5), c(30, 3), c(30, 30))
  })
  expect_true(all(is.na(ci$lower)) && all(is.na(pw)) && is.na(rf[1]) && all(is.na(pi$lower)))
})
