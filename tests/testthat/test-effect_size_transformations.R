# Test statistic transformations
library(testthat)

test_that("convert_effect_sizes correctly converts t(df) to r", {
  es_values <- c("t(10) = 2.5", "t(30) = -1.8", "t(5) = 0")
  es_types <- c("test statistic", "test statistic", "test statistic")

  expected_r <- c(
    2.5 / sqrt(2.5^2 + 10),  # Matches output from effectsize::t_to_r(2.5, 10)
    -1.8 / sqrt((-1.8)^2 + 30),
    0 / sqrt(0^2 + 5)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts F(df1=1, df2) to r", {
  es_values <- c("F(1, 20) = 4.5", "F(1, 50) = 2.0")
  es_types <- c("test statistic", "test statistic")

  expected_r <- c(
    sqrt(4.5) / sqrt((sqrt(4.5))^2 + 20), # Matches output from effectsize::F_to_r(4.5, 1, 20)
    sqrt(2.0) / sqrt((sqrt(2.0))^2 + 50)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes returns NA for F(df1 > 1, df2) and F < 1", {
  es_values <- c("F(2, 15) = 3.2", "F(3, 40) = 5.1", "F < 1")
  es_types <- c("test statistic", "test statistic", "test statistic")

  result <- convert_effect_sizes(es_values, es_types)
  expect_true(all(is.na(result)))  # Should return NA for non-convertible F-tests
})

test_that("convert_effect_sizes returns NA for invalid test statistic formats", {
  es_values <- c("t(df) = 2.5", "F(10) = 4.5", "f(1,20) 4.5", "random text")
  es_types <- c("test statistic", "test statistic", "test statistic", "test statistic")

  result <- convert_effect_sizes(es_values, es_types)
  expect_true(all(is.na(result)))  # All should fail conversion
})

test_that("convert_effect_sizes converts z-scores with N", {
  es_values <- c(" z = 2.81, N = 34", "z = 3.1, N = 68")
  es_types <- c("test statistic", "test statistic")

  expected_r <- c(
    2.81 / sqrt(2.81^2 + 34), # Matches output from effectsize::z_to_r(2.81, 34)
    3.1 / sqrt(3.1^2 + 68)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes converts chi-square(1, N) to r", {
  es_values <- c("x2(1, N = 12) = 5", "X2(1,N=50)=3.84", "χ2(1, N = 30) = 4")
  es_types <- rep("test statistic", length(es_values))

  expected_r <- c(
    sqrt(5 / 12),
    sqrt(3.84 / 50),
    sqrt(4 / 30)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})


test_that("convert_effect_sizes correctly returns r for 'r' and 'phi'", {
  es_values <- c("0.5", "-0.7")
  es_types <- c("r", "phi")
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, c(0.5, -0.7), tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts r2 to r", {
  es_values <- c("0.25", "0.49")
  es_types <- c("r2", "r2")
  expected_r <- c(sqrt(0.25), sqrt(0.49))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts d to r and retains sign", {
  es_values <- c("0.5", "-0.5")
  es_types <- c("d", "d")
  # Expected conversion: d * sqrt(d^2 + 4)
  expected_r <- c( 0.5 / sqrt(0.5^2 + 4), -0.5 / sqrt(0.5^2 + 4)) # Matches output from effectsize::d_to_r(0.5)
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts odds ratios to r", {
  es_values <- c("2", "0.5")
  es_types <- c("or", "or")
  ds1 <- log(2) * sqrt(3) / pi
  ds2 <- log(0.5) * sqrt(3) / pi
  expected_r <- c(
    ds1 / sqrt((ds1^2 + 4)),
    ds2 / sqrt((ds2^2 + 4))
  ) # Matches effectsize::oddsratio_to_r(c(.5, 2))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts eta squared to r", {
  es_values <- c("0.2", "0.8")
  es_types <- c("etasq", "etasq")
  ds1 <- 2 * sqrt(0.2 / (1 - 0.2))
  ds2 <- 2 * sqrt(0.8 / (1 - 0.8))
  expected_r <- c(
    ds1 / sqrt((ds1^2 + 4)),
    ds2 / sqrt((ds2^2 + 4))
  ) # Matches esc::pearsons_r(eta = c(.2, .8))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts f to r", {
  es_values <- c("0.3", "1")
  es_types <- c("f", "f")
  ds1 <- 2 * 0.3
  ds2 <- 2 * 1
  expected_r <- c(
    ds1 / sqrt((ds1^2 + 4)),
    ds2 / sqrt((ds2^2 + 4))
  ) # Matches esc::pearsons_r(f = c(.3, 1))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts f^2 to r", {
  es_values <- c("0.1", "0.2")
  es_types <- c("f^2", "f^2")
  # Formula: r = sqrt(f^2 / (1 + f^2))
  expected_r <- c(
    sqrt(0.1 / (1 + 0.1)),
    sqrt(0.2 / (1 + 0.2))
  )
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts cohen's f^2 aliases to r", {
  es_values <- c("0.1", "0.15", "0.2")
  es_types <- c("f^2", "cohen's f^2", "f2")
  expected_r <- c(
    sqrt(0.1 / (1 + 0.1)),
    sqrt(0.15 / (1 + 0.15)),
    sqrt(0.2 / (1 + 0.2))
  )
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes warns for unknown effect types and returns NA", {
  es_values <- c("0.5")
  es_types <- c("unknown type")
  expect_warning(result <- convert_effect_sizes(es_values, es_types), "Unknown effect size types")
  expect_true(is.na(result))
})

test_that("convert_effect_sizes handles missing values", {
  es_values <- c(NA, "0.5")
  es_types <- c("r", NA)
  result <- convert_effect_sizes(es_values, es_types)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
})

# Omnibus test to ensure that order is maintained etc
test_that("convert_effect_sizes works for all supported effect sizes", {
  es_values <- c(
    "0.5",             # r
    "-0.3",            # phi
    "0.16",            # r2
    "0.8",             # d (positive)
    "-0.8",            # d (negative)
    "2",               # odds ratio
    "0.3",             # etasq
    "0.5",             # f
    "t(10) = 2.5",     # test statistic: t-test
    "F(1, 20) = 4.5",   # test statistic: F-test (df1 == 1)
    "z = 2.81, N = 34",  # test statistic: z-test
    "χ2(1, N = 12) = 5", # test statistic: chi-square with df1 == 1
    NA,
    2,
    1
  )

  es_types <- c(
    "r",
    "phi",
    "r2",
    "d",
    "d",
    "odds ratio",
    "etasq",
    "f",
    "test statistic",
    "test statistic",
    "test statistic",
    "test statistic",
    "invalid",
    "none",
    NA
  )

  expected_r <- c(
    0.5,                                # r
    -0.3,                               # phi
    sqrt(0.16),                         # r2: sqrt(0.16) = 0.4
    0.8 / sqrt(0.8^2 + 4),                # d (positive)
    -0.8 / sqrt(0.8^2 + 4),               # d (negative, sign retained)
    { ds <- log(2) * sqrt(3) / pi; sqrt(ds^2 / (ds^2 + 4)) },  # odds ratio
    { ds <- 2 * sqrt(0.3 / (1 - 0.3)); sqrt(ds^2 / (ds^2 + 4)) },# etasq
    { ds <- 2 * 0.5; sqrt(ds^2 / (ds^2 + 4)) },               # f
    2.5 / sqrt(2.5^2 + 10),               # t-test
    { t_val <- sqrt(4.5); t_val / sqrt(t_val^2 + 20) },         # F-test
    2.81 / sqrt(2.81^2 + 34),              # z-test
    sqrt(5 / 12),                          # chi-square test with df1 == 1
    NA,
    NA,
    NA
  )

  expect_warning(result <- convert_effect_sizes(es_values, es_types))
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("chi-square that cannot come from a 2 x 2 table is not converted", {
  es_values <- c("X2(1, N = 100) = 140.71", "X2(2, N = 64) = 11.28", "X2(1) = 6.7", "X2(1, N = 45) = 43.02")
  es_types <- rep("test statistic", length(es_values))

  result <- convert_effect_sizes(es_values, es_types, return_reasons = TRUE)

  # 140.71 / 100 > 1 would give r = 1.19; it must not be capped to 1 either
  expect_true(is.na(result$r[1]))
  expect_match(result$na_reason[1], "chi-squared / N > 1")
  expect_true(is.na(result$r[2]))
  expect_match(result$na_reason[2], "df > 1")
  expect_true(is.na(result$r[3]))
  expect_match(result$na_reason[3], "without N")
  expect_equal(result$r[4], sqrt(43.02 / 45))
  expect_true(is.na(result$na_reason[4]))

  # Default return stays a plain numeric vector
  expect_equal(convert_effect_sizes(es_values, es_types), c(NA, NA, NA, sqrt(43.02 / 45)))
})

test_that("chi-square followed by further text is parsed", {
  expect_equal(convert_effect_sizes("X2(1, N = 72) = 3.886, N = 72, p = 0.049", "test statistic"), sqrt(3.886 / 72))
  result <- convert_effect_sizes("X2(3, N = 705) = 42.167, p < .001", "test statistic", return_reasons = TRUE)
  expect_true(is.na(result$r))
  expect_match(result$na_reason, "df > 1")
})

test_that("chi-square with N given without 'N =' is parsed", {
  expect_equal(convert_effect_sizes("X2(1, 85) = 7.873", "test statistic"), sqrt(7.873 / 85))
})

test_that("add_common_effect_sizes honours dataset flags for non-convertible effects", {
  fred_data <- data.frame(
    id = 1:3,
    es_value_o = c("X2(1, N = 119) = 110.60", "X2(1, N = 50) = 5", "X2(1, N = 100) = 140.71"),
    es_type_o = "test statistic",
    es_value_r = c("t(10) = 2.5", "X2(1, N = 40) = 2", "t(10) = 2.5"),
    es_type_r = "test statistic",
    es_not_convertible_o = c("chi-squared from a GLMM likelihood-ratio test", "", NA),
    es_o = c(0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )

  out <- suppressMessages(add_common_effect_sizes(fred_data))

  expect_true(is.na(out$es_o[1]))
  expect_equal(out$es_o_na_reason[1], "chi-squared from a GLMM likelihood-ratio test")
  expect_equal(out$es_o[2], sqrt(5 / 50))
  expect_true(is.na(out$es_o_na_reason[2]))
  # Not refilled from the existing es_o column
  expect_true(is.na(out$es_o[3]))
  expect_match(out$es_o_na_reason[3], "chi-squared / N > 1")
  # Missing flag column for the replication side is ignored
  expect_equal(out$es_r[2], sqrt(2 / 40))
})

test_that("convert_effect_sizes handles decimal df, leading decimals, aliases, and quiet mode", {
  vals <- c("t(84) = .27", "T(10) = 2.5", "t(173.36) = 5.37", "t(64) = -.88", "F(1, 94) = .13", "F(1, 62.9) = 8.5", "Z = -8.31, N = 1137 ,")
  types <- rep("test statistic", length(vals))
  expected <- c(
    0.27 / sqrt(0.27^2 + 84),
    2.5 / sqrt(2.5^2 + 10),
    5.37 / sqrt(5.37^2 + 173.36),
    -0.88 / sqrt(0.88^2 + 64),
    sqrt(0.13) / sqrt(0.13 + 94),
    sqrt(8.5) / sqrt(8.5 + 62.9),
    -8.31 / sqrt(8.31^2 + 1137)
  )
  expect_equal(convert_effect_sizes(vals, types), expected, tolerance = 1e-6)

  expect_equal(convert_effect_sizes(c("0.25", "0.05", "0.10"), c("r-squared", "f-square", "f-squared")),
               c(sqrt(0.25), sqrt(0.05 / 1.05), sqrt(0.10 / 1.10)), tolerance = 1e-6)

  # Known non-convertible types ("hazard ratio", "cohen's dz") do not warn as unknown
  expect_no_warning(nc <- suppressMessages(convert_effect_sizes(c("1.5", "0.4"), c("hazard ratio", "cohen's dz"))))
  expect_true(all(is.na(nc)))

  # Out-of-domain inputs return NA_real_ without NaN warnings
  expect_no_warning(bad <- convert_effect_sizes(c("-0.1", "0", "1.0", "-0.2"), c("r2", "or", "etasq", "f2"), quiet = TRUE))
  expect_true(all(is.na(bad)))
  expect_type(bad, "double")

  # quiet = TRUE suppresses unknown type warnings and missing/non-convertible messages
  expect_silent(res_q <- convert_effect_sizes(c("0.5", NA), c("unknown type", "beta"), quiet = TRUE))
  expect_type(res_q, "double")
})

test_that("add_common_effect_sizes clamps converted +/-1 correlations to +/-0.9999", {
  df <- data.frame(
    id = c("a", "b"),
    es_value_o = c("1", "-1"),
    es_type_o = c("r", "r"),
    es_value_r = c("1", "0.5"),
    es_type_r = c("r2", "r"),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(add_common_effect_sizes(df))
  expect_equal(out$es_o, c(0.9999, -0.9999))
  expect_equal(out$es_r, c(0.9999, 0.5))
})

test_that("p_from_r returns NA_real_ and guards |r| >= 1 and N <= 2", {
  expect_no_warning(p_all_na <- p_from_r(c(NA_real_, 1, -1, 1.5, 0.5), c(50, 50, 50, 50, 2)))
  expect_true(all(is.na(p_all_na)))
  expect_type(p_all_na, "double")

  expect_lt(p_from_r(0.5, 50), 0.001)
  expect_equal(p_from_r(0, 50), 1)
})

test_that("align_effect_direction preserves es_r when es_o is 0 or NA and flows cleanly into add_uncertainty", {
  df <- data.frame(
    es_o = c(0.3, -0.3, 0, NA_real_),
    es_r = c(0.2, -0.2, 0.15, -0.25),
    n_o = c(50, 50, 50, 50),
    n_r = c(60, 60, 60, 60)
  )
  out <- align_effect_direction(df)
  expect_equal(out$es_o, c(0.3, 0.3, 0, NA_real_))
  expect_equal(out$es_r, c(0.2, 0.2, 0.15, -0.25))

  unc <- add_uncertainty(out)
  expect_true(is.na(unc$p_value_o[4]))
  expect_true(!is.na(unc$p_value_r[4]))
  expect_true(is.numeric(unc$ci.lower_r))
})

test_that("code_replication_outcomes classifies inconsistent opposite and handles NA p-values", {
  df <- data.frame(
    es_o = c(0.4, 0.4, 0.4, 0.4, NA_real_, 0.4),
    es_r = c(-0.35, 0.15, 0.70, 0.38, 0.05, -0.2),
    p_value_o = c(0.001, 0.001, 0.001, 0.001, NA_real_, 0.001),
    p_value_r = c(0.001, 0.01, 0.001, 0.001, 0.60, NA_real_),
    ci.lower_r = c(-0.50, 0.05, 0.55, 0.25, -0.15, NA_real_),
    ci.upper_r = c(-0.20, 0.25, 0.85, 0.50, 0.25, NA_real_)
  )

  expect_no_warning(out <- code_replication_outcomes(df))
  expect_equal(out$consistency[1], "inconsistent, opposite")
  expect_equal(out$consistency[2], "inconsistent, smaller")
  expect_equal(out$consistency[3], "inconsistent, larger")
  expect_equal(out$consistency[4], "consistent")
  expect_equal(out$result[1], "failed replication")
  expect_equal(out$result[4], "successful replication")
  # Missing p_value_o or p_value_r must not falsely yield "failed replication"
  expect_true(is.na(out$result[5]))
  expect_true(is.na(out$result[6]))
})

test_that("column-length validation errors cleanly in add_common_effect_sizes, add_uncertainty, add_sampling_variances", {
  df <- data.frame(es_value_o = "0.3", es_type_o = "r", es_o = 0.3, n_o = 50, stringsAsFactors = FALSE)
  expect_error(add_common_effect_sizes(df, es_value_columns = "es_value_o", es_type_columns = c("es_type_o", "es_type_r"), es_common_names = "es_o"),
               "must be equal")
  expect_error(add_uncertainty(df, es_value_columns = "es_o", N_columns = "n_o", vi_columns = "vi_o",
                               ci_lower_columns = "cl", ci_upper_columns = "cu", p_values = c("p1", "p2")),
               "must be equal")
  expect_error(add_sampling_variances(df, es_value_columns = "es_o", N_columns = "n_o", vi_columns = c("v1", "v2")),
               "must be equal")
})

test_that("add_replication_power and augment_for_zcurve guard NA, non-positive n_r, and |r| >= 1", {
  df <- data.frame(es_o = c(NA_real_, 0.3, 1.0, 0.3, 0.3), n_o = c(50, 50, 50, 50, 50), n_r = c(2, 2, 50, 0, -5))
  out <- add_replication_power(df)
  expect_equal(out$power_r, c(NA_real_, 0, NA_real_, NA_real_, NA_real_))
  expect_type(out$power_r, "double")

  expect_no_warning(zc <- augment_for_zcurve(df))
  expect_true(is.na(zc$z[1]))
  expect_true(!is.na(zc$z[2]))
  expect_true(is.na(zc$z[3]))
  expect_type(zc$z, "double")
})
