test_that("offline read_fred and load_fred_data preserve test statistics and numeric p-values", {
  skip_on_cran()
  old_offline <- Sys.getenv("FRED_OFFLINE", unset = NA)
  on.exit({
    if (is.na(old_offline)) Sys.unsetenv("FRED_OFFLINE") else Sys.setenv(FRED_OFFLINE = old_offline)
  }, add = TRUE)
  use_FReD_offline(TRUE)

  d_char <- read_fred(retain_es_as_character = TRUE)
  expect_true(is.numeric(d_char$pval_value_o) && is.numeric(d_char$pval_value_r))
  expect_true(is.character(d_char$es_value_o) && is.character(d_char$es_value_r))
  expect_true(is.numeric(read_fred(retain_es_as_character = FALSE)$es_value_o))

  fred <- suppressWarnings(suppressMessages(load_fred_data()))
  expect_gt(sum(!is.na(fred$es_o)), 1100)
  expect_gt(sum(!is.na(fred$es_r)), 1100)
})

test_that("clean_variables and coerce_to_numeric handle scientific notation, factors, and missing id_var", {
  df <- data.frame(
    n_o = factor(c("100", "50")),
    n_r = c("120", "bad"),
    pval_value_o = c("4.02E-22", "0.05"),
    pval_value_r = c("5.66E-15", "0.12"),
    url_r = c(NA_character_, NA_character_),
    ref_o = c("Smith (2020)", "Jones (2019)"),
    stringsAsFactors = FALSE
  )

  expect_warning(coerced <- coerce_to_numeric(df, c("n_o", "n_r"), id_var = NULL, verbose = TRUE), "has issues in IDs: 2")
  expect_equal(coerced$n_o, c(100, 50))
  expect_equal(coerced$n_r, c(120, NA_real_))

  cleaned <- clean_variables(df)
  expect_true(is.numeric(cleaned$pval_value_o) && cleaned$pval_value_o[1] < 0.05)
})

test_that("update_offline_data reports failure on unreadable files without overwriting snapshots", {
  old_offline <- Sys.getenv("FRED_OFFLINE", unset = NA)
  on.exit({
    if (is.na(old_offline)) Sys.unsetenv("FRED_OFFLINE") else Sys.setenv(FRED_OFFLINE = old_offline)
  }, add = TRUE)
  use_FReD_offline(TRUE)

  bad_file <- tempfile(fileext = ".xlsx")
  writeLines("not a valid xlsx file", bad_file)
  on.exit(unlink(bad_file), add = TRUE)

  snap_path <- system.file("extdata", "snapshot", "data.RDS", package = "FReD")
  mtime_before <- file.info(snap_path)$mtime

  res <- suppressWarnings(suppressMessages(update_offline_data(data_file = bad_file, items = c("data", "data_description"))))
  expect_equal(res$failed, c("data", "data_description"))
  expect_equal(file.info(snap_path)$mtime, mtime_before)
  expect_true(get_param("FRED_OFFLINE"))
})
