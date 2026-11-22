test_that("inbuilt snapshot and read_fred return numeric p-values and character effect sizes by default", {
  snap <- readRDS(system.file("extdata", "snapshot", "data.RDS", package = "FReD"))
  expect_true(is.numeric(snap$pval_value_o))
  expect_true(is.numeric(snap$pval_value_r))
  expect_true(is.numeric(snap$n_o))
  expect_true(is.numeric(snap$n_r))
  expect_true(is.character(snap$es_value_o))
  expect_true(is.character(snap$es_value_r))

  # Test statistics must survive in the offline snapshot instead of being wiped to NA
  ts_o <- snap$es_value_o[tolower(snap$es_type_o) == "test statistic" & !is.na(snap$es_type_o)]
  expect_gt(sum(!is.na(ts_o)), 500)

  old_offline <- Sys.getenv("FRED_OFFLINE", unset = NA)
  on.exit({
    if (is.na(old_offline)) Sys.unsetenv("FRED_OFFLINE") else Sys.setenv(FRED_OFFLINE = old_offline)
  }, add = TRUE)
  use_FReD_offline(TRUE)

  d_char <- read_fred(retain_es_as_character = TRUE)
  expect_true(is.numeric(d_char$pval_value_o))
  expect_true(is.numeric(d_char$pval_value_r))
  expect_true(is.character(d_char$es_value_o))
  expect_true(is.character(d_char$es_value_r))

  d_num <- read_fred(retain_es_as_character = FALSE)
  expect_true(is.numeric(d_num$es_value_o))
  expect_true(is.numeric(d_num$es_value_r))
  expect_true(is.numeric(d_num$pval_value_o))
  expect_true(is.numeric(d_num$pval_value_r))
})

test_that("normalize_fred_data coerces character p-values including scientific notation and respects retain_es_as_character", {
  raw <- data.frame(
    fred_id = c("s1", "s2"),
    n_o = c("100", "50"),
    n_r = c("120", "60"),
    es_value_o = c("0.4", "t(48) = 2.5"),
    es_value_r = c("0.3", "X2(1, N = 60) = 6.0"),
    pval_value_o = c("4.02E-22", "0.05"),
    pval_value_r = c("5.66E-15", "0.12"),
    stringsAsFactors = FALSE
  )

  out_char <- normalize_fred_data(raw, retain_es_as_character = TRUE)
  expect_equal(out_char$id, c("s1", "s2"))
  expect_true(is.numeric(out_char$pval_value_o))
  expect_true(is.numeric(out_char$pval_value_r))
  expect_lt(out_char$pval_value_o[1], 0.05)
  expect_lt(out_char$pval_value_r[1], 0.05)
  expect_equal(out_char$es_value_o, c("0.4", "t(48) = 2.5"))
  expect_equal(out_char$es_value_r, c("0.3", "X2(1, N = 60) = 6.0"))

  out_num <- normalize_fred_data(raw, retain_es_as_character = FALSE)
  expect_true(is.numeric(out_num$es_value_o))
  expect_true(is.numeric(out_num$es_value_r))
  expect_equal(out_num$es_value_o, c(0.4, NA_real_))
  expect_equal(out_num$es_value_r, c(0.3, NA_real_))
})

test_that("clean_variables defensively coerces character numeric columns", {
  df <- data.frame(
    n_o = c("100", "50"),
    n_r = c("120", "60"),
    pval_value_o = c("4.02E-22", "0.05"),
    pval_value_r = c("5.66E-15", "0.12"),
    url_r = c(NA_character_, NA_character_),
    ref_o = c("Smith (2020)", "Jones (2019)"),
    stringsAsFactors = FALSE
  )

  out <- clean_variables(df)
  expect_true(is.numeric(out$n_o))
  expect_true(is.numeric(out$n_r))
  expect_true(is.numeric(out$pval_value_o))
  expect_true(is.numeric(out$pval_value_r))
  expect_true(out$pval_value_o[1] < 0.05)
})

test_that("coerce_to_numeric handles missing id_var cleanly", {
  df <- data.frame(x = c("1.5", "not_a_num"), stringsAsFactors = FALSE)
  expect_warning(out <- coerce_to_numeric(df, "x", id_var = NULL, verbose = TRUE), "row|IDs")
  expect_equal(out$x, c(1.5, NA_real_))
})

test_that("update_offline_data reports failure instead of silently saving inbuilt snapshot on corrupt file", {
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

  res <- suppressWarnings(suppressMessages(update_offline_data(data_file = bad_file, items = "data")))
  expect_equal(res$failed, "data")
  expect_length(res$succeeded, 0L)
  expect_equal(file.info(snap_path)$mtime, mtime_before)
  # FRED_OFFLINE must be restored to TRUE
  expect_true(get_param("FRED_OFFLINE"))
})

test_that("load_fred_data converts test statistics and returns numeric p-values", {
  skip_on_cran()
  old_offline <- Sys.getenv("FRED_OFFLINE", unset = NA)
  on.exit({
    if (is.na(old_offline)) Sys.unsetenv("FRED_OFFLINE") else Sys.setenv(FRED_OFFLINE = old_offline)
  }, add = TRUE)
  use_FReD_offline(TRUE)

  fred <- suppressWarnings(suppressMessages(load_fred_data()))
  expect_true(is.numeric(fred$pval_value_o))
  expect_true(is.numeric(fred$pval_value_r))
  expect_true(is.numeric(fred$ci.lower_o))
  expect_true(is.numeric(fred$ci.upper_o))
  expect_true(is.numeric(fred$ci.lower_r))
  expect_true(is.numeric(fred$ci.upper_r))
  # Retaining test-statistic strings recovers hundreds of converted effect sizes
  expect_gt(sum(!is.na(fred$es_o)), 1100)
  expect_gt(sum(!is.na(fred$es_r)), 1100)
})
