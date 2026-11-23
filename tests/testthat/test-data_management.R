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

test_that("read_fred_file reads Zenodo-style CSV and detects xlsx by content", {
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  # Zenodo's fred.csv: UTF-8 with BOM, all fields quoted, CRLF line endings, empty cells for missing
  lines <- c(
    '"effect_uid","ref_o","es_value_o","es_type_o","n_o","es_value_r","es_type_r","n_r","notes"',
    '"fe00001","Smith & Jones (2020)","t(48) = 2.1","test statistic","50","0.3","r","120",""'
  )
  con <- file(csv, "wb")
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), con)
  close(con)

  d <- read_fred_file(csv)
  expect_equal(names(d)[1], "effect_uid")
  expect_equal(d$ref_o, "Smith & Jones (2020)")
  expect_equal(d$es_value_o, "t(48) = 2.1")
  expect_true(is.na(d$notes))
  expect_false(is_xlsx_file(csv))

  xlsx_no_ext <- tempfile()
  on.exit(unlink(xlsx_no_ext), add = TRUE)
  openxlsx::write.xlsx(d, xlsx_no_ext)
  expect_true(is_xlsx_file(xlsx_no_ext))
  expect_equal(read_fred_file(xlsx_no_ext)$ref_o, "Smith & Jones (2020)")

  bad <- tempfile(fileext = ".csv")
  on.exit(unlink(bad), add = TRUE)
  writeLines(c("a,b", "1,2"), bad)
  expect_error(read_fred_file(bad), "lacks the columns")
})

test_that("Zenodo creators are formatted as APA names", {
  expect_equal(format_creator("Deressa, Teshome Kebede"), "Deressa, T. K.")
  expect_equal(format_creator("Lach, Radoslaw P."), "Lach, R. P.")
  expect_equal(format_creator("Doe, Jean-Paul"), "Doe, J.-P.")
  expect_equal(format_creator("FORRT"), "FORRT")
})
