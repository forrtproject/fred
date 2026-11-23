# Helpers for the FReD releases on Zenodo (https://doi.org/10.5281/zenodo.24461606)

# Concept record ID: resolves to the latest FReD version on Zenodo
FRED_ZENODO_CONCEPT_ID <- "24461606"

# Data file in each Zenodo version that the package downloads by default
FRED_ZENODO_DATA_FILE <- "fred.csv"

#' Get the record of the latest FReD version on Zenodo
#'
#' Resolves the concept record to its latest version through the Zenodo API, and
#' caches the record for the session.
#'
#' @param cache Return the record from cache, if already requested during this session?
#' @return The record as a list, parsed from the Zenodo JSON.
#' @noRd
zenodo_latest_record <- function(cache = TRUE) {
  if (cache && exists("zenodo_record", .cache, inherits = FALSE)) {
    return(.cache$zenodo_record)
  }
  record <- zenodo_get_json(paste0("https://zenodo.org/api/records/", FRED_ZENODO_CONCEPT_ID, "/versions/latest"))
  .cache$zenodo_record <- record
  record
}

zenodo_get_json <- function(url, query = NULL, timeout = 15) {
  response <- httr::GET(url, query = query, httr::timeout(timeout))
  httr::stop_for_status(response, task = "query the Zenodo API")
  httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
}

#' Get the download URL of a file in a Zenodo record
#' @noRd
zenodo_file_url <- function(file, record = zenodo_latest_record()) {
  keys <- vapply(record$files, function(f) f$key, character(1))
  i <- match(file, keys)
  if (is.na(i)) {
    stop("File '", file, "' not found in Zenodo record ", record$id, ".")
  }
  record$files[[i]]$links$self
}

#' URL of the FReD data file
#'
#' Returns `FRED_DATA_URL` if the user set it, otherwise the data file of the
#' latest FReD version on Zenodo.
#' @noRd
fred_data_url <- function() {
  url <- Sys.getenv("FRED_DATA_URL")
  if (nzchar(url)) {
    return(url)
  }
  zenodo_file_url(FRED_ZENODO_DATA_FILE)
}

#' Format a Zenodo record as an APA-style data citation (markdown)
#' @noRd
zenodo_citation <- function(record = zenodo_latest_record()) {
  meta <- record$metadata
  authors <- vapply(meta$creators, function(cr) format_creator(cr$name), character(1))
  if (length(authors) > 1) {
    authors <- paste0(paste(authors[-length(authors)], collapse = ", "), ", & ", authors[length(authors)])
  }
  paste0(
    authors, " (", substr(meta$publication_date, 1, 4), "). _", meta$title, "_",
    if (!is.null(meta$version)) paste0(" (Version ", meta$version, ")"),
    " [Data set]. Zenodo. <https://doi.org/", record$doi, ">"
  )
}

# "Deressa, Teshome Kebede" -> "Deressa, T. K."; names without a comma are kept as they are
format_creator <- function(name) {
  parts <- strsplit(name, ",\\s*")[[1]]
  if (length(parts) < 2) {
    return(name)
  }
  given <- strsplit(trimws(parts[2]), "\\s+")[[1]]
  initials <- vapply(given, function(g) {
    paste0(substr(strsplit(g, "-")[[1]], 1, 1), ".", collapse = "-")
  }, character(1))
  paste0(parts[1], ", ", paste(initials, collapse = " "))
}

#' Read a FReD data file
#'
#' Reads the data from a .csv file (the default download from Zenodo) or from
#' the first sheet of an .xlsx file. Files are read as .xlsx if they have that
#' extension or are zip archives (as .xlsx files are), and as .csv otherwise.
#' Throws an error if the data lacks the columns FReD needs.
#'
#' @param file Path to the data file.
#' @return A data frame.
#' @noRd
read_fred_file <- function(file) {
  fred_data <- if (is_xlsx_file(file)) {
    # openxlsx only reads files with an .xlsx extension (downloads may lack it)
    if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) {
      tmp <- tempfile(fileext = ".xlsx")
      file.copy(file, tmp)
      on.exit(unlink(tmp), add = TRUE)
      file <- tmp
    }
    openxlsx::read.xlsx(file, sheet = 1)
  } else {
    utils::read.csv(file,
      fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  required <- c("es_value_o", "es_type_o", "n_o", "es_value_r", "es_type_r", "n_r")
  missing <- setdiff(required, names(fred_data))
  if (length(missing) > 0) {
    stop("The FReD data in ", file, " lacks the columns: ", paste(missing, collapse = ", "))
  }
  fred_data
}

# .xlsx files are zip archives, which start with "PK"
is_xlsx_file <- function(file) {
  grepl("\\.xlsx$", file, ignore.case = TRUE) ||
    identical(readBin(file, "raw", 2L), as.raw(c(0x50, 0x4b)))
}

# Read the data from file; if that fails, download it from url and read it from there
safe_read_fred <- function(file, url) {
  tryCatch(read_fred_file(file), error = function(e) {
    tmp <- tempfile()
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    read_fred_file(tmp)
  })
}
