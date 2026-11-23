# Global imports - mostly for shiny
#' @import shiny
#' @import bslib
#' @import shinycssloaders
#' @import ggplot2
#' @rawNamespace import(plotly, except = c(last_plot))
#' @import dplyr
#' @importFrom dplyr %>%
#' @import checkmate
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @import markdown
#' @importFrom stats na.omit pnorm pt qnorm qt
#' @importFrom utils browseURL download.file
NULL

utils::globalVariables(c("."))

# Add cache
.cache <- rlang::new_environment()

# Dummy function calls to ensure R CMD check recognizes the usage
# these packages are used in the Shiny apps
dummy_function_calls <- function() {
  if (FALSE) {
    forcats::fct_relevel
    pdftools::pdf_text
    shinyjs::useShinyjs
    zcurve::zcurve
  }
}

.check_req_packages <- function(x, note = "") {
  res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
  if (!all(res)) {
    if (!interactive()) {
      stop(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x[!res], collapse = ", "),
           call. = FALSE
      )
    }
    op <- options("warn")
    on.exit(options(op))
    options(warn = 1)
    warning(note, "The following packages are required for this function but
                   cannot be loaded: ", paste0(x[!res], collapse = ", "),
            call. = FALSE)
    choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
    if (choice %in% c("Y", "y")) {
      utils::install.packages(x[!res])
      res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
      if (!all(res)) {
        stop("Not all packages could be installed successfully. The following could still not be loaded: ", paste0(x[!res], collapse = ", "),
             call. = FALSE
        )
      }
      return(TRUE)
    }
    stop("Cannot proceed without these packages.", call. = FALSE)
  }
}

#' Get FReD dataset citation
#'
#' Returns the citation for the latest version of the FReD dataset on Zenodo,
#' created from the metadata of the Zenodo record.
#'
#' @param cache Should the citation be returned from cache, if already requested during this session? Defaults to TRUE.
#' @return A markdown-formatted citation for the FReD dataset.

create_citation <- function(cache = TRUE) {
  if (get_param("FRED_OFFLINE")) {
    return(return_inbuilt("citation"))
  }
  tryCatch(fetch_citation(cache), error = function(e) {
    return_inbuilt("citation")
  })
}

# Citation of the latest Zenodo version; errors if it cannot be retrieved
fetch_citation <- function(cache = TRUE) {
  if (cache && exists("citation", .cache, inherits = FALSE)) {
    return(.cache$citation)
  }
  cit <- zenodo_citation(zenodo_latest_record(cache = cache))
  .cache$citation <- cit
  cit
}

#' Get the dataset changelog
#'
#' Returns the changelog of the FReD dataset as a markdown string. By default, this is
#' the `CHANGELOG.md` of the latest FReD version on Zenodo, which lists all versions,
#' including the earlier releases on OSF.
#'
#' @param changelog_file URL of a markdown changelog file to return instead of the Zenodo changelog.
#' @param cache Should the changelog be returned from cache, if already requested during this session? Defaults to TRUE.

get_dataset_changelog <- function(changelog_file = NULL, cache = TRUE) {
  if (get_param("FRED_OFFLINE")) {
    return(return_inbuilt("data_changelog"))
  }
  tryCatch(fetch_changelog(changelog_file, cache), error = function(e) {
    return_inbuilt("data_changelog")
  })
}

# Changelog from changelog_file or the latest Zenodo version; errors if it cannot be retrieved.
# Only the Zenodo changelog is cached.
fetch_changelog <- function(changelog_file = NULL, cache = TRUE) {
  if (!is.null(changelog_file)) {
    return(read_text_url(changelog_file))
  }
  if (cache && exists("changelog", .cache, inherits = FALSE)) {
    return(.cache$changelog)
  }
  changelog <- read_text_url(zenodo_file_url("CHANGELOG.md", zenodo_latest_record(cache = cache)))
  .cache$changelog <- changelog
  changelog
}

read_text_url <- function(url) {
  temp <- tempfile(fileext = ".md")
  download.file(url, temp, mode = "wb", quiet = TRUE)
  paste(readLines(temp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Load the FReD dataset
#'
#' This function loads the FReD dataset into R, and conducts variable transformations to  prepare for analyses.
#'
#' @inheritParams read_fred
#' @return A data frame with the processed FReD dataset
#' @export

load_fred_data <- function(data = get_param("FRED_DATA_FILE"), verbose = FALSE) {

  read_fred(data, verbose = verbose) %>%
    clean_variables() %>%
    add_common_effect_sizes() %>%
    align_effect_direction() %>%
    add_uncertainty() %>%
    add_replication_power() %>%
    code_replication_outcomes() %>%
    augment_for_zcurve()
}
