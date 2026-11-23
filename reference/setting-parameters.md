# Setting Parameters for the FReD Package

Generally, the FReD package is designed to work with the latest FReD
dataset. However, you might want to use an older version, or even your
own. For such advanced use, you can set parameters by setting
environment variables manually or in your `.Renviron` file. The
following environment variables can be set (before loading the package):

## Details

- `FRED_DATA_URL`: The URL of the FReD dataset, needs to return a .csv
  or .xlsx file. If not set, FReD downloads `fred.csv` from the latest
  version of the FReD release on Zenodo
  (<https://doi.org/10.5281/zenodo.24461606>). To use an earlier
  version, set it to that version's file, e.g.,
  `https://zenodo.org/api/records/24462293/files/fred.csv/content` for
  version 2026.11.1. For versions before 2026.11.2, use the .csv rather
  than the .xlsx file: openxlsx does not decode the escaped characters
  in their text cells (e.g., `&` is read as `&amp;`).

- `FRED_DATA_FILE`: The path to the .csv or .xlsx file, if you have
  downloaded it already. If the file exists, it will be used -
  otherwise, the data will be downloaded from `FRED_DATA_URL`.

- `RETRACTIONWATCH_DATA_FILE`: The path to the RetractionWatch database,
  if you have downloaded it already (or want it to be saved to a
  particular location). If the file exists, it will be used - otherwise,
  the file will be downloaded and saved there.

- `RETRACTIONWATCH_URL`: The URL to download the RetractionWatch
  database. Needs to return the .csv file.

- `FRED_OFFLINE`: Should FReD work offline (TRUE) or online (FALSE). If
  TRUE, FReD will not download the latest data every time it is loaded.
  Defaults to TRUE.

- `FRED_SUPPRESS_STARTUP_MENU`: Should the interactive menu checking for
  data updates be suppressed (TRUE) or shown (FALSE). If TRUE, all
  startup messages and interactive prompts related to offline data
  updates will be suppressed. Defaults to FALSE.

## Examples

``` r
## Not run:
Sys.setenv(FRED_DATA_URL = "https://zenodo.org/api/records/24462293/files/fred.csv/content")
## End(Not run)
```
