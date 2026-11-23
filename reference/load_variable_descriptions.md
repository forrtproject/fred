# Load variable descriptions

Reads the names and descriptions of the variables in FReD from the
codebook (`fred_codebook.csv`) of the FReD release on Zenodo, and adds
the main variables that the package derives from them. This can be used
for subsetting and describing the dataset.

## Usage

``` r
load_variable_descriptions(codebook_url = NULL)
```

## Arguments

- codebook_url:

  URL of the codebook (.csv with columns `Variable` and `Description`).
  Defaults to the Zenodo codebook described above.

## Value

A data frame with variable names (`Variable`) and descriptions
(`Description`)

## Details

The codebook comes from the same Zenodo version as the data if
`FRED_DATA_URL` points to a file in a Zenodo record, and from the latest
version otherwise.
