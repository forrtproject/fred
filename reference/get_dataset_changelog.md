# Get the dataset changelog

Returns the changelog of the FReD dataset as a markdown string. By
default, this is the `CHANGELOG.md` of the latest FReD version on
Zenodo, which lists all versions, including the earlier releases on OSF.

## Usage

``` r
get_dataset_changelog(changelog_file = NULL, cache = TRUE)
```

## Arguments

- changelog_file:

  URL of a markdown changelog file to return instead of the Zenodo
  changelog.

- cache:

  Should the changelog be returned from cache, if already requested
  during this session? Defaults to TRUE.
