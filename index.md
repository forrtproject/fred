# FReD

The goal of the FReD package is to provide various interfaces to the
FORRT Replication Database. The package includes a shiny app - the FReD
Explorer - as well as functions to directly access and analyse the
dataset.

## FReD Explorer

The FReD Explorer allows users to explore the dataset, and to assess
replicability and effect sizes of the included studies based on various
filters. It primarily serves as a gateway to meta-science. You can
access the FReD Explorer
[online](https://forrt-replications.shinyapps.io/fred_explorer/) or by
calling
[`run_explorer()`](https://forrt.org/fred/reference/run_explorer.md) in
your R console after you have loaded the package.

Outcome labels depend on the success criterion or, for Reported Success,
on the replication authors’ own description. A failed replication does
not by itself show that the original finding was false. Replications
also fail by chance, through low power or flaws in their own conduct, or
because samples, settings or materials differed.
[`vignette("success_criteria")`](https://forrt.org/fred/articles/success_criteria.md)
explains how the criteria differ.

## FReD Annotator

The FReD Annotator allows users to annotate lists of articles
(e.g. reading lists for classes, or reference lists for draft articles)
with any replication findings. This is intended to make it easier to use
replications in teaching and research, and to thus ensure that claims
are based on the best available evidence. You can access the FReD
Annotator
[online](https://forrt-replications.shinyapps.io/fred_annotator/).

## Installation

You can install the development version of FReD like so:

``` r

devtools::install_github("forrtproject/FReD")
```

## Launching the apps

``` r

library(FReD)
run_explorer()
```

## Accessing the data

To load the dataset and prepare it for analyses, you can use the
following code:

``` r

library(FReD)
load_fred_data()
```
