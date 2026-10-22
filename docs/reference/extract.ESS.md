# Extract tables from an ESS result

Extract tables from an ESS result

## Usage

``` r
extract(x, ...)

# S3 method for class 'ESS'
extract(x, what = "all", ...)
```

## Arguments

- x:

  An object of class \`"ESS"\` returned by \[emstans()\].

- ...:

  Not used.

- what:

  Which results: \`"cutscore"\` (modal cut scores per unit, named by the
  level each cut opens), \`"individual"\` (panelist, modal, median and
  average cuts), \`"detailed"\` (agreement and crosstabs), \`"summary"\`
  (impact data), \`"review"\` (item review), \`"profile"\`
  (inconsistency profiles of the modal results) or \`"all"\`. Default
  \`"all"\`.

## Value

A data frame or a named list of data frames. Level columns hold the
level labels as ordered factors.
