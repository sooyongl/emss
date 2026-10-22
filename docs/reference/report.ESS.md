# HTML report of an ESS result

Builds an HTML page (shown in the RStudio viewer or a browser) with the
results of \[emstans()\]. Level labels from the input are used
throughout.

## Usage

``` r
report(x, ...)

# S3 method for class 'ESS'
report(x, what = "all", ...)
```

## Arguments

- x:

  An object of class \`"ESS"\`.

- ...:

  Not used.

- what:

  Section(s) to show: \`"all"\`, \`"individual"\` (panelist and group
  cut scores), \`"detailed"\` (per-unit profile plot, crosstabs and
  agreement), \`"summary"\` (impact data), \`"review"\` (item review) or
  \`"setup"\` (input data overview). Default \`"all"\`.

## Value

A browsable HTML object.
