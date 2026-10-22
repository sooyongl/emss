# Plot an ESS result

Plot an ESS result

## Usage

``` r
# S3 method for class 'ESS'
plot(x, type = c("profile", "impact"), unit = NULL, ...)
```

## Arguments

- x:

  An object of class \`"ESS"\`.

- type:

  \`"profile"\` for the inconsistency profile of a unit's modal result,
  or \`"impact"\` for cut scores and impact across grades.

- unit:

  For \`type = "profile"\`, the unit to plot (default: the first). For
  \`type = "impact"\`, the item subset (default \`"All"\`).

- ...:

  Not used.

## Value

A ggplot object (for \`"impact"\`, a list of ggplot objects).
