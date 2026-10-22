# Check prepared input data

Stops with a clear message when the data cannot be analysed: item IDs
that are not unique, SD or EC that are not numbers, ratings for items
that are not in the item data, or a panelist who did not rate every item
of the grade exactly once within a round.

## Usage

``` r
validate_ess_data(x)
```

## Arguments

- x:

  An object of class \`"ess_data"\`.

## Value

\`x\`, invisibly.
