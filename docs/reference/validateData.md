# Check input data

Kept for compatibility: runs the checks of \[ess_data()\] on a list of
data frames in the workbook's sheet order (setup, panelists, ratings,
item data, examinee data) and reports the result as text.

## Usage

``` r
validateData(imprt_data)
```

## Arguments

- imprt_data:

  A list of data frames, or an object from \[ess_data()\].

## Value

\`"Data input is validated"\`, or the first problem found.
