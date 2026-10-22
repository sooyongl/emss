# Modal ALD for every item at once

Vectorized equivalent of \`apply(ald, 1, get_mode, cors)\`. For each row
(item) the most frequent ALD across columns (panelists) wins; when
several ALDs tie for most frequent, the one chosen by the panelist with
the highest correlation wins (stable order, so earlier panelists win
equal correlations), exactly as in \`get_mode()\`.

## Usage

``` r
row_modes(ald, cors)
```

## Arguments

- ald:

  items x panelists table of ALD labels.

- cors:

  panelist correlations, one per column of \`ald\`.

## Value

character vector of modal ALDs, one per row.
