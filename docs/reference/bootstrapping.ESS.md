# Bootstrap the modal cut scores of one unit

Resamples the unit's items with replacement, keeping each item's
location and modal aligned level, and re-estimates the cut scores in
every sample.

## Usage

``` r
bootstrapping(x, ...)

# S3 method for class 'ESS'
bootstrapping(
  x,
  unit,
  n = 100,
  seed = NULL,
  grid = c("items", "jittered"),
  ...
)
```

## Arguments

- x:

  An object of class \`"ESS"\`.

- ...:

  Former argument names, still accepted with a warning: \`gca_name\`
  (now \`unit\`) and \`boot_num\` (now \`n\`).

- unit:

  The unit to bootstrap, as named in \`extract(x, "cutscore")\`.

- n:

  Number of bootstrap samples. Default \`100\`.

- seed:

  Optional random seed, for reproducible draws.

- grid:

  Candidate cuts in each sample: \* \`"items"\` (default): the sampled
  items' locations, with the exact ESS minimum, the same estimator as
  \[emstans()\] (with \`smooth = TRUE\` in the original fit, the
  GAM-smoothed minimum). \* \`"jittered"\`: an evenly spaced grid over
  the location range (step 0.1 when the range is under 10, else 1), each
  point randomly moved by up to one step, shared by all cut levels; the
  cut is the minimum of a GAM-smoothed inconsistency curve over that
  grid. This is the original bootstrap of EmStanS 0.1.0 (the former name
  of this package).

## Value

An object of class \`"bootESS"\`: use \`print()\`, \`summary()\`,
\`extract()\`, \`report()\`.
