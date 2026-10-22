# Set cut scores manually

Replaces the modal (all tables) cut scores of one or more units and
recomputes agreement, item review and impact for them. Each cut's page
is the first item at or above the cut score.

## Usage

``` r
# S3 method for class 'ESS'
update(object, ...)
```

## Arguments

- object:

  An object of class \`"ESS"\`.

- ...:

  Named numeric vectors of cut scores, one value per cut, named by unit
  as in \`extract(object, "cutscore")\`, for example “ update(res,
  \`R1-All\` = c(-0.9, 0.4)) “.

## Value

An updated object of class \`"ESS"\`.
