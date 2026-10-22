# Generate a Fake Data Set for Embedded Standard Setting

\`genFakeDataSet()\` generates a simulated data set that follows the
input structure required by \`emstans()\`. The generated data can be
used to test functions in the emss package, demonstrate the expected
data format, or run examples without using operational data.

## Usage

``` r
genFakeDataSet(
  ngca,
  ntable,
  npanelist,
  cor_val,
  nlevel,
  sdinp = 1,
  ecinp = 0,
  n,
  ...,
  domains = c("Alg", "Geom", "Calc", "Prob")
)
```

## Arguments

- ngca:

  A numeric value indicating the number of grade clusters or GCAs to
  generate.

- ntable:

  A numeric value indicating the number of tables to generate within
  each GCA.

- npanelist:

  A numeric value indicating the number of panelists to generate within
  each table.

- cor_val:

  A numeric value indicating the intended correlation between item
  locations and aligned level descriptions.

- nlevel:

  A numeric value indicating the number of performance levels or aligned
  level-description categories.

- sdinp:

  A numeric value used to adjust the standard deviation of the values
  used in Weighted Embedded Standard Setting. Default is \`1\`.

- ecinp:

  A numeric value used to control essentially consistent items in the
  simulated item ratings. Default is \`0\`.

- n:

  A numeric value indicating the number of items to generate within each
  GCA.

- ...:

  Additional numeric arguments passed to \`runif()\`, typically the
  minimum and maximum score values used to generate item locations. For
  example, \`100, 300\` generates item locations between 100 and 300.

- domains:

  The content subdomains of the items, added to the item data as a
  \`Domain\` column. Either a character vector of subdomain names (for
  example \`c("Number", "Algebra", "Geometry")\`), or the number of
  subdomains, which are then named from \`"Alg"\`, \`"Geom"\`,
  \`"Calc"\`, \`"Prob"\`, \`"Stat"\`, \`"Num"\` (or \`"D1"\`, \`"D2"\`,
  ... beyond six). Items are split as evenly as possible across
  subdomains and assigned at random, so every subdomain spans the whole
  location range. \`NULL\` adds no \`Domain\` column. Must be named,
  since it follows \`...\`.

## Value

A list of simulated data frames formatted for use with \`emstans()\`.
The returned list contains test setup information, panelist information,
item ratings, item metadata, and student data.

## Details

This function is mainly intended for package testing, examples, and
demonstrations. Use the \`Domain\` column with \`emstans(by_domain =
TRUE)\` to also estimate cut scores within each subdomain. The returned
object mimics the data structure expected by \`emstans()\`, allowing
users to inspect the required format before preparing their own data.

## Examples

``` r
if (FALSE) { # \dontrun{
fake_data <- genFakeDataSet(
  ngca = 3,
  ntable = 5,
  npanelist = 5,
  cor_val = 0.2,
  nlevel = 3,
  sdinp = 1,
  ecinp = 0,
  n = 30,
  100,
  300
)

# Three named subdomains
fake_data <- genFakeDataSet(ngca = 2, ntable = 2, npanelist = 4,
                            cor_val = 0.6, nlevel = 3, sdinp = 1, ecinp = 0,
                            n = 45, 300, 500,
                            domains = c("Number", "Algebra", "Geometry"))
table(fake_data$item_data$GCA, fake_data$item_data$Domain)

# Four subdomains with default names (domains = 4), or none (domains = NULL)
} # }
```
