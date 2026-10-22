# Prepare Embedded Standard Setting input data

Builds the input for \[emstans()\] from data frames or files, in any
column layout. Ratings and items are required; setup information (level
labels, SD, EC) can come from a setup table, from arguments, or both,
with arguments taking precedence.

## Usage

``` r
ess_data(
  ratings,
  items,
  setup = NULL,
  examinees = NULL,
  levels = NULL,
  sd = NULL,
  ec = NULL,
  columns = NULL,
  quiet = TRUE
)
```

## Arguments

- ratings:

  Panelists' item alignments: a data frame or a path to a \`.csv\` file.
  One row per panelist, item and round.

- items:

  Item information, including the location column(s) such as \`RP67\`: a
  data frame or a path to a \`.csv\` file.

- setup:

  Optional setup table (one row per grade), a data frame or a path to a
  \`.csv\` file. Its \`Level Options\` column lists the level labels
  from lowest to highest, separated by commas.

- examinees:

  Optional score frequencies for impact data: a data frame or a path to
  a \`.csv\` file in long format, one row per grade and score, with
  three columns: grade (\`GCA\` or \`Grade\`), \`Score\` (on the same
  scale as the item locations) and \`Freq\` (number of examinees with
  that score). Wide tables with repeated Score/Freq/Grade blocks side by
  side are not accepted.

- levels:

  Level labels from lowest to highest. A character vector applies to
  every grade; a named list gives labels per grade, for example
  \`list(R1 = c("E", "M", "L"), R2 = c("B", "P", "A"))\`. Overrides the
  setup table.

- sd, ec:

  Scaling SD and essential-consistency threshold. A single number
  applies to every grade; a named vector gives values per grade.
  Override the setup table. Defaults are SD = 1 and EC = 0.

- columns:

  Optional column mapping for tables whose column names differ from the
  defaults. A named list per table, each a named list or vector of input
  column names or positions, for example \`list(ratings = list(item_id =
  "ItemID", level = 8))\`. The standard names are: ratings \`grade\`,
  \`round\`, \`table\`, \`panelist\`, \`item_id\`, \`level\`; items
  \`grade\`, \`item_id\`, \`domain\`, \`order\`; setup \`grade\`,
  \`levels\`, \`sd\`, \`ec\`; examinees \`grade\`, \`score\`, \`freq\`.

- quiet:

  If \`FALSE\`, report the column mapping that was used.

## Value

An object of class \`"ess_data"\`.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- ess_data(
  ratings = "ratings.csv",
  items   = "items.csv",
  levels  = c("Basic", "Intermediate", "Advanced"),
  columns = list(ratings = list(panelist = "Rater", level = "Judgment"))
)
res <- emstans(data = d, loc = "RP67")
} # }
```
