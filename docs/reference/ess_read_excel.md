# Read an emss Excel workbook

Reads the five-sheet emss workbook (Setup, Panelists, Ratings,
ItemMetaData, ExamineeMetaData) by sheet name, falling back to sheet
position when a name is not found, and returns \[ess_data()\].

## Usage

``` r
ess_read_excel(
  path,
  sheets = NULL,
  levels = NULL,
  sd = NULL,
  ec = NULL,
  columns = NULL,
  quiet = TRUE
)
```

## Arguments

- path:

  Path to the \`.xlsx\` file.

- sheets:

  Optional sheet names, a named character vector with any of \`setup\`,
  \`ratings\`, \`items\`, \`examinees\`.

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
