# emss 0.1.0.9000

* The package is renamed from EmStanS to emss. Use `library(emss)` and
  `emss::`; function names, including `emstans()`, are unchanged.

## Stage 3: method decisions and argument names

### Changes to results

* An item's empirical (operational) level is now 1 + the number of cut
  scores at or below its location, the rule the estimation itself uses.
  Previously it was assigned by the item's position relative to the cut
  items, and two cuts on the same item were merged into one, so the items at
  and above them were placed one level too low. Cut scores are unchanged;
  agreement, crosstabs and item review change only when two cuts fall on the
  same item: in the example studies, 58 items in 19 grade results (mostly
  CAAELP and ESS-Count).
* `bootstrapping()` now resamples items and takes the exact ESS minimum in
  every sample, the same estimator as `emstans()`. The earlier bootstrap
  (a jittered grid of candidate cuts with a GAM-smoothed minimum) remains
  available with `grid = "jittered"`; it now uses one grid per sample for
  all cut levels (each level previously drew its own grid but was reported
  on level 2's grid points).
* The item-review distance is unchanged ("location - (upper cut - 1)").

### New argument names

* `emstans(file, data, ..., grades, location, method, smooth, by_domain,
  domains, font_size, digits)`. Former names still work, with one warning
  listing the replacements: `filePath` -> `file`, `grade` -> `grades`,
  `loc` -> `location`, `WESS = TRUE/FALSE` -> `method = "weight"/"count"`,
  `gamest` -> `smooth`, `domain`/`select_domain` -> `by_domain`/`domains`,
  `targets` -> `ess_data(columns = ...)`, and `median` (not needed).
  Arguments after `data` must be named in full, so a former name is never
  partially matched to a new one (`domain` would otherwise have been taken
  as `domains`). Misspelled arguments are an error.
* `by_domain = TRUE` always includes the all-items unit of each grade.
* `bootstrapping(x, unit, n = 100, seed = NULL, grid = c("items", "jittered"))`;
  `gca_name` and `boot_num` still work, with a warning.
* Dependencies dropped: glue, purrr.

## Stage 2: new structure

### New input options

* `ess_data()` builds the input from data frames or CSV files with any
  column names: columns are mapped by name or position with `columns =`.
  Only ratings and items are required; level labels, SD and EC can come
  from a setup table, from the `levels`, `sd` and `ec` arguments, or both
  (arguments win). `ess_read_excel()` reads the emss workbook by sheet
  name. `emstans(data = )` accepts either, or the old list of data frames.
* Level labels are carried through to every table, plot and report, in
  level order, and may differ by grade.
* Ratings with several rounds are analysed separately by grade and round
  (units such as `R1-All Round 2`).
* `plot()` draws the inconsistency profile of a unit's modal result, or cut
  scores and impact across grades.

### Changes to results

* When a cut level had tied minima, the old `cal_minp()` mixed its
  candidates with other levels' (a `matrix(byrow = TRUE)` on column-ordered
  data). A level could then get another level's cut, which need not be a
  minimum for it at all: in one study, a level-3 cut was placed where 48
  items were inconsistent instead of at the unique minimum with 21. The tie
  rule (lower weight, then lower count, then earlier item) is now applied to
  each level separately. Ties are common under ESS-Count, whose counts are
  integers: across the example studies 57 of 840 grade-level cuts changed,
  48 under Count and 9 under Weight. The bug predates Stage 1. Running the
  old code with only this fix gives exactly the new cut scores.
* The EC-adjusted crosstab took level numbers from the levels present in a
  grade (the same bug fixed in the item review in Stage 1).
* Averages are rounded once from exact values; the old code rounded to 4
  decimals first, which moved one average cut in the Maine study by 0.01.

### Changes to the result object

* `emstans()` returns plain tables (panelist, modal, median and average
  cuts; agreement; item review; impact) plus the input data and settings.
  Code that read internal elements such as `res$tab1` must use `extract()`.
* `extract()` returns long tables with level labels; `extract(res,
  "cutscore")` names each cut by the level it opens (for example `M`, `L`).
* `bootstrapping()` gains a `seed` argument and reports the underlying error
  when every sample fails. Its results are tables from `extract()`; the
  Excel export (`boot_excel()`) was removed.

### Performance and size

* No HTML is built when `emstans()` runs; `report()` builds it on demand.
  Every example study now runs in about a second or less, including the
  report (previously up to 47 seconds).
* About 4,400 lines of code removed. Dependencies dropped: foreach, ggrepel,
  grid, kableExtra, tidyr, tidyselect, magrittr; knitr moved to Suggests.

## Stage 1


### Changes to results

* Cut scores are now taken from the exact minimum of the ESS inconsistency
  profile. Previously the profile (weights divided by SD) was rounded to two
  decimals before the minimum was found, which created false ties and could
  pick a different cut. Across the example studies this changed 30 of 634
  grade-level cuts, mostly on theta-scale locations with a scale-score SD,
  where the rounding merged real differences. Values are still rounded to
  nine decimals before comparison, which removes floating-point noise only,
  so ties in the exact profile are still broken by the lower count, then the
  lower cut.

### Bug fixes

* The package installs on case-sensitive file systems (macOS, Linux): the
  stale `Collate` field in DESCRIPTION has been removed.
* Examinee data in the wide layout (repeated Score/Freq/Grade blocks) is
  read again.
* The item review no longer drops a grade that has no inconsistent items,
  which made `emstans()` fail for such grades.
* The item review takes level numbers from the level labels, so grades in
  which no item is aligned to some level are compared at the right level.
* Levels numbered 10 and above are parsed correctly.
* `update()` matches manual cut scores to grades by name, so argument order
  no longer matters; unknown grade names and wrong numbers of cuts are
  errors. Its first argument is now `object`, matching the generic.
* Item locations are attached to ratings by `Item_ID` rather than by row
  position.

### Input checks

* `emstans()` now stops with a clear message when the location column is
  missing, non-numeric or incomplete, when item IDs are duplicated, when SD
  or EC are not numbers, when an ALD label is not one of the grade's Level
  Options (previously it became `LevelNA`), or when a panelist did not rate
  every item in the grade exactly once.

### Performance

* ESS-Count and ESS-Weight profiles are computed with sorted cumulative sums
  instead of an items x candidates table (identical results; 30x to 3,700x
  faster for that step, depending on test length).
* Modal ALDs across panelists are computed for all items at once (identical
  results; about 1,000x faster for that step).
* End to end, typical studies run about 2x faster.

### Tests

* Added a testthat suite: hand-computed and brute-force checks of the
  Count/Weight profiles and the selection rule, equivalence of the modal ALD
  rule, input validation, `update()` by name, and a snapshot of cut scores
  on simulated data.
