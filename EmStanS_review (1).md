# EmStanS code review and streamlining plan

Reviewed: EmStanS 0.1.0 (zip of 2026-10-20, last commit f0695c5 "Update site").
Tested on R 4.3.3 / Linux against all 25 files in `prerun/data/example_file`, with both ESS-Weight and ESS-Count.

## Baseline

- The package does not install on Linux or macOS. `Collate:` in DESCRIPTION spells three files with the wrong case (`main_function.r`, `report_function.r`, `summary_function.r`; git tracks them as `.R`). It works on Windows because that file system ignores case.
- Correction to an earlier version of this review: `4_excel_out.r`, `z_boot_excel.r`, and `z_test.r` are in the repository; they were only missing from the zip. `z_boot_excel.r` has live code (`boot_excel()`, `boot_exceldown()`). The other two are entirely commented out and could be deleted. The baseline results below were produced without `z_boot_excel.r`, which does not affect cut score estimation.
- With `Collate` removed, 18 of 25 example files run. Runtimes reach 45 s per file.
- 6 files fail: examinee data in the wide layout (repeated Score/Freq/Grade blocks). `reorganize_examinee()` handles that layout but its call is commented out in `dataReady_examineedata()`.
- 1 file (`NO_Math`) fails under ESS-Count only: grade M11 ends up with zero rows in the item-review table, and `tab4_output()` then indexes past the end of `group_split()`.

## The Count / Weight computation

`essCW()` is correct. It matches the published definition: for cut level l and candidate c, an item aligned at or above l is inconsistent when its location is below c, an item aligned below l is inconsistent when its location is at or above c, and Distance is |location - c|.

The expanded data.table approach builds n_items x n_candidates rows per cut level. A sort-and-cumulative-sum formulation gives the same answer with one binary search per candidate:

- count_up(c) = #{upper items with loc < c}, weight_up(c) = c * count_up - sum of those locs
- count_lo(c) = #{lower items with loc >= c}, weight_lo(c) = sum of those locs - c * count_lo

Checked against `essCW()` on 723 random cases (2 to 5 levels, 5 to 300 items, heavy ties): maximum difference 1e-12. Speed for one cut level: 31x faster at 100 items, 145x at 500, about 3,700x at 2,000. See `ess_fast.R`.

But `essCW()` is only about 6% of total runtime (profile of the aasr file, 20 s). About 60% is `estCutScore_mode()`, which builds modal ALDs by calling `get_mode()` once per item through `apply()`, and each call builds and sorts a data.frame. Rendering HTML widgets inside `emstans()` (`tab2_output()` etc.) is another 10% or more. Real end-to-end speed-up needs those two fixed as well.

## Correctness issues

1. **Rounding before the minimum.** `estCutScore()` divides weights by SD and rounds the profile to 2 decimals before finding the minimum. Across the example files, 35 of 342 panelist-level cut estimates change because of this. Worst when locations are on a theta scale but SD is on a scale-score metric (CAAELP: SD = 18.8, so weights collapse to ties).
2. **`update.ESS()` assigns manual cuts by position, not by name.** `update(res, "M3-All" = ..., "M1-All" = ...)` puts the M3 values on M1. Names that don't match any grade are silently ignored.
3. **Item-review Distance differs from estimation Distance.** In `gen_tab4()`, items aligned too low use `loc - (Cut_Score_upper - 1)`. The `- 1` assumes integer scale scores; on a theta scale it is off by a full unit. Estimation uses |loc - c|.
4. **Bootstrapping is not reproducible and mixes grids.** `boot_runESS()` uses `empirical = FALSE`, which builds a grid with `runif()` jitter (no seed), draws a different jittered grid for each cut level, then keeps only the first level's grid when it binds the columns. The bootstrap cut is also the minimum of a GAM fit rather than the ESS minimum, so it is not the same estimator as the point estimate.
5. **`emstans()` ignores three of its arguments.** `targets` (hard-coded `"ALD"` downstream), `median`, and `digits` are stored but never used.
6. **Locations are attached by row position** in `runESS()` (`bind_cols(location = location[[3]])`) rather than joined by `Item_ID`. Safe in every example file because every panelist rated every item, but fragile.
7. **Unclear errors for bad input.** A `loc` that isn't a column gives "object 'RP67' not found". A non-numeric SD column is silently coerced to NA.

## Structure and maintainability

- Internal names mirror the old Shiny tabs (`tab0` to `tab4`, `for_tab2_out`, `t1_1_1`) rather than ESS concepts.
- Computation and display are interleaved: `emstans()` builds DT tables, plotly widgets, and kable HTML for every grade, so the result object is large and slow to create.
- Repeated near-copies: `estCutScore()` / `estCutScore_manual()`, `tab1_group_out()` / `tab1_group_out_all()`, `ready_median_table()` / `ready_average_table()`, `runESS()` / `boot_runESS()`, and two near-identical 50-line branches in `get_data_info()`.
- 19 `eval(parse(text = ...))` calls and many lookups by regex on column names (`"_W"`, `"_p"`, `"_loc"`) or by `get_which(...) + 1` position.
- `foreach %do%` used 24 times as a plain loop; it can be dropped as a dependency.
- 24 packages in Imports, about a third of them display-only (bslib, DT, plotly, kableExtra, ggrepel, htmltools, psych).
- S3 signatures don't match their generics (`update.ESS(output, ...)`, `summary.ESS(x)`, `print.bootESS(x)`, `report.ESS(x, what)` with no `...`), which R CMD check flags. `extract()` masks `tidyr::extract()` and `magrittr::extract()`.
- No `tests/` directory.
- `inst/dev/zold_*.r` ships old code inside the installed package.

## Data in the repository

The 25 `prerun/data/example_file/*.xlsx` files are tracked in git, and the remote is github.com/sooyongl/EmStanS. They look like real state and client study inputs (CA, Maine, ELPA21, NRAS). They are excluded from the built package by `.Rbuildignore`, but not from the repository. If the repository is public, they are in its history, and removing them from the current tree does not remove them from history.

## Proposed plan

Stage 1, no change in results (except the bug fixes listed):
- Fix DESCRIPTION so the package installs everywhere; drop `Collate` and rely on `@include`.
- Restore wide examinee layout support; fix the M11 item-review crash; fix `update.ESS()` name matching.
- Replace `essCW()` with the cumulative-sum version and vectorize the modal-ALD step.
- Validate inputs up front with clear messages (location column exists and is numeric, SD/EC numeric, every panelist rated every item).
- Add a testthat suite: the Count/Weight equivalence test, a hand-computed toy example, and a snapshot of cut scores for the 18 currently working files so later refactors are checked against today's results.

Stage 2, restructure without changing results:
- Split the pipeline into named steps: `ess_prepare()` -> `ess_panelist_cuts()` -> `ess_aggregate()` -> `ess_diagnostics()` -> `ess_item_review()`.
- `emstans()` returns plain data frames; rendering moves into `report()` and `plot()` methods, and display packages move to Suggests where practical.
- Merge the duplicated functions; remove `eval(parse())`, `foreach`, and positional column lookups.

Stage 3, decisions that change results or the API (needs your call, and probably Dan Lewis's):
- Rounding before the minimum (issue 1).
- Item-review Distance definition (issue 3).
- Bootstrap estimator and seeding (issue 4).
- Argument names, for example `method = c("weight", "count")` instead of `WESS = TRUE`, and `aggregate = c("modal", "median", "mean")` actually wired up, with the old names kept as deprecated aliases.

## Stage 1 status (done, 2026-10-20)

Branch `stage1`, 3 commits on top of f0695c5; not pushed. Details in the branch's NEWS.md.

- Installs on Linux/macOS. 48 of 50 example runs succeed (was 37). The two remaining failures are the Maine RP60 file, where a rating refers to item `R5_VR702686`, which is not in that file's item sheet.
- Exact minimums adopted. 30 of 634 grade-level cuts changed, in 3 files (CAAELP 26, Sooyong 2, aasr 2). A control build that rounds to 2 decimals reproduces the baseline, apart from one cut that sat exactly on a rounding boundary, so every result change comes from the exact-minimum decision.
- essCW and modal ALD now take ~0% of runtime. End to end ~2x faster on most files. What remains: GAM smoothing run even when `gamest = FALSE` (~24% on CAAELP) and widget rendering inside `emstans()` (~17%), both Stage 2.
- R CMD check: 5 warnings / 2 notes, down from 6 / 2, with none new. `z_boot_excel.r` calls openxlsx without declaring it (pre-existing).
- testthat suite added (17 tests, synthetic data only).

## Stage 2 status (done, 2026-10-20)

Branch `stage2` (on top of `stage1`), 8 commits; not pushed. Details in the branch's NEWS.md.

Decisions taken: rounds analysed separately (grade x round); setup table optional with `levels`/`sd`/`ec` arguments overriding it; level labels may differ by grade; labels matched exactly (white space removed, as before); internal object layout may change.

- **New input layer:** `ess_data()` builds standard tables (ratings, items, levels, grades, examinees) from data frames or CSV, with column mapping by name or position; `ess_read_excel()` reads sheets by name. Validation runs once, on the standard tables.
- **New core:** `ess_units()`, `ess_fit()`, `ess_estimate()`, `ess_agreement()`, `ess_item_review()`, `ess_impact()` return tidy tables with integer levels; labels are attached only for display.
- **Equivalence:** reference snapshots of every computed table from the old pipeline (10 runs: aasr Weight/Count/GAM, multi-panelist and multi-table files). The new core reproduces all of them to stored precision, apart from the old code's intermediate rounding.
- **Two more bugs fixed in the old code first,** so the reference reflects correct behaviour:
  - `cal_minp()` mixed tied candidates across levels via `matrix(byrow = TRUE)`, so a level could get another level's cut. 57 of 840 cuts changed (48 Count, 9 Weight). Old code + this fix = new cuts on all 840.
  - `crosstab_ready()` had the same level-numbering bug as the Stage 1 item-review fix.
- **"EC Delta" is now recognized:** 19 of 25 example files name the EC column "EC Delta", which the old code found with a loose match.
- **Speed:** every example study runs in about a second or less including report (was up to 47 s). Code: 2,280 lines (was 6,681). Dependencies dropped: foreach, ggrepel, grid, kableExtra, tidyr, tidyselect, magrittr.
- **Tests:** 27 tests / 1,193 expectations, adding CSV mapping, labels, labels by grade, rounds and per-level tie-breaking.
- **R CMD check:** 1 WARNING (build machine locale), 3 NOTEs (LICENSE file, NEWS heading, vignette size).

## Stage 3: decisions that change results or the API

1. **Item-review distance:** "location − (upper cut − 1)" assumes integer scale scores and is a full unit off on theta scales. The estimation distance is |location − cut|. The EC-adjusted crosstab uses "distance < EC" while item status uses "≤ EC".
2. **Operational level by row position:** items are assigned by position relative to the cut item, not by location ≥ cut (the estimation rule). The two differ only when items share the cut location.
3. **Bootstrap:** uses a jittered grid (now seedable) and a GAM-smoothed minimum rather than the ESS minimum, and binds levels 3+ onto level 2's grid. It is not the same estimator as the point estimate.
4. **Argument names:** for example `method = c("weight", "count")` for `WESS`; `median` is unused (all of modal/median/average are reported); `domain`/`select_domain` could become `by_domain`. Old names would be kept as deprecated aliases.
5. **Smaller items:** `psych` used only for weighted kappa; NEWS heading and version number; vignette size (embedded widgets); the Maine RP60 file has a rating for an item not in its item sheet.

## Stage 3 status (done, 2026-10-20)

Branch `stage3` (on top of `stage2`), 1 commit; not pushed. 13 commits on top of f0695c5 in all.

Decisions: item-review distance kept as is; empirical level by location; new argument names with old ones as deprecated aliases; bootstrap uses the exact ESS minimum by default, with the jittered grid kept as an option.

- **Empirical level:** 1 + number of cut scores ≤ the item's location. Cut scores unchanged on all 48 example runs; empirical levels changed for 58 items, all in the 19 unit results where two cuts fall on one item.
- **Arguments:** `emstans(file, data, ..., grades, location, method, smooth, by_domain, domains, font_size, digits)`. Arguments after `data` must be named in full. Under the old signature, `domain` was partially matched to `domains`, which would silently have disabled domain units.
- **Bootstrap:** `bootstrapping(x, unit, n = 100, seed, grid = c("items", "jittered"))`. The jittered option now uses one grid per sample for all levels.
- **Tests:** 34 tests / 1,207 expectations. Both prerun scripts run clean (9 checks).
- **R CMD check:** 1 WARNING (build machine locale), 2 NOTEs (vignette size, LICENSE file).

## Before pushing

- Rebuild the pkgdown site (`docs/` still shows the old API).
- Set the version (suggest 0.2.0) and the NEWS heading.
- Decide on the 25 client data files in `prerun/data/`, which are tracked in git history.
- Optional: smaller vignettes; delete the old scratch scripts in `prerun/` (`main_function.R`, `test_bootstrapping.R`, `test_manual_cs.R`, ...), which use the old internals.
