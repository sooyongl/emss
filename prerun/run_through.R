# =============================================================================
# emss run-through: every step of the pipeline, one function at a time
# =============================================================================
#
# Run from the package root (the folder with DESCRIPTION), section by section.
# pkgload::load_all() makes internal functions available without ":::".
#
# Pipeline:
#   input   ess_read_excel() / ess_data()  -> standard tables (+ validation)
#   units   ess_units()                    -> one unit per grade x round x subset
#   fit     ess_profile(), select_cut_rows(), operational_levels(), ess_fit()
#   group   unit_ratings(), row_modes(), ess_estimate(), aggregate_cuts()
#   check   ess_agreement(), ess_item_review(), ess_impact()
#   object  ess_compute()                  -> class "ESS" (what emstans() returns)

pkgload::load_all(".")

xlsx <- "prerun/data/example_file/aasr_emstans_file_19October2024 (2).xlsx"
loc  <- "RP67"

# 1. Input ---------------------------------------------------------------------

## 1a. Read the workbook by sheet name into the standard tables
d <- ess_read_excel(xlsx, quiet = FALSE)   # quiet = FALSE shows the column mapping
d                                          # print.ess_data(): overview
names(d)                                   # ratings, items, levels, grades, examinees, columns
head(d$ratings)                            # level = integer rank; label = as typed
d$levels                                   # E < M < L for every grade
d$grades                                   # SD and EC per grade ("EC Delta" column)
head(d$items[, c("grade", "item_id", "domain", "order", "RP67")])

## 1b. Helpers used while reading
.ess_norm(c("Level Options", "level_options", "LEVEL.OPTIONS"))   # column-name matching
.ess_squish(" Very  Low ")                                        # label matching
.ess_stack_examinees(data.frame(Score...1 = 300, Freq...2 = 2, Grade...3 = "R1",
                                Score...4 = 310, Freq...5 = 1, Grade...6 = "R2",
                                check.names = FALSE))             # wide -> long

## 1c. Validation: each of these stops with a message saying what to fix
bad <- d$ratings[-1, ]                                   # one rating missing
try(ess_data(bad, d$items, levels = c("E", "M", "L"),
             columns = list(ratings = list(level = "label"))))
bad <- d$ratings; bad$label[1] <- "X"                    # unknown label
try(ess_data(bad[c("grade", "round", "table", "panelist", "item_id", "label")],
             d$items, levels = c("E", "M", "L"),
             columns = list(ratings = list(level = "label"))))
try(ess_units(d, "RP99"))                                # location column not found

# 2. Units -------------------------------------------------------------------------

units <- ess_units(d, loc)
names(units)                               # "R0-All" ... "R6-All"
u <- units[["R1-All"]]
str(u[c("grade", "subset", "round", "n_levels", "sd", "ec")])
head(data.frame(item_id = u$item_id, location = u$location))   # items in location order

## Domain subsets as extra units
names(ess_units(d, loc, by_domain = TRUE, domains = "VOC", grades = "R1"))

# 3. One panelist's fit, step by step ---------------------------------------------

ur <- unit_ratings(d, u)                   # items x panelists matrix of levels
dim(ur$levels); ur$panelist; ur$table
lev <- ur$levels[, 1]

## 3a. Count/Weight for one cut level at every candidate cut
cw <- essCW(data.frame(location = u$location, ALD = lev), cut_level = 2)$incon_default
head(cw)                                   # cut_score, counts, weights

## 3b. All cut levels together, weights divided by SD, noise stripped
prof <- ess_profile(u$location, lev, u$n_levels, u$sd)
head(prof)                                 # L2_C, L3_C, L2_W, L3_W

## 3c. Pick the cut row per level: minimum, then lower weight, count, earlier row
rows_w <- select_cut_rows(prof, "weight"); rows_w
rows_c <- select_cut_rows(prof, "count");  rows_c
u$location[rows_w]                         # the cut scores

## 3d. Empirical (operational) level of every item: 1 + number of cuts at or below it
table(operational_levels(u$location, u$location[rows_w]))
operational_levels(1:6, c(3, 3))           # two cuts on one item: level 2 is empty

## 3e. Everything above in one call
fit <- ess_fit(u$location, lev, u$n_levels, u$sd, method = "weight")
fit$cuts                                   # level, page, cut_score, count, weight
fit$correlation

## 3f. GAM-smoothed minimum (used when smooth = TRUE)
g <- gam_minimum(u$location, prof$L2_W)
g[c("score", "value", "row")]

## 3g. Floating-point noise and exact ties
exact_profile(data.frame(w = c(0.3, 0.1 + 0.2)))   # both 0.3 after stripping noise

# 4. Across panelists ------------------------------------------------------------------

## 4a. Modal level per item; ties go to the highest-correlation panelist
toy <- matrix(c(1, 2, 3,  2, 2, 1), nrow = 2, byrow = TRUE)
row_modes(toy, cors = c(0.1, 0.5, 0.9))   # item 1: 3-way tie -> 3; item 2: 2

## 4b. Every panelist and every modal group in every unit
est <- ess_estimate(d, units, method = "weight")
names(est)
head(est$panelist)
est$modal[est$modal$table == "All", ]
head(est$modal_items)

## 4c. Median and average of panelist cuts (per table and "All")
head(aggregate_cuts(est$panelist, units, "median"))
head(aggregate_cuts(est$panelist, units, "average"))

# 5. Diagnostics of the modal result ---------------------------------------------------

ag <- ess_agreement(est, units)
ag$summary                                 # exact and EC-adjusted agreement, weighted kappa
head(ag$crosstab)

rv <- ess_item_review(est, units)
table(rv$unit, rv$status)
head(rv[rv$status != "Consistent", ])

## Distance rule for one item (see Stage 3 notes on the "- 1")
item_distance(aligned = 3, operational = 2, location = 0.5, cuts = c(-0.9, 0.36))

im <- ess_impact(est, units, d)
im[im$unit == "R1-All", ]                  # pages per level; % columns NA (no examinees)

# 6. The ESS object ---------------------------------------------------------------------

settings <- list(location = loc, method = "weight", smooth = FALSE, by_domain = FALSE,
                 domains = NULL, font_size = 14, digits = 3, source = xlsx)
res <- ess_compute(d, units, settings)
class(res); names(res)
res                                        # same object emstans() returns

## Labels are attached only for display
level_label(res, grade = "R1", level = 2:3)
modal_cut_scores(res)[1:2]
str(ess_tables(res), max.level = 1)
head(labelled_profile(res, "R1-All"))

## Manual cuts go through the same computation
res_m <- ess_compute(d, units, settings, manual = list(`R1-All` = c(-0.9, 0.4)))
modal_cut_scores(res_m)[["R1-All"]]

# 7. Bootstrap internals -------------------------------------------------------------------

mi <- res$modal_items[res$modal_items$unit == "R3-All", ]
set.seed(1)
boot_sample_cuts(mi$location, mi$aligned, n_levels = 3, sd = 1, method = "weight", grid = "items")
boot_sample_cuts(mi$location, mi$aligned, n_levels = 3, sd = 1, method = "weight", grid = "jittered")

# 8. Former argument names -----------------------------------------------------------------

emstans_old_args(list(filePath = xlsx, loc = "RP67", WESS = FALSE, domain = "Domain"))
