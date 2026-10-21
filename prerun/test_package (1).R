# =============================================================================
# emss overall test: the functions a user calls
# =============================================================================
#
# Run from the package root, section by section. Uses the installed package
# (devtools::install() first), so only exported functions are used here.
# For the step-by-step internals see prerun/run_through.R.
#
# Each section prints its result; check() lines stop with a message if
# something is wrong.

library(emss)

check <- function(ok, what) {
  if (!isTRUE(ok)) stop("CHECK FAILED: ", what, call. = FALSE)
  message("ok: ", what)
}

xlsx <- "prerun/data/example_file/aasr_emstans_file_19October2024 (2).xlsx"
out_dir <- file.path(tempdir(), "emss_review")
dir.create(out_dir, showWarnings = FALSE)

# 1. Main function and print() --------------------------------------------------

t0 <- Sys.time()
res <- emstans(xlsx, location = "RP67", method = "weight")
Sys.time() - t0                       # about a second

res                                   # print(): settings and modal cut scores
print(res)                            # same thing, explicitly
summary(res)                          # modal / median / average + agreement per cut

check(inherits(res, "ESS"), "emstans() returns an ESS object")
check(length(extract(res, "cutscore")) == 7, "7 grades (R0-R6)")
check(all(vapply(extract(res, "cutscore"), function(x) identical(names(x), c("M", "L")), TRUE)),
      "cuts are labelled M and L")

## Count method and GAM smoothing
res_c <- emstans(xlsx, location = "RP67", method = "count")
res_c
res_g <- emstans(xlsx, location = "RP67", smooth = TRUE)
res_g

## Subsets of grades and domain units
emstans(xlsx, location = "RP67", grades = c("R1", "R2"))
emstans(xlsx, location = "RP67", grades = 1:2, by_domain = TRUE, domains = c("VOC", "COM"))

# 2. extract() -------------------------------------------------------------------

extract(res, "cutscore")                       # named list, one vector per unit
ind <- extract(res, "individual")              # panelist, modal, median, average
names(ind)
head(ind$panelist)
ind$modal[ind$modal$table == "All", ]
det <- extract(res, "detailed")                # agreement summary + crosstab
det$agreement
head(det$crosstab)
head(extract(res, "summary"))                  # impact
head(extract(res, "review"))                   # item review
head(extract(res, "profile")[["R1-All"]])      # inconsistency of every candidate cut
str(extract(res), max.level = 1)               # "all"

check(is.factor(extract(res, "review")$aligned) &&
        identical(levels(extract(res, "review")$aligned), c("E", "M", "L")),
      "labels are ordered factors E < M < L")

# 3. report() --------------------------------------------------------------------
# In RStudio these open in the Viewer. They are also saved to out_dir.

rep_all <- report(res)                         # all sections
rep_ind <- report(res, what = "individual")
rep_det <- report(res, what = "detailed")
rep_sum <- report(res, what = "summary")
rep_rev <- report(res, what = "review")
rep_set <- report(res, what = "setup")
if (interactive()) print(rep_all)
htmltools::save_html(rep_all, file.path(out_dir, "report_all.html"))
message("report saved to ", file.path(out_dir, "report_all.html"))

# 4. plot() -------------------------------------------------------------------------

plot(res, "profile", "R1-All")                 # inconsistency profile with the cuts
imp <- plot(res, "impact")                     # list: cut_scores (+ percents with examinees)
imp$cut_scores
ggplot2::ggsave(file.path(out_dir, "profile_R1.png"), plot(res, "profile", "R1-All"),
                width = 9, height = 5)

# 5. update(): manual cut scores ------------------------------------------------------

res_m <- update(res, `R1-All` = c(-0.9, 0.4), `R0-All` = c(-2.5, -1.2))  # any order
res_m                                          # header lists the manual units
check(identical(unname(extract(res_m, "cutscore")[["R1-All"]]), c(-0.9, 0.4)),
      "manual cuts assigned by name")
try(update(res, `R9-All` = c(1, 2)))           # unknown unit -> error listing units
try(update(res, `R1-All` = 0.5))               # wrong number of cuts -> error

# 6. bootstrapping() -------------------------------------------------------------------

b <- bootstrapping(res, "R3-All", n = 200, seed = 1)
b                                              # print.bootESS(): summary per cut
summary(b)
head(extract(b, "draws"))
extract(b, "plot")
if (interactive()) print(report(b))
check(all(b$draws$cut_score %in% res$modal_items$location), "default bootstrap cuts are item locations")

## The earlier jittered-grid bootstrap, as an option
b_j <- bootstrapping(res, "R3-All", n = 30, seed = 1, grid = "jittered")
b_j

# 7. Input other than the Excel workbook --------------------------------------------------

d <- ess_read_excel(xlsx)
d                                              # print.ess_data()

## 7a. CSV files with different column names, mapped by name and by position
r <- d$ratings
csv_ratings <- data.frame(Rater = r$panelist, Grade = r$grade, Item = r$item_id,
                          Judgment = r$label)
csv_items <- d$items[, c("grade", "item_id", "domain", "order", "RP67")]
names(csv_items) <- c("Grade", "ItemCode", "Domain", "Order_of_Difficulty", "RP67")
utils::write.csv(csv_ratings, file.path(out_dir, "ratings.csv"), row.names = FALSE)
utils::write.csv(csv_items, file.path(out_dir, "items.csv"), row.names = FALSE)

d_csv <- ess_data(
  ratings = file.path(out_dir, "ratings.csv"),
  items   = file.path(out_dir, "items.csv"),
  levels  = c("E", "M", "L"),
  ec      = d$grades$ec[1],
  columns = list(ratings = list(panelist = "Rater", item_id = 3, level = "Judgment"),
                 items   = list(item_id = "ItemCode")),
  quiet = FALSE)
res_csv <- emstans(data = d_csv, location = "RP67")
check(isTRUE(all.equal(extract(res_csv, "cutscore"), extract(res, "cutscore"))),
      "CSV input gives the same cut scores as the workbook")

## 7b. Custom labels (here: rename E/M/L)
r2 <- d$ratings
r2$label <- c(E = "Emerging", M = "Meeting", L = "Leading")[r2$label]
d_lab <- ess_data(r2[c("grade", "round", "table", "panelist", "item_id", "label")], d$items,
                  levels = c("Emerging", "Meeting", "Leading"),
                  columns = list(ratings = list(level = "label")))
res_lab <- emstans(data = d_lab, location = "RP67")
res_lab
check(identical(names(extract(res_lab, "cutscore")[[1]]), c("Meeting", "Leading")),
      "custom labels in the output")

## 7c. Two rounds: each grade x round is its own unit
r_round2 <- d$ratings; r_round2$round <- "2"
set.seed(2); i <- sample(nrow(r_round2), 200)
r_round2$label[i] <- sample(c("E", "M", "L"), 200, TRUE)
d_rounds <- ess_data(rbind(d$ratings, r_round2)[c("grade", "round", "table", "panelist", "item_id", "label")],
                     d$items, levels = c("E", "M", "L"),
                     columns = list(ratings = list(level = "label")))
res_rounds <- emstans(data = d_rounds, location = "RP67")
res_rounds
check(length(extract(res_rounds, "cutscore")) == 14, "7 grades x 2 rounds")

## 7d. Simulated data with several panelists and tables
fake <- genFakeDataSet(ngca = 2, cor_val = 0.6, n = 30, nlevel = 3, ntable = 2,
                       npanelist = 3, sdinp = 1, ecinp = 0, 100, 300)
res_fake <- emstans(data = fake, location = "Loc_RP50")
res_fake
extract(res_fake, "individual")$median

# 8. Former argument names still work (with one warning listing the new names)

res_old <- emstans(filePath = xlsx, loc = "RP67", WESS = FALSE)
check(isTRUE(all.equal(extract(res_old, "cutscore"), extract(res_c, "cutscore"))),
      "former argument names give the same result")
try(emstans(xlsx, locaton = "RP67"))                              # misspelled -> error

# 9. Input checks ------------------------------------------------------------------------

try(emstans(xlsx, location = "RP99"))                       # unknown location column
raw <- lapply(readxl::excel_sheets(xlsx), function(s) readxl::read_excel(xlsx, sheet = s))
validateData(raw)                                                 # compatibility wrapper
raw[[3]] <- raw[[3]][-1, ]
validateData(raw)                                                 # reports the missing rating

message("\nAll checks passed. Files written to ", out_dir)
