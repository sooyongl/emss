test_that("former argument names still work, with one warning", {
  d <- fake_study()
  expect_warning(old <- emstans(data = d, loc = "Loc_RP50", WESS = FALSE, gamest = FALSE,
                                median = "modal"),
                 "former argument names")
  new <- emstans(data = d, location = "Loc_RP50", method = "count")
  expect_equal(extract(old, "cutscore"), extract(new, "cutscore"))
})

test_that("former names are not partially matched to new ones", {
  # `domain` must not be taken as `domains`, nor `loc` as `location`.
  d <- fake_study()
  d$item_data$Domain <- rep(c("A", "B"), length.out = nrow(d$item_data))
  r <- suppressWarnings(emstans(data = d, loc = "Loc_RP50", grade = "M1",
                                domain = "Domain", select_domain = "A"))
  expect_equal(names(r$units), c("M1-All", "M1-A"))
  expect_error(emstans(data = d, location = "Loc_RP50", loca = "x"), "Unknown argument")
})

test_that("by_domain adds one unit per domain to the All units", {
  d <- fake_study()
  d$item_data$Domain <- rep(c("A", "B"), length.out = nrow(d$item_data))
  r <- emstans(data = d, location = "Loc_RP50", by_domain = TRUE)
  expect_equal(names(r$units), c("M1-All", "M1-A", "M1-B", "M2-All", "M2-A", "M2-B"))
})
