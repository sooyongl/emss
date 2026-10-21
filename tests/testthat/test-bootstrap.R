test_that("the default bootstrap resamples items and uses the exact minimum", {
  res <- run_fake()
  b <- bootstrapping(res, "M1-All", n = 20, seed = 1)
  expect_s3_class(b, "bootESS")
  expect_equal(nrow(b$draws), 20 * 2)
  # Exact minima are item locations.
  locs <- res$modal_items$location[res$modal_items$unit == "M1-All"]
  expect_true(all(b$draws$cut_score %in% locs))
  expect_equal(levels(b$draws$cut_level), c("Level1", "Level2", "Level3"))
})

test_that("bootstrap draws are reproducible with a seed", {
  res <- run_fake()
  expect_equal(bootstrapping(res, "M1-All", n = 10, seed = 7)$draws,
               bootstrapping(res, "M1-All", n = 10, seed = 7)$draws)
})

test_that("the jittered-grid bootstrap is still available", {
  res <- run_fake()
  b <- bootstrapping(res, "M1-All", n = 5, seed = 1, grid = "jittered")
  expect_equal(b$grid, "jittered")
  expect_equal(nrow(b$draws), 5 * 2)
})

test_that("bootstrap rejects unknown units and accepts former argument names", {
  res <- run_fake()
  expect_error(bootstrapping(res, "M9-All"), "must be one of")
  expect_warning(bootstrapping(res, gca_name = "M1-All", boot_num = 3, seed = 1), "now `unit` and `n`")
})
