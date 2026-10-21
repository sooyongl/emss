test_that("a location column that does not exist gives a clear error", {
  expect_error(
    suppressMessages(emstans(data = fake_study(), location = "RP67")),
    "Location column `RP67` is not in the item data"
  )
})

test_that("missing locations are reported by item", {
  d <- fake_study()
  d$item_data$Loc_RP50[3] <- NA
  expect_error(suppressMessages(emstans(data = d, location = "Loc_RP50")),
               "missing for item")
})

test_that("an ALD label outside the Level Options is an error, not LevelNA", {
  d <- fake_study()
  d$rating$ALD[1] <- "Levle2"
  expect_error(suppressMessages(emstans(data = d, location = "Loc_RP50")),
               "not found in the grade's level labels")
})

test_that("incomplete ratings are reported by panelist", {
  d <- fake_study()
  d$rating <- d$rating[-1, ]
  expect_error(suppressMessages(emstans(data = d, location = "Loc_RP50")),
               "not rated")
})

test_that("duplicated item IDs are an error", {
  d <- fake_study()
  d$item_data$Item_ID[2] <- d$item_data$Item_ID[1]
  expect_error(emstans(data = d, location = "Loc_RP50"), "unique")
})
