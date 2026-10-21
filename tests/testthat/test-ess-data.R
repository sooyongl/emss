test_that("CSV input with renamed columns gives the same cuts as the list input", {
  d <- fake_study()
  dir <- withr::local_tempdir()
  r <- d$rating
  names(r)[names(r) == "Panelist"] <- "Rater"
  names(r)[names(r) == "ALD"] <- "Judgment"
  r <- r[, c("Judgment", setdiff(names(r), "Judgment"))]   # level column first
  utils::write.csv(r, file.path(dir, "ratings.csv"), row.names = FALSE)
  utils::write.csv(d$item_data, file.path(dir, "items.csv"), row.names = FALSE)
  utils::write.csv(d$setup, file.path(dir, "setup.csv"), row.names = FALSE)

  csv <- ess_data(file.path(dir, "ratings.csv"), file.path(dir, "items.csv"),
                  setup = file.path(dir, "setup.csv"),
                  columns = list(ratings = list(panelist = "Rater", level = 1)))
  expect_s3_class(csv, "ess_data")
  expect_equal(extract(run_fake(csv), "cutscore"), extract(run_fake(), "cutscore"))
})

test_that("missing required columns are reported with how to map them", {
  d <- fake_study()
  r <- d$rating; names(r)[names(r) == "ALD"] <- "Judgment"
  expect_error(ess_data(r, d$item_data, setup = d$setup),
               "columns = list\\(ratings = list\\(level")
})

test_that("arguments override the setup table", {
  d <- fake_study()
  e <- ess_data(d$rating, d$item_data, setup = d$setup, sd = c(M2 = 2), ec = 0.5)
  expect_equal(e$grades$sd, c(1, 2))
  expect_equal(e$grades$ec, c(0.5, 0.5))
})

test_that("custom level labels appear in every output, in level order", {
  d <- fake_study()
  lab <- c("Basic", "Intermediate", "Advanced")
  d$rating$ALD <- lab[as.integer(sub("Level", "", d$rating$ALD))]
  res <- run_fake(ess_data(d$rating, d$item_data, setup = d$setup, levels = lab))
  expect_named(extract(res, "cutscore")[["M1-All"]], c("Intermediate", "Advanced"))
  rv <- extract(res, "review")
  expect_true(is.factor(rv$aligned))
  expect_equal(levels(rv$aligned), lab)
  # Same numbers as with the default labels.
  expect_equal(unname(unlist(extract(res, "cutscore"))),
               unname(unlist(extract(run_fake(), "cutscore"))))
})

test_that("labels may differ by grade", {
  d <- fake_study()
  labs <- list(M1 = c("E", "M", "L"), M2 = c("B", "P", "A"))
  k <- as.integer(sub("Level", "", d$rating$ALD))
  d$rating$ALD <- ifelse(d$rating$GCA == "M1", labs$M1[k], labs$M2[k])
  res <- run_fake(ess_data(d$rating, d$item_data, setup = d$setup, levels = labs))
  cuts <- extract(res, "cutscore")
  expect_named(cuts[["M1-All"]], c("M", "L"))
  expect_named(cuts[["M2-All"]], c("P", "A"))
})

test_that("each round is analysed separately", {
  d <- fake_study()
  r1 <- d$rating
  r2 <- d$rating
  r2$Round <- 2
  set.seed(3)
  flip <- sample(nrow(r2), 20)
  r2$ALD[flip] <- sample(c("Level1", "Level2", "Level3"), 20, TRUE)

  both <- run_fake(ess_data(rbind(r1, r2), d$item_data, setup = d$setup))
  only2 <- run_fake(ess_data(r2, d$item_data, setup = d$setup))
  only1 <- run_fake()

  cuts <- extract(both, "cutscore")
  expect_named(cuts, c("M1-All Round 1", "M1-All Round 2", "M2-All Round 1", "M2-All Round 2"))
  expect_equal(unname(cuts[["M1-All Round 2"]]), unname(extract(only2, "cutscore")[["M1-All"]]))
  expect_equal(unname(cuts[["M1-All Round 1"]]), unname(extract(only1, "cutscore")[["M1-All"]]))
})

test_that("a panelist rating an item twice in one round is an error", {
  d <- fake_study()
  expect_error(ess_data(rbind(d$rating, d$rating[1, ]), d$item_data, setup = d$setup),
               "rated more than once")
})

test_that("the wide examinee layout is stacked", {
  wide <- data.frame(Score...1 = c(300, 310), Freq...2 = c(2, 3), Grade...3 = "M1",
                     ...4 = NA, Score...5 = c(320, NA), Freq...6 = c(4, NA), Grade...7 = c("M2", NA),
                     check.names = FALSE)
  long <- emss:::.ess_stack_examinees(wide)
  expect_equal(nrow(long), 3)
  expect_equal(names(long), c("Score", "Freq", "Grade"))
})
