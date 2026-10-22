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

test_that("examinee scores must be a long score-frequency table", {
  d <- fake_study()
  long <- data.frame(GCA = c("M1", "M1", "M2"), Score = c(300, 310, 320), Freq = c(2, 3, 4))
  e <- ess_data(d$rating, d$item_data, setup = d$setup, examinees = long)
  expect_equal(sum(e$examinees$freq), 9)

  # Wide layout: Score/Freq/Grade blocks side by side, as read from Excel
  wide <- data.frame(Score...1 = c(300, 310), Freq...2 = c(2, 3), Grade...3 = "M1",
                     ...4 = NA, Score...5 = c(320, NA), Freq...6 = c(4, NA), Grade...7 = c("M2", NA),
                     check.names = FALSE)
  expect_error(ess_data(d$rating, d$item_data, setup = d$setup, examinees = wide),
               "one long table")

  # Freq is required, and must be a whole non-negative number
  expect_error(ess_data(d$rating, d$item_data, setup = d$setup, examinees = long[, 1:2]),
               "freq")
  long$Freq[2] <- 2.5
  expect_error(ess_data(d$rating, d$item_data, setup = d$setup, examinees = long),
               "whole, non-negative")

  # An empty sheet means no examinee data
  empty <- data.frame(Score = numeric(0), Freq = numeric(0), GCA = character(0))
  expect_equal(nrow(ess_data(d$rating, d$item_data, setup = d$setup, examinees = empty)$examinees), 0)
})

test_that("fake data can have any number of named subdomains", {
  sim <- function(...) withr::with_seed(1, genFakeDataSet(ngca = 2, ntable = 1, npanelist = 3, cor_val = 0.6,
                                                          nlevel = 3, sdinp = 1, ecinp = 0, n = 40, 300, 500, ...))
  d3 <- sim(domains = c("Number", "Algebra", "Geometry"))
  expect_equal(sort(as.vector(table(d3$item_data$Domain[d3$item_data$GCA == "M1"]))), c(13, 13, 14))
  expect_setequal(unique(d3$item_data$Domain), c("Number", "Algebra", "Geometry"))
  expect_setequal(unique(sim(domains = 4)$item_data$Domain), c("Alg", "Geom", "Calc", "Prob"))
  expect_false("Domain" %in% names(sim(domains = NULL)$item_data))
  expect_error(sim(domains = c("A", "A")), "distinct")
  expect_error(sim(domains = 2.5), "whole number")
  
  res <- suppressMessages(emstans(data = d3, location = "Loc_RP50", by_domain = TRUE))
  expect_setequal(names(extract(res, "cutscore")),
                  paste0(rep(c("M1", "M2"), each = 4), "-", c("All", "Algebra", "Geometry", "Number")))
})