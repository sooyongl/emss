# Direct (non-log-scale) probabilities, written independently of the package
gpcm_cum <- function(theta, deltas, k, a = 1, D = 1) {
  num <- exp(cumsum(c(0, D * a * (theta - deltas))))
  sum(num[(k + 1):length(num)]) / sum(num)
}
irt3 <- function(theta, a, b, c, D = 1) c + (1 - c) / (1 + exp(-D * a * (theta - b)))

test_that("Rasch locations are b plus the logit of rp", {
  pars <- data.frame(id = c("i1", "i2", "i3"), b = c(-1, 0, 1.5))
  loc <- ess_locations(pars, model = "rasch")
  expect_equal(loc$location, pars$b + log(0.67 / 0.33))
  expect_equal(ess_locations(pars, model = "rasch", rp = 0.5)$location, pars$b)
  expect_equal(loc$Item_ID, pars$id)
  expect_equal(loc$score, rep(1L, 3))
  # an `a` column is ignored for Rasch items
  pars$a <- c(2, 3, 4)
  expect_equal(ess_locations(pars, model = "rasch")$location, loc$location)
})

test_that("2PL locations solve P = rp, and rp can change item order", {
  pars <- data.frame(id = c("lo_a", "hi_a"), a = c(0.4, 2), b = c(0, 0.3))
  for (D in c(1, 1.702)) {
    loc <- ess_locations(pars, model = "2pl", D = D)
    expect_equal(irt3(loc$theta, pars$a, pars$b, 0, D), c(0.67, 0.67))
  }
  # by b the high-a item is harder; at RP67 the low-a item is
  loc <- ess_locations(pars, model = "2pl")
  expect_true(loc$location[1] > loc$location[2])
})

test_that("3PL guessing options give the documented probabilities", {
  pars <- data.frame(id = c("m1", "m2"), a = c(1.1, 0.8), b = c(-0.2, 0.6),
                     c = c(0.2, 0.25))
  D <- 1.702
  none <- ess_locations(pars, model = "3pl", D = D)
  resc <- ess_locations(pars, model = "3pl", D = D, guessing = "rescale")
  huyn <- ess_locations(pars, model = "3pl", D = D, guessing = "huynh")
  expect_equal(irt3(none$theta, pars$a, pars$b, pars$c, D), c(0.67, 0.67))
  expect_equal(irt3(resc$theta, pars$a, pars$b, pars$c, D),
               pars$c + (1 - pars$c) * 0.67)
  expect_equal(irt3(huyn$theta, pars$a, pars$b, pars$c, D), (2 + pars$c) / 3)
  # Huynh's criterion is b + log(2) / (D a) whatever c is
  expect_equal(huyn$theta, pars$b + log(2) / (D * pars$a))
  expect_equal(huyn$p, (2 + pars$c) / 3)
  # c = 0 reduces 3PL to 2PL
  pars0 <- transform(pars, c = 0)
  expect_equal(ess_locations(pars0, model = "3pl")$theta,
               ess_locations(pars0, model = "2pl")$theta)
})

test_that("3PL with rp at or below c is an error unless guessing is adjusted", {
  pars <- data.frame(id = "m1", a = 1, b = 0, c = 0.7)
  expect_error(ess_locations(pars, model = "3pl"), "above the lower asymptote")
  expect_silent(ess_locations(pars, model = "3pl", guessing = "rescale"))
})

test_that("GRM locations use the boundary curves", {
  pars <- data.frame(id = "g1", a = 1.3, b1 = -1, b2 = 0.2, b3 = 1.4)
  loc <- ess_locations(pars, model = "grm", D = 1.7)
  expect_equal(loc$Item_ID, c("g1_1", "g1_2", "g1_3"))
  expect_equal(loc$score, 1:3)
  expect_equal(loc$max_score, rep(3L, 3))
  expect_equal(loc$theta, c(-1, 0.2, 1.4) + qlogis(0.67) / (1.7 * 1.3))
  bad <- transform(pars, b2 = 2)
  expect_error(ess_locations(bad, model = "grm"), "must increase")
})

test_that("PCM / GPCM locations solve P(X >= k) = rp", {
  set.seed(20261022)
  for (r in 1:200) {
    K <- sample(1:5, 1)
    deltas <- sort(rnorm(K, 0, 1.2))
    if (r %% 3 == 0) deltas <- sample(deltas)          # disordered steps
    a <- runif(1, 0.3, 2.5)
    D <- sample(c(1, 1.702), 1)
    rp <- runif(1, 0.3, 0.9)
    pars <- as.data.frame(as.list(c(a = a, setNames(deltas, paste0("b", 1:K)))))
    pars$id <- "p"
    loc <- ess_locations(pars, model = "gpcm", rp = rp, D = D, item = "id")
    got <- mapply(gpcm_cum, loc$theta, k = loc$score,
                  MoreArgs = list(deltas = deltas, a = a, D = D))
    expect_equal(got, rep(rp, K), tolerance = 1e-9)
    # cumulative locations are ordered even when the steps are not
    expect_true(all(diff(loc$theta) > 0))
  }
})

test_that("one-step PCM equals Rasch and two-category GPCM equals 2PL", {
  pars <- data.frame(id = c("x", "y"), a = c(0.6, 1.8), b = c(-0.4, 0.9))
  expect_equal(ess_locations(pars, model = "pcm")$theta,
               ess_locations(pars, model = "rasch")$theta, tolerance = 1e-10)
  expect_equal(ess_locations(pars, model = "gpcm", D = 1.7)$theta,
               ess_locations(pars, model = "2pl", D = 1.7)$theta,
               tolerance = 1e-10)
  # with K = 1 the GRM is the 2PL too
  expect_equal(ess_locations(pars, model = "grm")$theta,
               ess_locations(pars, model = "2pl")$theta)
})

test_that("extreme steps are still solved", {
  pars <- data.frame(id = "e", a = 0.2, b1 = -6, b2 = 7)
  loc <- ess_locations(pars, model = "gpcm", rp = 0.95)
  got <- mapply(gpcm_cum, loc$theta, k = 1:2,
                MoreArgs = list(deltas = c(-6, 7), a = 0.2))
  expect_equal(got, c(0.95, 0.95), tolerance = 1e-9)
})

test_that("mixed-format tables work with a model column and NA padding", {
  pars <- data.frame(
    Item_ID = c("MC1", "CR1", "MC2", "CR2"),
    model   = c("3PL", "gpcm", "2pl", "grm"),
    a  = c(1.2, 0.9, 0.7, 1.1),
    b1 = c(-0.5, -1.0, 0.3, -0.8),
    b2 = c(NA, 0.2, NA, 0.4),
    b3 = c(NA, 1.1, NA, NA),
    c  = c(0.2, NA, NA, NA)
  )
  loc <- ess_locations(pars, D = 1.702)
  expect_equal(loc$Item_ID,
               c("MC1", "CR1_1", "CR1_2", "CR1_3", "MC2", "CR2_1", "CR2_2"))
  expect_equal(loc$model, c("3pl", rep("gpcm", 3), "2pl", "grm", "grm"))
  expect_equal(loc$max_score, c(1L, 3L, 3L, 3L, 1L, 2L, 2L))
  # each row agrees with the same item computed on its own
  one <- ess_locations(pars[2, ], D = 1.702)
  expect_equal(loc$theta[2:4], one$theta)
  # thresholds named by a character vector, columns named differently
  pars2 <- setNames(pars, c("id", "mod", "slope", "t1", "t2", "t3", "g"))
  loc2 <- ess_locations(pars2, model = pars2$mod, D = 1.702, item = "id",
                        a = "slope", b = c("t1", "t2", "t3"), c = "g")
  expect_equal(loc2$theta, loc$theta)
  # same via prefix
  loc3 <- ess_locations(pars2, model = pars2$mod, D = 1.702, item = 1,
                        a = "slope", b = "t", c = "g")
  expect_equal(loc3$theta, loc$theta)
})

test_that("reporting-scale transformation and separator", {
  pars <- data.frame(id = "g1", a = 1, b1 = -1, b2 = 1)
  loc <- ess_locations(pars, model = "grm", slope = 50, intercept = 500,
                       sep = "-")
  expect_equal(loc$location, 500 + 50 * loc$theta)
  expect_equal(loc$Item_ID, c("g1-1", "g1-2"))
})

test_that("bad input gives clear errors", {
  pars <- data.frame(id = c("i1", "i2"), b = c(0, 1))
  expect_error(ess_locations(pars), "Give `model`")
  expect_error(ess_locations(pars, model = "4pl"), "Unknown model: 4pl")
  expect_error(ess_locations(pars, model = c("2pl", "2pl", "2pl")), "length 1")
  expect_error(ess_locations(pars, model = "2pl"), "Column `a`.*not found")
  expect_error(ess_locations(transform(pars, a = 1), model = "3pl"),
               "Column `c`.*needed for 3PL")
  expect_error(ess_locations(pars, model = "2pl", a = 9), "not a column position")
  expect_error(ess_locations(pars, model = "rasch", rp = 1), "`rp` must be")
  expect_error(ess_locations(pars, model = "rasch", slope = -1), "`slope` must")
  expect_error(ess_locations(rbind(pars, pars), model = "rasch"), "Duplicated")
  expect_error(ess_locations(transform(pars, a = c(1, 0)), model = "2pl"),
               "must be positive.*i2")
  expect_error(ess_locations(transform(pars, b = c("0", "x")), model = "rasch"),
               "non-numeric values: x")
  # numbers stored as text are fine
  expect_equal(ess_locations(transform(pars, b = c("0", "1")), model = "rasch"),
               ess_locations(pars, model = "rasch"))
  poly <- data.frame(id = "p", b1 = 0, b2 = NA, b3 = 1)
  expect_error(ess_locations(poly, model = "pcm"), "NA only after")
  expect_error(ess_locations(data.frame(id = "d", b1 = 0, b2 = 1),
                             model = "rasch"), "more than one threshold")
  expect_error(ess_locations(data.frame(id = "d", b = 0, b1 = 1),
                             model = "rasch"), "keep one of them")
  expect_error(ess_locations(data.frame(id = "d", x = 0), model = "rasch"),
               "No threshold columns")
  clash <- data.frame(id = c("Q1", "Q1_1"), b1 = c(0, 0), b2 = c(1, NA),
                      model = c("pcm", "rasch"))
  expect_error(ess_locations(clash), "clash")
})
