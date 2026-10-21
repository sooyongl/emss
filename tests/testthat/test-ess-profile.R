test_that("essCW matches a hand-computed example", {
  # Level 1 items at 400, 410, 420 and an unexpectedly hard one at 500;
  # Level 2 items at 440, 450, 460.
  d <- data.frame(location = c(400, 410, 420, 440, 450, 460, 500),
                  ALD      = c(1,   1,   1,   2,   2,   2,   1))
  prof <- EmStanS:::essCW(d, cut_level = 2)$incon_default

  expect_equal(prof$cut_score, d$location)
  expect_equal(prof$counts,  c(4, 3, 2, 1, 2, 3, 4))
  expect_equal(prof$weights, c(130, 100, 80, 60, 60, 70, 150))
})

test_that("essCW matches the brute-force definition, including ties", {
  set.seed(1)
  for (i in 1:200) {
    n   <- sample(c(3, 15, 60), 1)
    nl  <- sample(2:5, 1)
    loc <- if (i %% 2) round(rnorm(n, 500, 30)) else rnorm(n)   # ties when rounded
    ald <- sample(seq_len(nl), n, replace = TRUE)
    for (l in 2:nl) {
      got <- EmStanS:::essCW(data.frame(location = loc, ALD = ald), cut_level = l)$incon_default
      ref <- reference_profile(loc, ald, l)
      expect_equal(got$counts,  unname(ref[, "count"]))
      expect_equal(got$weights, unname(ref[, "weight"]), tolerance = 1e-10)
    }
  }
})

test_that("selection picks lowest weight, then lowest count", {
  # Weight ties at 440 and 450 (both 60); 440 has the lower count (1 vs 2).
  prof <- data.frame(L2_C = c(4, 3, 2, 1, 2, 3, 4),
                     L2_W = c(130, 100, 80, 60, 60, 70, 150))
  expect_equal(unname(EmStanS:::select_cut_rows(prof, "weight")), 4)
  expect_equal(unname(EmStanS:::select_cut_rows(prof, "count")), 4)
})

test_that("tie-breaking is applied separately for each cut level", {
  # Level 2 ties on weight at rows 2 and 3 (row 3 has the lower count);
  # level 3 has a unique minimum at row 5. The old cal_minp() mixed these
  # candidate sets and returned row 2 for level 2.
  prof <- data.frame(L2_C = c(5, 4, 3, 4, 6), L3_C = c(6, 5, 4, 3, 1),
                     L2_W = c(9, 2, 2, 7, 9),  L3_W = c(9, 8, 6, 4, 1))
  expect_equal(unname(EmStanS:::select_cut_rows(prof, "weight")), c(3, 5))
})

test_that("minimums are exact, not taken after rounding to 2 decimals", {
  # Weights that differ only in the third decimal: rounding to 2 decimals
  # would tie them and fall back to the first row.
  prof <- EmStanS:::exact_profile(data.frame(L2_C = c(1, 1, 1), L2_W = c(0.004, 0.001, 0.003)))
  expect_equal(unname(EmStanS:::select_cut_rows(prof, "weight")), 2)
})

test_that("exact_profile restores ties broken by floating-point noise", {
  prof <- data.frame(L2_C = c(2, 1), L2_W = c(0.3, 0.1 + 0.2))  # 0.1 + 0.2 != 0.3
  expect_false(prof$L2_W[1] == prof$L2_W[2])
  prof <- EmStanS:::exact_profile(prof)
  # Tied on weight, so the lower count (row 2) wins rather than noise.
  expect_equal(unname(EmStanS:::select_cut_rows(prof, "weight")), 2)
})

test_that("empirical level: an item at or above a cut is in that level", {
  loc <- c(1, 2, 3, 4, 5, 6)
  expect_equal(EmStanS:::operational_levels(loc, c(3, 5)), c(1, 1, 2, 2, 3, 3))
  # Two cuts on the same item: the level between them is empty, and the item
  # at the cut belongs to the higher level (the old position rule said 2).
  expect_equal(EmStanS:::operational_levels(loc, c(3, 3)), c(1, 1, 3, 3, 3, 3))
})
