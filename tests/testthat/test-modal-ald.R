test_that("row_modes() matches the original per-item rule", {
  set.seed(2)
  for (i in 1:150) {
    n <- sample(c(1, 8, 40), 1)
    p <- sample(1:7, 1)
    k <- sample(2:4, 1)
    ald <- tibble::as_tibble(
      matrix(paste0("Level", sample(seq_len(k), n * p, TRUE)), n, p),
      .name_repair = ~ as.character(seq_along(.x))
    )
    cors <- if (i %% 2) round(runif(p), 1) else rep(0.5, p)  # tied correlations
    if (i %% 5 == 0) cors[1] <- NA
    expect_identical(
      EmStanS:::row_modes(ald, cors),
      unname(apply(ald, 1, get_mode_reference, cors))
    )
  }
})

test_that("ties go to the highest-correlation panelist", {
  ald <- data.frame(a = c("Level1", "Level2"), b = c("Level2", "Level2"),
                    c = c("Level3", "Level1"))
  # Row 1: three-way tie -> panelist c (cor 0.9) wins.
  # Row 2: Level2 is the unique mode regardless of correlations.
  expect_identical(EmStanS:::row_modes(ald, c(0.1, 0.5, 0.9)),
                   c("Level3", "Level2"))
})
