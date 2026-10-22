test_that("emstans() runs end to end with Weight and Count", {
  for (w in c("weight", "count")) {
    res <- run_fake(method = w)
    expect_s3_class(res, "ESS")
    cuts <- extract(res, "cutscore")
    expect_named(cuts, c("M1-All", "M2-All"))
    expect_true(all(lengths(cuts) == 2))          # 3 levels -> 2 cuts
    expect_true(all(vapply(cuts, function(x) all(diff(x) > 0), logical(1))))
  }
})

test_that("cut scores are stable (regression snapshot)", {
  res <- run_fake(method = "weight")
  expect_snapshot_value(lapply(extract(res, "cutscore"), unname), style = "json2")
  res_c <- run_fake(method = "count")
  expect_snapshot_value(lapply(extract(res_c, "cutscore"), unname), style = "json2")
})

test_that("every grade appears in the item review", {
  res <- run_fake(method = "count")
  expect_setequal(unique(extract(res, "review")$unit), names(extract(res, "cutscore")))
})

test_that("update() assigns manual cuts by name, whatever the order", {
  res <- run_fake(method = "weight")
  upd <- suppressWarnings(suppressMessages(
    update(res, `M2-All` = c(250, 280), `M1-All` = c(150, 220))
  ))
  cuts <- extract(upd, "cutscore")
  expect_equal(unname(cuts[["M1-All"]]), c(150, 220))
  expect_equal(unname(cuts[["M2-All"]]), c(250, 280))
})

test_that("update() rejects unknown names and wrong lengths", {
  res <- run_fake(method = "weight")
  expect_error(update(res, `M9-All` = c(1, 2)), "Unknown unit")
  expect_error(update(res, `M1-All` = 150), "needs 2 numeric")
})


test_that("count and weight of a manual cut are taken at the cut score itself", {
  res <- run_fake()
  u <- res$units[["M1-All"]]
  mi <- res$modal_items[res$modal_items$unit == "M1-All", ]
  # a cut strictly between two item locations
  cs <- c(mean(u$location[8:9]), mean(u$location[20:21]))
  new <- update(res, `M1-All` = cs)
  m <- new$modal[new$modal$unit == "M1-All" & new$modal$table == "All", ]
  for (j in 1:2) {
    k <- j + 1
    bad <- (mi$aligned >= k & mi$location < cs[j]) | (mi$aligned < k & mi$location >= cs[j])
    expect_equal(m$count[j], sum(bad))
    expect_equal(m$weight[j], sum(abs(mi$location[bad] - cs[j])) / u$sd)
  }
  expect_equal(m$page, vapply(cs, function(x) which(u$location >= x)[1], integer(1)))
})
