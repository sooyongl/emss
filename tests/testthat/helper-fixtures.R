# Shared synthetic fixtures. Only simulated data is used in the test suite;
# real study files stay out of the package.

# Brute-force ESS profile straight from the definition, used as the reference.
reference_profile <- function(location, ald, cut_level, candidates = location) {
  t(vapply(candidates, function(c) {
    upper_inc <- ald >= cut_level & location <  c
    lower_inc <- ald <  cut_level & location >= c
    c(count  = sum(upper_inc) + sum(lower_inc),
      weight = sum((c - location)[upper_inc]) + sum((location - c)[lower_inc]))
  }, numeric(2)))
}

# One small simulated study, built once per test run.
fake_study <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      withr::with_seed(20261020, {
        cache <<- generateMockDataSet(ngca = 2, cor_val = 0.6, n = 30, nlevel = 3,
                                 ntable = 1, npanelist = 3, sdinp = 1, ecinp = 0,
                                 100, 300)
      })
    }
    cache
  }
})

run_fake <- function(data = fake_study(), ...) {
  suppressWarnings(suppressMessages(emstans(data = data, location = "Loc_RP50", ...)))
}
