# A logreg fit that fails without a previous good fit retries from an
# intercept-only start; if the final fit still fails, no design is reported.

sim_mdd <- function(delta) stats::t.test(stats::rnorm(200, delta))$p.value < .01

test_that("steep MDD curve no longer ends at the boundary with power .5", {
  set.seed(1)  # failed in 1.1.1: design .01, power .5
  ds <- suppressWarnings(find.design(simfun = sim_mdd, boundaries = c(.01, .8), power = .8,
                                     surrogate = "logreg", evaluations = 250,
                                     integer = FALSE, silent = TRUE))
  expect_gt(ds$final$design[[1]], .15)
  expect_lt(ds$final$design[[1]], .35)
  expect_gt(ds$final$power, .7)
})

test_that("the intercept-only retry fits data the plain fit cannot", {
  dat <- list(list(x = .01, y = rep(0, 30)), list(x = .27, y = c(rep(1, 18), rep(0, 12))),
              list(x = .53, y = rep(1, 30)), list(x = .8, y = rep(1, 30)))
  fit <- mlpwr:::logi.fit(dat, aggregate_fun = mean, lastfit = list())
  expect_false(fit$badfit)
})

test_that("a final logreg fit that fails gives NA and a warning", {
  local_mocked_bindings(logi.fit = function(...) list(fitfun = function(x) 0.5,
    fitfun.sd = NULL, badfit = TRUE, last.mod = NULL), .package = "mlpwr")
  set.seed(2)
  expect_warning(ds <- find.design(simfun = sim_mdd, boundaries = c(.01, .8), power = .8,
                                   surrogate = "logreg", evaluations = 100,
                                   integer = FALSE, silent = TRUE),
                 "logreg surrogate could not be fitted")
  expect_true(is.na(ds$final$design[[1]]))
  expect_true(is.na(ds$final$power))
  expect_true(is.na(ds$final$cost))
  expect_gt(length(ds$dat), 0)
})
