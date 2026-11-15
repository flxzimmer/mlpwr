test_that("ci termination works when the prediction is all designs (bad prediction)", {
  fit <- list(fitfun.sd = function(x) x[1] / 100)
  dat <- list(list(x = 1, y = c(TRUE, FALSE)))
  many <- list(points = data.frame(V1 = c(1, 5, 3), V2 = c(2, 2, 2)))
  one <- list(points = data.frame(V1 = 3, V2 = 2))
  ct <- function(pred, ci) mlpwr:::check.term(evaluations = NULL, ci = ci, time = NULL,
                                             dat = dat, time_temp = NULL, fit = fit,
                                             pred = pred, ci_perc = .95)
  # interval = sd * 2 * 1.96; largest sd over the designs is .05 -> .196
  expect_false(ct(many, ci = .19))
  expect_true(ct(many, ci = .2))
  # a single design behaves as before (sd .03 -> .1176)
  expect_false(ct(one, ci = .11))
  expect_true(ct(one, ci = .12))
})

test_that("find.design with ci and a bad 2-D fit runs to the end", {
  null2 <- function(n, k) stats::t.test(stats::rnorm(n))$p.value < .05
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = null2, boundaries = list(n = c(20, 200), k = c(1, 10)),
                                     costfun = function(n, k) n + k, power = .8,
                                     evaluations = 300, ci = .01, silent = TRUE))
  expect_s3_class(ds, "designresult")
})
