test_that("fewer than 4 designs no longer give NA optimiser start values", {
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                     evaluations = 200, n.startsets = 3, silent = TRUE))
  expect_false(anyNA(ds$final$design))
  expect_false(is.na(ds$final$power))
})

test_that("too small initial designs give a clear error before simulating", {
  set.seed(1)
  calls <- 0
  counting <- function(n) { calls <<- calls + 1; TRUE }
  expect_error(find.design(simfun = counting, boundaries = c(20, 200), power = .8,
                           n.startsets = 2, silent = TRUE), "must be at least 3")
  expect_error(find.design(simfun = function(n, k) TRUE,
                           boundaries = list(n = c(2, 20), k = c(2, 20)), power = .8,
                           n.startsets = 1, silent = TRUE), "must be at least 3")
  expect_identical(calls, 0)
})

test_that("get.pred start values are the existing designs, most evaluated first", {
  dat <- list(list(x = 20, y = c(TRUE, FALSE)), list(x = 200, y = rep(TRUE, 5)),
              list(x = 110, y = c(TRUE, TRUE, FALSE)))
  fit <- list(fitfun = function(x) min(1, x / 150), fitfun.sd = NULL, badfit = FALSE)
  set.seed(1)
  pred <- mlpwr:::get.pred(fit = fit, dat = dat, power = .8, costfun = function(x) sum(x),
                           cost = NULL, boundaries = list(n = c(20, 200)), task = "desiredpower",
                           aggregate_fun = mean, integer = TRUE, use_noise = FALSE)
  expect_false(anyNA(pred$points))
})
