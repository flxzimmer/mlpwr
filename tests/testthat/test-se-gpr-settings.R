# The extra GPR that only supplies an SE (ci termination, final SE for
# surrogates without their own SE) uses the user's settings.

test_that("ci termination's SE GPR uses the user's noise_fun and aggregate_fun", {
  calls <- 0
  nf <- function(d) { calls <<- calls + 1; stats::var(d$y) / length(d$y) + 1e-4 }
  dat <- lapply(c(20, 40, 60, 80, 100), function(n)
    list(x = n, y = as.numeric(stats::runif(30) < n / 120)))
  pred <- list(points = data.frame(V1 = 60))
  set.seed(2)
  mlpwr:::check.term(evaluations = NULL, ci = .01, time = NULL, dat = dat,
                     time_temp = NULL, fit = list(), pred = pred, ci_perc = .95,
                     noise_fun = nf)
  expect_gt(calls, 0)
})

test_that("defaults give the same SE as before", {
  dat <- lapply(c(20, 40, 60, 80, 100), function(n)
    list(x = n, y = as.numeric(stats::runif(30) < n / 120)))
  set.seed(3)
  old <- mlpwr:::fit.surrogate(dat = dat, surrogate = "gpr", noise_fun = "bernoulli")$fitfun.sd(60)
  set.seed(3)
  new <- mlpwr:::fit.surrogate(dat = dat, surrogate = "gpr", control = list(),
                               aggregate_fun = mean, use_noise = TRUE,
                               noise_fun = mlpwr:::raw_noise_fun("logistic"))$fitfun.sd(60)
  expect_identical(old, new)
})

test_that("logreg + ci with a custom noise_fun runs and calls it", {
  calls <- 0
  nf <- function(d) { calls <<- calls + 1; mean(d$y) * (1 - mean(d$y)) / length(d$y) + 1e-4 }
  set.seed(4)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200),
                                     power = .8, surrogate = "logreg", evaluations = 400,
                                     ci = .001, noise_fun = nf, silent = TRUE))
  expect_s3_class(ds, "designresult")
  expect_gt(calls, 0)
})
