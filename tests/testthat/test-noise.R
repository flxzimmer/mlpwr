# Noise variances for the GPR surrogate (R/noise.R): noise_fun = "logistic"
# (default) and "bernoulli" (mlpwr <= 1.1.1)

mk_dat <- function(x, s, k) lapply(seq_along(x), function(i)
  list(x = c(n = x[i]), y = c(rep(1, s[i]), rep(0, k[i] - s[i]))))

test_that("noise: 'bernoulli' is the variance of mlpwr <= 1.1.1", {
  d <- mk_dat(c(10, 20, 30, 40), c(3, 10, 18, 20), c(20, 20, 20, 20))
  xv <- data.frame(n = c(10, 20, 30, 40))
  expect_identical(mlpwr:::noise_variances(d, xv, "bernoulli"), mlpwr:::getweight(d, weight.type = "var"))
})

test_that("noise: both methods give finite positive variances at p_hat = 0 / 1 and small k", {
  d <- mk_dat(c(10, 20, 30, 40, 50), c(0, 0, 2, 3, 3), c(3, 2, 3, 3, 3))
  xv <- data.frame(n = c(10, 20, 30, 40, 50))
  for (m in c("bernoulli", "logistic")) {
    v <- mlpwr:::noise_variances(d, xv, m)
    expect_true(all(is.finite(v) & v > 0), info = m)
  }
  # complete separation in the logistic fit
  d2 <- mk_dat(1:6, c(0, 0, 0, 5, 5, 5), rep(5, 6))
  v <- mlpwr:::noise_variances(d2, data.frame(n = 1:6), "logistic")
  expect_true(all(is.finite(v) & v > 0))
  # variances never exceed the Bernoulli maximum .25 / k
  expect_true(all(v <= .25 / 5 + 1e-12))
})

test_that("noise: 'bernoulli' rewards lucky draws, 'logistic' much less", {
  xv <- data.frame(n = c(10, 20, 30, 40))
  d1 <- mk_dat(xv$n, c(10, 14, 16, 19), rep(20, 4))
  d2 <- mk_dat(xv$n, c(10, 14, 20, 19), rep(20, 4))   # design 3 luckier
  vb1 <- mlpwr:::noise_variances(d1, xv, "bernoulli"); vb2 <- mlpwr:::noise_variances(d2, xv, "bernoulli")
  vl1 <- mlpwr:::noise_variances(d1, xv, "logistic"); vl2 <- mlpwr:::noise_variances(d2, xv, "logistic")
  expect_lt(vb2[3], vb1[3] / 3)
  expect_gt(vl2[3], vl1[3] / 2)
})

test_that("noise: a user function gets the whole list(x, y) element", {
  d <- mk_dat(c(10, 20), c(3, 10), c(20, 20))
  seen <- list()
  nf <- function(e) { seen[[length(seen) + 1]] <<- e; 1 }
  expect_identical(mlpwr:::noise_variances(d, data.frame(n = c(10, 20)), nf), c(1, 1))
  expect_identical(seen, d)
})

test_that("noise: invalid noise_fun values give a clear error", {
  for (bad in list("raw", "pooled", "Bernoulli", c("logistic", "bernoulli"), NA_character_, 1, NULL))
    expect_error(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                             evaluations = 200, surrogate = "gpr", noise_fun = bad, silent = TRUE),
                 '"logistic".*"bernoulli".*function')
})

run_nf <- function(seed, ...) {
  set.seed(seed)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                     evaluations = 200, silent = TRUE, ...))
  ds[c("time_used", "call", "simfun", "costfun", "fit", "settings")] <- NULL
  ds
}

test_that("noise: 'logistic' equals 'bernoulli' for logreg, user noise_fun and use_noise = FALSE", {
  expect_identical(run_nf(5), run_nf(5, noise_fun = "bernoulli"))      # logreg (default 1-D surrogate)
  expect_identical(run_nf(5, surrogate = "gpr", use_noise = FALSE),
                   run_nf(5, surrogate = "gpr", use_noise = FALSE, noise_fun = "bernoulli"))
  expect_identical(run_nf(5, ci = .3), run_nf(5, ci = .3, noise_fun = "bernoulli"))  # SE GPR of the ci rule
})

test_that("noise: the default for gpr is 'logistic' and differs from 'bernoulli'", {
  d0 <- run_nf(8, surrogate = "gpr")
  d1 <- run_nf(8, surrogate = "gpr", noise_fun = "logistic")
  d2 <- run_nf(8, surrogate = "gpr", noise_fun = "bernoulli")
  expect_identical(d0, d1)
  expect_false(identical(d0$final, d2$final) && identical(d0$dat, d2$dat))
})

test_that("noise: both methods run end to end with gpr in 1-D and 2-D", {
  f2 <- function(n, k) stats::rbinom(1, 1, stats::pnorm(-2.5 + .02 * n + .1 * k)) == 1
  for (m in c("bernoulli", "logistic")) {
    set.seed(6)
    ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                       evaluations = 200, surrogate = "gpr", noise_fun = m, silent = TRUE))
    expect_true(is.finite(ds$final$power), info = m)
    set.seed(7)
    ds <- suppressWarnings(find.design(simfun = f2, boundaries = list(n = c(10, 100), k = c(1, 20)),
                                       costfun = function(n, k) 2 * n + 5 * k, power = .8, evaluations = 300,
                                       noise_fun = m, silent = TRUE))
    expect_true(is.finite(ds$final$power), info = m)
  }
})

test_that("noise: continue keeps the stored noise_fun; results without settings keep \"bernoulli\"", {
  set.seed(9)
  ds1 <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 200, surrogate = "gpr", noise_fun = "bernoulli",
                                      silent = TRUE))
  set.seed(10)
  ds2 <- suppressWarnings(find.design(continue = ds1, evaluations = 60, silent = TRUE))
  expect_identical(ds2$settings$noise_fun, "bernoulli")
  ds1$settings <- NULL   # as stored by mlpwr 1.1.1
  set.seed(10)
  ds3 <- suppressWarnings(find.design(continue = ds1, evaluations = 60, silent = TRUE))
  expect_identical(ds3$settings$noise_fun, "bernoulli")
  set.seed(10)
  ds4 <- suppressWarnings(find.design(continue = ds1, evaluations = 60, noise_fun = "logistic", silent = TRUE))
  expect_identical(ds4$settings$noise_fun, "logistic")
})

test_that("noise: 'logistic' with results other than 0/1 falls back to 'bernoulli' (1.1.1)", {
  prop <- function(n) mean(stats::runif(5) < stats::plogis(-2 + n / 50))
  fd <- function(...) {
    set.seed(1)
    suppressWarnings(find.design(simfun = prop, boundaries = c(20, 200), power = .5,
      evaluations = 200, surrogate = "gpr", silent = TRUE, ...))
  }
  expect_message(ds_def <- fd(), 'noise_fun = "bernoulli".*instead of the default "logistic"')
  ds_ber <- suppressMessages(fd(noise_fun = "bernoulli"))
  expect_identical(ds_def$settings$noise_fun, "bernoulli")
  expect_identical(ds_def$final$design, ds_ber$final$design)
  expect_identical(ds_def$final$power, ds_ber$final$power)
  # binary results later in a search do not matter; non-binary ones in the
  # noise variances fall back silently
  d <- list(list(x = c(n = 1), y = c(.2, .4)), list(x = c(n = 2), y = c(.5, .7, 1)))
  expect_identical(mlpwr:::noise_variances(d, data.frame(n = 1:2), "logistic"),
                   mlpwr:::noise_variances(d, data.frame(n = 1:2), "bernoulli"))
  # "bernoulli" (mlpwr 1.1.1), the other surrogates and the SE GPR of the ci
  # rule accept proportions, as in mlpwr 1.1.1
  run <- function(...) {
    set.seed(1)
    suppressWarnings(suppressMessages(find.design(simfun = prop, boundaries = c(20, 200),
      power = .5, evaluations = 200, silent = TRUE, ...)))
  }
  expect_true(is.finite(run(surrogate = "gpr", noise_fun = "bernoulli")$final$se))
  expect_true(is.finite(run(surrogate = "logreg")$final$se))
  expect_true(is.finite(run(surrogate = "logreg", ci = .01)$final$se))
  # TRUE/FALSE and 0/1 are fine
  d <- list(list(x = c(n = 1), y = c(TRUE, FALSE)), list(x = c(n = 2), y = c(1, 0, 1)))
  expect_true(mlpwr:::binary_results(d))
  # a noise_fun function accepts any results
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = function(n, k) stats::rnorm(1, n / 300, .1),
                                     boundaries = list(n = c(20, 200), k = c(1, 2)),
                                     power = .5, evaluations = 200, silent = TRUE,
                                     noise_fun = function(e) stats::var(e$y) / length(e$y)))
  expect_s3_class(ds, "designresult")
  # without noise variances (gpr, use_noise = FALSE, no ci) they are not needed
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = function(n, k) stats::rnorm(1, n / 300, .1),
                                     boundaries = list(n = c(20, 200), k = c(1, 2)),
                                     power = .5, evaluations = 200, silent = TRUE,
                                     use_noise = FALSE))
  expect_s3_class(ds, "designresult")
})
