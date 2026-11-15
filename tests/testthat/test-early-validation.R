test_that("power/cost errors come before any simulation", {
  set.seed(1)
  calls <- 0
  counting <- function(n) { calls <<- calls + 1; TRUE }
  expect_error(find.design(simfun = counting, boundaries = c(20, 200), evaluations = 400,
                           silent = TRUE),
               "Either the power or cost argument must be supplied")
  expect_error(find.design(simfun = counting, boundaries = c(20, 200), power = .8,
                           cost = 100, evaluations = 400, silent = TRUE),
               "Only one of power or cost argument can be supplied")
  expect_identical(calls, 0)
})

test_that("the ci/surrogate message also comes before the simulations", {
  set.seed(1)
  calls <- 0
  counting <- function(n) { calls <<- calls + 1; TRUE }
  expect_message(
    expect_error(find.design(simfun = counting, boundaries = c(20, 200), ci = .1,
                             evaluations = 400, silent = TRUE), "Either the power"),
    "Additionally fitting a GPR")
  expect_identical(calls, 0)
})

test_that("power outside (0, 1) gives a clear error before any simulation", {
  calls <- 0
  counting <- function(n) { calls <<- calls + 1; TRUE }
  for (pw in list(0, 1, 1.2, -.5, 80))
    expect_error(find.design(simfun = counting, boundaries = c(20, 200), power = pw,
                             evaluations = 400, silent = TRUE),
                 "power must be strictly between 0 and 1")
  for (pw in list(NA, NA_real_, c(.8, .9), ".8", Inf))
    expect_error(find.design(simfun = counting, boundaries = c(20, 200), power = pw,
                             evaluations = 400, silent = TRUE),
                 "power must be a single number")
  expect_identical(calls, 0)
})

test_that("a custom criterion can have a target of 1 or more", {
  # e.g. a ratio of error rates (vignette("extensions")): any target is allowed
  # with a custom aggregate_fun or noise_fun
  sf <- function(n) stats::rnorm(1, mean = n / 100)
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sf, boundaries = c(20, 200), power = 1.2,
                                     aggregate_fun = function(x) mean(x), surrogate = "reg",
                                     use_noise = FALSE, evaluations = 200, silent = TRUE))
  expect_s3_class(ds, "designresult")
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sf, boundaries = c(20, 200), power = 1.2,
                                     noise_fun = function(x) stats::var(x$y) / length(x$y),
                                     surrogate = "gpr", evaluations = 200, silent = TRUE))
  expect_s3_class(ds, "designresult")
})

test_that("an invalid surrogate gives a clear error before any simulation", {
  calls <- 0
  counting <- function(n) { calls <<- calls + 1; TRUE }
  for (sg in list("GPR", "Logreg", "kriging", NA_character_, 1))
    expect_error(find.design(simfun = counting, boundaries = c(20, 200), power = .8,
                             surrogate = sg, evaluations = 400, silent = TRUE),
                 'surrogate must be "gpr", "svr", "logreg" or "reg"')
  expect_identical(calls, 0)
})

test_that("logreg and reg with several design parameters give a clear error", {
  calls <- 0
  counting <- function(n, k) { calls <<- calls + 1; TRUE }
  for (sg in c("logreg", "reg"))
    expect_error(find.design(simfun = counting, boundaries = list(n = c(20, 200), k = c(1, 5)),
                             costfun = function(n, k) n + 5 * k, power = .8,
                             surrogate = sg, evaluations = 400, silent = TRUE),
                 paste0('surrogate = "', sg, '" works with one design parameter only'))
  expect_identical(calls, 0)
})
