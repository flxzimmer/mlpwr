test_that("continue carries over the settings of the previous search", {
  agg <- function(x) mean(x)
  set.seed(1)
  ds1 <- suppressWarnings(
    find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                evaluations = 200, setsize = 30, ci_perc = .9, time = 1e5,
                use_noise = FALSE, control = list(covtype = "gauss"),
                aggregate_fun = agg, silent = TRUE)
  )
  expect_identical(ds1$settings$setsize, 30)
  set.seed(2)
  ds2 <- suppressWarnings(find.design(continue = ds1, evaluations = 90, silent = TRUE))
  for (nm in c("setsize", "ci_perc", "time", "use_noise", "control", "integer", "noise_fun"))
    expect_identical(ds2$settings[[nm]], ds1$settings[[nm]], label = nm)
  expect_identical(ds2$aggregate_fun, agg)
  # use_noise = FALSE carried: no SE
  expect_true(is.na(ds2$final$se))
  # 90 more evaluations in updates of setsize 30 (not recomputed from evaluations)
  expect_equal(ds2$evaluations_used, ds1$evaluations_used + 90)
  expect_identical(ds2$n_updates, 3)

  # explicitly given arguments still win
  set.seed(2)
  ds3 <- suppressWarnings(find.design(continue = ds1, evaluations = 40, setsize = 20,
                                      use_noise = TRUE, silent = TRUE))
  expect_identical(ds3$settings$setsize, 20)
  expect_true(ds3$settings$use_noise)
  expect_false(is.na(ds3$final$se))
})

test_that("results without stored settings (mlpwr <= 1.1.1) can still be continued", {
  set.seed(1)
  ds1 <- suppressWarnings(
    find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                evaluations = 200, silent = TRUE)
  )
  ds1$settings <- NULL
  set.seed(2)
  ds2 <- suppressWarnings(find.design(continue = ds1, evaluations = 100, silent = TRUE))
  expect_equal(ds2$evaluations_used, ds1$evaluations_used + 100)
  expect_identical(ds2$settings$setsize, ceiling(100 * .2 / 4))
  # the methods of mlpwr 1.1.1 are kept
  expect_identical(ds2$settings$noise_fun, "bernoulli")
  expect_identical(ds2$settings$optimizer, "genoud")
})

test_that("a result made by mlpwr 1.1.1 is continued with its noise_fun and optimizer", {
  # made with CRAN mlpwr 1.1.1 (2-D GPR search, 300 evaluations, seed 11)
  ds1 <- readRDS(test_path("fixtures", "designresult_1.1.1.rds"))
  expect_null(ds1$settings)
  run <- function(...) {
    set.seed(2)
    suppressWarnings(find.design(continue = ds1, evaluations = 60, silent = TRUE, ...))
  }
  ds2 <- run()
  expect_identical(ds2$settings$noise_fun, "bernoulli")
  expect_identical(ds2$settings$optimizer, "genoud")
  expect_identical(ds2$aggregate_fun, ds1$aggregate_fun)
  expect_equal(ds2$evaluations_used, ds1$evaluations_used + 60)
  # the same as giving these settings explicitly
  ds3 <- run(noise_fun = "bernoulli", optimizer = "genoud")
  expect_identical(ds3$final, ds2$final)
  expect_identical(ds3$dat, ds2$dat)
  # explicitly given values still win
  ds4 <- run(noise_fun = "logistic", optimizer = "auto")
  expect_identical(ds4$settings$noise_fun, "logistic")
  expect_identical(ds4$settings$optimizer, "auto")
})

test_that("time or ci alone can end the search (evaluations = NULL)", {
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200),
                                     power = .8, evaluations = NULL, time = 1,
                                     silent = TRUE))
  expect_s3_class(ds, "designresult")
  # setsize is that of the default evaluations = 4000
  expect_identical(ds$settings$setsize, ceiling(4000 * .2 / 4))
  expect_true(is.finite(ds$final$power))
  # continue a time-only search, also with evaluations = NULL
  set.seed(2)
  ds2 <- suppressWarnings(find.design(continue = ds, evaluations = NULL, silent = TRUE))
  expect_gt(ds2$evaluations_used, ds$evaluations_used)
  set.seed(1)
  ds3 <- suppressWarnings(suppressMessages(find.design(simfun = sim_ttest,
    boundaries = c(20, 200), power = .8, evaluations = NULL, ci = .5, setsize = 20,
    silent = TRUE)))
  expect_identical(ds3$n_updates, 0)
  # no termination rule at all would never stop
  expect_error(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                           evaluations = NULL, silent = TRUE),
               "evaluations, ci and time")
})

test_that("continue uses an explicitly given power or cost", {
  set.seed(1)
  ds1 <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 150, silent = TRUE))
  set.seed(2)
  same <- suppressWarnings(find.design(continue = ds1, evaluations = 50, silent = TRUE))
  expect_identical(same$power, .8)
  set.seed(2)
  newpow <- suppressWarnings(find.design(continue = ds1, power = .6, evaluations = 50, silent = TRUE))
  expect_identical(newpow$power, .6)
  set.seed(2)
  tocost <- suppressWarnings(find.design(continue = ds1, cost = 100, evaluations = 50, silent = TRUE))
  expect_null(tocost$power)
  expect_identical(tocost$cost, 100)
})
