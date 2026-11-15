test_that("autosave writes dat_autosave.RData inside autosave_dir", {
  dir <- tempfile("autosave")
  dir.create(dir)
  on.exit(unlink(c(dir, paste0(dir, "dat_autosave.RData")), recursive = TRUE))
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                     evaluations = 100, silent = TRUE, autosave_dir = dir))
  f <- file.path(dir, "dat_autosave.RData")
  expect_true(file.exists(f))
  expect_false(file.exists(paste0(dir, "dat_autosave.RData")))
  env <- new.env()
  load(f, envir = env)
  expect_identical(env$dat, ds$dat)
})

test_that("a missing autosave_dir is created", {
  dir <- file.path(tempfile("autosave_new"), "sub")
  on.exit(unlink(dirname(dir), recursive = TRUE))
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = function(n) stats::t.test(stats::rnorm(n), mu = 0.3)$p.value < .05,
                                     boundaries = c(20, 200), power = .8,
                                     evaluations = 100, silent = TRUE, autosave_dir = dir))
  expect_true(file.exists(file.path(dir, "dat_autosave.RData")))
})

test_that("an invalid autosave_dir fails before any simulation", {
  calls <- 0
  sf <- function(n) { calls <<- calls + 1; stats::t.test(stats::rnorm(n), mu = 0.3)$p.value < .05 }
  for (bad in list(c(tempdir(), tempdir()), NA_character_, 1))
    expect_error(find.design(simfun = sf, boundaries = c(20, 200), power = .8,
                             evaluations = 100, silent = TRUE, autosave_dir = bad),
                 "autosave_dir must be a single directory name")
  expect_identical(calls, 0)
})
