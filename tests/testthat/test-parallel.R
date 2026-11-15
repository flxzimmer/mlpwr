# Opt-in parallel simfun evaluation (find.design(parallel = ...)).
# Tests that start workers are skipped on CRAN. Workers load mlpwr from the
# library paths of the test process, i.e. the installed version.

skip_par <- function() {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("parallelly")
}

# parallel = TRUE on a one-core machine: the stream path, in process (no workers
# are started, so these tests also run on CRAN)
local_mocked_bindings(n_cores = function() 1L)

par_run <- function(parallel, seed = 1, simfun = sim_ttest, evaluations = 200, ...) {
  set.seed(seed)
  ds <- suppressWarnings(find.design(simfun = simfun, boundaries = c(20, 200), power = .8,
                                     evaluations = evaluations, silent = TRUE,
                                     parallel = parallel, ...))
  # the main RNG state after the search is part of the result
  list(final = ds$final, dat = ds$dat, n_updates = ds$n_updates, after = runif(1))
}

test_that("parallel: invalid values are rejected before any simulation", {
  for (p in list(0, -1, 1.5, NA, "yes", c(TRUE, TRUE)))
    expect_error(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                             evaluations = 50, silent = TRUE, parallel = p),
                 "parallel must be", label = deparse(p))
})

test_that("parallel: a missing future.apply gives an install.packages() hint", {
  local_mocked_bindings(pkg_available = function(pkg) pkg != "future.apply")
  expect_error(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                           evaluations = 50, silent = TRUE, parallel = TRUE),
               'install.packages("future.apply")', fixed = TRUE)
  local_mocked_bindings(pkg_available = function(pkg) FALSE)
  expect_error(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                           evaluations = 50, silent = TRUE, parallel = 2),
               'install.packages(c("future", "future.apply", "parallelly"))', fixed = TRUE)
  # the default does not need the packages
  set.seed(1)
  expect_no_error(suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200),
                                               power = .8, evaluations = 50, silent = TRUE)))
})

test_that("parallel: in-process mode (1 worker) is reproducible and silences simfun output", {
  skip_par()
  noisy <- function(n) {
    cat("simfun output\n"); message("simfun message"); warning("simfun warning")
    sim_ttest(n)
  }
  set.seed(1)
  expect_silent(ds <- find.design(simfun = noisy, boundaries = c(20, 200), power = .8,
                                  evaluations = 100, silent = TRUE, parallel = TRUE))
  expect_identical(par_run(TRUE), par_run(TRUE))
  # a different seed gives different simulations
  expect_false(identical(par_run(TRUE)$dat, par_run(TRUE, seed = 2)$dat))
})

test_that("parallel: an explicit 1 is the ordinary sequential mode", {
  set.seed(1)
  expect_silent(r1 <- par_run(1))
  expect_identical(r1, par_run(FALSE))
})

test_that("parallel: results don't depend on the number of workers", {
  skip_on_cran()
  skip_par()
  r1 <- par_run(TRUE)
  expect_identical(par_run(2), r1)
  expect_identical(par_run(4), r1)
})

test_that("parallel: the startup message names the number of workers", {
  skip_par()
  set.seed(1)
  expect_message(capture.output(suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200),
                                              power = .8, evaluations = 50, parallel = TRUE))),
                 "Running simulations on 1 parallel worker")
})

test_that("parallel: a simfun error names the design", {
  skip_on_cran()
  skip_par()
  bad <- function(n) if (n > 150) stop("boom") else TRUE
  set.seed(1)
  expect_error(find.design(simfun = bad, boundaries = c(20, 200), power = .8,
                           evaluations = 100, silent = TRUE, parallel = 2),
               "simfun failed at design n = 200.*boom")
  bad2 <- function(n, k) if (k > 3) stop("too many groups") else TRUE
  set.seed(1)
  expect_error(find.design(simfun = bad2, boundaries = list(n = c(10, 50), k = c(1, 5)),
                           power = .8, evaluations = 100, silent = TRUE, parallel = TRUE),
               "simfun failed at design n = [0-9]+, k = [45].*too many groups")
})

test_that("parallel: the simfun wrapper is undone also when mlpwr keeps srcrefs", {
  # R_KEEP_PKG_SOURCE=yes (GitHub Actions) installs fix.argtype() with srcrefs,
  # so the body of the wrapper has srcref attributes
  user_fun <- function(n, k) n + k
  e <- new.env()
  assign("fun", user_fun, envir = e)
  w <- eval(parse(text = "function(x) {\n    do.call(fun, as.list(x))\n}",
                  keep.source = TRUE))
  environment(w) <- e
  expect_false(is.null(attr(body(w), "srcref")))
  u <- mlpwr:::unwrap_simfun(w)
  expect_true(u$islist)
  expect_identical(u$fun, user_fun)
  # and the wrapper as built by fix.argtype()
  u2 <- mlpwr:::unwrap_simfun(mlpwr:::fix.argtype(user_fun, list(n = c(1, 2), k = c(1, 2))))
  expect_true(u2$islist)
  expect_identical(u2$fun, user_fun)
  # an ordinary function(x) simfun is left alone
  f <- function(x) x[1]
  expect_identical(mlpwr:::unwrap_simfun(f), list(fun = f, islist = FALSE))
})

test_that("parallel: globals and package functions used in simfun reach the workers", {
  skip_on_cran()
  skip_par()
  skip_if_not_installed("pwr")
  # defined in the global environment, as a user script would
  ge <- globalenv()
  assign("par_test_shift", 0.4, envir = ge)
  assign("par_test_helper", function(m) m - mean(m) + stats::rnorm(length(m)), envir = ge)
  sf <- function(n, k) {
    x <- par_test_helper(rnorm(n * k))
    # a package function called without pkg::, from a package attached here
    stopifnot(pwr.t.test(n = 20, d = .5)$power > 0)
    t.test(x, mu = par_test_shift / sqrt(k))$p.value < .05
  }
  environment(sf) <- ge
  attached <- "package:pwr" %in% search()
  suppressPackageStartupMessages(library(pwr))
  on.exit({
    rm(par_test_shift, par_test_helper, envir = ge)
    if (!attached) detach("package:pwr")
  }, add = TRUE)
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sf, boundaries = list(n = c(10, 60), k = c(1, 5)),
                                     costfun = function(n, k) n + 5 * k, power = .8,
                                     evaluations = 200, silent = TRUE, parallel = 2))
  expect_s3_class(ds, "designresult")
  expect_gte(ds$evaluations_used, 200)
})

test_that("parallel: the user's future plan is restored, also after an error", {
  skip_par()
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  par_run(TRUE)
  expect_true(inherits(future::plan(), "sequential"))
  bad <- function(n) stop("boom")
  set.seed(1)
  expect_error(find.design(simfun = bad, boundaries = c(20, 200), power = .8,
                           evaluations = 50, silent = TRUE, parallel = TRUE), "boom")
  expect_true(inherits(future::plan(), "sequential"))

  skip_on_cran()
  future::plan(future::multisession, workers = 2)
  # an explicit worker count replaces the user's plan for the call only
  par_run(2)
  expect_true(inherits(future::plan(), "multisession"))
  # parallel = TRUE uses the user's (non-sequential) plan as is
  set.seed(1)
  expect_message(capture.output(suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200),
                                              power = .8, evaluations = 50, parallel = TRUE))),
                 "2 parallel workers (using your future::plan())", fixed = TRUE)
  expect_true(inherits(future::plan(), "multisession"))
})

test_that("parallel: continue reuses the setting", {
  skip_par()
  set.seed(1)
  ds1 <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 100, silent = TRUE, parallel = TRUE))
  expect_identical(ds1$settings$parallel, TRUE)
  set.seed(2)
  expect_message(capture.output(ds2 <- suppressWarnings(find.design(continue = ds1,
                                                                    evaluations = 40))),
                 "Running simulations on 1 parallel worker")
  expect_identical(ds2$settings$parallel, TRUE)
  # explicitly given wins
  set.seed(2)
  ds3 <- suppressWarnings(find.design(continue = ds1, evaluations = 40, silent = TRUE,
                                      parallel = FALSE))
  expect_false(ds3$settings$parallel)
  # the default is stored as FALSE
  set.seed(1)
  ds0 <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 50, silent = TRUE))
  expect_false(ds0$settings$parallel)
})

test_that("parallel: autosave works", {
  skip_par()
  d <- tempfile("autosave_par")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                     evaluations = 60, silent = TRUE, parallel = TRUE,
                                     autosave_dir = d))
  e <- new.env()
  load(file.path(d, "dat_autosave.RData"), envir = e)
  expect_identical(e$dat, ds$dat)
})

test_that("parallel: minrun in addval keeps sampling until there is variance", {
  skip_par()
  par <- list(nworkers = 1, dnames = "n")
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  set.seed(1)
  dat <- addval(simfun = function(n) runif(1) < .1, points = matrix(c(20, 40, 60), ncol = 1),
                each = 2, minrun = TRUE, par = par)
  for (d in dat) expect_gt(stats::var(d$y), 0)
})
