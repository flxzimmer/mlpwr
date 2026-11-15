test_that("counters and fit ignore same-named global variables", {
  run <- function() {
    set.seed(1)
    find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                evaluations = 300, silent = TRUE)
  }
  ref <- suppressWarnings(run())

  # exists() searched the global environment, so plant variables there
  planted <- c("n_updates", "n.bad.fits", "n.bad.predictions", "fit")
  old <- mget(planted, envir = globalenv(), ifnotfound = list(NULL))
  on.exit({
    rm(list = planted, envir = globalenv())
    for (nm in planted) if (!is.null(old[[nm]])) assign(nm, old[[nm]], envir = globalenv())
  })
  for (nm in planted[1:3]) assign(nm, 1000, envir = globalenv())
  assign("fit", list(last.mod = "not a model"), envir = globalenv())
  env_run <- suppressWarnings(run())

  expect_lt(ref$n_updates, 1000)
  expect_identical(env_run$n_updates, ref$n_updates)
  expect_identical(env_run$n.bad.fits, ref$n.bad.fits)
  expect_identical(env_run$n.bad.predictions, ref$n.bad.predictions)
  expect_identical(env_run$final$design, ref$final$design)
})
