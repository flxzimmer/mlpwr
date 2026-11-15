# A design that occurs twice within one addval() batch was stored twice,
# and the next addval() at that design failed (dat[[ind]] with two indices).

test_that("duplicates within one batch are merged into one design", {
  f <- function(n) n
  pts <- data.frame(n = c(1, 2, 1))
  dat <- mlpwr:::addval(f, points = pts, each = 2)
  expect_length(dat, 2)
  expect_identical(as.numeric(dat[[1]]$x), 1)
  expect_identical(dat[[1]]$y, c(1, 1, 1, 1))
  expect_identical(dat[[2]]$y, c(2, 2))
  # a later batch at the same design adds to it
  dat <- mlpwr:::addval(f, dat = dat, points = data.frame(n = 1), each = 1)
  expect_length(dat, 2)
  expect_length(dat[[1]]$y, 5)
})

test_that("find.design works on narrow integer boundaries", {
  set.seed(1)
  f <- function(n) stats::rbinom(1, 1, c(.5, .8, .95)[n]) == 1
  ds <- suppressWarnings(find.design(simfun = f, boundaries = c(1, 3),
                                     power = .8, evaluations = 200, silent = TRUE))
  expect_s3_class(ds, "designresult")
  ns <- vapply(ds$dat, function(d) as.numeric(d$x), numeric(1))
  expect_false(anyDuplicated(ns) > 0)
})

test_that("parallel path: duplicates within one batch are merged too", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("parallelly")
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  set.seed(1)
  f <- function(n) n
  par <- list(nworkers = 1L, dnames = "n")
  pts <- data.frame(n = c(1, 2, 1))
  dat <- mlpwr:::addval(f, points = pts, each = 2, par = par)
  expect_length(dat, 2)
  expect_identical(as.numeric(dat[[1]]$x), 1)
  expect_identical(dat[[1]]$y, c(1, 1, 1, 1))
  expect_identical(dat[[2]]$y, c(2, 2))
  dat <- mlpwr:::addval(f, dat = dat, points = data.frame(n = 1), each = 1, par = par)
  expect_length(dat, 2)
  expect_length(dat[[1]]$y, 5)
})
