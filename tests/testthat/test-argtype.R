test_that("fix.argtype ignores defaulted non-design arguments", {
  b <- list(n = c(1, 10), k = c(1, 10))
  vec <- function(x, verbose = FALSE) paste(length(x), verbose)
  expect_identical(mlpwr:::fix.argtype(vec, b)(c(3, 4)), "2 FALSE")
  vecdots <- function(x, ...) length(x)
  expect_identical(mlpwr:::fix.argtype(vecdots, b)(c(3, 4)), 2L)

  # unchanged cases
  sep <- function(n, k) n * 10 + k
  expect_identical(mlpwr:::fix.argtype(sep, b)(c(3, 4)), 34)
  sepdef <- function(n, k = 5) n * 10 + k
  expect_identical(mlpwr:::fix.argtype(sepdef, b)(c(3, 4)), 34)
  sepextra <- function(n, k, verbose = FALSE) n * 10 + k
  expect_identical(mlpwr:::fix.argtype(sepextra, b)(c(3, 4)), 34)
  one <- function(x) sum(x)
  expect_identical(mlpwr:::fix.argtype(one, b)(c(3, 4)), 7)
  # unnamed boundaries: as before
  expect_identical(mlpwr:::fix.argtype(sepdef, unname(b))(c(3, 4)), 34)
  # 1-D: always the plain function
  expect_identical(mlpwr:::fix.argtype(vec, list(n = c(1, 10))), vec)
})

test_that("a 2-D vector simfun with a defaulted extra argument works in find.design", {
  g <- function(x, verbose = FALSE) {
    stopifnot(length(x) == 2)
    stats::t.test(stats::rnorm(x[1] * x[2] / 10), mu = .3)$p.value < .05
  }
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = g, boundaries = list(n = c(10, 50), k = c(2, 10)),
                                     costfun = function(x) x[1] * x[2], power = .8,
                                     evaluations = 150, silent = TRUE))
  expect_length(ds$final$design, 2)
})
