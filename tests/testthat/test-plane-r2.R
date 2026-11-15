# The plane check's R^2 (plane_r2) must equal summary(lm(y ~ ., ...))$r.squared
# bit for bit, so the accept/reject decision of every fit stays the same.

old_r2 <- function(xvars, y) {
  suppressWarnings(summary(stats::lm(y ~ ., cbind(xvars, y = y)))$r.squared)
}

test_that("plane_r2 is bit-identical to summary(lm())$r.squared", {
  set.seed(42)
  for (d in 1:3) for (n in c(3, 5, 9, 25, 60)) for (k in 1:6) {
    if (n <= d) next
    xvars <- as.data.frame(matrix(round(runif(n * d, 1, 500)), n, d))
    names(xvars) <- c("n", "k", "m")[seq_len(d)]
    y <- switch(k,
      runif(n),                                         # noise
      0.3 + as.matrix(xvars) %*% rep(1e-3, d) + rnorm(n, sd = 1e-4),  # near plane
      1 - exp(-xvars[[1]] / 100),                       # curve
      rep(0.5, n),                                      # flat
      as.numeric(as.matrix(xvars) %*% rep(1e-3, d)),    # exact plane
      plogis(rnorm(n)))
    y <- as.numeric(y)
    expect_identical(mlpwr:::plane_r2(xvars, y), old_r2(xvars, y))
  }
  # collinear and constant design columns, integer columns
  xv <- data.frame(n = c(10L, 20L, 30L, 40L, 50L), k = c(2L, 4L, 6L, 8L, 10L), m = 3L)
  y <- c(.1, .4, .35, .8, .9)
  expect_identical(mlpwr:::plane_r2(xv, y), old_r2(xv, y))
  # fallbacks: missing values, a parameter named y
  y2 <- c(.1, NA, .35, .8, .9)
  expect_identical(mlpwr:::plane_r2(xv, y2), old_r2(xv, y2))
  xy <- data.frame(y = c(1, 2, 3, 4, 7), k = c(3, 1, 4, 1, 5))
  expect_identical(mlpwr:::plane_r2(xy, y), old_r2(xy, y))
})
