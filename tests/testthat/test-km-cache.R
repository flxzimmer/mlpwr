km_fit_quiet <- function(...) {
  out <- NULL
  utils::capture.output(out <- tryCatch(suppressWarnings(DiceKriging::km(..., control = list(trace = FALSE))),
                                        error = function(e) NULL))
  out
}

km_random_case <- function(d, covtype, extra, seed) {
  set.seed(seed)
  n <- 6 + 3 * d
  X <- as.data.frame(matrix(round(stats::runif(n * d, 1, 40)), n, d))
  names(X) <- paste0("v", seq_len(d))
  X <- unique(X)
  y <- stats::plogis(-2 + rowSums(sweep(as.matrix(X), 2, seq_len(d), "/")) / 15 + stats::rnorm(nrow(X), sd = .1))
  args <- list(design = X, response = y, covtype = covtype)
  if (identical(extra, "noise")) args$noise.var <- stats::runif(nrow(X), 1e-4, 4e-3)
  if (identical(extra, "nugget")) args$nugget <- 1e-3
  if (identical(extra, "nugget.estim")) args$nugget.estim <- TRUE
  mod <- do.call(km_fit_quiet, args)
  # new points: random, on an integer grid, and design points
  G <- as.matrix(expand.grid(lapply(seq_len(d), function(j) seq(1, 40, length.out = if (d == 3) 9 else 25))))
  Z <- rbind(matrix(stats::runif(30 * d, -5, 45), 30, d), G, as.matrix(X[1:3, , drop = FALSE]))
  list(mod = mod, Z = unname(Z), nd = nrow(X))
}

test_that("the km predictor matches predict.km (all covtypes, nugget/noise, 1-3 D)", {
  rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-3))
  k <- 0
  nfit <- 0
  for (d in 1:3) for (covtype in c("gauss", "exp", "matern3_2", "matern5_2", "powexp"))
    for (extra in c("none", "noise", "nugget", "nugget.estim")) {
      k <- k + 1
      cs <- km_random_case(d, covtype, extra, seed = 100 + k)
      mod <- cs$mod
      if (is.null(mod)) next
      nfit <- nfit + 1
      Z <- cs$Z
      ref <- DiceKriging::predict.km(mod, newdata = Z, type = "UK", checkNames = FALSE,
                                     light.return = TRUE)
      {
        kmc <- mlpwr:::km_cache(mod)
        info <- paste(d, covtype, extra)
        b <- kmc$meansd_batch(Z)
        expect_lt(rel(b$mean, ref$mean), 1e-10, label = info)
        # at design points with a nugget the sd is the root of a small
        # difference; compare variances there
        expect_lt(max(abs(b$sd^2 - ref$sd^2)) / mod@covariance@sd2, 1e-10, label = info)
        expect_lt(rel(b$sd[1:30], ref$sd[1:30]), 1e-10, label = info)
        expect_lt(rel(kmc$mean_screen(Z), ref$mean), 1e-10, label = info)
        expect_lt(rel(kmc$mean_batch(Z), ref$mean), 1e-10, label = info)
        idx <- c(1:5, 31:35, nrow(Z) - 0:2)
        pm <- vapply(idx, function(i) kmc$mean(Z[i, ]), 0)
        ps <- vapply(idx, function(i) kmc$sd(Z[i, ]), 0)
        expect_lt(rel(pm, ref$mean[idx]), 1e-10, label = info)
        expect_lt(rel(ps[1:10], ref$sd[idx][1:10]), 1e-10, label = info)
        cc <- DiceKriging::covMat1Mat2(mod@covariance, mod@X, Z[1:40, , drop = FALSE], nugget.flag = TRUE)
        expect_lt(max(abs(kmc$cross_cov(Z[1:40, , drop = FALSE]) - cc)) / mod@covariance@sd2, 1e-12,
                  label = info)
      }
    }
  expect_gt(nfit, 50)
})

test_that("blocked screening gives the same values as one block", {
  cs <- km_random_case(2, "matern5_2", "noise", seed = 7)
  kmc <- mlpwr:::km_cache(cs$mod)
  e <- environment(kmc$meansd_batch)
  full <- kmc$meansd_batch(cs$Z)
  e$screen_block <- 13 * cs$nd
  # equal up to rounding (block sizes can change the BLAS summation order)
  expect_equal(kmc$meansd_batch(cs$Z), full, tolerance = 1e-12)
  expect_equal(kmc$mean_screen(cs$Z), full$mean, tolerance = 1e-12)
})

test_that("other km models (iso, trend) are predicted like predict.km", {
  set.seed(3)
  X <- data.frame(a = stats::runif(10, 0, 10), b = stats::runif(10, 0, 5))
  y <- sin(X$a) + X$b / 3
  Z <- matrix(stats::runif(20, 0, 5), 10)
  for (m in list(km_fit_quiet(design = X, response = y, iso = TRUE),
                 km_fit_quiet(formula = ~a, design = X, response = y))) {
    kmc <- mlpwr:::km_cache(m)
    ref <- DiceKriging::predict.km(m, newdata = Z, type = "UK", checkNames = FALSE)
    expect_equal(kmc$meansd_batch(Z)$mean, ref$mean, tolerance = 1e-12)
    expect_equal(kmc$sd(Z[2, ]), ref$sd[2], tolerance = 1e-12)
  }
  expect_null(mlpwr:::km_cache(NULL))
})
