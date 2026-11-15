# Exact integer-grid search (find.design(optimizer = "auto"), the default)

get_pred <- function(...) mlpwr:::get.pred(...)

# fixed analytic surrogate on a 2-D design space
fake_fit <- function() {
  list(fitfun = function(x) 1 - exp(-sqrt(x[1]) / 5 - x[2] / 12),
       fitfun.sd = function(x) 0.015 + 0.01 * cos(x[1] / 7 + x[2] / 3),
       badfit = FALSE)
}
fake_dat <- function() {
  des <- expand.grid(n = c(10, 80, 150), k = c(5, 17, 30))
  lapply(seq_len(nrow(des)), function(i) list(x = as.numeric(des[i, ]), y = rep(c(0, 1), 10)))
}
bnd2 <- list(n = c(10, 150), k = c(5, 30))
# vectorised over the grid as in find.design() (see fix.argtype()); a plain
# function(x) that cannot be vectorised is scanned only on small grids
cf2 <- mlpwr:::fix.argtype(function(x) 5 * x[1] + 21.7 * x[2], bnd2,
  vectorise = TRUE)

# brute force over the grid with the pointwise objective, first minimum in
# lexicographic order (first parameter slowest)
brute <- function(fn, bnd) {
  G <- as.matrix(rev(expand.grid(rev(lapply(bnd, function(b) b[1]:b[2])))))
  v <- apply(G, 1, fn)
  as.numeric(G[which.min(v), ])
}

test_that("exact search finds the true grid optimum on a fixed surrogate", {
  fit <- fake_fit()
  mid <- sapply(bnd2, mean)
  relu <- function(x) ifelse(x > 0, x, 0)
  set.seed(1)
  p <- get_pred(fit = fit, dat = fake_dat(), power = .9, costfun = cf2, cost = NULL,
                boundaries = bnd2, task = "desiredpower", aggregate_fun = mean,
                integer = TRUE, use_noise = TRUE)
  fn <- function(x) relu(.9 - fit$fitfun(x) - fit$fitfun.sd(x) * .3) * 1e5 + cf2(x) / cf2(mid)
  expect_identical(as.numeric(p$points), brute(fn, bnd2))

  # without the SD term
  p <- get_pred(fit = fit, dat = fake_dat(), power = .9, costfun = cf2, cost = NULL,
                boundaries = bnd2, task = "desiredpower", aggregate_fun = mean,
                integer = TRUE, use_noise = FALSE)
  fn <- function(x) relu(.9 - fit$fitfun(x)) * 1e5 + cf2(x) / cf2(mid)
  expect_identical(as.numeric(p$points), brute(fn, bnd2))

  # cost threshold
  p <- get_pred(fit = fit, dat = fake_dat(), power = NULL, costfun = cf2, cost = 600,
                boundaries = bnd2, task = "costthreshold", aggregate_fun = mean,
                integer = TRUE, use_noise = TRUE)
  fn <- function(x) relu(cf2(x) - 600) / 600 * 1e5 - fit$fitfun(x)
  expect_identical(as.numeric(p$points), brute(fn, bnd2))
})

test_that("exact search with the cached km and glm predictors matches brute force", {
  set.seed(3)
  dat <- fake_dat()
  for (i in seq_along(dat)) dat[[i]]$y <- stats::rbinom(30, 1, 1 - exp(-sqrt(dat[[i]]$x[1]) / 5 - dat[[i]]$x[2] / 12))
  fit <- mlpwr:::fit.surrogate(dat = dat, surrogate = "gpr", lastfit = NULL)
  mid <- sapply(bnd2, mean)
  relu <- function(x) ifelse(x > 0, x, 0)
  p <- get_pred(fit = fit, dat = dat, power = .95, costfun = cf2, cost = NULL,
                boundaries = bnd2, task = "desiredpower", aggregate_fun = mean,
                integer = TRUE, use_noise = TRUE)
  fn <- function(x) relu(.95 - fit$fitfun(x) - fit$fitfun.sd(x) * .3) * 1e5 + cf2(x) / cf2(mid)
  expect_identical(as.numeric(p$points), brute(fn, bnd2))

  # batch km predictions agree with the pointwise ones
  G <- as.matrix(expand.grid(10:150, 5:30))
  kmc <- environment(fit$fitfun)$kmc
  b <- kmc$meansd_batch(G)
  expect_equal(b$mean, apply(G, 1, fit$fitfun), tolerance = 1e-12)
  expect_equal(b$sd, apply(G, 1, fit$fitfun.sd), tolerance = 1e-10)

  # 1-D logreg (direct glm predictor)
  dat1 <- lapply(c(20, 60, 100, 140), function(n) list(x = n, y = stats::rbinom(40, 1, n / 160)))
  fit1 <- mlpwr:::fit.surrogate(dat = dat1, surrogate = "logreg", lastfit = NULL)
  linp <- environment(fit1$fitfun)$linp
  expect_equal(linp$predict_batch(matrix(20:140)), sapply(20:140, fit1$fitfun),
               tolerance = 1e-14, ignore_attr = TRUE)
  p <- get_pred(fit = fit1, dat = dat1, power = .6, costfun = function(x) sum(x), cost = NULL,
                boundaries = list(n = c(20, 140)), task = "desiredpower", aggregate_fun = mean,
                integer = TRUE, use_noise = TRUE)
  fn <- function(x) relu(.6 - fit1$fitfun(x)) * 1e5 + x / 80
  expect_identical(as.numeric(p$points), brute(fn, list(n = c(20, 140))))
})

test_that("ties go to the lowest lexicographic index", {
  srch <- mlpwr:::exact_grid_search
  D <- cbind(c(1, 1), c(4, 9))
  # constant objective: the lower corner
  expect_identical(srch(function(x) 0, D), c(1, 1))
  # three tied minima: the smallest first parameter wins, then the second
  fn <- function(x) if (all(x == c(3, 2)) || all(x == c(2, 8)) || all(x == c(2, 9))) -1 else 0
  expect_identical(srch(fn, D), c(2, 8))
  # the grid is enumerated with the first parameter slowest
  g <- mlpwr:::exact_grid_dims(D)
  expect_identical(mlpwr:::exact_grid_points(g, 0:10),
                   cbind(c(rep(1, 9), 2, 2), c(1:9, 1:2)))
  # a batch screen that is slightly off does not decide between near-ties
  fn <- function(x) sum(x)
  expect_identical(srch(fn, D, fn_batch = function(X, idx) rowSums(X) + c(1e-12, -1e-12)), c(1, 1))
})

test_that("optimizer = 'genoud' reproduces the old genoud call", {
  fit <- fake_fit()
  dat <- fake_dat()
  set.seed(7)
  p <- get_pred(fit = fit, dat = dat, power = .9, costfun = cf2, cost = NULL,
                boundaries = bnd2, task = "desiredpower", aggregate_fun = mean,
                integer = TRUE, use_noise = TRUE, optimizer = "genoud")
  s1 <- .Random.seed
  # mlpwr 1.1.1's call
  cands <- t(sapply(dat[1:4], function(d) d$x))
  mid <- sapply(bnd2, mean)
  relu <- function(x) ifelse(x > 0, x, 0)
  fn <- function(x) relu(.9 - fit$fitfun(x) - fit$fitfun.sd(x) * 3/10) * 10^5 + cf2(x)/cf2(mid)
  set.seed(7)
  old <- rgenoud::genoud(fn, nvars = 2, data.type.int = TRUE, Domains = cbind(c(10, 5), c(150, 30)),
                         print.level = 0, boundary.enforcement = 1, pop.size = 20,
                         starting.values = cands)
  expect_identical(as.numeric(p$points), old$par)
  expect_identical(s1, .Random.seed)
})

test_that("the exact search consumes the same random draws as genoud", {
  fit <- fake_fit()
  for (task in c("desiredpower", "costthreshold")) {
    pw <- if (task == "desiredpower") .9
    cs <- if (task == "costthreshold") 600
    seeds <- lapply(c("auto", "genoud"), function(o) {
      set.seed(11)
      get_pred(fit = fit, dat = fake_dat(), power = pw, costfun = cf2, cost = cs,
               boundaries = bnd2, task = task, aggregate_fun = mean,
               integer = TRUE, use_noise = TRUE, optimizer = o)
      .Random.seed
    })
    expect_identical(seeds[[1]], seeds[[2]])
  }
  # in 1-D genoud finds the grid optimum, so whole searches are identical
  ds <- lapply(c("auto", "genoud"), function(o) {
    set.seed(5)
    r <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 300, silent = TRUE, optimizer = o))
    list(final = r$final, dat = r$dat, seed = .Random.seed)
  })
  expect_identical(ds[[1]], ds[[2]])
})

test_that("the scan budget, plain genoud above it, and continuous designs", {
  fn <- function(x) sum((x - c(300.4, 400.6))^2)
  fb <- function(X, idx) (X[, 1] - 300.4)^2 + (X[, 2] - 400.6)^2
  sv <- rbind(c(10, 10), c(450, 450))
  od <- mlpwr:::optimize_design
  plain <- function(D) rgenoud::genoud(fn, nvars = 2, data.type.int = TRUE, Domains = D, print.level = 0,
                                       pop.size = 20, starting.values = sv, boundary.enforcement = 1)$par
  # grid points x designs <= 1e5: full scan (here 250 x 400 = 1e5)
  D <- cbind(c(1, 1), c(10, 25)); fq <- function(x) sum((x - c(3.4, 20.6))^2)
  set.seed(2); expect_identical(od(fq, D, TRUE, sv[1, , drop = FALSE], n_designs = 400, boundary.enforcement = 1), c(3, 21))
  # just above the budget (250 x 401): genoud, same draws as the plain call
  plainq <- function(D) rgenoud::genoud(fq, nvars = 2, data.type.int = TRUE, Domains = D, print.level = 0,
                                        pop.size = 20, starting.values = sv[1, , drop = FALSE], boundary.enforcement = 1)$par
  set.seed(2); a <- od(fq, D, TRUE, sv[1, , drop = FALSE], n_designs = 401, boundary.enforcement = 1)
  set.seed(2); expect_identical(a, plainq(D))
  # above the budget: exactly genoud (same point, same random draws)
  D <- cbind(c(1, 1), c(500, 501))
  set.seed(2); a <- od(fn, D, TRUE, sv, fn_batch = fb, n_designs = 20, boundary.enforcement = 1); sa <- .Random.seed
  set.seed(2); g <- plain(D)
  expect_identical(sa, .Random.seed)
  expect_identical(a, g)
  # "genoud" is the plain genoud call, whatever the grid
  set.seed(2); og <- od(fn, D, TRUE, sv, optimizer = "genoud", fn_batch = fb, n_designs = 20,
                        boundary.enforcement = 1)
  set.seed(2); expect_identical(og, plain(D))
  # continuous
  set.seed(2); a <- od(fn, cbind(c(1, 1), c(10, 10)), FALSE, sv[1, , drop = FALSE],
                       optimizer = "auto", boundary.enforcement = 2, gradient.check = FALSE)
  set.seed(2); g <- od(fn, cbind(c(1, 1), c(10, 10)), FALSE, sv[1, , drop = FALSE],
                       optimizer = "genoud", boundary.enforcement = 2, gradient.check = FALSE)
  expect_identical(a, g)
})

test_that("optimizer is checked, stored and reused by continue", {
  expect_error(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                           evaluations = 100, silent = TRUE, optimizer = "nope"))
  set.seed(1)
  ds1 <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 150, silent = TRUE, optimizer = "genoud"))
  expect_identical(ds1$settings$optimizer, "genoud")
  set.seed(2)
  ds2 <- suppressWarnings(find.design(continue = ds1, evaluations = 50, silent = TRUE))
  expect_identical(ds2$settings$optimizer, "genoud")
  set.seed(2)
  ds3 <- suppressWarnings(find.design(continue = ds1, evaluations = 50, silent = TRUE,
                                      optimizer = "auto"))
  expect_identical(ds3$settings$optimizer, "auto")
  # the default
  set.seed(1)
  ds4 <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                      evaluations = 100, silent = TRUE))
  expect_identical(ds4$settings$optimizer, "auto")
  # results without a stored optimizer continue with the default
  ds1$settings$optimizer <- NULL
  set.seed(2)
  ds5 <- suppressWarnings(find.design(continue = ds1, evaluations = 50, silent = TRUE))
  expect_identical(ds5$settings$optimizer, "auto")
})


test_that("grid costs: vectorised where it reproduces apply exactly, else apply", {
  b <- list(n = c(10, 150), k = c(5, 30))
  grid <- mlpwr:::exact_grid_dims(cbind(c(10, 5), c(150, 30)), Inf)
  X <- mlpwr:::exact_grid_points(grid, seq_len(grid$n) - 1)
  Xf <- X / 7  # non-integer values: rounding must match too
  Dom <- cbind(c(10, 5), c(150, 30))
  calls <- 0
  funs <- list(
    lin = function(n, k) 5 * n + 21.7 * k,
    extra = function(n, k, a = 3.1) a * n + k^1.5,
    pm = function(n, k) pmin(n, 40) * 2.3 + ifelse(k > 10, k, 0),
    iff = function(n, k) if (n > 10) n + k else 2 * k,
    mx = function(n, k) max(n, k),
    sm = function(n, k) sum(n, k),
    warn = function(n, k) { if (length(n) > 1) warning("vec"); n + k },
    chatty = function(n, k) { if (length(n) > 1) cat("hi\n"); n + k },
    rng = function(n, k) { if (length(n) > 1) stats::runif(1); n + k },
    counted = function(n, k) { calls <<- calls + 1; n * k }
  )
  for (nm in names(funs)) {
    cf <- mlpwr:::fix.argtype(funs[[nm]], b)
    expect_false(is.null(attr(cf, "vectorised")))
    vec_ok <- nm %in% c("lin", "extra", "pm", "counted")
    for (M in list(X, Xf)) {
      # costs that cannot be vectorised: pointwise on at most
      # exact_scan_pointwise rows, else NULL (the scan then uses genoud)
      if (!vec_ok) {
        expect_null(mlpwr:::grid_costs(cf, M, NULL, Dom), info = nm)
        M <- M[seq_len(mlpwr:::exact_scan_pointwise), ]
      }
      ref <- apply(M, 1, cf)
      set.seed(5); s0 <- .Random.seed
      expect_identical(mlpwr:::grid_costs(cf, M, NULL, Dom), ref, info = nm)
      expect_identical(.Random.seed, s0, info = nm)
      cache <- new.env(parent = emptyenv())
      expect_identical(mlpwr:::grid_costs(cf, M, seq_len(nrow(M)) - 1, Dom,
        cache), ref, info = nm)
      expect_identical(.Random.seed, s0, info = nm)
      expect_identical(cache$vectorised, vec_ok, info = nm)
    }
  }
  # vectorised: one call plus the check rows instead of one call per point
  calls <- 0
  cf <- mlpwr:::fix.argtype(funs$counted, b)
  mlpwr:::grid_costs(cf, X, seq_len(nrow(X)) - 1, Dom, new.env())
  expect_lt(calls, 20)
  # a failed vectorisation is not retried in the same search
  cf <- mlpwr:::fix.argtype(function(n, k) {
    calls <<- calls + 1; if (n > 10) n + k else k }, b)
  cache <- new.env(parent = emptyenv())
  calls <- 0
  mlpwr:::grid_costs(cf, X[1:50, ], 0:49, Dom, cache)
  expect_equal(calls, 50 + 1)
  calls <- 0
  mlpwr:::grid_costs(cf, X[51:100, ], 50:99, Dom, cache)
  expect_equal(calls, 50)

  # single-vector costfuns: vectorised over a data frame of the columns where
  # that is exact, else pointwise on at most exact_scan_pointwise points and
  # NULL (no scan) above
  one <- function(x) x[1] * 2 + x[2]
  cf1 <- mlpwr:::fix.argtype(one, b, vectorise = TRUE)
  expect_false(is.null(attr(cf1, "vectorised")))
  expect_null(attr(mlpwr:::fix.argtype(one, b), "vectorised"))
  cache <- new.env(parent = emptyenv())
  expect_identical(mlpwr:::grid_costs(cf1, X, seq_len(nrow(X)) - 1, Dom,
    cache), apply(X, 1, one))
  expect_true(cache$vectorised)
  cfl <- function(n, k) 5 * n + 7 * k
  cf2 <- mlpwr:::fix.argtype(function(x) do.call(cfl, as.list(x)), b,
    vectorise = TRUE)
  expect_identical(mlpwr:::grid_costs(cf2, X, NULL, Dom), apply(X, 1, cf2))
  calls <- 0
  sx <- mlpwr:::fix.argtype(function(x) { calls <<- calls + 1; sum(x) }, b,
    vectorise = TRUE)
  cache <- new.env(parent = emptyenv())
  expect_null(mlpwr:::grid_costs(sx, X, seq_len(nrow(X)) - 1, Dom, cache))
  expect_false(cache$vectorised)
  # later updates of the search skip the scan (genoud directly)
  expect_true(mlpwr:::scan_refused(cache, Dom))
  expect_false(mlpwr:::scan_refused(cache, Dom + 1))
  expect_lt(calls, 20)
  m <- mlpwr:::exact_scan_pointwise
  expect_identical(mlpwr:::grid_costs(sx, X[1:m, ], 0:(m - 1), Dom, cache),
    rowSums(X[1:m, ]))
  expect_null(mlpwr:::grid_costs(sx, X[1:(m + 1), ], NULL, Dom))
  # one design parameter: the costfun is called with the column vector
  c1 <- mlpwr:::fix.argtype(function(n) 3 * n + 1, list(n = c(1, 500)),
    vectorise = TRUE)
  X1 <- matrix(1:500)
  expect_identical(mlpwr:::grid_costs(c1, X1, NULL, cbind(1, 500)),
    3 * (1:500) + 1)
  for (f1 in list(function(x) 3 * x[1] + 1,
    function(x) do.call(function(n) 3 * n + 1, as.list(x)))) {
    c1 <- mlpwr:::fix.argtype(f1, list(n = c(1, 500)), vectorise = TRUE)
    cache <- new.env(parent = emptyenv())
    expect_identical(mlpwr:::grid_costs(c1, X1, 0:499, cbind(1, 500), cache),
      3 * (1:500) + 1)
    expect_true(cache$vectorised)
  }
  set.seed(2)
  Xr <- matrix(stats::rnorm(3000) * 10^sample(-12:12, 3000, TRUE), ncol = 3)
  expect_identical(rowSums(Xr), apply(Xr, 1, function(x) sum(x)))

  # the default costfun inside find.design is vectorised and gives the same
  # result as an equivalent costfun without the vectorised form
  simfun <- function(n, k) stats::rbinom(1, 1, 1 - exp(-sqrt(n) / 6 - sqrt(k) / 4))
  run <- function(cf) {
    set.seed(11)
    suppressWarnings(find.design(simfun = simfun,
      boundaries = list(n = c(2, 60), k = c(2, 40)), power = .8,
      evaluations = 300, costfun = cf, silent = TRUE))
  }
  d0 <- run(NULL)
  expect_false(is.null(attr(d0$costfun, "vectorised")))
  d1 <- run(function(x) x[1] + x[2])
  expect_identical(d0$final, d1$final)
  expect_identical(d0$dat, d1$dat)
  # a costfun that cannot be vectorised: no scans on this grid (2,301
  # points > exact_scan_pointwise), i.e. exactly the genoud search
  d2 <- run(function(x) sum(x))
  set.seed(11)
  d3 <- suppressWarnings(find.design(simfun = simfun,
    boundaries = list(n = c(2, 60), k = c(2, 40)), power = .8,
    evaluations = 300, costfun = function(x) sum(x), silent = TRUE,
    optimizer = "genoud"))
  expect_identical(d2$final, d3$final)
  expect_identical(d2$dat, d3$dat)
})

test_that("grid points cached per search equal the recomputed ones", {
  grid <- mlpwr:::exact_grid_dims(cbind(c(2, -1, 0), c(14, 3, 6)))
  cache <- new.env(parent = emptyenv())
  ch <- mlpwr:::exact_grid_chunks(grid, chunk = 100, cache = cache)
  expect_identical(do.call(rbind, ch), mlpwr:::exact_grid_points(grid, 0:(grid$n - 1)))
  expect_identical(lengths(ch) / 3, c(100, 100, 100, 100, grid$n - 400))
  # reused for the same grid, rebuilt for another one
  expect_identical(mlpwr:::exact_grid_chunks(grid, chunk = 100, cache = cache), ch)
  g2 <- mlpwr:::exact_grid_dims(cbind(c(2, -1), c(5, 3)))
  expect_identical(mlpwr:::exact_grid_chunks(g2, chunk = 100, cache = cache)[[1]],
                   mlpwr:::exact_grid_points(g2, 0:(g2$n - 1)))
  # the search gives the same point with and without the cache
  fn <- function(x) sum((x - c(7.3, 0.2, 4.6))^2)
  D <- cbind(c(2, -1, 0), c(14, 3, 6))
  expect_identical(mlpwr:::exact_grid_search(fn, D, chunk = 100, cache = cache),
                   mlpwr:::exact_grid_search(fn, D, chunk = 100))
  expect_identical(mlpwr:::exact_grid_search(fn, D, chunk = 100, cache = cache),
                   c(7, 0, 5))
})

test_that("the vectorised scan matches a straightforward reference implementation", {
  # reference grid points: expand.grid in lexicographic order (first slowest)
  lex <- function(bnd) unname(as.matrix(rev(expand.grid(rev(lapply(bnd, function(b) b[1]:b[2]))))))
  D3 <- cbind(c(2, -1, 0), c(14, 3, 6))
  g3 <- mlpwr:::exact_grid_dims(D3)
  expect_identical(mlpwr:::exact_grid_points(g3, 0:(g3$n - 1)),
                   lex(list(c(2, 14), c(-1, 3), c(0, 6))) + 0)
  expect_identical(mlpwr:::exact_grid_points(g3, c(7, 100, 3)),
                   mlpwr:::exact_grid_points(g3, 0:(g3$n - 1))[c(8, 101, 4), ])

  # reference km screening: one predict.km call for the whole grid
  ref_meansd <- function(kmc, X) {
    mod <- environment(kmc$meansd_batch)$mod
    nd <- as.data.frame(X)
    names(nd) <- colnames(mod@X)
    p <- DiceKriging::predict.km(mod, newdata = nd, type = "UK", light.return = TRUE,
                                 checkNames = FALSE)
    list(mean = p$mean, sd = p$sd)
  }

  relu <- function(x) ifelse(x > 0, x, 0)
  # the design the search chose (get.pred replaces it by all designs after a
  # bad fit or a bad prediction)
  chosen <- function(p) as.numeric(if (is.data.frame(p$bad.points)) p$bad.points else p$points)
  cases <- list(
    list(bnd = list(n = c(10, 300)), cf = function(x) 3 * x),
    list(bnd = list(n = c(10, 150), k = c(5, 30)), cf = function(x) 5 * x[1] + 21.7 * x[2]),
    list(bnd = list(a = c(2, 20), b = c(1, 12), c = c(3, 15)),
         cf = function(x) round(x[1] * x[2] / 3) + 2 * x[3]))
  set.seed(11)
  for (cs in cases) {
    bnd <- cs$bnd; cf <- cs$cf; d <- length(bnd)
    # the costfun as find.design() passes it (vectorised form attached, see
    # fix.argtype()); a plain function(x) would not be scanned on > 100 points
    cfv <- mlpwr:::fix.argtype(cf, bnd, vectorise = TRUE)
    des <- unique(round(sapply(bnd, function(b) stats::runif(9, b[1], b[2]))))
    des <- matrix(des, ncol = d)
    rel <- function(x) mean((x - sapply(bnd, `[`, 1)) / sapply(bnd, diff))
    dat <- lapply(seq_len(nrow(des)), function(i)
      list(x = des[i, ], y = stats::rbinom(30, 1, stats::plogis(-2 + 5 * rel(des[i, ])))))
    fit <- mlpwr:::fit.surrogate(dat = dat, surrogate = "gpr", lastfit = NULL)
    kmc <- environment(fit$fitfun)$kmc
    skip_if(is.null(kmc))
    G <- lex(bnd) + 0
    # several blocks of columns give the same values as one matrix
    environment(kmc$meansd_batch)$screen_block <- 7 * nrow(des) * 64
    expect_identical(kmc$meansd_batch(G), ref_meansd(kmc, G))
    expect_identical(kmc$mean_screen(G), ref_meansd(kmc, G)$mean)

    mid <- sapply(bnd, mean)
    for (power in c(.8, .01)) {   # .01: power met everywhere, ties in cost
      p <- get_pred(fit = fit, dat = dat, power = power, costfun = cfv, cost = NULL,
                    boundaries = bnd, task = "desiredpower", aggregate_fun = mean,
                    integer = TRUE, use_noise = TRUE, cache = new.env())
      fn <- function(x) relu(power - fit$fitfun(x) - fit$fitfun.sd(x) * .3) * 1e5 + cf(x) / cf(mid)
      v <- apply(G, 1, fn)
      expect_identical(chosen(p), as.numeric(G[which.min(v), ]))
      expect_identical(unname(fn(chosen(p))), unname(min(v)))
    }
    cost <- cf(mid)
    p <- get_pred(fit = fit, dat = dat, power = NULL, costfun = cfv, cost = cost,
                  boundaries = bnd, task = "costthreshold", aggregate_fun = mean,
                  integer = TRUE, use_noise = TRUE, cache = new.env())
    fn <- function(x) relu(cf(x) - cost) / cost * 1e5 - fit$fitfun(x)
    v <- apply(G, 1, fn)
    expect_identical(chosen(p), as.numeric(G[which.min(v), ]))
  }

  # ties: a constant objective along the second parameter picks its lowest value
  fn <- function(x) abs(x[1] - 4)
  expect_identical(mlpwr:::exact_grid_search(fn, D3), c(4, -1, 0))
})
