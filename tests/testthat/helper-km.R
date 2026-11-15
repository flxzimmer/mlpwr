# Fake km: succeeds (or fails) according to a schedule, so the patience loop's
# handling of failed attempts can be tested without relying on km internals.
with_km_schedule <- function(fail_at, code) {
  k <- 0
  fails <- function() {
    k <<- k + 1
    k %in% fail_at || identical(fail_at, "all")
  }
  orig <- DiceKriging::km
  local_mocked_bindings(km = function(...) {
    if (fails()) stop("non-finite value supplied by optim")
    orig(...)
  }, .package = "DiceKriging")
  force(code)
}

# Near-linear data. In 1-D such a fit is accepted (only a flat 1-D fit is
# rejected as a plane); in 2-D the R^2 plane check still rejects it, so all
# patience attempts are made.
lin_dat <- function() {
  lapply(c(100, 400, 700, 1000), function(n) list(x = n, y = rnorm(20, 0.6 + 0.0002 * n, 0.03)))
}
lin_dat2 <- function() {
  des <- expand.grid(n = c(100, 550, 1000), k = c(1, 5, 10))
  lapply(seq_len(nrow(des)), function(i) {
    list(x = as.numeric(des[i, ]), y = rnorm(20, 0.6 + 0.0002 * des$n[i] + 0.01 * des$k[i], 0.03))
  })
}

# pmsims-like assurance aggregate and bootstrap noise (noise_fun gets list(x, y))
lin_agg <- function(x) stats::quantile(x, probs = .2)
lin_noise <- function(x) stats::var(replicate(20, lin_agg(sample(x$y, replace = TRUE))))
