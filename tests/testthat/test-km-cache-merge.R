test_that("cached GP predictor always matches the model kept after the patience loop", {
  # several successful attempts (2-D near-linear data keeps them going), some
  # failed ones in between and at the end: the cache must follow `mod`
  check <- function(fail_at) {
    set.seed(2)
    g <- with_km_schedule(fail_at, mlpwr:::gauss.fit(lin_dat2(), patience = 6, control = list(),
                                                     use_noise = TRUE, aggregate_fun = lin_agg,
                                                     noise_fun = lin_noise))
    mod <- environment(g$fitfun)$mod
    for (x in list(c(300, 2), c(800, 7))) {
      p <- DiceKriging::predict.km(mod, newdata = data.frame(t(x)) |> stats::setNames(colnames(mod@X)),
                                   type = "UK", se.compute = TRUE, light.return = TRUE, checkNames = FALSE)
      expect_equal(g$fitfun(x), p$mean, tolerance = 1e-12)
      expect_equal(g$fitfun.sd(x), p$sd, tolerance = 1e-12)
    }
  }
  check(integer(0))
  check(c(3, 6))
  check(c(2, 4, 5, 6))
})
