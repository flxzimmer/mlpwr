test_that("a failed last km attempt keeps the last successful model", {
  set.seed(1)
  g <- with_km_schedule(100, mlpwr:::gauss.fit(lin_dat2(), patience = 100, control = list(),
                                               use_noise = TRUE, aggregate_fun = lin_agg,
                                               noise_fun = lin_noise))
  expect_true(g$badfit)
  expect_true(is.finite(g$fitfun(c(500, 3))))
  expect_true(is.finite(g$fitfun.sd(c(500, 3))))
})
