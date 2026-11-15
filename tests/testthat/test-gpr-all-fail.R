test_that("if every km attempt fails, gauss.fit gives a clear error", {
  set.seed(1)
  expect_error(
    with_km_schedule("all", mlpwr:::gauss.fit(lin_dat2(), patience = 5, control = list(),
                                              use_noise = TRUE, aggregate_fun = lin_agg,
                                               noise_fun = lin_noise)),
    "all 5 attempts")
})
