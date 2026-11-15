test_that("a nearly linear 1-D GP fit is no longer rejected as a plane", {
  set.seed(1)
  g <- mlpwr:::gauss.fit(lin_dat(), patience = 100, control = list(), use_noise = TRUE,
                         aggregate_fun = lin_agg, noise_fun = lin_noise)
  expect_false(g$badfit)
})

test_that("the 2-D plane check is unchanged", {
  set.seed(1)
  g <- mlpwr:::gauss.fit(lin_dat2(), patience = 3, control = list(), use_noise = TRUE,
                         aggregate_fun = lin_agg, noise_fun = lin_noise)
  expect_true(g$badfit)
})
