test_that("simulations_data works without an SD function", {
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                     evaluations = 150, use_noise = FALSE, silent = TRUE))
  expect_null(ds$fit$fitfun.sd)
  sd <- simulations_data(ds)
  expect_true(all(is.na(sd$SE_surrogate)))
  expect_equal(nrow(sd), length(ds$dat))
})

test_that("simulations_data uses the aggregate_fun of the search", {
  agg <- function(x) stats::quantile(x, probs = .2, names = FALSE)
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = function(n) stats::rnorm(1, n / 200),
                                     boundaries = c(20, 200), power = .5, evaluations = 150,
                                     aggregate_fun = agg, noise_fun = function(x) stats::var(x$y) / length(x$y),
                                     surrogate = "gpr", silent = TRUE))
  sd <- simulations_data(ds)
  want <- vapply(ds$dat, function(d) agg(d$y), numeric(1))
  expect_equal(sort(sd$power), sort(unname(want)))
  # SE from the search's noise_fun (no binomial NaNs for non-0/1 results)
  se <- sqrt(vapply(ds$dat, function(d) stats::var(d$y) / length(d$y), numeric(1)))
  expect_false(anyNA(sd$SE))
  expect_equal(sort(sd$SE), sort(unname(se)))
})

test_that("default results are unchanged (mean aggregate, with SD)", {
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                     evaluations = 150, surrogate = "gpr", silent = TRUE))
  sd <- simulations_data(ds)
  expect_equal(sort(sd$power), sort(vapply(ds$dat, function(d) mean(d$y), numeric(1))))
  expect_false(anyNA(sd$SE_surrogate))
})
