test_that("silent = TRUE prints nothing", {
  set.seed(1)
  out <- capture.output(ds <- suppressWarnings(
    find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                evaluations = 100, silent = TRUE)))
  expect_identical(out, character(0))
})
