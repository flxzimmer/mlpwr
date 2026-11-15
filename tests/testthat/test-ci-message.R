test_that("the ci/GPR message is spelled correctly", {
  set.seed(1)
  expect_message(
    try(find.design(simfun = sim_ttest, boundaries = c(20, 200), ci = .1, silent = TRUE),
        silent = TRUE),
    "Consider switching to GPR")
})
