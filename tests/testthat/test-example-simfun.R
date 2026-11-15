test_that("every simfun listed in ?example.simfun exists", {
  for (nm in c("ttest", "anova", "glm2", "irt1", "irt2", "multi1"))
    expect_true(is.function(example.simfun(nm)), label = nm)
})
