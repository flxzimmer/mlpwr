test_that("goodvals is accepted; non-default values warn that it is unused", {
  run <- function(...) {
    set.seed(1)
    suppressMessages(find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                                 evaluations = 150, silent = TRUE, ...))
  }
  ref <- suppressWarnings(run())
  expect_no_warning(withCallingHandlers(run(goodvals = "high"), warning = function(w) {
    if (grepl("goodvals", conditionMessage(w))) stop("unexpected goodvals warning")
    invokeRestart("muffleWarning")
  }))
  expect_warning(low <- run(goodvals = "low"), "goodvals is currently unused")
  expect_identical(low$final, ref$final)
})
