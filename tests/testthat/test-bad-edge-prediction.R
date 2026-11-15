test_that("a bad final prediction at the edge reports that design, not NA", {
  warns <- character()
  set.seed(1)
  # no effect: power never reaches .8, the prediction ends up at an edge
  ds <- withCallingHandlers(
    find.design(simfun = sim_null, boundaries = c(20, 200), power = .8,
                evaluations = 300, silent = TRUE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("No good design found", warns)))
  expect_false(anyNA(ds$final$design))
  expect_true(as.numeric(ds$final$design) %in% c(20, 200))
  expect_false(is.na(ds$final$power))
  expect_false(is.na(ds$final$se))
})
