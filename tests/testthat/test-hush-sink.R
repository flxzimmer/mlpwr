test_that("hush() removes only its own sink", {
  out <- tempfile()
  sink(out)
  n_before <- sink.number()
  res <- mlpwr:::hush({ cat("hidden\n"); 42 })
  n_after <- sink.number()
  cat("visible\n")
  sink()
  expect_identical(res, 42)
  expect_identical(n_after, n_before)
  expect_identical(readLines(out), "visible")
})

test_that("find.design can run inside capture.output()", {
  set.seed(1)
  txt <- capture.output(ds <- suppressWarnings(
    find.design(simfun = sim_ttest, boundaries = c(20, 200), power = .8,
                evaluations = 100, silent = FALSE)))
  expect_s3_class(ds, "designresult")
  expect_true(any(grepl("Updates", txt)))
})
