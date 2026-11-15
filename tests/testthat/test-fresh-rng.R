test_that("find.design works in a session where the RNG was never used", {
  skip_on_cran()
  script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressMessages(library(mlpwr))",
    "stopifnot(!exists('.Random.seed', envir = globalenv()))",
    "f <- function(n) stats::t.test(stats::rnorm(n), mu = 0.3)$p.value < .05",
    "ds <- suppressWarnings(find.design(simfun = f, boundaries = c(20, 200), power = .8,",
    "                                   evaluations = 100, silent = TRUE))",
    "cat(is.integer(ds$seed), length(ds$final$design), '\\n')"
  ), script)
  rscript <- file.path(R.home("bin"), "Rscript")
  # R_LIBS via Sys.setenv(): system2(env = ) does not work on Windows
  old <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old), add = TRUE)
  out <- system2(rscript, c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("TRUE 1", out)), info = paste(out, collapse = "\n"))
})
