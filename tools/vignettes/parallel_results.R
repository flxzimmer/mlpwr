#!/usr/bin/env Rscript
# Precomputes the timing comparison shown in vignettes/parallel.Rmd and writes
# inst/extdata/parallel_Vignette_results.RData (object `bench`).
#   Rscript tools/vignettes/parallel_results.R [--lib LIB] [--workers 1,2,4,8]
# Run from the package root, on a quiet machine if possible (nice is fine).
args <- commandArgs(TRUE)
getarg <- function(f, d) { i <- match(f, args); if (is.na(i)) d else args[i + 1] }
lib <- getarg("--lib", NULL)
workers <- as.integer(strsplit(getarg("--workers", "1,2,4,8"), ",")[[1]])
only <- getarg("--only", "multi1,irt1,ttest")
suppressPackageStartupMessages(library(mlpwr, lib.loc = lib))

workloads <- list(
  multi1 = list(label = "glmer (example.simfun(\"multi1\"))", sf = example.simfun("multi1"),
                b = c(20, 200), ev = 500, pw = .8),
  irt1   = list(label = "mirt (example.simfun(\"irt1\"))", sf = example.simfun("irt1"),
                b = c(40, 300), ev = 300, pw = .9),
  ttest  = list(label = "t-test (example.simfun(\"ttest\"))", sf = example.simfun("ttest"),
                b = c(20, 200), ev = 1000, pw = .8))
workloads <- workloads[strsplit(only, ",")[[1]]]

rows <- list()
for (wl in names(workloads)) {
  cfg <- workloads[[wl]]
  set.seed(99)
  x <- round(mean(cfg$b))
  ms <- 1000 * system.time(for (i in 1:10) cfg$sf(x))[["elapsed"]] / 10
  for (p in c(list(FALSE), as.list(workers))) {
    set.seed(1)
    t <- system.time(ds <- suppressWarnings(find.design(cfg$sf, boundaries = cfg$b, power = cfg$pw,
                       evaluations = cfg$ev, silent = TRUE, parallel = p)))[["elapsed"]]
    rows[[length(rows) + 1]] <- data.frame(workload = wl, simfun = cfg$label, ms_per_eval = round(ms, 1),
      evaluations = ds$evaluations_used, parallel = if (isFALSE(p)) "FALSE" else as.character(p),
      seconds = round(t, 1), design = ds$final$design[[1]], power = round(ds$final$power, 4))
    cat(sprintf("%-7s parallel=%-5s %7.1f s  design %g\n", wl, rows[[length(rows)]]$parallel, t,
                ds$final$design[[1]]))
  }
}
bench <- do.call(rbind, rows)
rownames(bench) <- NULL
attr(bench, "machine") <- sprintf("%s, R %s, %d cores, load average %s", Sys.info()[["sysname"]],
  getRversion(), parallel::detectCores(),
  tryCatch(strsplit(readLines("/proc/loadavg"), " ")[[1]][1], error = function(e) "NA"))
print(bench)
attr(bench, "mlpwr_version") <- as.character(packageVersion("mlpwr", lib.loc = lib))
out <- file.path("inst", "extdata", "parallel_Vignette_results.RData")
if (dir.exists(dirname(out))) { save(bench, file = out); cat("wrote", out, "\n") }
