#!/usr/bin/env Rscript
# Recomputes the precomputed search results that the study vignettes load
# (inst/extdata/<name>.RData, object `res`). A release step: run it with the
# final code of the release installed, from the package root:
#   Rscript tools/vignettes/regenerate_results.R [--lib LIB] [--only ANOVA,GLM]
# Each vignette is rendered (into a temporary directory) with
# MLPWR_VIGNETTE_REGENERATE=true: its search chunks then run, with the code and
# seeds shown in the vignette, and a hidden chunk saves `res` into
# inst/extdata. Without that variable the vignettes only load these files, so
# building the package never runs the searches or writes files.
# The timings of vignettes/parallel.Rmd come from tools/vignettes/parallel_results.R.
args <- commandArgs(TRUE)
getarg <- function(f, d) { i <- match(f, args); if (is.na(i)) d else args[i + 1] }
lib <- getarg("--lib", NULL)
if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
vigs <- c(ANOVA = "ANOVA_Vignette.Rmd", GLM = "GLM_Vignette.Rmd",
          IRT = "IRT_Vignette.Rmd", MLM = "MLM_Vignette.Rmd",
          ttest = "ttest_Vignette.Rmd")
only <- getarg("--only", NULL)
if (!is.null(only)) vigs <- vigs[strsplit(only, ",")[[1]]]
stopifnot(file.exists("DESCRIPTION"), dir.exists(file.path("inst", "extdata")))
cat("mlpwr", as.character(utils::packageVersion("mlpwr")), "from",
    dirname(system.file(package = "mlpwr")), "\n")
Sys.setenv(MLPWR_VIGNETTE_REGENERATE = "true")
vdir <- normalizePath("vignettes")
# one R session per vignette, rendered in its global environment as by
# R CMD build (so the saved functions refer to the global environment and the
# files stay small)
for (v in vigs) {
  out <- tempfile("vig")
  dir.create(out)
  expr <- sprintf(paste0("rmarkdown::render(%s, output_dir = %s, ",
    "intermediates_dir = %s, knit_root_dir = %s, quiet = TRUE)"),
    deparse(file.path(vdir, v)), deparse(out), deparse(out), deparse(vdir))
  t <- system.time(st <- system2(file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(expr)), env = paste0("R_LIBS=",
    paste(.libPaths(), collapse = .Platform$path.sep))))[["elapsed"]]
  if (st != 0) stop("rendering ", v, " failed")
  cat(sprintf("%-22s rendered in %6.0f s\n", v, t))
}
print(file.info(list.files(file.path("inst", "extdata"), full.names = TRUE))[, c("size", "mtime")])
