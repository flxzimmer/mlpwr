# Small, fast simulation functions shared by the regression tests.
sim_ttest <- function(n) stats::t.test(stats::rnorm(n), mu = 0.3)$p.value < .05
sim_null <- function(n) stats::t.test(stats::rnorm(n))$p.value < .05
