plot_quietly <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot(...)
}

test_that("type = 'line' (documented) and 'hline' both give the 2-D line plot", {
  set.seed(1)
  ds <- suppressWarnings(find.design(
    simfun = example.simfun("anova"),
    boundaries = list(n = c(10, 150), n.groups = c(5, 30)),
    costfun = function(n, n.groups) 5 * n + 20 * n.groups, power = .8,
    evaluations = 300, silent = TRUE))
  pl_line <- plot_quietly(ds, type = "line")
  pl_hline <- plot_quietly(ds, type = "hline")
  expect_s3_class(pl_line, "ggplot")
  expect_equal(ggplot2::ggplot_build(pl_line)$data, ggplot2::ggplot_build(pl_hline)$data)
})

test_that("without an SD function, ribbon and error bars are switched off", {
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 200),
                                     power = .8, evaluations = 200, use_noise = FALSE,
                                     silent = TRUE))
  expect_null(ds$fit$fitfun.sd)
  pl <- plot_quietly(ds, addribbon = TRUE)
  expect_false(any(vapply(pl$layers, function(l) inherits(l$geom, "GeomRibbon"), TRUE)))
})

test_that("continuous designs are plotted on a fine grid, integer designs unchanged", {
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = function(p) stats::runif(1) < p,
                                     boundaries = c(0, 1), power = .5, evaluations = 200,
                                     integer = FALSE, surrogate = "gpr", silent = TRUE))
  pl <- plot_quietly(ds)
  line <- ggplot2::ggplot_build(pl)$data[[2]]
  expect_equal(nrow(line), 101)
  expect_equal(range(line$x), c(0, 1))

  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = sim_ttest, boundaries = c(20, 40),
                                     power = .8, evaluations = 200, silent = TRUE))
  line <- ggplot2::ggplot_build(plot_quietly(ds))$data[[2]]
  expect_equal(sort(line$x), 20:40)
})

test_that("the 2-D iso-power line is found (default trim no longer errors)", {
  simfun_ttest2 <- function(nA, nB) {
    stats::t.test(stats::rnorm(nA, 5, 2), stats::rnorm(nB, 6, 1), alternative = "less")$p.value < .01
  }
  set.seed(1)
  ds <- suppressWarnings(find.design(simfun = simfun_ttest2,
                                     boundaries = list(nA = c(5, 200), nB = c(5, 200)),
                                     costfun = function(nA, nB) 1.5 * nA + nB, power = .95,
                                     evaluations = 600, silent = TRUE))
  pl <- plot_quietly(ds, type = "hline")
  iso <- ggplot2::ggplot_build(pl)$data[[1]]
  expect_gt(nrow(iso), 20)
  # the iso-power line passes (close to) the final design
  fin <- as.numeric(ds$final$design)
  near <- iso[which.min(abs(iso$x - fin[1])), ]
  expect_lt(abs(ds$fit$fitfun(c(near$x, near$y)) - ds$final$power), .001)
})

test_that("1-D slice of a 2-D design draws observed points at the free parameter", {
  set.seed(1)
  ds <- suppressWarnings(find.design(
    simfun = example.simfun("anova"),
    boundaries = list(n = c(10, 150), n.groups = c(5, 30)),
    costfun = function(n, n.groups) 5 * n + 20 * n.groups, power = .8,
    evaluations = 300, silent = TRUE))
  x <- vapply(ds$dat, function(d) as.numeric(d$x), numeric(2))
  n_fixed <- x[1, which.max(table(x[1, ])[as.character(x[1, ])])]
  pl <- plot_quietly(ds, design = list(n = n_fixed, n.groups = NA), adderrorbars = TRUE)
  b <- ggplot2::ggplot_build(pl)$data
  geoms <- vapply(pl$layers, function(l) class(l$geom)[1], "")
  pts <- b[[which(geoms == "GeomPoint")]]
  want <- sort(x[2, x[1, ] == n_fixed])
  expect_equal(sort(pts$x), want)
  bars <- b[[which(geoms == "GeomErrorbar")]]
  expect_equal(sort(unique(bars$x)), unique(want))
})
