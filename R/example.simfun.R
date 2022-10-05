#' Example simulation functions
#'
#' These simulation functions can be loaded as examples or templates. They correspond to the functions in the 'simulation_functions' vignette. The vignette contains the code and a description of the simfuns.
#'
#' @param name Name of the simulation function as a character. Currently available are: 'ttest','anova','glm2','irt1','irt2','multi2'.
#'
#' @return The returned object is the simulation function.
#' @export
#'
#' @examples simfun = example.simfun('ttest')
#' simfun(400)
#'
example.simfun <- function(name) {

  switch(name, ttest = simfun_ttest, anova = simfun_anova,
         glm2 = simfun_glm2, irt1 = simfun_irt1, irt2 = simfun_irt2,
         multi1 = simfun_multi1)
}

simfun_ttest <- function(N) {
  # Generate a data set
  dat <- stats::rnorm(n = N, mean = 0.3)
  # Test the hypothesis
  res <- stats::t.test(dat)
  res$p.value < 0.01
}


simfun_anova <- function(n, n.groups) {

  # Generate a data set
  groupmeans <- stats::rnorm(n.groups, sd = 0.2)  # generate groupmeans using cohen's f=.2
  dat <- sapply(groupmeans, function(x) stats::rnorm(n,
                                                     mean = x, sd = 1))  # generate data
  dat <- dat |>
    as.data.frame() |>
    tidyr::gather()  # format

  # Test the hypothesis
  res <- stats::aov(value ~ key, data = dat)  # perform ANOVA
  summary(res)[[1]][1, 5] < 0.01  # extract significance
}


simfun_glm2 <- function(N) {

  logistic <- function(x) 1/(1 + exp(-x))

  # generate data
  dat <- data.frame(pred1 = stats::rnorm(N), pred2 = stats::rnorm(N))
  beta <- c(1.2, 0.8)  # parameter weights
  prob <- logistic(as.matrix(dat) %*% beta)  # get probability
  dat$criterion <- stats::runif(N) < prob  # draw according to probability

  # test hypothesis
  mod <- stats::glm(criterion ~ pred1 + pred2, data = dat,
                    family = stats::binomial)
  summary(mod)$coefficients["pred2", "Pr(>|z|)"] <
    0.01
}

simfun_irt1 <- function(N) {

  # generate data
  dat <- mirt::simdata(a = c(1.04, 1.2, 1.19, 0.61,
                             1.31, 0.83, 1.46, 1.27, 0.51, 0.81), d = c(0.06,
                                                                        -1.79, -1.15, 0.88, -0.2, -1.87, 1.23, -0.08,
                                                                        -0.71, 0.6), N = N, itemtype = "2PL")

  # test hypothesis
  mod <- mirt::mirt(dat)  # Fit 2PL Model
  constrained <- "F = 1-4
          CONSTRAIN = (1-4, a1)"
  mod_constrained <- mirt::mirt(dat, constrained)  # Fit 2PL with equal slopes

  res <- mirt::anova(mod_constrained, mod)  # perform model comparison
  res$p[2] < 0.01  # extract significance
}


simfun_irt2 <- function(N1, N2) {

  # generate data
  a1 <- a2 <- c(1.04, 1.2, 1.19, 0.61, 1.31, 0.83,
                1.46, 1.27, 0.51, 0.81)
  d1 <- d2 <- c(0.06, -1.79, -1.15, 0.88, -0.2, -1.87,
                1.23, -0.08, -0.71, 0.6)
  a2[1] <- a2[1] + 0.3
  d2[1] <- d2[1] + 0.5
  dat1 <- mirt::simdata(a = a1, d = d1, N = N1, itemtype = "2PL")
  dat2 <- mirt::simdata(a = a2, d = d2, N = N2, itemtype = "2PL")
  dat <- as.data.frame(rbind(dat1, dat2))
  group <- c(rep("1", N1), rep("2", N2))

  # fit models
  mod1 <- mirt::multipleGroup(dat, 1, group = group)
  mod2 <- mirt::multipleGroup(dat, 1, group = group,
                              invariance = c("slopes", "intercepts"))

  # test hypothesis
  res <- mirt::anova(mod2, mod1)

  # extract significance
  res$p[2] < 0.01
}


simfun_multi1 <- function(N) {

  # generate data
  params <- list(theta = 0.5, beta = c(2, -0.2, -0.4,
                                       -0.6))
  dat <- expand.grid(herd = 1:ceiling(N/4), period = factor(1:4))[1:N,
  ]
  dat$x <- lme4::simulate.formula(~period + (1 |
                                               herd), newdata = dat, family = stats::poisson,
                                  newparams = params)[[1]]


  # test hypothesis
  mod <- lme4::glmer(x ~ period + (1 | herd), data = dat,
                     family = stats::poisson)  # fit model
  pvalues <- summary(mod)[["coefficients"]][2:4,
                                            "Pr(>|z|)"]
  any(pvalues < 0.01)
}
