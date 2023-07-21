

fit.surrogate <- function(dat, surrogate, lastfit = 0,
                          control = list(),aggregate_fun=mean,use_noise=TRUE,noise_fun="bernoulli",anchor=NULL,plot_progress = FALSE,power = NULL) {


  if (!is.list(lastfit))
    lastfit <- NULL

  switch(surrogate, reg = reg.fit(dat,aggregate_fun=aggregate_fun), logreg = logi.fit(dat,aggregate_fun=aggregate_fun),
         svr = svm.fit(dat, lastfit = lastfit,aggregate_fun=aggregate_fun),
         gpr = gauss.fit(dat, patience = 100, control,use_noise=use_noise,aggregate_fun=aggregate_fun,noise_fun=noise_fun,anchor=anchor,plot_progress = plot_progress,power=power))

}


# linear
# ------------------------------------------------------------------



reg.fit <- function(dat,aggregate_fun) {

  datx <- todataframe(dat, aggregate = TRUE,aggregate_fun=aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
  weight <- getweight(dat, weight.type = "freq")

  mod <- stats::lm(y ~ ., datx, weights = weight)

  fitfun <- function(x) {
    names(x) <- names(xvars)
    stats::predict(mod, newdata = data.frame(t(x)))
  }

  # fitfun.sd = function(x) { names(x) =
  # names(xvars)
  # stats::predict(mod,newdata=data.frame(t(x)),se.fit=TRUE)$se.fit}

  re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = FALSE)

  return(re)
}



logi.fit <- function(dat, trans = TRUE,aggregate_fun) {

  datx <- todataframe(dat, aggregate = TRUE,aggregate_fun = aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
  weight <- getweight(dat, weight.type = "freq")

  if (!trans)
    mod <- stats::glm(y ~ ., datx, weights = weight,
                      family = stats::binomial)

  if (trans) {
    trans <- function(x) x/2 + 0.5
    invtrans <- function(y) y * 2 - 1
    fam <- stats::quasi(link = "logit", variance = "mu(1-mu)")
    linkfun <- function(mu) log(mu/(1 - mu))
    linkfun.new <- function(mu) linkfun(trans(mu))
    fam$linkfun <- linkfun.new
    linkinv <- function(mu) 1/(1 + exp(-mu))
    linkinv.new <- function(mu) invtrans(linkinv(mu))
    fam$linkinv <- linkinv.new
    fam$mu.eta <- function(mu) 2 * exp(-mu)/(exp(-mu) +
                                               1)^2
    mod <- stats::glm(y ~ ., datx, weights = weight,
                      family = fam)
  }
  fitfun <- function(x) {
    names(x) <- names(xvars)
    stats::predict(mod, newdata = data.frame(t(x)),
                   type = "response")
  }

  # fitfun.sd = function(x) { names(x) =
  # names(xvars)
  # stats::predict(mod,newdata=data.frame(t(x)),type='response',se.fit=TRUE)$se.fit}

  re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = FALSE)

  return(re)
}




# ---------------------------------------------------------------------



fixpred <- function(x) {
  # get pred to [0,1] Interval
  x[x > 1] <- 1 - 10^(-10)
  x[x < 0] <- 10^(-10)
  x
}

logloss <- function(true.y, pred, true.w) {
  # y true value, p prediction
  pred <- fixpred(pred)
  stats::weighted.mean(-(true.y * log2(pred) + (1 -
                                                  true.y) * log2(1 - pred)), true.w)
}

svm.fit <- function(dat, lastfit, tune = TRUE,aggregate_fun) {

  datx <- todataframe(dat, aggregate = TRUE, pseudo = FALSE,aggregate_fun = aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
  weight <- getweight(dat, weight.type = "freq")

  # Treat the case of equal power values in the
  # data [especially the case of zero
  # variance]- add (a negligibly small amount
  # of) random noise
  if (any(ind <- duplicated(datx$y))) {
    a <- datx$y[ind]
    a <- a + stats::rnorm(length(a), sd = 1e-07)
    a[a > 1] <- 1 - (a[a > 1] - 1)
    a[a < 0] <- -a[a < 0]
    datx$y[ind] <- a
  }

  # Determine if it is the first round
  firstround <- is.null(lastfit$pars_history)

  if (firstround) {
    # setup parameter history
    pars_history <- list()

    # Default Pars
    pars <- list(cost = 100, epsilon = 0.1, gamma = 0.05)

    # Tuning Grid for first round
    tunepars <- list(cost = c(10^seq(-1, 2), 2 *
                                10^seq(-1, 2), 5 * 10^seq(-1, 2)), gamma = 10^seq(-4,
                                                                                  1), epsilon = min(getweight(dat, "sd")))

  }

  if (!firstround) {

    pars_history <- lastfit$pars_history
    n_previous <- length(pars_history)
    pars <- pars_history[[n_previous]]
    TUNEFACTORS <- seq(0.75, 1.25, 0.25)
    tunepars <- list(cost = pars$cost * TUNEFACTORS,
                     gamma = pars$gamma * TUNEFACTORS, epsilon = min(getweight(dat,
                                                                               "sd")))

    # Wider Search every 10 Iterations
    if (n_previous%%10 == 0) {
      tunepars <- list(cost = c(tunepars$cost,
                                10^seq(-1, 2)), gamma = c(tunepars$gamma,
                                                          10^seq(-4, 1)), epsilon = tunepars$epsilon)
    }
  }


  if (tune) {
    ctrl <- WeightSVM::tune.control(cross = min(10,
                                                nrow(datx)), error.fun = logloss)
    a <- WeightSVM::tune_wsvm(y ~ ., data = datx,
                              weight = weight, ranges = tunepars, tunecontrol = ctrl)
    pars <- a$best.parameter
    mod <- a$best.model
  }


  if (!tune) {
    mod <- WeightSVM::wsvm(y ~ ., data = datx,
                           cost = pars$cost, epsilon = pars$epsilon,
                           gamma = pars$gamma, weight = weight)
  }

  # Save used parameters
  pars_history <- rlist::list.append(pars_history,
                                     as.data.frame(pars))


  # setup function that outputs power
  fitfun <- function(x) {
    names(x) <- names(xvars)
    stats::predict(mod, newdata = data.frame(t(x)))
  }


  # check if a plane was fitted
  badfit <- TRUE
  somevals <- apply(xvars, 1, function(x) fitfun(x))
  isplane <- suppressWarnings(summary(stats::lm(y ~
                                                  ., cbind(xvars, y = somevals)))$r.squared >
                                0.98)
  if (!is.na(isplane) && !isplane)
    badfit <- FALSE


  re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = badfit,
             lastfit = list(pars_history = pars_history))

  return(re)
}


# gpr
# ---------------------------------------------------------------------


gauss.fit <- function(dat, patience = 100, control,use_noise,aggregate_fun,noise_fun="bernoulli",anchor=NULL,plot_progress = FALSE,power=NULL) {


  datx <- todataframe(dat, aggregate = TRUE,aggregate_fun = aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]

  if(!is.function(noise_fun) && noise_fun == "bernoulli") weight <- getweight(dat, weight.type = "var")
  else weight = sapply(dat,noise_fun)

  # add anchor at n = 0
  if (!is.null(anchor)) {
    datx = rbind(datx,c(0,anchor))
    xvars = rbind(xvars,0)
    weight = c(weight,min(weight))
  }

  if (any(ind <- duplicated(datx$y))) {
    a <- datx$y[ind]
    a <- a + stats::rnorm(length(a), sd = 1e-07)
    a[a > 1] <- 1 - (a[a > 1] - 1)
    a[a < 0] <- -a[a < 0]
    datx$y[ind] <- a
  }

  # setup gpr arguments
  # "gauss"
  #matern5_2
  args <- list(design = xvars,covtype="matern5_2", response = datx$y,
               noise.var = weight, control = list(trace = FALSE))
  if(!use_noise) args$noise.var = NULL
  # args = rlist::list.append(args, control)
  args <- c(args, control)

  n.try <- 0

  badfit <- TRUE

  while (n.try < patience & badfit) {

    n.try <- n.try + 1
    if(n.try==80) args$noise.var = args$noise.var*10
    if(n.try==90) args$noise.var = args$noise.var*10

    # Fit the GPR
    mod <- tryCatch(do.call(DiceKriging::km, args = args),
                    error = function(x) {
                      return(NULL)
                    })

    # keep fallback mod in case nothing is found later
    if(!is.null(mod)) fallback_mod = mod
    if (is.null(mod)& n.try==patience) {
      mod = fallback_mod
      next
    }
    if (is.null(mod))
      next


    # setup function that outputs power
    fitfun <- function(x) {
      names(x) <- names(xvars)
      DiceKriging::predict.km(mod, newdata = data.frame(t(x)),
                              type = "UK")$mean
    }

    # check if a plane was fitted
    # new plane check
    if(ncol(datx)==2) {
      # xvals = data.frame(V1 = runif(50) * (max(xvars)-min(xvars)) + min(xvars))
      # somevals <- apply(xvals, 1, function(x) fitfun(x))
      # isplane <- max(somevals)-min(somevals)<.02
      xvals = data.frame(V1 = runif(10) * (max(xvars)-min(xvars)) + min(xvars))
      somevals <- apply(xvals, 1, function(x) fitfun(x))
      isplane <- max(somevals)-min(somevals)<.001
      # xvals2 = data.frame(V1 = runif(3) * (max(xvars)-min(xvars)) + min(xvars))
      # somevals <- apply(xvals2, 1, function(x) fitfun(x))
      # isplane <- isplane | (max(somevals)-min(somevals)<.04)
      if (!is.na(isplane) && !isplane)
        badfit <- FALSE
    }

    if(ncol(datx)>2) {
      # old plane check
      somevals <- apply(xvars, 1, function(x) fitfun(x))
      isplane <- suppressWarnings(summary(stats::lm(y ~ .,
                                                    cbind(xvars, y = somevals)))$r.squared > 0.98)
      if (!is.na(isplane) && !isplane)
        badfit <- FALSE
    }

  }

  # setup function that outputs sd
  fitfun.sd <- function(x) {
    names(x) <- names(xvars)
    DiceKriging::predict.km(mod, newdata = data.frame(t(x)),
                            type = "UK")$sd
  }
  if(!use_noise) fitfun.sd = NULL

  # if(!exists("fitfun")) browser()

  if (plot_progress) {
  x = seq(min(datx$V1),max(datx$V1))
  y = sapply(x,fitfun)
  plot(x,y,type="l")
  points(datx$V1,datx$y)
  abline(h=power)
  }

  re <- list(fitfun = fitfun, fitfun.sd = fitfun.sd,
             badfit = badfit)

  return(re)
}








