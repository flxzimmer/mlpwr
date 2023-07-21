

fit.surrogate <- function(dat, surrogate, lastfit = 0,
    control = list(),aggregate_fun=mean,use_noise=TRUE) {


    if (!is.list(lastfit))
        lastfit <- NULL

    switch(surrogate, reg = reg.fit(dat,aggregate_fun=aggregate_fun), logreg = logi.fit(dat,aggregate_fun=aggregate_fun),
        svr = svm.fit(dat, lastfit = lastfit,aggregate_fun=aggregate_fun), gpr = gauss.fit(dat,
            patience = 100, control,use_noise=use_noise,aggregate_fun=aggregate_fun),
        splines = splines.fit(dat,use_noise=use_noise,aggregate_fun=aggregate_fun,noise_fun=noise_fun),
        power = power.fit(dat,use_noise=use_noise,aggregate_fun=aggregate_fun,noise_fun=noise_fun))

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


# svr
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



gauss.fit <- function(dat, patience = 100, control,use_noise,aggregate_fun) {

    datx <- todataframe(dat, aggregate = TRUE,aggregate_fun = aggregate_fun)
    xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
    weight <- getweight(dat, weight.type = "var")

    if (any(ind <- duplicated(datx$y))) {
        a <- datx$y[ind]
        a <- a + stats::rnorm(length(a), sd = 1e-07)
        a[a > 1] <- 1 - (a[a > 1] - 1)
        a[a < 0] <- -a[a < 0]
        datx$y[ind] <- a
    }


    # setup gpr arguments
    args <- list(design = xvars, response = datx$y,
        noise.var = weight, control = list(trace = FALSE))
    if(!use_noise) args$noise.var = NULL
    # args = rlist::list.append(args, control)
    args <- c(args, control)

    n.try <- 0

    badfit <- TRUE

    while (n.try < patience & badfit) {

        n.try <- n.try + 1

        # Fit the GPR
        mod <- tryCatch(do.call(DiceKriging::km, args = args),
            error = function(x) {
                return(NULL)
            })

        if (is.null(mod))
            next

        # setup function that outputs power
        fitfun <- function(x) {
            names(x) <- names(xvars)
            DiceKriging::predict.km(mod, newdata = data.frame(t(x)),
                type = "UK")$mean
        }

        # check if a plane was fitted
        somevals <- apply(xvars, 1, function(x) fitfun(x))
        isplane <- suppressWarnings(summary(stats::lm(y ~
            ., cbind(xvars, y = somevals)))$r.squared >
            0.98)
        if (!is.na(isplane) && !isplane)
            badfit <- FALSE

    }

    # setup function that outputs sd
    fitfun.sd <- function(x) {
        names(x) <- names(xvars)
        DiceKriging::predict.km(mod, newdata = data.frame(t(x)),
            type = "UK")$sd
    }
    if(!use_noise) fitfun.sd = NULL

    # if(!exists("fitfun")) browser()

    re <- list(fitfun = fitfun, fitfun.sd = fitfun.sd,
        badfit = badfit)

    return(re)
}




# powerfun -----------------------------------------------------------------

power.fit <- function(dat,use_noise,aggregate_fun,noise_fun="freq") {

  library(mgcv)
  library(caret)
  library(boot)

  datx <- todataframe(dat, aggregate = TRUE,aggregate_fun = aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]

  # if(!is.function(noise_fun) && noise_fun == "freq") weight <- getweight(dat, weight.type = "freq")
  # else weight = sapply(dat,noise_fun)
  weight <- getweight(dat, weight.type = "freq")

  #add anchor at n = 0
  # datx = rbind(datx,c(0,.5))
  # xvars = rbind(xvars,0)
  # weight = c(weight,max(weight)*5)

  weight <- weight/mean(weight) # normalize weights
  datx$weight = weight # add weight to dataframe

  re = fitpowerlaw(datx=datx)
  mod = re$mod


  fitfun <- function(x) {
    names(x) <- names(xvars)
    re = stats::predict(mod, newdata = data.frame(t(x)),
                        type = "response")
    if(length(x)<length(re)) re = re[1]
    return(re)
  }

  # print(best_k)
  # fitfun(1392)
  # x = seq(min(datx$V1),max(datx$V1))
  # x = seq(min(datx$V1),700)
  # x = datx$V1
  # plot(x,sapply(x,fitfun))

  re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = FALSE)

  return(re)
}



fitpowerlaw = function(datx) {
  strx = "y"
  names(datx)[1] = "n"

  inv.power.law = as.formula(paste0(strx,"~(1 - a) - b * n^c1"))

  alg="default"
  ctrl = nls.control(warnOnly = T)


  startsets = list(
    list(a=1,b=-.5,c1=-.01),
    list(a=1.0701839,b=-0.1922454,c1=-0.1280311),
    list(a=0.9992658,b= -0.91970387,c1= -0.50102893),
    list(a=1.0075304,b= -0.62656251,c1= -0.44011308),
    list(a=1.0623583,b= -0.17973726,c1= -0.12874478),
    list(a=1.0812181,b=  -4.889454,c1= -0.4177285),
    list(a=0.9588333,b= -14.663574,c1= -0.8061297),
    list(a=0.9572533,b=  -8.868742,c1= -0.6343645),
    list(a=1.1094262,b=  -1.423001,c1= -0.2730920),
    list(a=0.9249856,b= -11.082680,c1= -0.7504548),
    list(a=0.9659449 ,b= -7.337569,c1= -0.6676266)
  )
  r2 = 0

  for (i in 1:length(startsets)) {

    tryCatch({
      mod = nls(inv.power.law,datx,start=startsets[[i]],weights = weight,algorithm=alg,control = ctrl)
      # Fit indices
      r2 = 1- sum(resid(mod)^2) / sum((datx[[strx]]-mean(datx[[strx]]))^2)
    },error=function(e) NA)

    if (r2>.95) break

  }

  # try other algorithm if still bad
  if(r2<.9) {
    alg="port"
    ctrl = nls.control(warnOnly = T)
    mod = nls(inv.power.law,datx,start=list(a=1,b=-.5,c1=-.01),weights = weight,algorithm=alg,control = ctrl)
    r2 = 1- sum(resid(mod)^2) / sum((datx[[strx]]-mean(datx[[strx]]))^2)
  }

  a = summary(mod)$parameters[1,1]
  b = summary(mod)$parameters[2,1]
  c1 = summary(mod)$parameters[3,1]


  return(list(mod=mod,r2=r2))
}



# splines -----------------------------------------------------------------

splines.fit <- function(dat,use_noise,aggregate_fun,noise_fun="freq") {

  library(mgcv)
  library(caret)
  library(boot)

  datx <- todataframe(dat, aggregate = TRUE,aggregate_fun = aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]

  # if(!is.function(noise_fun) && noise_fun == "freq") weight <- getweight(dat, weight.type = "freq")
  # else weight = sapply(dat,noise_fun)
  weight <- getweight(dat, weight.type = "freq")

  #add anchor at n = 0
  datx = rbind(datx,c(0,.5))
  xvars = rbind(xvars,0)
  weight = c(weight,max(weight)*5)
  weight <- weight/mean(weight) # normalize weights

  datx$weight = weight

  method = "scam"

  if (method=="gam") {
    # Define the tuning grid
    tuneGrid <- expand.grid(k = 3:(nrow(datx)-1))

    # Define a function to fit a gam model for a given value of k
    fit_gam <- function(k) {
      # Fit the model
      fit <- mgcv::gam(y ~ s(V1, k = k), data = datx,weights=weight)

      # Perform cross-validation
      cv_results <- cv.glm(datx, fit, K = nrow(datx))

      # Return the cross-validation results
      cv_results$delta[1]
    }

    # Fit a gam model for each value of k in the tuning grid
    results <- sapply(tuneGrid$k, fit_gam)

    # Select the best value of k based on the cross-validation results
    best_k <- tuneGrid$k[which.min(results)]

    # Fit the final model using the best value of k
    mod <- mgcv::gam(y ~ s(V1, k = best_k), data = datx,weights=weight)

    # old code
    # degree = min(nrow(datx),7)
    # mod <- mgcv::gam(y ~ s(V1, k = degree), data = datx,weights=weight)

    fitfun <- function(x) {
      names(x) <- names(xvars)
      mgcv::predict.gam(mod, newdata = data.frame(t(x)),
                        type = "response")
    }
  }

  if (method=="smooth.spline") {

    mod <- smooth.spline(datx$V1, datx$y)

    fitfun <- function(x) {
      names(x) <- names(xvars)
      stats:::predict.smooth.spline(mod, x = data.frame(t(x)),
                                    type = "response")$y |> as.numeric()
    }
  }

  if (method=="scam") {

    library(scam)

    # Fit a monotonic spline to the data
    mod <- tryCatch(
      scam(y ~ s(V1, k = nrow(datx), bs = "cr"),weights=weight, data = datx)
      ,error=\(e)NULL)
    if(is.null(mod)) {
      mod = scam(y ~ s(V1, k = nrow(datx), bs = "cr"),weights=weight, data = datx,optimizer="optim")
    }

    fitfun <- function(x) {
      names(x) <- names(xvars)
      scam:::predict.scam(mod, newdata = data.frame(t(x)),
                          type = "response") |> as.numeric()
    }
  }



  # print(best_k)
  # fitfun(1392)
  # x = seq(min(datx$V1),max(datx$V1))
  # x = seq(min(datx$V1),700)
  # x = datx$V1
  # plot(x,sapply(x,fitfun))

  # x = seq(min(datx$V1),max(datx$V1))
  # y = sapply(x,fitfun)
  # plot(x,y,type="l")
  # points(datx$V1,datx$y)

  re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = FALSE)

  return(re)
}






