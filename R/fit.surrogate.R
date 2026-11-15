

fit.surrogate <- function(dat, surrogate, lastfit,
    control = list(),aggregate_fun=mean,use_noise=TRUE,noise_fun="logistic") {

    switch(surrogate, reg = reg.fit(dat,aggregate_fun=aggregate_fun), logreg = logi.fit(dat,aggregate_fun=aggregate_fun, lastfit = lastfit),
        svr = svm.fit(dat, lastfit = lastfit,aggregate_fun=aggregate_fun), gpr = gauss.fit(dat,
            patience = 100, control,use_noise=use_noise,aggregate_fun=aggregate_fun,noise_fun=noise_fun))

}


# linear
# ------------------------------------------------------------------



reg.fit <- function(dat,aggregate_fun) {

    datx <- todataframe(dat, aggregate = TRUE,aggregate_fun=aggregate_fun)
    xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
    weight <- getweight(dat, weight.type = "freq")

    mod <- stats::lm(y ~ ., datx, weights = weight)
    linp <- lin_cache(mod, names(xvars))

    fitfun <- function(x) {
        if (!is.null(linp) && linp$accepts(x))
            return(linp$predict(x))
        names(x) <- names(xvars)
        stats::predict(mod, newdata = data.frame(t(x)))
    }

    # fitfun.sd = function(x) { names(x) =
    # names(xvars)
    # stats::predict(mod,newdata=data.frame(t(x)),se.fit=TRUE)$se.fit}

    re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = FALSE)

    return(re)
}



logi.fit <- function(dat, trans = TRUE, aggregate_fun, lastfit) {

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

        mod <- tryCatch(stats::glm(y ~ ., datx, weights = weight,
            family = fam, control = list(maxit = 50)), error = \(e) NULL)

        # The retries below start from values that can lie far from the
        # optimum, so glm.fit's step-halving and boundary warnings ("step
        # size truncated", "algorithm stopped at boundary value", ...) are
        # expected there and say nothing to the user: they are suppressed
        # in the retry fits only.

        # try again with starting values from previous run
        if (is.null(mod) & !is.null(lastfit$last.mod)) {
          mod <- tryCatch(suppressWarnings(stats::glm(y ~ ., datx,
              weights = weight, family = fam, control = list(maxit = 50),
              start = stats::coef(lastfit$last.mod))),
              error = \(e) NULL)
        }

        # retry from an intercept-only start (the same valid linear
        # predictor at every design), so that a first fit that failed
        # without a previous good fit can recover. Retry only: fits that
        # succeed above are unchanged.
        if (is.null(mod)) {
          ybar <- stats::weighted.mean(datx$y, weight)
          ybar <- min(max(ybar, 0.001), 0.999)
          start0 <- c(linkfun.new(ybar), rep(0, ncol(xvars)))
          mod <- tryCatch(suppressWarnings(stats::glm(y ~ ., datx,
              weights = weight, family = fam, control = list(maxit = 50),
              start = start0)),
              error = \(e) NULL)
        }
    }

    if (!is.null(mod)) {
      last.mod = mod
      badfit = FALSE
      linp <- lin_cache(mod, names(xvars), stats::family(mod)$linkinv)
      fitfun <- function(x) {
        if (!is.null(linp) && linp$accepts(x))
            return(linp$predict(x))
        names(x) <- names(xvars)
        stats::predict(mod, newdata = data.frame(t(x)),
            type = "response")
      }
    } else {
      last.mod = NULL
      badfit = TRUE
      fitfun <- function(x) 0.5
    }

    re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = badfit,last.mod = last.mod)

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
        # Tune on the matrix interface: the CV fits and predictions skip
        # model.frame/model.matrix. Folds come from the same sample(n) call,
        # and each fold sees the same rows in the same order, so the CV
        # errors and the chosen parameters are unchanged. The final model is
        # fitted exactly as tune_wsvm(y ~ ., ...) fits its best.model
        # (formula interface, same call), so fitfun/predict are unchanged.
        ctrl$best.model <- FALSE
        a <- WeightSVM::tune_wsvm(as.matrix(xvars), datx$y,
            weight = weight, ranges = tunepars, tunecontrol = ctrl)
        pars <- a$best.parameter
        mod <- suppressWarnings(do.call(WeightSVM::wsvm, c(list(formula = y ~
            ., weight = weight, data = datx), lapply(a$best.parameters,
            unlist))))
        mod$call <- match.call(WeightSVM::tune_wsvm, quote(WeightSVM::tune_wsvm(y ~
            ., data = datx, weight = weight, ranges = tunepars,
            tunecontrol = ctrl)))
        mod$call[[1]] <- as.symbol("best.tune_wsvm")
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
    isplane <- plane_r2(xvars, somevals) > 0.999
    if (!is.na(isplane) && !isplane)
        badfit <- FALSE


    re <- list(fitfun = fitfun, fitfun.sd = NULL, badfit = badfit,
        pars_history = pars_history)

    return(re)
}


# gpr
# ---------------------------------------------------------------------


# DiceKriging::km() with one known, harmless warning muffled: R-devel (4.7)
# warns when a logical index is recycled to a non-multiple length,
# and kmNuggets.init() of DiceKriging 1.6.1 indexes an n x n
# matrix with a logical dist() vector. It only affects a starting value of the
# variance. All other warnings pass through.
km_quiet <- function(args) {
    msg <- gettext("object length is not a multiple of subscript length", domain = "R")
    withCallingHandlers(do.call(DiceKriging::km, args = args),
        warning = function(w) {
            if (conditionMessage(w) %in% c(msg, "object length is not a multiple of subscript length"))
                invokeRestart("muffleWarning")
        })
}

gauss.fit <- function(dat, patience = 100, control,use_noise,aggregate_fun,noise_fun="logistic") {

    datx <- todataframe(dat, aggregate = TRUE,aggregate_fun = aggregate_fun)
    xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]

    # noise variances ("logistic", "bernoulli" or a user function; see
    # R/noise.R). The character methods are skipped without use_noise
    # (they draw no random numbers, so the results are unchanged); a user
    # function is still called, as before.
    weight <- if (use_noise || is.function(noise_fun))
        noise_variances(dat, xvars, noise_fun)

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

    # last successfully fitted model; fitfun/fitfun.sd look it up when
    # called, so a failed later attempt must not overwrite it
    mod <- NULL
    kmc <- NULL
    have_mod <- FALSE

    while (n.try < patience & badfit) {

        n.try <- n.try + 1

        # Fit the GPR
        mod_try <- tryCatch(km_quiet(args),
            error = function(x) {
                return(NULL)
            })
        if (is.null(mod_try))
            next
        mod <- mod_try
        # cached predictor (R/km_cache.R; agrees with predict.km up to
        # rounding); built only from a successful fit, so after a failed
        # attempt it still matches mod
        kmc <- km_cache(mod)
        have_mod <- TRUE

        # setup function that outputs power
        fitfun <- function(x) {
            if (!is.null(kmc) && kmc$accepts(x))
                return(kmc$mean(x))
            names(x) <- names(xvars)
            DiceKriging::predict.km(mod, newdata = data.frame(t(x)),
                type = "UK")$mean
        }

        # check if a plane was fitted
        somevals <- apply(xvars, 1, function(x) fitfun(x))
        isplane <- plane_r2(xvars, somevals) > 0.98
        # 1-D: a nearly linear fit is a fine learning curve; reject it only
        # if it is actually flat (deterministic, no extra random draws, and
        # fits accepted by the R^2 rule are not affected)
        if (ncol(xvars) == 1 && !is.na(isplane) && isplane)
            isplane <- diff(range(somevals)) < 0.001
        if (!is.na(isplane) && !isplane)
            badfit <- FALSE

    }

    if (!have_mod)
        stop("The GPR surrogate could not be fitted: all ", patience,
            " attempts of DiceKriging::km() failed.")

    # setup function that outputs sd
    fitfun.sd <- function(x) {
        if (!is.null(kmc) && kmc$accepts(x))
            return(kmc$sd(x))
        names(x) <- names(xvars)
        DiceKriging::predict.km(mod, newdata = data.frame(t(x)),
            type = "UK")$sd
    }
    if(!use_noise) fitfun.sd = NULL

    re <- list(fitfun = fitfun, fitfun.sd = fitfun.sd,
        badfit = badfit)

    return(re)
}










# R^2 of a linear fit of y on the design parameters (the plane check),
# i.e. summary(lm(y ~ ., cbind(xvars, y = y)))$r.squared, without the formula
# and summary machinery: the same QR fit (lm.fit on the intercept plus the
# columns of xvars, which is what model.matrix builds) and the same mss / rss
# formulas as summary.lm for an unweighted model with intercept, so the value
# is bit-identical. Uses lm() itself where its behaviour differs (missing or
# non-finite values, a design parameter named y).
plane_r2 <- function(xvars, y) {
    if (!all(is.finite(y)) || "y" %in% names(xvars) ||
        !all(vapply(xvars, is.numeric, logical(1))) ||
        !all(vapply(xvars, function(v) all(is.finite(v)), logical(1))))
        return(suppressWarnings(summary(stats::lm(y ~ ., cbind(xvars,
            y = y)))$r.squared))
    z <- stats::lm.fit(cbind(1, as.matrix(xvars)), y)
    if (z$rank == 0)
        return(0)
    f <- z$fitted.values
    mss <- sum((f - mean(f))^2)
    rss <- sum(z$residuals^2)
    mss/(mss + rss)
}
