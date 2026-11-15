# Noise variances for the GPR surrogate (gauss.fit).
#
# noise_fun = "bernoulli" (mlpwr <= 1.1.1): the aggregate at design i is
# p_hat_i = s_i / k_i (s_i successes in k_i simulations) with variance
# p_hat_i (1 - p_hat_i) / k_i, i.e. the design's own proportion is plugged in
# (getweight(), with one pseudo draw added when all draws agree). This makes
# the weight 1 / var_i a function of the design's own noise: for p > .5 a
# design whose draws came out high gets a smaller variance, hence more
# weight, and the GPR mean is pulled upwards (downwards for p < .5).
#
# noise_fun = "logistic" (default from 1.2.0): p_i from a binomial GLM,
# logit(p) linear in the design parameters, fitted to all designs. Each
# design's own draws enter its p_i only through a fit with (number of design
# parameters + 1) coefficients, so the weights are nearly independent of the
# noise they weight. p is kept inside [1 / (2 (k_i + 1)), 1 - 1 / (2 (k_i + 1))]
# so that p_hat = 0 or 1 and small k_i give a positive variance.
#
# Only the GPR surrogate's own fit uses "logistic". Everything else that
# gets noise_fun (the extra GPR that supplies the SE for other surrogates and
# for the ci rule) treats "logistic" like "bernoulli" (raw_noise_fun()), so
# their results are those of the plug-in variance.
noise_fun_values <- c("logistic", "bernoulli")

check_noise_fun <- function(noise_fun) {
    if (is.function(noise_fun) || (is.character(noise_fun) &&
        length(noise_fun) == 1 && !is.na(noise_fun) &&
        noise_fun %in% noise_fun_values))
        return(invisible(noise_fun))
    stop("noise_fun must be \"logistic\" (default), \"bernoulli\" (the ",
        "variance of mlpwr <= 1.1.1) or a function, not ",
        paste(deparse(noise_fun), collapse = " "), call. = FALSE)
}

# find.design(surrogate = ): one of the four names; "logreg" and "reg" model
# one design parameter only
surrogate_values <- c("gpr", "svr", "logreg", "reg")

check_surrogate <- function(surrogate, n_dims) {
    if (!(is.character(surrogate) && length(surrogate) == 1 &&
        !is.na(surrogate) && surrogate %in% surrogate_values))
        stop("surrogate must be \"gpr\", \"svr\", \"logreg\" or \"reg\" ",
            "(in lower case), not ", paste(deparse(surrogate), collapse = " "),
            call. = FALSE)
    if (n_dims > 1 && surrogate %in% c("logreg", "reg"))
        stop("surrogate = \"", surrogate, "\" works with one design ",
            "parameter only; use \"gpr\" or \"svr\" for ", n_dims,
            " design parameters.", call. = FALSE)
    invisible(surrogate)
}

# find.design(power = ): a single number. For statistical power (the default
# aggregate_fun = mean with a character noise_fun) it must lie strictly
# between 0 and 1; a custom criterion (aggregate_fun or noise_fun given as
# functions, e.g. a ratio) can have any target.
check_power <- function(power, aggregate_fun, noise_fun) {
    if (!(is.numeric(power) && length(power) == 1 && is.finite(power)))
        stop("power must be a single number, not ",
            paste(deparse(power), collapse = " "), call. = FALSE)
    if (identical(aggregate_fun, mean) && is.character(noise_fun) &&
        !(power > 0 && power < 1))
        stop("power must be strictly between 0 and 1 (e.g. power = .8), ",
            "not ", power, ".", call. = FALSE)
    invisible(power)
}

# "logistic" -> "bernoulli" for the SE-only GPR fits (see above)
raw_noise_fun <- function(noise_fun) {
    if (identical(noise_fun, "logistic")) "bernoulli" else noise_fun
}

# The logistic regression ("logistic", used by the GPR surrogate's own fit)
# models 0/1 (or TRUE/FALSE) simulation results. With other results
# find.design() falls back to "bernoulli" (with a message), and so does
# noise_variances() if such results only turn up later in a search: that is
# exactly the variance of mlpwr 1.1.1, which uses the mean of any results.
# NA results are left to the fits.
binary_results <- function(dat) {
    y <- as.numeric(unlist(lapply(dat, function(v) v$y), use.names = FALSE))
    !any(!is.na(y) & y != 0 & y != 1)
}

noise_variances <- function(dat, xvars, noise_fun) {
    if (is.function(noise_fun)) return(sapply(dat, noise_fun))
    if (identical(noise_fun, "logistic") && binary_results(dat))
        return(logistic_var(dat, xvars))
    if (identical(noise_fun, "logistic"))
        return(getweight(dat, weight.type = "var"))
    if (identical(noise_fun, "bernoulli"))
        return(getweight(dat, weight.type = "var"))
    check_noise_fun(noise_fun)
}

logistic_var <- function(dat, xvars) {
    k <- vapply(dat, function(v) length(v$y), numeric(1))
    s <- vapply(dat, function(v) sum(as.numeric(v$y)), numeric(1))
    # design parameters scaled to [0, 1] for a well-conditioned fit
    m <- nrow(xvars)
    X <- matrix(1, m, ncol(xvars) + 1)
    for (j in seq_along(xvars)) {
        v <- xvars[[j]]
        lo <- min(v)
        d <- max(v) - lo
        X[, j + 1] <- if (d > 0) (v - lo)/d else v * 0
    }
    p <- binomial_fitted(X, s/k, k)
    if (is.null(p) || anyNA(p)) p <- rep(sum(s)/sum(k), length(k))
    lo <- 1/(2 * (k + 1))
    p <- pmin(pmax(p, lo), 1 - lo)
    p * (1 - p)/k
}

# Fitted values of the binomial GLM logit(p) ~ X - 1 (X has an intercept
# column), y = s / k, prior weights k: the same numbers as
# glm(cbind(s, k - s) ~ ., family = binomial) and glm.fit(X, y, weights = k,
# family = binomial()), i.e. glm.fit's IRLS loop with its starting values,
# QR solver and tolerance (epsilon 1e-8, maxit 25) and the family's own C
# link functions, but without glm.fit's set-up and summary work (about 4x
# faster for the few designs of a search). Any branch that glm.fit would
# handle specially (non-finite coefficients or deviance, fitted values on
# the boundary, uninformative observations) hands over to glm.fit itself,
# so the result is always glm.fit's.
binomial_fitted <- function(X, y, wts) {
    fam <- binomial_family()
    glmfit <- function() {
        mod <- tryCatch(suppressWarnings(stats::glm.fit(X, y, weights = wts,
            family = fam)), error = function(e) NULL)
        if (is.null(mod)) NULL else as.numeric(mod$fitted.values)
    }
    if (any(wts <= 0)) return(glmfit())
    eta <- fam$linkfun((wts * y + 0.5)/(wts + 1))
    mu <- fam$linkinv(eta)
    devold <- sum(fam$dev.resids(y, mu, wts))
    start <- NULL
    tol <- min(1e-07, 1e-08/1000)
    for (iter in 1:25) {
        mu.eta.val <- fam$mu.eta(eta)
        if (any(mu.eta.val == 0)) return(glmfit())
        z <- eta + (y - mu)/mu.eta.val
        w <- sqrt((wts * mu.eta.val^2)/fam$variance(mu))
        fit <- stats::.lm.fit(X * w, z * w, tol = tol)
        if (any(!is.finite(fit$coefficients))) return(glmfit())
        start[fit$pivot] <- fit$coefficients
        eta <- drop(X %*% start)
        mu <- fam$linkinv(eta)
        dev <- sum(fam$dev.resids(y, mu, wts))
        if (!is.finite(dev) || !fam$validmu(mu)) return(glmfit())
        if (abs(dev - devold)/(0.1 + abs(dev)) < 1e-08) break
        devold <- dev
    }
    as.numeric(mu)
}

binomial_family <- local({
    fam <- NULL
    function() {
        if (is.null(fam)) fam <<- stats::binomial()
        fam
    }
})
