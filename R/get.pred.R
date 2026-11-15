
get.pred <- function(fit, dat, power, costfun, cost,
    boundaries, task,aggregate_fun,integer,use_noise,optimizer = "auto",
    cache = NULL) {

    datx <- todataframe(dat, aggregate = TRUE, aggregate_fun = aggregate_fun )
    xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
    # up to 4 most promising previous candidates (fewer if there are
    # fewer designs, which would otherwise give NA start values)
    freqs <- sapply(dat, function(x) length(x$y))
    cands <- as.matrix(xvars[utils::head(order(freqs, decreasing = TRUE), 4),
        , drop = FALSE])

    midpars <- sapply(boundaries, mean)
    boundmins <- sapply(boundaries, function(x) x[1])
    boundmaxs <- sapply(boundaries, function(x) x[2])
    Domains <- cbind(boundmins, boundmaxs)
    # the grid's costs cannot be computed cheaply in this search: genoud
    # directly (same result as optimize_design() falling back to it)
    if (scan_refused(cache, Domains))
        optimizer <- "genoud"


    if (task == "desiredpower") {

        greediness <- 3  # 0: not greedy, higher values are more greedy

        fn <- function(x) relu(power - fit$fitfun(x) -
            fit$fitfun.sd(x) * greediness/10) * 10^5 +
            costfun(x)/costfun(midpars)

        if (!use_noise | is.null(fit$fitfun.sd))
            fn <- function(x) relu(power - fit$fitfun(x)) *
                10^5 + costfun(x)/costfun(midpars)

        # the same objective for the rows of a matrix (screening the grid in
        # the exact search; fn itself decides between the best candidates)
        fn_batch <- function(X, idx) {
            cm <- costfun(midpars)
            costs <- grid_costs(costfun, X, idx, Domains, cache)
            if (is.null(costs))
                return(NULL)
            if (!use_noise | is.null(fit$fitfun.sd))
                return(relu(power - batch_mean(fit$fitfun, X)) * 10^5 +
                    costs/cm)
            p <- batch_meansd(fit$fitfun, fit$fitfun.sd, X)
            relu(power - p$mean - p$sd * greediness/10) * 10^5 + costs/cm
        }

        if(integer) par <- optimize_design(fn, Domains, integer, cands,
            optimizer = optimizer, fn_batch = fn_batch,
            n_designs = length(dat), cache = cache, boundary.enforcement = 1)

        if(!integer)
          par <- optimize_design(fn, Domains, integer, cands,
            optimizer = optimizer, boundary.enforcement = 2,
            gradient.check = FALSE)

          badprediction <- abs(fit$fitfun(par) -
            power) > 0.4

    }


    if (task == "costthreshold") {

        fn <- function(x) relu(costfun(x) - cost)/cost *
            10^5 - fit$fitfun(x)

        fn_batch <- function(X, idx) {
            costs <- grid_costs(costfun, X, idx, Domains, cache)
            if (is.null(costs))
                return(NULL)
            relu(costs - cost)/cost * 10^5 - batch_mean(fit$fitfun, X)
        }

        par <- optimize_design(fn, Domains, integer, cands,
            optimizer = optimizer, fn_batch = fn_batch,
            n_designs = length(dat), cache = cache, boundary.enforcement = 1)

        badprediction <- FALSE
    }

    points <- data.frame(t(par))
    edgeprediction <- FALSE
    if (!fit$badfit & (any(points == boundmins) | any(points == boundmaxs))) {
        edgeprediction <- TRUE
    }
    # sample all locations if prediction is bad
    # an edgeprediction should be updated at for
    # falsification, it is not the same as other
    # 'bad' predictions (e.g. because of too few
    # data)
    if (fit$badfit | (badprediction & !edgeprediction)) {
        bad.points <- points
        points <- datx[, 1:(length(datx) - 1), drop = FALSE]
    } else {
      bad.points <- NA
    }

    re <- list(points = points, badprediction = badprediction,
        edgeprediction = edgeprediction, bad.points = bad.points)

    return(re)
}
