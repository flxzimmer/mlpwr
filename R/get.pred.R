
get.pred <- function(fit, dat, power, costfun, cost,
    boundaries, task) {

    datx <- todataframe(dat, aggregate = TRUE)
    xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
    # 3 most promising previous candidates
    freqs <- sapply(dat, function(x) length(x$y))
    cands <- as.matrix(xvars[order(freqs, decreasing = TRUE)[1:4],
        ])

    midpars <- sapply(boundaries, mean)
    boundmins <- sapply(boundaries, function(x) x[1])
    boundmaxs <- sapply(boundaries, function(x) x[2])
    Domains <- cbind(boundmins, boundmaxs)


    if (task == "desiredpower") {

        greediness <- 3  # 0: not greedy, higher values are more greedy

        fn <- function(x) relu(power - fit$fitfun(x) -
            fit$fitfun.sd(x) * greediness/10) * 10^5 +
            costfun(x)/costfun(midpars)

        if (is.null(fit$fitfun.sd))
            fn <- function(x) relu(power - fit$fitfun(x)) *
                10^5 + costfun(x)/costfun(midpars)

        newre <- rgenoud::genoud(fn, nvars = nrow(Domains),
            data.type.int = TRUE, Domains = Domains,
            print.level = 0, boundary.enforcement = 1,
            pop.size = 20, starting.values = cands)


        badprediction <- abs(fit$fitfun(newre$par) -
            power) > 0.05
    }


    if (task == "costthreshold") {

        fn <- function(x) relu(costfun(x) - cost)/cost *
            10^5 - fit$fitfun(x)

        newre <- rgenoud::genoud(fn, nvars = nrow(Domains),
            data.type.int = TRUE, Domains = Domains,
            print.level = 0, boundary.enforcement = 1,
            pop.size = 20, starting.values = cands)

        badprediction <- FALSE
    }

    points <- data.frame(t(newre$par))
    edgeprediction <- FALSE
    if (any(points == boundmins) | any(points == boundmaxs)) {
        edgeprediction <- TRUE
    }
    # sample all locations if prediction is bad
    # an edgeprediction should be updated at for
    # falsification, it is not the same as other
    # 'bad' predictions (e.g. because of too few
    # data)
    if (badprediction & !edgeprediction) {
        points <- datx[, 1:(length(datx) - 1), drop = FALSE]
    }


    re <- list(points = points, badprediction = badprediction,
        edgeprediction = edgeprediction)

    return(re)
}
