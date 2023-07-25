
get.pred <- function(fit, dat, power, costfun, cost,
    boundaries, task,aggregate_fun,integer,use_noise) {

    datx <- todataframe(dat, aggregate = TRUE, aggregate_fun = aggregate_fun )
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

        if (!use_noise | is.null(fit$fitfun.sd))
            fn <- function(x) relu(power - fit$fitfun(x)) *
                10^5 + costfun(x)/costfun(midpars)

        if(integer) newre <- rgenoud::genoud(fn, nvars = nrow(Domains),
            data.type.int = integer, Domains = Domains,
            print.level = 0, boundary.enforcement = 1,
            pop.size = 20, starting.values = cands)

        if(!integer)
          newre <- rgenoud::genoud(fn, nvars = nrow(Domains), data.type.int = integer, Domains = Domains, print.level = 0, boundary.enforcement = 2,pop.size = 20, starting.values = cands,gradient.check=F)


        # x = 20:400
        # y = sapply(x,fit$fitfun)

          # badprediction <- abs(fit$fitfun(newre$par) -
          #   power) > 0.05
          badprediction <- abs(fit$fitfun(newre$par) -
            power) > 0.4

        # if(badprediction) browser()
          # print(badprediction)
          # print(newre$par)
    }


    if (task == "costthreshold") {

        fn <- function(x) relu(costfun(x) - cost)/cost *
            10^5 - fit$fitfun(x)

        newre <- rgenoud::genoud(fn, nvars = nrow(Domains),
            data.type.int = integer, Domains = Domains,
            print.level = 0, boundary.enforcement = 1,
            pop.size = 20, starting.values = cands)

        badprediction <- FALSE
    }

    points <- data.frame(t(newre$par))
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
