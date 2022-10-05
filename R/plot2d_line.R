plot2d_line <- function(ds, trim) {

    dat <- ds$dat
    fit <- ds$fit

    dat_obs <- todataframe(dat)
    boundaries <- ds$boundaries
    final <- ds$final
    costfun <- ds$costfun

    ns <- seq(boundaries[[1]][1], boundaries[[1]][2],
        0.5)
    kstart <- mean(boundaries[[2]])

    eqpower.k <- sapply(ns, function(n) {
        fn <- function(k) abs(fit$fitfun(c(n, k)) -
            final$power)
        a <- stats::optim(kstart, fn, method = "L-BFGS-B",
            lower = boundaries[[2]][[1]], upper = boundaries[[2]][[2]],
            control = list(factr = 1e+11))
        valid <- a$value < 0.001
        if (valid)
            return(a$par) else return(NA)
    })

    eqpower <- data.frame(n = ns, k = eqpower.k)
    eqpower <- eqpower[!is.na(eqpower$k), ]

    eqcost.k <- sapply(ns, function(n) {
        fn <- function(k) abs(costfun(c(n, k)) - final$cost)
        a <- stats::optim(kstart, fn, method = "L-BFGS-B",
            lower = boundaries[[2]][[1]], upper = boundaries[[2]][[2]],
            control = list(factr = 1e+11))
        valid <- a$value < 0.001
        if (valid)
            return(a$par) else return(NA)
    })

    eqcost <- data.frame(n = ns, k = eqcost.k)
    eqcost <- eqcost[!is.na(eqcost$k), ]

    if (trim) {

        # trim power and cost to a common area
        powermins <- apply(eqpower, 2, min)
        powermaxs <- apply(eqpower, 2, max)
        costmins <- apply(eqcost, 2, min)
        costmaxs <- apply(eqcost, 2, max)
        mins <- apply(rbind(powermins, costmins), 2,
            max)
        maxs <- apply(rbind(powermaxs, costmaxs), 2,
            min)
        ind1 <- apply(eqpower, 1, function(x) !any(x <
            mins))
        ind2 <- apply(eqpower, 1, function(x) !any(x >
            maxs))
        eqpower <- eqpower[ind1 & ind2, ]
        ind1 <- apply(eqcost, 1, function(x) !any(x <
            mins))
        ind2 <- apply(eqcost, 1, function(x) !any(x >
            maxs))
        eqcost <- eqcost[ind1 & ind2, ]

        # trim observed data
        ind1 <- apply(dat_obs[, 1:2], 1, function(x) !any(x <
            mins))
        ind2 <- apply(dat_obs[, 1:2], 1, function(x) !any(x >
            maxs))
        dat_obs <- dat_obs[ind1 & ind2, ]

        if (nrow(eqpower) == 0 | nrow(eqcost) == 0 |
            nrow(dat_obs) == 0)
            stop("Plotting area is trimmed by default, leading to a too small area in this case. Try setting trim=FALSE")
    }

    # final value
    fin <- final$design
    names(fin) <- c("n", "k")

    # axis labels
    xlab <- names(boundaries)[1]
    ylab <- names(boundaries)[2]


    # labels
    powerlabel <- paste0("Power = ", round(final$power,
        3))
    costlabel <- paste0("Cost = ", round(final$cost,
        2))
    pointlabel <- "Designs"
    crosslabel <- "Optimal design"

    levels <- c(powerlabel, costlabel, pointlabel,
        crosslabel)

    powerlabel <- factor(powerlabel, levels = levels)
    costlabel <- factor(costlabel, levels = levels)
    pointlabel <- factor(pointlabel, levels = levels)
    crosslabel <- factor(crosslabel, levels = levels)

    # labelorder
    labelorder <- c(3, 2, 1, 4)

    pl2 <- ggplot2::ggplot()

    pl2 <- pl2 + ggplot2::geom_line(ggplot2::aes(x = eqpower$n,
        y = eqpower$k, col = powerlabel)) + ggplot2::geom_line(ggplot2::aes(x = eqcost$n,
        y = eqcost$k, col = costlabel)) + ggplot2::geom_point(ggplot2::aes(x = dat_obs$V1,
        y = dat_obs$V2, col = pointlabel)) + ggplot2::geom_point(ggplot2::aes(x = fin$n,
        y = fin$k, col = crosslabel), shape = 3, size = 5) +
        ggplot2::theme_bw() + ggplot2::scale_colour_manual(breaks = levels,
        values = c("black", "#B2182B", "#2166AC", "green")[labelorder],
        guide = ggplot2::guide_legend(title = "", override.aes = list(linetype = c("blank",
            "solid", "solid", "blank")[labelorder],
            shape = c(20, NA, NA, 3)[labelorder]))) +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
        ggplot2::theme(legend.position = "bottom")

    pl2
}


