plot2d_heat <- function(ds,color.width,color.gradient) {

    dat <- ds$dat
    fit <- ds$fit
    aggregate_fun <- ds$aggregate_fun


    dat_obs <- todataframe(dat, aggregate = TRUE, aggregate_fun = aggregate_fun )
    boundaries <- ds$boundaries
    final <- ds$final
    costfun <- ds$costfun

    # power
    vals <- lapply(boundaries, function(x) x[1]:x[2])
    powerfit <- as.data.frame(expand.grid(vals))
    names(powerfit) <- c("n", "k")
    powerfit$power <- apply(powerfit, 1, fit$fitfun)

    # control color band width (only for diverging)
    if (color.gradient == "diverging") {
    dist = color.width/2
    toofar <- abs(powerfit$power - final$power) > dist
    powerfit$power[toofar] <- NA
    }

    # control color band width (only for linear)
    if (color.gradient == "linear") {
      dist = .3
      toofar <- abs(powerfit$power - final$power) > dist
      powerfit$power[toofar] <- NA
    }


    # cost
    ns <- seq(boundaries[[1]][1], boundaries[[1]][2],
        0.5)
    kstart <- mean(boundaries[[2]])

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

    levels <- c(costlabel, pointlabel, crosslabel)
    powerlabel

    powerlabel <- factor(powerlabel, levels = levels)
    costlabel <- factor(costlabel, levels = levels)
    pointlabel <- factor(pointlabel, levels = levels)
    crosslabel <- factor(crosslabel, levels = levels)

    # labelorder
    labelorder <- c(1, 2, 3)


    pl2 <- ggplot2::ggplot()

    pl2 <- pl2 + ggplot2::geom_tile(ggplot2::aes(x = powerfit$n,
        y = powerfit$k, fill = powerfit$power)) + ggplot2::geom_line(ggplot2::aes(x = eqcost$n,
        y = eqcost$k, col = costlabel)) + ggplot2::geom_point(ggplot2::aes(x = dat_obs$V1,
        y = dat_obs$V2, col = pointlabel)) + ggplot2::geom_point(ggplot2::aes(x = fin$n,
        y = fin$k, col = crosslabel), shape = 4, size = 5,
        stroke = 1.3) + ggplot2::theme_bw()


      if (color.gradient=="diverging") {
        pl2 = pl2 + ggplot2::scale_fill_gradient2(low = "white",
                                                        mid = "#2166AC", high = "white", midpoint = final$power,
                                                        space = "Lab", na.value = NA, guide = ggplot2::guide_colourbar(title = "Power",
                                                                                                                       title.position = "left", barwidth = 8),
                                                        aesthetics = "fill")
      }

    if (color.gradient=="linear") {
      pl2 = pl2 + ggplot2::scale_fill_gradient2(low = "white", mid = "#2166AC", high = "#D62828",  midpoint = final$power,                                               space = "Lab", na.value = NA, guide = ggplot2::guide_colourbar(title = "Power",
                                                                                                               title.position = "left", barwidth = 8),
                                                aesthetics = "fill")
    }

      pl2 = pl2 + ggplot2::scale_colour_manual(breaks = levels,
                                                 values = c("#B2182B", "black", "darkorchid4")[labelorder],
                                                 guide = ggplot2::guide_legend(title = "", override.aes = list(linetype = c("solid",
                                                                                                                            "blank", "blank")[labelorder], shape = c(NA,
                                                                                                                                                                     19, 4)[labelorder], stroke = c(NA, NA, 1.3)[labelorder], size = c(1, 3, 3)[labelorder])))  + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
        ggplot2::theme(legend.position = "bottom")

    pl2

}


