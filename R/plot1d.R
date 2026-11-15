plot1d <- function(ds, design, adderrorbars, addribbon) {

    dat <- ds$dat
    fit <- ds$fit
    aggregate_fun <- ds$aggregate_fun

    ## choose plot

    if(is.null(fit$fitfun.sd)) adderrorbars = addribbon = FALSE

    # set default options if arguments aren't
    # specified
    if (is.null(addribbon) & is.null(adderrorbars)) {

      addribbon <- TRUE
      adderrorbars <- FALSE

      # if (!is.null(fit$fitfun.sd)) {
      # addribbon = TRUE adderrorbars = FALSE }
      # else { addribbon = FALSE adderrorbars =
      # TRUE }
    }
    if (is.null(addribbon))
      addribbon <- FALSE
    if (is.null(adderrorbars))
      adderrorbars <- FALSE


    # Actual SD
    dat_obs <- todataframe(dat, aggregate = TRUE, aggregate_fun = aggregate_fun )
    if (adderrorbars) dat_obs$sd <- getweight(dat, "sd")

    boundaries <- ds$boundaries

    # grid of design values to plot the prediction at: all integers, or an
    # evenly spaced grid for continuous designs (integer = FALSE)
    plotgrid <- function(b) {
        if (isFALSE(ds$settings$integer))
            seq(b[1], b[2], length.out = 101)
        else seq(b[1], b[2])
    }

    # treat 2D simfun differntly, convert to 1D
    # according to specification
    if (!is.null(design)) {

        namesx <- names(boundaries)
        specified <- !sapply(design, is.na)

        # vary the non-specified variable
        boundariesx <- unlist(boundaries[!specified])
        ns <- plotgrid(boundariesx)

        nsx <- lapply(ns, function(x) {
            a <- c()
            a[specified] <- as.numeric(design[specified])
            a[!specified] <- x
            a
        })

        # select from dat_obs
        ind <- dat_obs[c(specified, FALSE, FALSE)] ==
            as.numeric(design[specified])
        dat_obs <- dat_obs[ind, ]
        # observed designs are drawn at the free (non-specified) parameter
        dat_obs$xobs <- dat_obs[[paste0("V", which(!specified))]]

        # setup xlab
        a1 <- names(ds$final$design)[!specified]
        a2 <- paste(names(design)[specified], "=",
            design[specified], sep = " ", collapse = ",")
        xlab <- paste0(a1, " (", a2, ")")

    }

    # 1D Case
    if (is.null(design)) {
        boundariesx <- unlist(boundaries)
        xlab <- names(ds$final$design)
        dat_obs$xobs <- dat_obs$V1
        ns <- plotgrid(boundariesx)
        nsx <- ns
    }


    # Prediction
    dat_pred <- data.frame(n = ns, y = sapply(nsx,
        fit$fitfun), type = "Prediction")


    # Estimated SD
    if(!(!adderrorbars&!addribbon)) {
    dat_sd <- data.frame(n = ns, pred = dat_pred$y,
        sd = sapply(nsx, fit$fitfun.sd))
    dat_sd$ymin <- dat_sd$pred - dat_sd$sd
    dat_sd$ymax <- dat_sd$pred + dat_sd$sd
}
    # Optional: Censor the ribbon to plausible
    # values (between 0 and 1) dat_sd$ymin =
    # sapply(dat_sd$pred - dat_sd$sd,function(x)
    # max(0,x)) dat_sd$ymax = sapply(dat_sd$pred
    # + dat_sd$sd,function(x) min(1,x))


    ## build plot

    pl2 <- ggplot2::ggplot()
    if (addribbon)
        pl2 <- pl2 + ggplot2::geom_ribbon(ggplot2::aes(ymin = dat_sd$ymin,
            ymax = dat_sd$ymax, x = ns), fill = "grey70")

    if (adderrorbars)
        pl2 <- pl2 + ggplot2::geom_errorbar(ggplot2::aes(ymin = dat_obs$y -
            dat_obs$sd, ymax = dat_obs$y + dat_obs$sd,
            x = dat_obs$xobs))


    pl2 <- pl2 + ggplot2::geom_line(ggplot2::aes(x = dat_pred$n,
        y = dat_pred$y)) + ggplot2::geom_point(ggplot2::aes(x = dat_obs$xobs,
        y = dat_obs$y)) + ggplot2::theme_bw() + ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::xlab(xlab) + ggplot2::ylab("Power") +
        ggplot2::theme(legend.position = "bottom")


}




