

check.term <- function(evaluations, ci, time, dat,
    time_temp, fit, pred, ci_perc, aggregate_fun = mean,
    noise_fun = "logistic", control = list()) {

    re <- FALSE

    # check termination (evaluations)
    if (!is.null(evaluations)) {
        used <- usedevaluations(dat)
        evaluations.remaining <- evaluations - used
        if (evaluations.remaining <= 0)
            re <- TRUE
    }

    # check termination (ci)
    if (!is.null(ci) && !is.na(ci)) {

        if (is.null(fit$fitfun.sd))
            # the SE-only GPR uses the user's aggregate_fun, noise_fun,
            # and control like the main fit (use_noise stays TRUE:
            # the ci rule needs an SE)
            fit$fitfun.sd <- fit.surrogate(dat = dat,
                surrogate = "gpr", control = control,
                aggregate_fun = aggregate_fun, use_noise = TRUE,
                noise_fun = raw_noise_fun(noise_fun))$fitfun.sd

        # after a bad prediction, pred$points holds all designs: use the
        # largest SD among them
        sdval <- max(apply(as.matrix(pred$points), 1,
            function(p) fit$fitfun.sd(as.numeric(p))))
        interval <- sdval * stats::qnorm(ci_perc +
            (1 - ci_perc)/2) * 2
        if (interval < ci)
            re <- TRUE

    }

    # check termination (time)
    if (!is.null(time)) {
        time_used <- as.numeric(timer(time_temp))
        if (time_used > time)
            re <- TRUE
    }


    return(re)

}
