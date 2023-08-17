

check.term <- function(evaluations, ci, time, dat,
    time_temp, fit, pred, ci_perc) {

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
            fit$fitfun.sd <- fit.surrogate(dat = dat,
                surrogate = "gpr")$fitfun.sd
        if(is.null(fit$fitfun.sd)) return(re)

        # print(str(pred))
        sdval <- fit$fitfun.sd(as.numeric(pred$points))
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
