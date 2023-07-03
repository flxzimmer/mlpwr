
#' Find optimal study designs
#'
#' Perform a surrogate modeling approach to search for optimal study design parameters. For further guidance on how to use the package and the `find.design` function specifically, see the \href{https://github.com/flxzimmer/mlpwr}{Readme.md file}.
#'
#' @param simfun function to generate hypothesis test results with. Takes design parameters as input and outputs a logical (result of the hypothesis test). The function can take the designs through one argument as a vector or through multiple arguments. For example, function(x) where x is later used with x=c(n,k) for two design parameters n and k is valid. Also valid is a definition using function(n,k).
#' @param boundaries list containing lower and upper bounds of the design space. The list should consist of named vectors, each containing the upper and lower bound for the respective design parameter dimensions. For one design parameter dimension, can also be a vector containing the upper and lower bounds.
#' @param power numeric; desired statistical power
#' @param evaluations integer; number of dgf evaluations to be performed before termination
#' @param ci numeric; desired width of the confidence interval at the predicted value on termination.
#' @param ci_perc numeric; specifying the desired confidence interval, e.g. 95% or 99%.
#' @param time integer; seconds until termination
#' @param costfun function that takes a vector of design parameters as input and outputs a cost, e.g. monetary costs. Necessary for simfuns with multiple input dimensions.
#' @param cost numeric; cost threshold. Design parameter set with highest power is searched among sets that fulfill this cost threshold.
#' @param surrogate character; which surrogate model should be used. The default is 'logreg' for one design parameter and 'gpr' for multiple design parameters. The current options are: 'gpr', 'svr', 'logreg', 'reg' for one-dimensional designs and 'gpr' and 'svr' for multi-dimensional designs.
#' @param n.startsets integer; number of startsets used per dimension of simfun
#' @param setsize The number of draws from the simfun in each iteration
#' @param init.perc numeric; percentage of evaluations used for the initialization phase
#' @param dat list of data from a previous design result.
#' @param silent logical; suppresses output during the search.
#' @param autosave_dir character; file location for saving the dat object after each update.
#' @param control list specifying arguments passed to the surrogate models. For example, list(covtype='gauss') can be used with the gpr surrogate to use a different covariance structure than the default.
#' @param continue Object of class designresult as created by the find.design function. Will be used to continue the search, using all collected simulation results so far.
#'
#' @return function returns an object of class designresult
#' @export
#'
#' @examples \donttest{
#' #Load a simulation function
#' simfun = example.simfun('ttest')
#' # Perform the search
#' ds = find.design(simfun = simfun, boundaries = c(100,300), power = .95)
#' # Output the results
#' summary(ds)
#' # Plot results
#' plot(ds)
#'
#' # Two-dimensional simulation function:
#' simfun = example.simfun('anova')
#' # Perform the search
#' res = find.design(simfun = simfun,
#'  costfun = function(n,n.groups) 5*n+20*n.groups,
#'  boundaries = list(n = c(10, 150), n.groups = c(5, 30)),
#'  power = .95)
#' # Output the results
#' summary(ds)
#' # Plot results
#' plot(ds)
#'}
find.design <- function(simfun, boundaries, power = NULL,
    evaluations = 4000, ci = NULL, ci_perc = 0.95,
    time = NULL, costfun = NULL, cost = NULL, surrogate = NULL,
    n.startsets = 4, init.perc = 0.2, setsize = 100,
    continue = NULL, dat = NULL, silent = FALSE, autosave_dir = NULL,
    control = list()) {

    # save seed for reproducibility
    seed <- .Random.seed

    # Start the clock
    time_temp <- timer()

    # initiate continue
    if (!is.null(continue)) {
        simfun <- continue$simfun
        costfun <- continue$costfun
        boundaries <- continue$boundaries
        power <- continue$power
        cost <- continue$cost
        dat <- continue$dat
        # take new surrogate if it is not NULL
        # and different than before
        surrogate <- ifelse(!is.null(surrogate) &&
            surrogate != continue$surrogate, surrogate,
            continue$surrogate)
    }

    # set a default costfunction (identity) if
    # not specified
    if (is.null(costfun))
        costfun <- function(x) sum(x)

    # convert boundaries to list and set name
    # from simfun
    if (!is.list(boundaries)) {
        boundaries <- list(boundaries)
        names(boundaries) <- names(as.list(args(simfun)))[1]
    }

    # determine number of points
    n.points <- n.startsets * length(boundaries)

    # Set setsize according to a percentage of
    # evaluations, if available
    if (!is.null(evaluations))
        setsize <- ceiling(evaluations * init.perc/n.points)

    # adjust number of evaluations for continuing
    # a search
    if (!is.null(dat)) {
        evaluations <- usedevaluations(dat) + evaluations
    }

    simfun <- fix.argtype(simfun, boundaries)
    costfun <- fix.argtype(costfun, boundaries)


    # Generate initialization data if not
    # available
    if (is.null(dat)) {
        points <- initpoints(boundaries = boundaries,
            n.points = n.points)
        dat <- addval(simfun = simfun, points = points,
            each = setsize, autosave_dir = autosave_dir)
    }

    # choose surrogate (if not specified)
    if (is.null(surrogate)) {
        if (length(boundaries) == 1)
            surrogate <- "logreg"
        if (length(boundaries) > 1)
            surrogate <- "gpr"
    }

    # warn if ci is termination critrerion
    # without gpr surrogate
    if (!is.null(ci) && surrogate != "gpr")
        warning("Additionally fitting a GPR each update for calculating the SE. Consider switchting to GPR to speed up the estimation")



    # Error if neither power or cost is given or
    # both are given
    if (is.null(power) & is.null(cost))
        stop("Either the power or cost argument must be supplied")
    if (!is.null(power) & !is.null(cost))
        stop("Only one of power or cost argument can be supplied")

    # determine the optimization task to perform
    if (!is.null(cost))
        task <- "costthreshold"
    if (!is.null(power))
        task <- "desiredpower"

    ############################################################################## start
    ############################################################################## the
    ############################################################################## search
    repeat {

        # FIT: Fit a surrogate model
        fit <- fit.surrogate(dat = dat, surrogate = surrogate,
            lastfit = ifelse(exists("fit"), fit, 0),
            control = control)

        # count bad fits (e.g. plane fitted)
        if (fit$badfit)
            n.bad.fits <- ifelse(exists("n.bad.fits"),
                n.bad.fits + 1, 1)

        # PREDICTION: Get a prediction from the
        # fitted model
        pred <- get.pred(fit = fit, dat = dat, power = power,
            costfun = costfun, cost = cost, boundaries = boundaries,
            task = task)

        # count bad predictions (no sensible
        # value found)
        if (pred$badprediction)
            n.bad.predictions <- ifelse(exists("n.bad.predictions"),
                n.bad.predictions + 1, 1)

        # count edge predictions (value on edge
        # of parameter space) and issue warning
        # if happened too often
        if (pred$edgeprediction) {
            n.edgepredictions <- ifelse(exists("n.edgepredictions"),
                n.edgepredictions + 1, 1)
            if (n.edgepredictions == 5)
                warning("The predicted value consistently lies at the edge of the parameter space. Consider adjusting the boundaries.")
        }

        # check TERMINATION
        is.terminate <- check.term(evaluations = evaluations,
            ci = ci, time = time, dat = dat, time_temp = time_temp,
            fit = fit, pred = pred, ci_perc = ci_perc)
        if (is.terminate)
            break

        # UPDATING: sample from the DGF
        dat <- addval(simfun = simfun, dat = dat, points = pred$points,
            each = max(ceiling(setsize/nrow(pred$points)),
                1), autosave_dir = autosave_dir)

        # number of performed updates
        n_updates <- ifelse(exists("n_updates"), n_updates +
            1, 1)

        # print current progress
        if (!silent)
            print.progress(n_updates = n_updates, evaluations_used = usedevaluations(dat),
                time_used = timer(time_temp))

    }
    ##############################################################################

    # move to next line
    cat("\n")

    # Optional for the final output: Generate SD
    # from a GP if using a different surrogate
    if (is.null(fit$fitfun.sd))
        fit$fitfun.sd <- fit.surrogate(dat = dat, surrogate = "gpr")$fitfun.sd

    # Stop the clock
    time_used <- timer(time_temp)

    # Warning if ending with a bad prediction
    if (pred$badprediction) {
        pred$points <- pred$bad.points
        warning("No good design found after the final update.")
    }


    final <- list(design = pred$points, power = fit$fitfun(as.numeric(pred$points)),
        cost = costfun(as.numeric(pred$points)), se = fit$fitfun.sd(as.numeric(pred$points)))
    names(final$design) <- names(boundaries)

    # Collect Results
    re <- list(final = final, dat = dat, evaluations_used = usedevaluations(dat),
        surrogate = surrogate, fit = fit, time_used = time_used,
        n_updates = ifelse(exists("n_updates"), n_updates,
            0), n.bad.predictions = ifelse(exists("n.bad.predictions"),
            n.bad.predictions, 0), n.bad.fits = ifelse(exists("n.bad.fits"),
            n.bad.fits, 0), call = match.call(), seed = seed,
        costfun = costfun, boundaries = boundaries,
        simfun = simfun, cost = cost, power = power)

    class(re) <- "designresult"

    return(re)
}


