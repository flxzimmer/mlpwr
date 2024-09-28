
#' Find optimal study designs
#'
#' Perform a surrogate modeling approach to search for optimal study design parameters. For further guidance on how to use the package and the `find.design` function specifically, see the \href{https://github.com/flxzimmer/mlpwr}{Readme.md file}.
#'
#' @param simfun function to generate hypothesis test results with. Takes design parameters as input and outputs a logical (result of the hypothesis test). The function can take the designs through one argument as a vector or through multiple arguments. For example, function(x) where x is later used with x=c(n,k) for two design parameters n and k is valid. Also valid is a definition using function(n,k).
#' @param boundaries list containing lower and upper bounds of the design space. The list should consist of named vectors, each containing the upper and lower bound for the respective design parameter dimensions. For one design parameter dimension, can also be a vector containing the upper and lower bounds.
#' @param power numeric; desired statistical power
#' @param evaluations integer; number of simfun evaluations to be performed before termination
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
#' @param autosave_dir character; file location for saving the `dat` object after each update. The `dat` object is saved in `autosave_dir/dat_autosave.Rdata`. It can be loaded for example using `load(paste0(autosave_dir,"/dat_autosave.Rdata"))`.
#' @param control list specifying arguments passed to the surrogate models. For example, list(covtype='gauss') can be used with the gpr surrogate to use a different covariance structure than the default.
#' @param continue Object of class designresult as created by the find.design function. Will be used to continue the search, using all collected simulation results so far.
#' @param goodvals character to indicate whether higher or lower criterion values are preferable given equal cost. For statistical power, higher values are better, so its "high", otherwise "low".
#' @param aggregate_fun function to aggregate results of the evaluations of the simulation function. The default is mean as for statistical power.
#' @param noise_fun function to calculate the noise or variance of the aggregated results of the Monte Carlo evaluations. Can also be the character "bernoulli" (default) to indicate the variance of the bernoulli distribution used for statistical power. This function is \eqn{p(1-p)/n} with \eqn{p} being the statistical power and \eqn{n} being the number of performed evaluations.
#' @param integer logical to indicate whether the design paramaters are integers or not. The default is "TRUE" which is suitable for example for sample size.
#' @param use_noise logical to indicate whether noise variance should be used. The default is TRUE.
#' @param goodvals character indicating whether higher or lower criterion values are preferable given equal cost; the default is "high" for statistical power, the other option is "low".
#' @param aggregate_fun function to aggregate results of the evaluations of the simulation function; the default is `mean`, as for statistical power.
#' @param noise_fun function to calculate the noise or variance of the aggregated results of the Monte Carlo evaluations; can also be the character value "bernoulli" (default) to indicate the variance of the Bernoulli distribution used for statistical power. This function is \eqn{p(1-p)/n}, where \eqn{p} is the statistical power and \eqn{n} is the number of performed evaluations.
#' @param integer logical  indicating whether the design parameters are integers or not; the default is `TRUE`, which is suitable for sample size, for example.
#' @param use_noise logical indicating whether noise variance should be used; the default is `TRUE`.
#'
#' @return function returns an object of class designresult
#' @export
#'
#' @examples \donttest{
#' ## T-test example:
#'
#' # Load a simulation function
#' simfun <- example.simfun('ttest')
#' # Perform the search
#' ds <- find.design(simfun = simfun, boundaries = c(100,300), power = .95)
#' # Output the results
#' summary(ds)
#' # Plot results
#' plot(ds)
#'
#' ## Two-dimensional simulation function:
#'
#' simfun <- example.simfun('anova')
#' # Perform the search
#' ds <- find.design(simfun = simfun,
#'  costfun = function(n,n.groups) 5*n+20*n.groups,
#'  boundaries = list(n = c(10, 150), n.groups = c(5, 30)),
#'  power = .95)
#' # Output the results
#' summary(ds)
#' # Plot results
#' plot(ds)
#'
#'
#' ##  Mixed model example with a custom, two-dimensional simulation function:
#'
#' library(lme4)
#' library(lmerTest)
#'
#' # Simulation function
#' simfun_multilevel <- function(n.per.school,n.schools) {
#'
#'   # generate data
#'   group = rep(1:n.schools,each=n.per.school)
#'   pred = factor(rep(c("old","new"),n.per.school*n.schools),levels=c("old","new"))
#'   dat = data.frame(group = group, pred = pred)
#'
#'   params <- list(theta = c(.5,0,.5), beta = c(0,1),sigma = 1.5)
#'   names(params$theta) = c("group.(Intercept)","group.prednew.(Intercept)","group.prednew")
#'   names(params$beta) = c("(Intercept)","prednew")
#'   dat$y <- simulate.formula(~pred + (1 + pred | group), newdata = dat, newparams = params)[[1]]
#'
#'   # test hypothesis
#'   mod <- lmer(y ~ pred + (1 + pred | group), data = dat)
#'   pvalue <- summary(mod)[["coefficients"]][2,"Pr(>|t|)"]
#'   pvalue < .01
#' }
#' # Cost function
#' costfun_multilevel <- function(n.per.school, n.schools) {
#'   100 * n.per.school + 200 * n.schools
#' }
#' # Perform the search, can take a few minutes to run
#' ds <- find.design(simfun = simfun_multilevel, costfun = costfun_multilevel,
#' boundaries = list(n.per.school = c(5, 25), n.schools = c(10, 30)), power = .95,
#' evaluations = 1000)
#' # Output the results
#' summary(ds)
#' # Plot results
#' plot(ds)
#'
#'}
find.design <- function(simfun, boundaries, power = NULL,
    evaluations = 4000, ci = NULL, ci_perc = 0.95,
    time = NULL, costfun = NULL, cost = NULL, surrogate = NULL,
    n.startsets = 4, init.perc = 0.2, setsize = NULL,
    continue = NULL, dat = NULL, silent = FALSE, autosave_dir = NULL,
    control = list(),goodvals="high",aggregate_fun = mean,noise_fun = "bernoulli",integer=TRUE,use_noise=TRUE) {

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

    # set a default costfunction (identity) if not specified
    if (is.null(costfun)) costfun <- function(x) sum(x)

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
    if (is.null(setsize) & !is.null(evaluations))
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
            n.points = n.points, integer=integer)
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

    # For SVR, check if necessary package is available:
    if (surrogate == "svr" && !requireNamespace("WeightSVM", quietly = TRUE)) {
        message("Package WeightSVM is not installed. Please install it to use this surrogate.")
    }

    # warn if ci is termination critrerion
    # without gpr surrogate
    if (!is.null(ci) && surrogate != "gpr")
        message("Additionally fitting a GPR each update for calculating the SE. Consider switchting to GPR to speed up the estimation")



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
        if (exists("fit")) {
          lastfit <- fit
        } else {
          lastfit <- NULL
        }
        fit <- fit.surrogate(dat = dat, surrogate = surrogate,
            lastfit = lastfit, control = control, aggregate_fun = aggregate_fun, use_noise = use_noise,noise_fun=noise_fun)

        # count bad fits (e.g. plane fitted)
        if (fit$badfit)
            n.bad.fits <- ifelse(exists("n.bad.fits"),
                n.bad.fits + 1, 1)

        # PREDICTION: Get a prediction from the
        # fitted model
        pred <- get.pred(fit = fit, dat = dat, power = power,
            costfun = costfun, cost = cost, boundaries = boundaries,
            task = task,aggregate_fun = aggregate_fun,integer=integer,use_noise=use_noise)

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
            print_progress(n_updates = n_updates, evaluations_used = usedevaluations(dat),
                time_used = timer(time_temp))

    }
    ##############################################################################

    # move to next line
    cat("\n")

      # Warning if ending with a bad prediction
    if (pred$badprediction | fit$badfit) {
        pred$points <- pred$bad.points
        warning("No good design found after the final update.")
    }

    # bad prediction and no edge result -> no specific result to report
    noresult = pred$badprediction & !pred$edgeprediction

    # calculate final power and cost
    if(!noresult) {
      power_final = fit$fitfun(as.numeric(pred$points))
      cost_final = costfun(as.numeric(pred$points))
    } else {
      power_final = NA
      cost_final = NA
    }

    # Optional for the final output: Generate SD
    # from a GP if using a different surrogate
    if (!noresult && use_noise && is.null(fit$fitfun.sd))
        fit$fitfun.sd <- fit.surrogate(dat = dat, surrogate = "gpr",aggregate_fun = aggregate_fun,use_noise = use_noise, noise_fun=noise_fun)$fitfun.sd

    # calculate final SE
    if (!noresult & use_noise) final_se = fit$fitfun.sd(as.numeric(pred$points))
    else final_se = NA

    # Stop the clock
    time_used <- timer(time_temp)

    final <- list(design = pred$points, power = power_final,
        cost = cost_final, se = final_se)

    names(final$design) <- names(boundaries)

    # Collect Results
    re <- list(final = final, dat = dat, evaluations_used = usedevaluations(dat),
        surrogate = surrogate, fit = fit, time_used = time_used,
        n_updates = ifelse(exists("n_updates"), n_updates,
            0), n.bad.predictions = ifelse(exists("n.bad.predictions"),
            n.bad.predictions, 0), n.bad.fits = ifelse(exists("n.bad.fits"),
            n.bad.fits, 0), call = match.call(), seed = seed,
        costfun = costfun, boundaries = boundaries,
        simfun = simfun, cost = cost, power = power,aggregate_fun=aggregate_fun)

    class(re) <- "designresult"

    return(re)
}


