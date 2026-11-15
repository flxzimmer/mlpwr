
#' Find optimal study designs
#'
#' Perform a surrogate modeling approach to search for optimal study design parameters. For further guidance on how to use the package and the `find.design` function specifically, see the \href{https://github.com/flxzimmer/mlpwr}{Readme.md file}.
#'
#' @param simfun function to generate hypothesis test results with. Takes design parameters as input and outputs a logical (result of the hypothesis test). The function can take the designs through one argument as a vector or through multiple arguments. For example, function(x) where x is later used with x=c(n,k) for two design parameters n and k is valid. Also valid is a definition using function(n,k). Further arguments with default values (e.g. function(x, verbose = FALSE)) are left at their defaults.
#' @param boundaries list containing lower and upper bounds of the design space. The list should consist of named vectors, each containing the upper and lower bound for the respective design parameter dimensions. For one design parameter dimension, can also be a vector containing the upper and lower bounds.
#' @param power numeric; desired statistical power, strictly between 0 and 1. With a custom `aggregate_fun` or a `noise_fun` function, the desired value of the aggregated criterion instead (e.g. a ratio, which can be 1 or larger).
#' @param evaluations integer; number of simfun evaluations to be performed before termination
#' @param ci numeric; desired width of the confidence interval at the predicted value on termination.
#' @param ci_perc numeric; specifying the desired confidence interval, e.g. 95% or 99%.
#' @param time integer; seconds until termination
#' @param costfun function that takes a vector of design parameters as input and outputs a cost, e.g. monetary costs. Necessary for simfuns with multiple input dimensions. For the exhaustive search over integer grids (see `optimizer`), costfun is evaluated for all grid points in one call where that gives exactly the pointwise values, e.g. for `function(n, k) 5 * n + 20 * k` or `function(x) 5 * x[1] + 20 * x[2]`. A costfun that cannot be evaluated that way (e.g. `function(x) sum(x)` or one with `if`) is used in the exhaustive search only on grids of at most 100 points; larger grids then use `rgenoud::genoud`.
#' @param cost numeric; cost threshold. Design parameter set with highest power is searched among sets that fulfill this cost threshold.
#' @param surrogate character; which surrogate model should be used. The default is 'logreg' for one design parameter and 'gpr' for multiple design parameters. The current options are: 'gpr', 'svr', 'logreg', 'reg' for one-dimensional designs and 'gpr' and 'svr' for multi-dimensional designs. Other values give an error.
#' @param n.startsets integer; number of startsets used per dimension of simfun. `n.startsets` times the number of design parameters must be at least 3.
#' @param setsize The number of draws from the simfun in each iteration. By default, `evaluations` times `init.perc` divided by the number of initial designs (`n.startsets` times the number of design parameters); without `evaluations` (e.g. `evaluations = NULL` with `time` or `ci` as the only termination rule), the value for the default `evaluations = 4000` is used.
#' @param init.perc numeric; percentage of evaluations used for the initialization phase
#' @param dat list of data from a previous design result.
#' @param silent logical; suppresses output during the search.
#' @param autosave_dir character; a directory in which the `dat` object is saved after each update, as the file `dat_autosave.RData` (an existing file of that name is overwritten). The directory is created if it does not exist. It can be loaded for example using `load(file.path(autosave_dir, "dat_autosave.RData"))`.
#' @param control list specifying arguments passed to the surrogate models. For example, list(covtype='gauss') can be used with the gpr surrogate to use a different covariance structure than the default.
#' @param continue Object of class designresult as created by the find.design function. Will be used to continue the search, using all collected simulation results so far. The settings of the previous search (e.g. `setsize`, `ci`, `time`, `control`, `aggregate_fun`, `noise_fun`, `integer`, `use_noise`, `autosave_dir`, `optimizer`, `parallel`) are reused unless they are given explicitly; so is the target (`power` or `cost`). `evaluations` is the number of additional evaluations. Results from mlpwr 1.1.1 and earlier store no settings (except `aggregate_fun`): for them, `continue` uses `noise_fun = "bernoulli"` and `optimizer = "genoud"`, the methods of those versions (unless given explicitly), so the continued search keeps its method; other settings take their defaults.
#' @param goodvals currently unused; kept for backward compatibility. The search always treats higher criterion values (e.g. statistical power) as better, which corresponds to the value "high" (default). Other values give a warning.
#' @param aggregate_fun function to aggregate results of the evaluations of the simulation function; the default is `mean`, as for statistical power.
#' @param noise_fun how the noise variance of the aggregated Monte Carlo results at each design is obtained. Either a function, which is applied to each element of the simulation data (a list with the design `x` and the simulation results `y`) and returns the variance of the aggregate, or one of the character values `"logistic"` (default) and `"bernoulli"` for the variance \eqn{p(1-p)/n} of statistical power, where \eqn{n} is the number of performed evaluations at a design and \eqn{p} its power. With `"bernoulli"`, \eqn{p} is the design's own proportion of successes, as in mlpwr 1.1.1 and earlier (the default there). With `"logistic"`, the GPR surrogate takes \eqn{p} from a logistic regression of all simulation results on the design parameters instead: the design's own proportion gives designs with lucky draws more weight and biases the surrogate (upwards for power above .5). `"logistic"` only changes the fit of the GPR surrogate; elsewhere (the other surrogates, which do not use the noise variance, and the additional GPR that supplies the standard error for them and for the `ci` rule) it acts like `"bernoulli"`. Both character values are meant for statistical power, i.e. the mean of 0/1 (or `TRUE`/`FALSE`) simulation results: they ignore `aggregate_fun` (`p` is the mean of the results). The logistic regression of `"logistic"` needs 0/1 results; if `simfun` returns other results (e.g. proportions), a GPR search with `"logistic"` (and `use_noise = TRUE`) uses `"bernoulli"` instead, as mlpwr 1.1.1 did, says so in a message and stores `"bernoulli"` in the settings of the result. Use `noise_fun = "bernoulli"` to reproduce results of mlpwr 1.1.1; `continue` also uses it for results from mlpwr 1.1.1 (see `continue`).
#' @param integer logical  indicating whether the design parameters are integers or not; the default is `TRUE`, which is suitable for sample size, for example.
#' @param use_noise logical indicating whether noise variance should be used; the default is `TRUE`.
#' @param optimizer character; how the next design is chosen from the surrogate model in each update. With `"auto"` (default), integer designs are searched exhaustively when the grid of integer design parameter combinations within the boundaries is small enough that this is faster than `rgenoud::genoud` (the number of grid points times the number of simulated designs is at most 100,000): the objective is evaluated at every grid point and the best one is taken (ties go to the point with the smallest first design parameter, then the smallest second, and so on). Larger grids and continuous designs (`integer = FALSE`) use `rgenoud::genoud`. With `"genoud"`, `rgenoud::genoud` alone is used, as in mlpwr 1.1.1 and earlier, e.g. to reproduce earlier results. Both settings use the random number stream in the same way, so they give identical results whenever `genoud` finds the best grid point (nearly always for one design parameter).
#' @param parallel run the simulations on several CPU cores. `FALSE` (default): one after the other, as in earlier versions. `TRUE`: use all cores but one. A whole number: use that many cores (`1` is the same as `FALSE`). Needs the packages future, future.apply and parallelly. See the section "Parallel computation".
#'
#' @section Parallel computation:
#' When one call of `simfun` takes a noticeable time (roughly 10 milliseconds
#' or more, e.g. fitting a mixed model with lme4, an IRT model with mirt, or a
#' glmer), most of the search time is spent in the simulations. `parallel = TRUE`
#' then runs the simulations of each step on all CPU cores but one, which can
#' make the search several times faster. For fast simulation functions such as
#' a t-test, starting the workers and sending them work costs more than it
#' saves, so leave the default.
#'
#' No other setup is needed. Install the packages once with
#' `install.packages(c("future", "future.apply", "parallelly"))`. Objects and
#' packages that `simfun` uses are sent to the workers automatically, and it
#' works the same on Windows, macOS and Linux (the workers are background R
#' sessions). Only the simulations run in parallel; fitting the surrogate model
#' and choosing the next designs stay in your R session. Changes that `simfun`
#' makes outside itself (e.g. with `<<-`) are lost, because it runs in another
#' R session.
#'
#' Results are reproducible: with the same [set.seed()] you get the same
#' result, whatever the number of cores (`parallel = 2` and `parallel = 8` give
#' identical designs). The simulations use different random numbers than the
#' sequential mode, though (each simulation gets its own L'Ecuyer-CMRG random
#' number stream), so a parallel run does not reproduce a run with
#' `parallel = FALSE`; both are equally valid. The setting is stored in the
#' result, and `continue` reuses it.
#'
#' Advanced: to run the simulations elsewhere, e.g. on several machines or a
#' cluster, set a plan with the future package before the search, e.g.
#' `future::plan(future::cluster, workers = c("host1", "host2"))`, and use
#' `parallel = TRUE`. A non-sequential plan set by you is then used as it is.
#' With a number, `find.design()` sets up its own local workers for the call
#' and restores your plan afterwards. mlpwr has to be installed on the workers.
#'
#' A walkthrough with timings is in `vignette("parallel", package = "mlpwr")`.
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
#' ## Parallel computation: the simulations on 2 CPU cores (needs the packages
#' ## future, future.apply and parallelly; pays off for slow simfuns only,
#' ## see vignette("parallel", package = "mlpwr")):
#' if (requireNamespace("future.apply", quietly = TRUE) &&
#'     requireNamespace("parallelly", quietly = TRUE)) {
#'   set.seed(1)
#'   ds <- find.design(simfun = simfun,
#'    costfun = function(n,n.groups) 5*n+20*n.groups,
#'    boundaries = list(n = c(10, 150), n.groups = c(5, 30)),
#'    power = .95, evaluations = 300, parallel = 2)
#'   summary(ds)
#' }
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
    control = list(),goodvals="high",aggregate_fun = mean,noise_fun = "logistic",integer=TRUE,use_noise=TRUE,
    optimizer = c("auto", "genoud"), parallel = FALSE) {

    # goodvals is not implemented: higher values are always treated as better
    if (!identical(goodvals, "high"))
        warning("goodvals is currently unused: higher criterion values are always treated as better.")

    # save seed for reproducibility (in a fresh session the RNG has no state
    # yet; initialise it as R would on first use, without drawing a number)
    if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        set.seed(NULL)
    seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)

    # arguments given explicitly in this call (they take precedence over
    # the settings stored in a continued result)
    supplied <- names(match.call())[-1]

    # Start the clock
    time_temp <- timer()

    # initiate continue
    if (!is.null(continue)) {
        simfun <- continue$simfun
        costfun <- continue$costfun
        boundaries <- continue$boundaries
        # keep the target of the previous search unless a new power or
        # cost is given explicitly
        if (!any(c("power", "cost") %in% supplied)) {
            power <- continue$power
            cost <- continue$cost
        }
        dat <- continue$dat
        # take new surrogate if it is not NULL
        # and different than before
        surrogate <- ifelse(!is.null(surrogate) &&
            surrogate != continue$surrogate, surrogate,
            continue$surrogate)
        # carry over the settings of the previous search unless given
        # explicitly. Results from mlpwr <= 1.1.1 only store aggregate_fun;
        # they keep the noise variance and optimiser of those versions.
        stored <- continue$settings
        if (is.null(stored)) {
            stored <- list(noise_fun = "bernoulli", optimizer = "genoud")
            if (!is.null(continue$aggregate_fun))
                stored$aggregate_fun <- continue$aggregate_fun
        }
        carried <- c("aggregate_fun", "noise_fun", "integer", "use_noise",
            "control", "ci", "ci_perc", "time", "setsize", "n.startsets",
            "init.perc", "autosave_dir", "optimizer", "parallel")
        for (nm in setdiff(intersect(names(stored), carried), supplied)) {
            assign(nm, stored[[nm]])
        }
    }

    # optimiser for choosing designs from the surrogate (see optimize_design)
    optimizer <- match.arg(optimizer)
    check_noise_fun(noise_fun)
    check_autosave_dir(autosave_dir)
    if (is.null(evaluations) && is.null(ci) && is.null(time))
        stop("At least one of evaluations, ci and time must be given ",
            "(the search would never stop).", call. = FALSE)

    # set a default costfunction (identity) if not specified
    if (is.null(costfun)) {
        costfun <- function(x) sum(x)
        # row sums for the grid scan (see grid_costs())
        attr(costfun, "vectorised") <- function(X) rowSums(X)
    }

    # convert boundaries to list and set name
    # from simfun
    if (!is.list(boundaries)) {
        boundaries <- list(boundaries)
        names(boundaries) <- names(as.list(args(simfun)))[1]
    }

    # determine number of points
    n.points <- n.startsets * length(boundaries)

    # Set setsize according to a percentage of
    # evaluations (of the default 4000 evaluations if no number is given,
    # e.g. for a search that ends after a given time)
    if (is.null(setsize))
        setsize <- ceiling((if (is.null(evaluations)) 4000 else evaluations) *
            init.perc/n.points)

    # adjust number of evaluations for continuing
    # a search
    if (!is.null(dat) && !is.null(evaluations)) {
        evaluations <- usedevaluations(dat) + evaluations
    }

    simfun <- fix.argtype(simfun, boundaries)
    costfun <- fix.argtype(costfun, boundaries, vectorise = TRUE)


    # (checks and settings come before the initial simulations, so that
    # invalid arguments fail fast)
    # choose surrogate (if not specified)
    if (is.null(surrogate)) {
        if (length(boundaries) == 1)
            surrogate <- "logreg"
        if (length(boundaries) > 1)
            surrogate <- "gpr"
    }
    check_surrogate(surrogate, length(boundaries))

    # For SVR, check if necessary package is available:
    if (surrogate == "svr" && !requireNamespace("WeightSVM", quietly = TRUE)) {
        message("Package WeightSVM is not installed. Please install it to use this surrogate.")
    }

    # warn if ci is termination criterion
    # without gpr surrogate
    if (!is.null(ci) && surrogate != "gpr")
        message("Additionally fitting a GPR each update for calculating the SE. Consider switching to GPR to speed up the estimation")

    # Error if neither power or cost is given or
    # both are given
    if (is.null(power) & is.null(cost))
        stop("Either the power or cost argument must be supplied")
    if (!is.null(power) & !is.null(cost))
        stop("Only one of power or cost argument can be supplied")
    if (!is.null(power))
        check_power(power, aggregate_fun, noise_fun)

    # the initial design needs at least 3 points (2 are the boundaries)
    if (is.null(dat) && n.points < 3)
        stop("n.startsets times the number of design parameters must be at least 3")

    # determine the optimization task to perform
    if (!is.null(cost))
        task <- "costthreshold"
    if (!is.null(power))
        task <- "desiredpower"

    # opt-in parallel simfun evaluation: start the workers (the user's
    # future::plan() is restored on exit, also on error or interrupt)
    check_parallel_arg(parallel)
    par_env <- parallel_setup(parallel, dnames = names(boundaries), silent = silent)
    par <- par_env$par
    if (!is.null(par_env))
        on.exit(par_env$restore(), add = TRUE)

    # Generate initialization data if not
    # available
    if (is.null(dat)) {
        points <- initpoints(boundaries = boundaries,
            n.points = n.points, integer=integer)
        dat <- addval(simfun = simfun, points = points,
            each = setsize, autosave_dir = autosave_dir, par = par)
    }

    # noise_fun = "logistic" needs 0/1 results where the logistic regression
    # is used (the GPR surrogate with noise variances; elsewhere it acts as
    # "bernoulli"). With other results the search falls back to "bernoulli",
    # the method of mlpwr 1.1.1, before the first fit; the fallback is stored
    # in the settings, so continue keeps it.
    if (surrogate == "gpr" && use_noise && identical(noise_fun, "logistic") &&
        !binary_results(dat)) {
        message("simfun returned results other than 0/1 (or TRUE/FALSE), so ",
            "the search uses noise_fun = \"bernoulli\" (the method of mlpwr ",
            "1.1.1) instead of the default \"logistic\".")
        noise_fun <- "bernoulli"
    }

    # initialise the fit and the counters locally (exists() would also
    # find variables of the same name in the caller's or global environment)
    # (a continued search starts from the previous fit, e.g. for the SVR
    # tuning history)
    fit <- if (!is.null(continue)) continue$fit else NULL
    n_updates <- 0
    n.bad.fits <- 0
    n.bad.predictions <- 0
    n.edgepredictions <- 0
    # costs on the design grid, reused across updates by the exact search
    opt_cache <- new.env(parent = emptyenv())

    ############################################################################## start
    ############################################################################## the
    ############################################################################## search
    repeat {

        # FIT: Fit a surrogate model
        lastfit <- fit
        fit <- fit.surrogate(dat = dat, surrogate = surrogate,
            lastfit = lastfit, control = control, aggregate_fun = aggregate_fun, use_noise = use_noise,noise_fun=noise_fun)

        # count bad fits (e.g. plane fitted)
        if (fit$badfit)
            n.bad.fits <- n.bad.fits + 1

        # PREDICTION: Get a prediction from the
        # fitted model
        pred <- get.pred(fit = fit, dat = dat, power = power,
            costfun = costfun, cost = cost, boundaries = boundaries,
            task = task,aggregate_fun = aggregate_fun,integer=integer,use_noise=use_noise,
            optimizer = optimizer, cache = opt_cache)

        # count bad predictions (no sensible
        # value found)
        if (pred$badprediction)
            n.bad.predictions <- n.bad.predictions + 1

        # count edge predictions (value on edge
        # of parameter space) and issue warning
        # if happened too often
        if (pred$edgeprediction) {
            n.edgepredictions <- n.edgepredictions + 1
            if (n.edgepredictions == 5)
                warning("The predicted value consistently lies at the edge of the parameter space. Consider adjusting the boundaries.")
        }

        # check TERMINATION
        is.terminate <- check.term(evaluations = evaluations,
            ci = ci, time = time, dat = dat, time_temp = time_temp,
            fit = fit, pred = pred, ci_perc = ci_perc,
            aggregate_fun = aggregate_fun, noise_fun = noise_fun,
            control = if (surrogate == "gpr") control else list())
        if (is.terminate)
            break

        # UPDATING: sample from the DGF
        dat <- addval(simfun = simfun, dat = dat, points = pred$points,
            each = max(ceiling(setsize/nrow(pred$points)),
                1), autosave_dir = autosave_dir, par = par)

        # number of performed updates
        n_updates <- n_updates + 1

        # print current progress
        if (!silent)
            print_progress(n_updates = n_updates, evaluations_used = usedevaluations(dat),
                time_used = timer(time_temp))

    }
    ##############################################################################

    # move to next line (after the progress output)
    if (!silent)
        cat("\n")

      # Warning if ending with a bad prediction
    # (get.pred only replaces the prediction by all designs, keeping it in
    # bad.points, if the fit is bad or the prediction is bad and not at the
    # edge; a bad edge prediction is kept as it is)
    # the final logreg fit failed (flat fallback, see logi.fit): report no
    # design
    logreg_failed <- surrogate == "logreg" && isTRUE(fit$badfit)
    if (logreg_failed) {
        warning("No design found: the logreg surrogate could not be fitted to the simulation results. Try more evaluations or surrogate = \"gpr\".")
    } else if (pred$badprediction | fit$badfit) {
        if (fit$badfit | (pred$badprediction & !pred$edgeprediction))
            pred$points <- pred$bad.points
        warning("No good design found after the final update.")
    }

    # bad prediction and no edge result -> no specific result to report
    noresult = (pred$badprediction & !pred$edgeprediction) | logreg_failed

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
        fit$fitfun.sd <- fit.surrogate(dat = dat, surrogate = "gpr",
            control = if (surrogate == "gpr") control else list(),
            aggregate_fun = aggregate_fun,use_noise = use_noise,
            noise_fun = raw_noise_fun(noise_fun))$fitfun.sd  # SE only: "logistic" acts as "bernoulli"

    # calculate final SE
    if (!noresult & use_noise) final_se = fit$fitfun.sd(as.numeric(pred$points))
    else final_se = NA

    # Stop the clock
    time_used <- timer(time_temp)

    final <- list(design = pred$points, power = power_final,
        cost = cost_final, se = final_se)

    names(final$design) <- names(boundaries)
    if (logreg_failed) {
        final$design <- final$design[1, , drop = FALSE]
        final$design[] <- NA
    }

    # Collect Results
    re <- list(final = final, dat = dat, evaluations_used = usedevaluations(dat),
        surrogate = surrogate, fit = fit, time_used = time_used,
        n_updates = n_updates, n.bad.predictions = n.bad.predictions,
        n.bad.fits = n.bad.fits, call = match.call(), seed = seed,
        costfun = costfun, boundaries = boundaries,
        simfun = simfun, cost = cost, power = power,aggregate_fun=aggregate_fun,
        settings = list(aggregate_fun = aggregate_fun, noise_fun = noise_fun,
            integer = integer, use_noise = use_noise, control = control,
            ci = ci, ci_perc = ci_perc, time = time, setsize = setsize,
            n.startsets = n.startsets, init.perc = init.perc,
            autosave_dir = autosave_dir,
            optimizer = optimizer,
            parallel = parallel))

    class(re) <- "designresult"

    return(re)
}


