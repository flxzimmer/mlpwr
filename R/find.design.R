
#' Sample Size Finding
#'
#' @param dgfun function to generate hypotesis test results with. takes a vector of design parameters as input and outputs logical (result of one hypothesis test)
#' @param boundaries list containing lower and upper bounds of the design space
#' @param power numeric; power value for the 'desired power' task
#' @param runs integer; number of dgf evaluations to be performed before termination
#' @param ci numeric; desired width of the confidence interval at the predicted value on termination.
#' @param ci_perc numeric; specifying the desired confidence interval, e.g. 95% or 99%.
#' @param time integer; seconds until termination
#' @param costfun function that takes a vector of design parameters as input and outputs a cost, e.g. monetary costs. Used for dgfuns with multiple input dimensions.
#' @param max_cost numeric; cost threshold for the respective task
#' @param surrogate character; which surrogate model should be used. The default is "logreg" for one design parameter and "gpr" for multiple design parameters. The current options are: "gpr", "svr", "logreg", "reg" for one-dimensional designs and "gpr" and "svr" for multi-dimensional designs.
#' @param n.startsets integer; number of startsets used per dimension of dgfun
#' @param setsize The number of draws from the data generating function in each iteration
#' @param init.perc numeric; percentage of runs used for the initialization phase
#' @param dat list of data from a previous design result.
#' @param Ntry integer; number of locations to start a search for optimal paramters in the prediction phase.
#' @param silent logical; supresses output during the search.
#' @param autosave_dir character; file location for saving the dat object after each update.
#' @param control list specifying arguments passed to the surrogate models. For example, list(covtype="gauss") can be used with the gpr surrogate to use a different covariance structure than the default.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
find.design = function(dgfun,
                   boundaries,
                   power=NULL,
                   runs = 1000,
                   ci = NULL,
                   ci_perc = .95,
                   time = NULL,
                   costfun = NULL,
                   max_cost = NULL,
                   surrogate = NULL,
                   n.startsets = 4,
                   init.perc = .2,
                   setsize = 100,
                   dat = NULL,
                   Ntry = 2,
                   silent =FALSE,
                   autosave_dir=NULL,
                   control = list()
                   ) {

  seed <- .Random.seed

  # Start the clock
  time_temp = timer()

  # convert boundaries to list
  if (!is.list(boundaries)) boundaries=list(n=boundaries)

  # set a default costfunction (identity) if not specified
  if (is.null(costfun)) costfun = function(x)sum(x)

  # Error if neither power or max_cost is given or both are given
  if (is.null(power)& is.null(max_cost)) stop("Either the power or max_cost argument must be supplied")
  if (!is.null(power)& !is.null(max_cost)) stop("Only one of power or max_cost argument can be supplied")

  # determine the optimization task to perform
  if (!is.null(max_cost)) task = "costthreshold"
  if (!is.null(power)) task = "desiredpower"

  # determine number of points
  n.points = n.startsets*length(boundaries)

  # Set setsize according to a percentage of runs, if available
  if (!is.null(runs)) setsize = ceiling(runs*init.perc/n.points)

  # adjust number of runs for continuing a search
  if(!is.null(dat)) {
    runs = usedruns(dat) + runs
  }

  # Generate initialization data if not available
  if(is.null(dat)) {
    points = initpoints(boundaries=boundaries,n.points = n.points)
    dat = addval(dgfun=dgfun,points=points,each=setsize,autosave_dir=autosave_dir)
  }

  #choose surrogate (if not specified)
  if (is.null(surrogate)) {
  if (length(boundaries)==1) surrogate = "logreg"
  if (length(boundaries)>1) surrogate = "gpr"
  }

# warn if ci is termination critrerion without gpr surrogate
  if (!is.null(ci)&& surrogate!="gpr") warning("Additionally fitting a GPR each update for calculating the SE. Consider switchting to GPR to speed up the estimation")


##############################################################################
  # start the search
  repeat{

    # FIT: Fit a surrogate model
    fit = fit.surrogate(dat = dat,surrogate=surrogate,lastfit=ifelse(exists("fit"),fit,0),control=control)

    # count bad fits (e.g. plane fitted)
    if (fit$badfit) n.bad.fits = ifelse(exists("n.bad.fits"),n.bad.fits + 1,1)

    # PREDICTION: Get a prediction from the fitted model
    pred = get.pred(fit=fit,dat=dat,power=power,costfun = costfun, max_cost=max_cost,boundaries=boundaries,task=task,Ntry=Ntry)

    # count bad predictions (no sensible value found)
    if (pred$badprediction) n.bad.predictions = ifelse(exists("n.bad.predictions"),n.bad.predictions + 1,1)

    # count edge predictions (value on edge of parameter space) and issue warning if happened too often
    if (pred$edgeprediction) {
      n.edgepredictions = ifelse(exists("n.edgepredictions"),n.edgepredictions + 1,1)
      if(n.edgepredictions==5) warning("The predicted value consistently lies at the edge of the parameter space. Consider adjusting the boundaries.")
    }

    # check TERMINATION
    is.terminate = check.term(runs=runs,ci=ci,time=time,dat=dat,time_temp=time_temp,fit =fit,pred=pred,ci_perc=ci_perc)
    if (is.terminate) break

    # UPDATING: sample from the DGF
    dat = addval(dgfun=dgfun,dat=dat,points=pred$points,each= max(ceiling(setsize/nrow(pred$points)),1),autosave_dir=autosave_dir)

    # number of performed updates
    n_updates = ifelse(exists("n_updates"),n_updates + 1,1)

    # print current progress
    if(!silent) print.progress(n_updates=n_updates,runs_used=usedruns(dat),time_used = timer(time_temp))

  }
  ##############################################################################

  # move to next line
  cat("\n")


  # Optional for the final output: Generate SD from a GP if using a different surrogate
  if (is.null(fit$fitfun.sd)) fit$fitfun.sd = fit.surrogate(dat = dat,surrogate="gpr")$fitfun.sd

  # Stop the clock
  time_used = timer(time_temp)

  # Warning if ending with a bad prediction
  if (pred$badprediction) warning("No good design found after the final update.")

  final = list(
    design = pred$points.notgreedy,
    power = fit$fitfun(as.numeric(pred$points.notgreedy)),
    cost = costfun(as.numeric(pred$points.notgreedy)),
    se = fit$fitfun.sd(as.numeric(pred$points.notgreedy))
  )
  names(final$design) = names(boundaries)

  # Collect Results
  re = list(
    final = final,
    dat = dat,
    runs_used = usedruns(dat),
    surrogate = surrogate,
    fit = fit,
    time_used = time_used,
    n_updates = ifelse(exists("n_updates"),n_updates,0),
    n.bad.predictions = ifelse(exists("n.bad.predictions"), n.bad.predictions, 0),
    n.bad.fits = ifelse(exists("n.bad.fits"),n.bad.fits,0),
    call = match.call(),
    seed = seed,
    costfun = costfun,
    boundaries = boundaries
    )

  class(re) = "designresult"

  return(re)
}


