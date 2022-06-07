
#' Sample Size Finding
#'
#' @param dgfun
#' @param boundaries Kann vector oder liste sein
#' @param power
#' @param terminate
#' @param runs
#' @param ci
#' @param ci_perc
#' @param time
#' @param costfun
#' @param max_cost
#' @param surrogate
#' @param n.startsets Startsets per Dimension
#' @param continue
#' @param save_dir
#' @param setsize The number of draws from the data generating function in each iteration
#' @param init.perc
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
                   ci = .03,
                   ci_perc = .95,
                   time = 120,
                   costfun = NULL,
                   max_cost = NULL,
                   surrogate = "gpr",
                   n.startsets = 4,
                   init.perc = .1,
                   setsize = 50,
                   dat = NULL,
                   save_dir = NULL,
                   Ntry = 2
                   ) {


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

  # Generate initialization data if not available
  if(is.null(dat)) {
    points = initpoints(boundaries=boundaries,n.points = n.points)
    dat = addval(dgfun=dgfun,points=points,each=setsize)
  }


##############################################################################
  # start the search
  repeat{

    # FIT: Fit a surrogate model
    fit = fit.surrogate(dat = dat,surrogate=surrogate,lastfit=ifelse(exists("fit"),fit,0))

    # count bad fits (e.g. plane fitted)
    if (fit$badfit) n.bad.fits = ifelse(exists("n.bad.fits"),n.bad.fits + 1,1)

    # check termination
    is.terminate = check.term(runs=runs,ci=ci,time=time,dat=dat,time_temp=time_temp)
    if (is.terminate) break

    # PREDICTION: Get a prediction from the fitted model
    pred = get.pred(fit=fit,dat=dat,power=power,costfun = costfun, max_cost=max_cost,boundaries=boundaries,greedy=TRUE,task=task,Ntry=Ntry)

    # count bad predictions (no sensible value found)
    if (pred$badprediction) n.bad.predictions = ifelse(exists("n.bad.predictions"),n.bad.predictions + 1,1)

    # UPDATING: sample from the DGF
    dat = addval(dgfun=dgfun,dat=dat,points=pred$points,each= max(ceiling(setsize/nrow(pred$points)),1))

    # number of performed updates
    n.updates = ifelse(exists("n.updates"),n.updates + 1,1)

  }
  ##############################################################################

  # Generate final prediction (not Greedy)
  pred = get.pred(fit=fit,dat=dat,power=power,costfun = costfun, max_cost=max_cost,boundaries=boundaries,greedy=FALSE,task=task,Ntry=Ntry)

  # Optional for the final output: Generate SD from a GP if not available

  # Stop the clock
  time_used = timer(time_temp,detailled=T)


  # Collect Results
  re = list(
    design = pred$points,
    dat = dat,
    runs = usedruns(dat),
    fit = fit,
    time_used = time_used,
    n.updates = ifelse(exists("n.updates"),n.updates,0),
    n.bad.predictions = ifelse(exists("n.bad.predictions"), n.bad.predictions, 0),
    n.bad.fits = ifelse(exists("n.bad.fits"),n.bad.fits,0),
    call = match.call()
    )

  class(re) = "designresult"

  return(re)
}


