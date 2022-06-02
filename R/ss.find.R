
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
#' @param n.startsets
#' @param dat
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
ss.find = function(dgfun,
                   boundaries,
                   power=.8,
                   terminate = "runs",
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
                   save_dir = NULL
                   ) {

  # convert boundaries to list
  if (!is.list(boundaries)) {
    boundaries=list(n=boundaries)
  }

  # set a default costfunction (identity) if not specified
  if (is.null(costfun)) {
    costfun = function(x)sum(x)
  }


  # determine the optimization task to perform
  if (!is.null(max_cost)) task = "costthreshold"
  if (!is.null(goal)) task = "desiredpower"


  # Start the clock
  time_temp = timer()


  # Set setsize according to a percentage of runs
  if ("runs" %in% terminate) setsize = round(runs*init.perc/n.startsets)


  # Generate initialization data
  if(is.null(dat)) {
    points = initpoints(boundaries=boundaries,n.points = n.startsets*length(boundaries))
    dat = addval(dgfun=dgfun,points=points,each=setsize)
  }

  ##############################################################################
  # start the search
  repeat{

    # fit surrogate model
    fit = fit.surrogate(dat = dat,
                  lastfit=ifelse(exists("fit"),fit,NULL),
                  surrogate=surrogate)

    # generate prediction
    pred = get.pred(fit=fit,dat=dat,goal=power,costfun = costfun, max_cost=max_cost,boundaries=boundaries,greedy=TRUE)


    # number of failed predictions
    if (!pred$useprediction) {
      n.failed.predictions = ifelse(exists("n.failed.predictions"),n.failed.predictions + 1,1)
    }

    # check termination
    is.terminate = check.term(terminate,...)

    # add points to the data
    dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=setsize/nrow(points))
    dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=each)


    # number of performed updates
    n.updates = ifelse(exists("n.updates"),n.updates + 1,1)

  }
  ##############################################################################

  # Generate final prediction (not Greedy)
  pred = get.pred(fit=fit,dat=dat,goal=power,costfun = costfun, max_cost=max_cost,boundaries=boundaries,greedy=FALSE,task=task)


  # Get Final SD if not yet there
  if (is.null(new.n.sd)) {
    if (use_data_sd) new.n.sd = get.sd(dat,fit$new.n)

    if (!use_data_sd) { #Use SD from the learner
      if(is.na(fit$new.n.sd)){ # Generate from GP if missing
        fit2 = tryCatch(gauss.fit(dat = dat,goal=power,carryover = fit$carryover,design=design,fixed_cost=fixed_cost,cost=cost,...),error=function(x) {return(x)})
        new.n.sd = fit2$fun.sd(fit$new.n)
      } else {
        new.n.sd = fit$new.n.sd
      }
    }
  }

  # Stop the clock
  time_used = timer(time_temp,detailled=T)

  # Collect Results
  re = list(value=fit$new.n,value.sd = new.n.sd,data = dat,runs=usedruns(dat),fun=fit$fun,fun.sd = fit$fun.sd,time_used=time_used,exact=fit$exact,value.y=fit$fun(fit$new.n),n.updates = n.updates,failed.fitictions = failed.fitictions,cost.y=cost(fit$new.n))

  return(re)
}


