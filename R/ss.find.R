
#' Sample Size Finding
#'
#' @param runfun
#' @param goal
#' @param goal.width
#' @param alpha
#' @param guess
#' @param bounds
#' @param CI
#' @param budget
#' @param stopearly
#' @param setsize The number of draws from the data generating function in each iteration
#' @param learner
#' @param startset.size
#' @param startbudget
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ss.find = function(learner,runfun,design,goal=NULL,goal.ci=NA,CI=.95,budget = NULL,setsize=NULL,n.startsets=NULL,seed=NULL,dat=NULL,dat.only=FALSE,limitmaxbudget=TRUE,fixed_cost=NULL,cost=function(x)sum(x),...) {

  use_data_sd = TRUE

  # Start the clock
  time_temp = timer()

  # Initialization Setup
  if (is.null(n.startsets)){
    n.startsets=4*length(design)
  }
  if (is.null(setsize)){
    setsize = if (!is.null(budget)) round(budget*.1/n.startsets) else 50
  }

  if (!is.null(seed)) set.seed(seed)

  if(is.null(dat)) {
  dat = addval(runfun,design=design,n.points=n.startsets,each=setsize,seed=seed)
  }
  if(dat.only) return (dat)

  pred=NULL
  new.n.sd = NULL
  n.iter = 0
  failed.predictions = 0

  ##############################################################################
  repeat{ # start the search
    print("################################")

    # generate prediction
    pred = tryCatch(learner(dat = dat,goal=goal,carryover = pred$carryover,design=design,fixed_cost=fixed_cost,cost=cost,greedy=TRUE,...),error=function(x) {
      print(x)
      return(x)
    })

    useprediction = FALSE
    if (is.null(pred$new.n)) {
      warning("no predicted value")
    } else {
      useprediction = !pred$toofar
      print(pred$new.n)
      print(as.numeric(pred$new.n.y))
      # check if prediction lies within design
      for (i in 1:length(pred$new.n)) {
        if (pred$new.n[i]<design[[i]][1]|pred$new.n[i]>design[[i]][2]) useprediction = FALSE
      }
      }

    # check CI
    if(!is.na(goal.ci)&& n.iter>0&useprediction) {

      if (use_data_sd) new.n.sd = get.sd(dat,pred$new.n)

      if (!use_data_sd) { #Use SD from the learner
        if(is.na(pred$new.n.sd)){ # Generate from GP if missing
          pred2 = tryCatch(gauss.pred(dat = dat,goal=goal,carryover = pred$carryover,design=design,fixed_cost=fixed_cost,cost=cost,...),error=function(x) {return(x)})
          new.n.sd = pred2$fun.sd(pred$new.n)
        } else {
          new.n.sd = pred$new.n.sd
        }
      }

      if(new.n.sd*qnorm(CI+(1-CI)/2)<goal.ci) break
    }

    # check budget
    used = usedruns(dat)

    if (limitmaxbudget& used>15000) {warning("max budget used, didn't converge?");break}
    if (!is.na(budget)){
      budget.remaining = budget - used
      if(budget.remaining<=0) break
}

    #add value to the dataset / random if toofar away
    if (useprediction) {
      points = data.frame(t(pred$new.n))
      if(!is.null(fixed_cost)) { # add multiple candidates for fixed cost condition
        points = pred$points
      }
      dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=setsize/nrow(points))
    }
    if (!useprediction) {
      datx = todataframe(dat)
      points = datx[,1:(length(datx)-1),drop=FALSE]
      each = max(round(setsize/nrow(datx)),1) # make sure at least one point is added
      dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=each)
      failed.predictions = failed.predictions + 1
    }

    n.iter = n.iter + 1

  }
  ##############################################################################

  # Fit learner a final time for adjustments (nonGreedy run)
  pred = tryCatch(learner(dat = dat,goal=goal,carryover = pred$carryover,design=design,fixed_cost=fixed_cost,cost=cost,greedy=FALSE,...),error=function(x) {
    print(x)
    return(x)
  })

  # Get Final SD if not yet there
  if (is.null(new.n.sd)) {
    if (use_data_sd) new.n.sd = get.sd(dat,pred$new.n)

    if (!use_data_sd) { #Use SD from the learner
      if(is.na(pred$new.n.sd)){ # Generate from GP if missing
        pred2 = tryCatch(gauss.pred(dat = dat,goal=goal,carryover = pred$carryover,design=design,fixed_cost=fixed_cost,cost=cost,...),error=function(x) {return(x)})
        new.n.sd = pred2$fun.sd(pred$new.n)
      } else {
        new.n.sd = pred$new.n.sd
      }
    }
}

  # Stop the clock
  time_used = timer(time_temp,detailled=T)

  re = list(value=pred$new.n,value.sd = new.n.sd,data = dat,budget=usedruns(dat),fun=pred$fun,fun.sd = pred$fun.sd,time_used=time_used,exact=pred$exact,value.y=pred$fun(pred$new.n),n.iter = n.iter,failed.predictions = failed.predictions,cost.y=cost(pred$new.n))

  return(re)
}


