


# Search -------------------------------------------------------


#' Sample Size Finding Algorithm
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
      print(pred$new.n.y)
      # check if prediction lies within design
      for (i in 1:length(pred$new.n)) {
        if (pred$new.n[i]<design[[i]][1]|pred$new.n[i]>design[[i]][2]) useprediction = FALSE
      }
      }

    # check CI
    if(!is.na(goal.ci)&& n.iter>0&useprediction) {

      if (use_data_sd) new.n.sd = get.sd(dat,pred$new.n)

      # if (is.na(new.n.sd)) browser()
      # print(new.n.sd)


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
      dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=round(setsize/nrow(datx)))
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


# Linear Regression -------------------------------------------------------


#' Title
#'
#' @param dat
#' @param goal
#' @param last.out
#' @param ...
#' @param aggregate
#' @param use.weight
#' @param fun
#'
#' @return
#' @export
#'
#' @examples
reg.pred = function(dat,goal=.8,cost=function(x)sum(x),design=NULL,greedy=TRUE,...) {

  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type="freq")

  mod =lm(y~.,datx,weights=weight)

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  predfun.sd = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),se.fit=TRUE)$se.fit}

  # generate prediction
  res = get.pred(xvars,predfun,goal,cost,Ntry=3,design=design,dat=dat,greedy=greedy)

  re=list(new.n = res$new.n,new.n.sd = predfun.sd(res$new.n),new.n.y=predfun(res$new.n),fun=predfun,fun.sd = predfun.sd,toofar=res$toofar,exact=res$exact)
  return(re)
}


# Logistic Regression -----------------------------------------------------


#' Title
#'
#' @param dat
#' @param goal
#' @param last.out
#' @param ...
#' @param aggregate
#' @param fun
#' @param weight.type
#' @param cost
#' @param trans
#'
#' @return
#' @export
#'
#' @examples 1
logi.pred = function(dat,goal=.8,cost=function(x)sum(x),trans=TRUE,design=NULL,greedy=TRUE,...) {

  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type="freq")

  if (!trans) mod = glm(y~.,datx,weights=weight,family = binomial)

  if (trans) {
    trans = function(x) x/2 +.5
    invtrans = function(y) y*2-1
    fam = quasi(link="logit", variance = "mu(1-mu)")
    linkfun = function(mu) log(mu/(1-mu))
    linkfun.new = function(mu) linkfun(trans(mu))
    fam$linkfun = linkfun.new
    linkinv = function(mu) 1/(1+exp(-mu))
    linkinv.new = function(mu) invtrans(linkinv(mu))
    fam$linkinv = linkinv.new
    fam$mu.eta = function(mu) 2 * exp(-mu) / (exp(-mu)+1)^2

    mod = glm(y~.,datx,weights=weight,family = fam)
  }

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response")}

  predfun.sd = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response",se.fit=TRUE)$se.fit}

  # generate prediction
  res = get.pred(xvars,predfun,goal,cost,Ntry=3,design=design,dat=dat,greedy=greedy)

  re=list(new.n = res$new.n,new.n.sd = predfun.sd(res$new.n),new.n.y=predfun(res$new.n),fun=predfun,fun.sd = predfun.sd,toofar=res$toofar,exact=res$exact)

  return(re)
}




# SVM---------------------------------------------------------------------


#' Title
#'
#' generate a prediction for the sample size at the specified power
#'
#' @param dat
#' @param alpha
#' @param goal
#' @param bounds
#' @param round_res
#' @param last.out
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
svm.pred = function(dat,goal=NULL,carryover=NULL,cost=function(x)sum(x),tune=TRUE,epsil=FALSE,design=NULL,fixed_cost=NULL,greedy=TRUE,...) {

  datx = todataframe(dat,aggregate=TRUE,pseudo=FALSE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type="freq")

  #Treat the case of equal power values in the data [especially the case of zero variance]- add (some tiny amount of) random noise
  if(any(ind <- duplicated(datx$y))) datx$y[ind] = datx$y[ind] + rnorm(length(datx$y[ind]),sd=.0000001)

  # #Setup Low Anchor
  # anchor = c(rep(0,nrow(xvars)),0)
  # datx = rbind(datx,anchor)
  # weight = c(weight,max(weight))
  # xvars = datx[,1:(length(datx)-1),drop=FALSE]
  #
  # # Setup High Anchor
  # anchor = c(apply(xvars,2,max)*2,1)
  # datx = rbind(datx,anchor)
  # weight = c(weight,max(weight))
  # xvars = datx[,1:(length(datx)-1),drop=FALSE]

  # Determine if first round
  firstround = is.null(carryover$pars_history)

  if (firstround) {
    pars_history = list()
    pars = list(cost=100,epsilon=.1,gamma=.05) # Default Pars
    tunepars = list(cost=10^seq(-1,2),gamma=10^seq(-4,1),epsilon=min(getweight(dat,"sd")))
    if(epsil) tunepars$epsilon = 10^seq(-4,-1)
    }

    if (!firstround) {
    pars_history = carryover$pars_history
    n_previous = length(pars_history)
    pars = pars_history[[n_previous]]
    TUNEFACTORS = seq(.75,1.25,.25)
    tunepars = list(cost=pars$cost*TUNEFACTORS,gamma=pars$gamma*TUNEFACTORS,epsilon= pars$epsilon * TUNEFACTORS)

    # if (n_previous %% 10 == 0) {
    #   tunepars = list(cost=c(tunepars$cost,10^seq(-1,2)),gamma=c(tunepars$gamma,10^seq(-4,1)),epsilon=c(tunepars$epsilon,min(getweight(dat,"sd"))))
    # }
    if (n_previous %% 10 == 0) {
      tunepars = list(cost=c(tunepars$cost,10^seq(-1,2)),gamma=c(tunepars$gamma,10^seq(-4,1)),epsilon=min(getweight(dat,"sd")))
    }
    }

  if (tune) {
    tunepars$epsilon = min(getweight(dat,"sd"))
    ctrl = tune.control(cross=min(10,nrow(datx)))
    a = tune_wsvm(y ~ .,data = datx,weight=weight,ranges=tunepars,tunecontrol=ctrl)
    pars= a$best.parameters
    mod = a$best.model
  }

  if (!tune) {
    mod = wsvm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,weight=weight)
  }

  # Save used parameters
  pars_history = list.append(pars_history,as.data.frame(pars))

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  # freqs = getweight(dat,"freq")
  # predfun.sd = function(x) sum(colMeans((x - xvars)^2*freqs)) # pseudo SD fun

  if (!is.null(goal)) {
  difs = apply(xvars,1,function(x) predfun(x)-goal)
  isdumb = !any(difs>0) | !any(difs<0)   # Check if it predicts all values to be on one side of the plane
  if (isdumb) {warning("Weird Function")}
}
  somevals = apply(xvars,1,function(x) predfun(x))
  isplane = summary(lm(y~.,cbind(xvars,y = somevals)))$r.squared>.999 #
  if (isplane) {warning("Plane predicted")}

  # generate prediction
  res = get.pred(xvars,predfun,goal,cost,Ntry=20,design=design,fixed_cost=fixed_cost,dat=dat,greedy=greedy)

  re=list(new.n = res$new.n,new.n.sd = NA,new.n.y=predfun(res$new.n),fun=predfun,fun.sd = NA,carryover=list(pars_history = pars_history),toofar=res$toofar,exact=res$exact,points=res$points)

  return(re)
}


# Gaussian Process Regression ---------------------------------------------


#' Title
#'
#' @param dat
#' @param alpha
#' @param goal
#' @param goal.width
#' @param predtype
#' @param bounds
#' @param criterium
#' @param round_res
#' @param last.out
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
gauss.pred = function(dat,goal=NULL,cost=function(x)sum(x),design=NULL,fixed_cost=NULL,greedy=TRUE,...) {

  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type="var")


  n.try = 0
  mins = apply(xvars,2,min)
  maxs = apply(xvars,2,max)

  isflat = TRUE

  while (n.try<100 & isflat) {

    n.try = n.try+1

    mod = tryCatch(DiceKriging::km(design=xvars, response=datx$y,noise.var=weight,control=list(trace=FALSE)),error=function(x) {return(NULL)})

    if (is.null(mod)) next

    predfun = function(x) {
      names(x) = names(xvars)
      DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK")$mean
    }

    isflat = abs(predfun(mins-.5)-predfun(maxs+.5))<.001

  }

  if (n.try==100) warning("model not converged")

  predfun.sd = function(x) {
    names(x) = names(xvars)
    DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK")$sd
  }

  somevals = apply(xvars,1,function(x) predfun(x))
  isplane = summary(lm(y~.,cbind(xvars,y = somevals)))$r.squared>.98 #
  if (isplane|| is.na(isplane)) {warning("Plane predicted")}

  # generate prediction

  res = get.pred(xvars,predfun,goal,cost,Ntry=20,design=design,fixed_cost=fixed_cost,dat=dat,greedy=greedy)


  re=list(new.n = res$new.n,new.n.sd = predfun.sd(res$new.n),new.n.y=predfun(res$new.n),fun=predfun,fun.sd = predfun.sd,toofar=res$toofar,exact=res$exact,points=res$points)

  # if(is.na(re$new.n.sd)) browser()


  return(re)
}


