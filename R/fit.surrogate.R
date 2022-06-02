

fit.surrogate = function(dat,surrogate,lastfit=NULL){

  switch(surrogate,
               reg = reg.fit(dat),
               logreg = logi.fit(dat),
               svr = svm.fit(dat,lastfit=lastfit),
               gpr = gauss.fit(dat)
               )

  # implement basic check here, like if the fit is a plane. Then skip the prediction phase in this case.

}



#' Linear Regression
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
reg.fit = function(dat) {

  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type="freq")

  mod =lm(y~.,datx,weights=weight)

  fitfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  fitfun.sd = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),se.fit=TRUE)$se.fit}

  re = list(fitfun=fitfun,fitfun.sd=fitfun.sd)

  return(re)
}



#' Logistic Regression
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
logi.fit = function(dat,trans=TRUE) {

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

  fitfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response")}

  fitfun.sd = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response",se.fit=TRUE)$se.fit}

  re = list(fitfun=fitfun,fitfun.sd=fitfun.sd)

  return(re)
}




#' SVR
#'
#'
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
svm.fit = function(dat,goal=NULL,carryover=NULL,cost=function(x)sum(x),tune=TRUE,design=NULL,fixed_cost=NULL,greedy=TRUE,...) {

  datx = todataframe(dat,aggregate=TRUE,pseudo=FALSE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type="freq")

  #Treat the case of equal power values in the data [especially the case of zero variance]- add (a negligibly small amount of) random noise
  if(any(ind <- duplicated(datx$y))) {
    a = datx$y[ind]
    a = a + rnorm(length(a),sd=.0000001)
    a[a>1] = 1-(a[a>1]-1)
    a[a<0] = -a[a<0]
    datx$y[ind] = a
  }

  # Determine if first round
  firstround = is.null(carryover$pars_history)

  if (firstround) {
    pars_history = list()
    pars = list(cost=100,epsilon=.1,gamma=.05) # Default Pars

    tunepars = list(cost=c(10^seq(-1,2),2*10^seq(-1,2),5*10^seq(-1,2)),gamma=10^seq(-4,1),epsilon=min(getweight(dat,"sd")))

  }

  if (!firstround) {
    pars_history = carryover$pars_history
    n_previous = length(pars_history)
    pars = pars_history[[n_previous]]
    TUNEFACTORS = seq(.75,1.25,.25)
    tunepars = list(cost=pars$cost*TUNEFACTORS,gamma=pars$gamma*TUNEFACTORS,epsilon= min(getweight(dat,"sd")))

    if (n_previous %% 10 == 0) { # Wider Search every 10 Iterations
      tunepars = list(cost=c(tunepars$cost,10^seq(-1,2)),gamma=c(tunepars$gamma,10^seq(-4,1)),epsilon=tunepars$epsilon)
    }
  }

  if (tune) {

    ctrl = tune.control(cross=min(10,nrow(datx)),error.fun=logloss)
    a = tune_wsvm(y ~ .,data = datx,weight=weight,ranges=tunepars,tunecontrol=ctrl)
    pars= a$best.parameter
    mod = a$best.model

  }


  if (!tune) {
    mod = wsvm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,weight=weight)
  }

  # Save used parameters
  pars_history = list.append(pars_history,as.data.frame(pars))

  fitfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  if (!is.null(goal)) {
    difs = apply(xvars,1,function(x) fitfun(x)-goal)
    isdumb = !any(difs>0) | !any(difs<0)   # Check if it predicts all values to be on one side of the plane
    if (isdumb) {warning("Weird Function")}
  }


  # move this check outside the fitting function?
  somevals = apply(xvars,1,function(x) fitfun(x))
  isplane = summary(lm(y~.,cbind(xvars,y = somevals)))$r.squared>.98 #
  if (isplane|| is.na(isplane)) {warning("Plane fitted")}


  re = list(fitfun=fitfun,fitfun.sd=fitfun.sd)

  # # generate prediction
  # res = get.pred(xvars,fitfun,goal,cost,Ntry=20,design=design,fixed_cost=fixed_cost,dat=dat,greedy=greedy)
  #
  # re=list(new.n = res$new.n,new.n.sd = NA,new.n.y=fitfun(res$new.n),fun=fitfun,fun.sd = NA,carryover=list(pars_history = pars_history),toofar=res$toofar,exact=res$exact,points=res$points)

  return(re)
}


#' Gaussian Process Regression
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
gauss.fit = function(dat) {

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

    fitfun = function(x) {
      names(x) = names(xvars)
      DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK")$mean
    }

    isflat = abs(fitfun(mins-.5)-fitfun(maxs+.5))<.001

  }

  if (n.try==100) warning("model not converged")

  fitfun.sd = function(x) {
    names(x) = names(xvars)
    DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK")$sd
  }

  # move this check outside the fitting function?
  somevals = apply(xvars,1,function(x) fitfun(x))
  isplane = summary(lm(y~.,cbind(xvars,y = somevals)))$r.squared>.98 #
  if (isplane|| is.na(isplane)) {warning("Plane fitted")}


  re = list(fitfun=fitfun,fitfun.sd=fitfun.sd)

  return(re)
}


