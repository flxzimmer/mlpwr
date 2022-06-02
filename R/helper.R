
# less packages
packages = c( "simr" , "simpackage"  ,"lme4",  "Matrix" ,  "mirt" , "lattice" , "randtoolbox" ,"rngWELL" , "WeightSVM", "sn", "stats4" , "e1071","DiceKriging" , "digest" , "rlist", "faux", "parallel", "grid" , "MASS" , "gridExtra", "ggplot2","stats", "graphics","grDevices",  "utils" , "datasets","methods", "base","pso","optimr","spatstat","pwr","RColorBrewer","dplyr","remotes")

# more packages
packages2= c( "simr" , "simpackage"  ,"lme4",  "Matrix" ,  "mirt" , "lattice", "randtoolbox" ,"rngWELL" , "WeightSVM", "sn", "stats4" , "e1071","DiceKriging" , "digest" , "rlist", "faux", "parallel", "grid" , "MASS" , "gridExtra", "ggplot2","stats", "graphics","grDevices",  "utils" , "datasets","methods", "base","plotly","pso","optimr","spatstat","pwr","RColorBrewer","dplyr","remotes")


#' Title
#'
#' @param all
#'
#' @return
#' @export
#'
#' @examples
load.libs = function(all=FALSE) {
  if (!all)  for (i in packages) library(i,character.only=T)
  if (all)  for (i in packages2) library(i,character.only=T)
}


#' Title
#'
#' @param fun_nr
#' @param task
#' @param budget
#' @param goal.ci
#'
#' @return
#' @export
#'
#' @examples
load.cond = function(fun_nr,task,budget,goal.ci) {

  goal = .8
  analytical = NA
  true_power.fun = NULL
  cost=function(x)x #default cost function

  if(fun_nr==1) { # T-test 1D
    runfun = runfun.ttest(delta = .4)
    design = list(n = c(50,200))
    budgets = c(1000,2000,4000)
    goal.cis = c(.05,.04,.03)
  }

  if(fun_nr==2) { # ANOVA
    runfun = runfun.anova()
    design = list(n = c(20,90),k=c(5,25))
    cost = function(x) x[1]*1.9+x[2]*6

    budgets = c(1000,3000,9000)
    goal.cis = c(.06,.05,.04)
    fixed_cost = 174
  }

  if(fun_nr==3) { # T-test Skewed Normal
    runfun = runfun.skewed2(delta = .4,alpha =10)
    design = list(n = c(50,200))
    analytical = power.t.test(delta = .4, sig.level = .05, power = goal,type = "two.sample", alternative = "two.sided")$n
    budgets = c(1000,2000,4000)
    goal.cis = c(.05,.04,.03)
  }

  if(fun_nr==4) { # IRT
    itempars = runfun.irt.itempars(delta=.3,n.items = 20,seed=1)
    runfun = runfun.irt(itempars)
    design = list(n = c(50,250))
    analytical = 154
    budgets = c(1000,2000,4000)
    goal.cis = c(.05,.04,.03)
  }

  if(fun_nr==5) { # Mixed Models
    runfun = runfun.simr()
    design = list(n = c(10,20))
    # analytical is not available here
    budgets = c(1000,2000,4000)
    goal.cis = c(.05,.04,.03)
  }

  if(fun_nr==6) { # Mixed Models 2D / n Study years, k Clusters
    runfun = runfun.simr2()
    design = list(n = c(5,50),k=c(3,30))
    cost = function(x) x[1]*10+x[2]*5
    budgets = c(1000,3000,9000)
    goal.cis = c(.05,.04,.03)
    fixed_cost = 320
  }


  if(task=="B") {
    fixed_cost=NULL

    if(!is.na(budget)) {

      if(budget=="low")budget = budgets[1]
      if(budget=="mid")budget = budgets[2]
      if(budget=="high")budget = budgets[3]
    }


    if(!is.na(goal.ci)) {

    if(goal.ci=="low")goal.ci = goal.cis[1]
    if(goal.ci=="mid")goal.ci = goal.cis[2]
    if(goal.ci=="high")goal.ci = goal.cis[3]
    }
  }

  if(task=="C") {
    goal = NULL

    if(!is.na(budget)) {

      if(budget=="low")budget = budgets[1]
      if(budget=="mid")budget = budgets[2]
      if(budget=="high")budget = budgets[3]
    }
    if(!is.na(goal.ci)) {

      if(goal.ci=="low")goal.ci = goal.cis[1]
      if(goal.ci=="mid")goal.ci = goal.cis[2]
      if(goal.ci=="high")goal.ci = goal.cis[3]
    }
  }

  re = list(fun_nr=fun_nr,task=task,runfun=runfun,design=design,cost=cost,budget=budget,goal.ci=goal.ci,analytical=analytical,goal=goal,true_power.fun= true_power.fun,fixed_cost=fixed_cost)
  return(re)
}


skeweddist = function(n,alpha = 4) {
  #skeweddist: alpha = 0 corresponds to normal distribution.
  # Choose alpha = 4 and adjust all other values to give mean~0 and sd~1.
  delta = alpha/sqrt(1+alpha^2)
  omega = sqrt(1/(1-2*delta^2/pi))
  xi = -omega*delta*sqrt(2/pi)

  skewness = (4-pi)/2*(delta*sqrt(2/pi))^3/(1-2*delta^2/pi)^(3/2)

  return(as.numeric(sn::rsn(n=n, xi=xi, omega=omega, alpha=alpha)))
}


timer = function(obj=NULL,detailled = FALSE) {
  # usage:
  # a = timer()
  # timer(a)

  if (is.null(obj)) {
    obj <- proc.time()
    return(obj)
  }

  if (!is.null(obj)) {

    if (!detailled){
      timex = proc.time() - obj
      t = timex[3]
      a = paste(round(t/60/60,2),"Hours |",round(t/60,2),"Minutes |",round(t,2),"Seconds")
    }

    if(detailled){
        timex = proc.time() - obj
        return(timex)
      }
    }
}


relu = function(x) {

  if (x<0) 0 else x

}



fixpred = function(x) { # get pred to [0,1] Interval
  x[x>1] = 1 - 10^(-10)
  x[x<0] = 10^(-10)
  x
}


logloss = function(true.y,pred,true.w) { #y true value, p prediction
  pred = fixpred(pred)
  weighted.mean(-(true.y * log2(pred) + (1-true.y) * log2(1-pred)),true.w)
}


hush=function(code){

  os = Sys.info()['sysname']
  st = "/dev/null"
  if (os =="Windows") st  = "NUL"

  sink(st) # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}


usedruns = function(dat) {
  return(sum(sapply(dat, function(x) length(x$y))))
}



#' Title
#'
#' @param dat
#' @param aggregate
#' @param pseudo
#'
#' @return
#' @export
#'
#' @examples
todataframe = function(dat,aggregate=TRUE,pseudo=FALSE) {

  dim.design=length(dat[[1]]$x)

  if (aggregate) {
  temp = t(sapply(dat,function(v) c(as.numeric(v$x),mean(as.numeric(v$y)))))
  }

  if (!aggregate & !pseudo) {
    temp = lapply(dat, function(v) {
      tempx = data.frame(y=v$y)
      for (i in 1:dim.design) {
        tempx[,i+1]=v$x[i]
      }
      return(tempx)
    })
    temp = do.call(rbind,temp)
    temp = temp[,c(2:length(temp),1)]
  }


  if (!aggregate & pseudo) {
    temp = lapply(dat, function(v) {
      tempx = data.frame(y=rep(mean(v$y),length(v$y)))
      for (i in 1:dim.design) {
        tempx[,i+1]=v$x[i]
      }
      return(tempx)
    })
    temp = do.call(rbind,temp)
    temp = temp[,c(2:length(temp),1)]
  }

  temp = apply(temp,2,as.numeric)
  temp = as.data.frame(temp)
  names(temp) = c(paste0("V",1:dim.design),"y")

  return(temp)
}




get.sd = function(dat,value) {

  ind = which(sapply(dat,function(ele) all(ele$x==value)))
  if (length(ind)==0) return(10)
  else {
    return(getweight(dat,"sd")[ind])
  }
}




#' Title
#'
#' @param dat
#' @param weight.type
#' @param correct_zero Adds one number to the data if it consists only of 0s or only of 1s. Variance can only then be calculated.
#'
#' @return
#' @export
#'
#' @examples
getweight = function(dat,weight.type="freq",correct_zero=T) {


  if(is.null(weight.type)) return(NULL)

  fun = function(vec,weight.type) {
    vec = as.numeric(vec)
    p = mean(vec)
    n = length(vec)
      if (correct_zero) {
        if (is.na(p==0)) browser()
        if (p==0) vec = c(vec,1)
        if (p==1) vec = c(vec,0)
        p = mean(vec)
    }
    variance = p * (1-p) / n

    if (weight.type == "freq") return(n)
    if (weight.type == "var") return(variance)
    if (weight.type == "inv_var") return(1/variance)
    if (weight.type == "sd") return(sqrt(variance))
    if (weight.type == "inv_sd") return(1/sqrt(variance))
  }

  w = sapply(dat,function(v) fun(v$y,weight.type))
  return(w)
}










