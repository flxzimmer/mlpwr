
folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"

# less packages
packages = c( "simr" , "simpackage"  ,"lme4",  "Matrix" ,  "mirt" , "lattice" ,  "pwrml", "randtoolbox" ,"rngWELL" , "WeightSVM", "sn", "stats4" , "e1071","DiceKriging" , "digest" , "rlist", "faux", "parallel", "grid" , "MASS" , "gridExtra", "ggplot2","stats", "graphics","grDevices",  "utils" , "datasets","methods", "base","pso","optimr","spatstat","pwr")

# more packages
packages2= c( "simr" , "simpackage"  ,"lme4",  "Matrix" ,  "mirt" , "lattice" ,  "pwrml", "randtoolbox" ,"rngWELL" , "WeightSVM", "sn", "stats4" , "e1071","DiceKriging" , "digest" , "rlist", "faux", "parallel", "grid" , "MASS" , "gridExtra", "ggplot2","stats", "graphics","grDevices",  "utils" , "datasets","methods", "base","plotly","pso","optimr","spatstat","pwr")


#' Title
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
    # goal.cis = c(.025,.02,.015)
    goal.cis = c(.05,.04,.03)
  }

  # if(fun_nr==2) { # T-test 2D
  #   runfun = runfun.wilson2()
  #   design = list(n = c(100,1200),k=c(5,30))
  #   cost = function(x) x[1]*5+x[2]*100
  #   budgets = c(2000,4000,8000)
  #   # budgets = c(1000,2000,4000)
  #   goal.cis = c(.06,.05,.04)
  #   fixed_cost = 3500
  # }
  if(fun_nr==2) { # ANOVA
    runfun = runfun.anova()
    design = list(n = c(20,90),k=c(5,25))
    # cost = function(x) x[1]*x[2]*5+x[2]*100
    # cost = function(x) x[1]*1+x[2]*6
    cost = function(x) x[1]*1.9+x[2]*6

    budgets = c(2000,4000,8000)
    # budgets = c(1000,2000,4000)
    goal.cis = c(.06,.05,.04)
    # fixed_cost = 150
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
    # hyp <- setup_hypothesis(type = "1PLvs2PL", altpars = itempars)
    # ncps <- calculate_ncps(hyp=hyp,sampling=TRUE,sampling.npers = 10^5,approx.npers=10^5)
    # analytical = ssize(hyp=hyp,ncp=ncps["LR"],alpha=.05,power=goal)
    # analytical = 1336
    analytical = 150
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
    # cost = function(x) x[1]*x[2]*5+x[1]*100
    # analytical is not available here
    # budgets = c(1000,2000,4000)
    budgets = c(2000,8000,16000)
    goal.cis = c(.05,.04,.03)
    fixed_cost = 320
  }


  # if(task=="A") {
  #   # fixed_cost=NULL
  #   # goal.ci = NULL
  #   if(budget=="low")budget = budgets[1]
  #   if(budget=="mid")budget = budgets[2]
  #   if(budget=="high")budget = budgets[3]
  # }

  if(task=="B") {
    fixed_cost=NULL
    # budget = NULL

    # if(is.na(goal.ci)){
    #   goal.ci=.025
    # }

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
    # budget = NULL

    # if(is.na(goal.ci)){
    #   goal.ci=.025
    # }

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





#' Title
#'
#' @param obj
#' @param detailled
#'
#' @return
#' @export
#'
#' @examples
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
      # print(a)
    }

    if(detailled){
        timex = proc.time() - obj
        return(timex)
      }
    }
}


hush=function(code){
  # st = paste(sample(1:20,1),".txt")
  # sink(st)

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

# usedruns = function(dat) {
#   return(sum(sapply(dat, function(x) x$h)))
# }

# updatestats = function(x,alpha=.05,CI=.95) {
#
#   x$h = length(x$p)
#   x$power = mean(x$p<alpha)
#   x$var = x$power * (1-x$power) /x$h
#   radius = qnorm((1-CI)/2) * sqrt(x$var)
#   x$CI = sapply(c(-1,1),function(d) x$power + qnorm((1+d*CI)/2) * sqrt(x$var))
#   x$alpha = alpha
#   return(x)
# }



#' Title
#'
#' @param dat
#' @param aggregate
#'
#' @return
#' @export
#'
#' @examples
todataframe = function(dat,aggregate=TRUE,pseudo=FALSE) {

  dim.design=length(dat[[1]]$x)
  # if ( length(dat)>10)

  if (aggregate) {
  temp = t(sapply(dat,function(v) c(as.numeric(v$x),mean(as.numeric(v$y)))))
  # temp = apply(temp,2,as.numeric)
  # temp = as.data.frame(temp)
  # names(temp)[length(temp)] = "y"
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
    # names(temp)[2:length(temp)] = names(dat[[1]]$x)
    temp = temp[,c(2:length(temp),1)]
    # temp = matrix(temp,ncol=length(temp))
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
    # names(temp)[2:length(temp)] = names(dat[[1]]$x)
    temp = temp[,c(2:length(temp),1)]
  }

  temp = apply(temp,2,as.numeric)
  temp = as.data.frame(temp)
  names(temp) = c(paste0("V",1:dim.design),"y")
  # names(temp)[length(temp)] = "y"

  # # Handling the case of zero variance in the data (causes error)
  # if(var(datx$power)==0) {
  #
  #   datx$power = datx$power+rnorm(nrow(datx))/1000 # adding some variance
  # }
  return(temp)
}




get.sd = function(dat,value) {
  # value = pred$new.n
  ind = which(sapply(dat,function(ele) all(ele$x==value)))
  if (length(ind)==0) return(10)
  else {
    return(getweight(dat,"sd")[ind])
  }
}


get.closest = function(dat,new.n,predfun.sd=NULL) {
  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  xvars.z= scale(xvars)
  new.n.z = matrix(new.n,nrow=1)
  new.n.z = scale(new.n.z,center=attributes(xvars.z)$`scaled:center`,scale = attributes(xvars.z)$`scaled:scale`)
  dist = apply(xvars.z,1,function(x) sum((x-new.n.z)^2))
  ind = which(dist==min(dist))
  closest = as.numeric(xvars[ind,])
  if (!is.null(predfun.sd)) closest.sd = predfun.sd(closest)
  if (is.null(predfun.sd)) closest.sd = getweight(dat,"sd")[ind]
  re = list(value=closest,y = datx[ind,ncol(datx)],sd =closest.sd)
  return(re)
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

  # if(weight.type=="relevance"& is.null(predfun)) weight.type= "freq"

  # if(weight.type=="relevance"& !is.null(predfun)) {
  #   datx = todataframe(dat,aggregate=TRUE)
  #   xvars = datx[,1:(length(datx)-1),drop=FALSE]
  #   lastpred = apply(xvars,2,mean)
  #
  #   cost=function(x)sum(x)
  #   fn = function(x)abs(predfun(x)-.8)^(2)*10^5+cost(x)/cost(lastpred)
  #   }

  fun = function(vec,weight.type) {
    vec = as.numeric(vec)
    p = mean(vec)
    # if (is.na(p)) browser()
    n = length(vec)
      if (correct_zero) {
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


  # if (weight.type == "relevance"& !is.null(predfun)) {
  #
  #   fun= function(x) 1/fn(as.numeric(x))
  #   w = sapply(dat,function(v) fun(v$x))
  #
  #   return(w)
  #   }

  w = sapply(dat,function(v) fun(v$y,weight.type))
  return(w)
}




#' Title
#'
#' @param design
#' @param n.points
#'
#' @return
#' @export
#'
#' @examples
initpoints = function(design,n.points,seed=sample(1:10^3),random=F) {
  # s = as.matrix(sobol(n = n.points-2,dim=length(design),scrambling=1,seed=seed),ncol=length(design))
  # pmin = pmax = c()
  # for (i in 1:length(design)) {
  #   dmin = design[[i]][1]
  #   dmax = design[[i]][2]
  #   s[,i] = dmin + s[,i]*(dmax-dmin)
  #   pmin[i] = dmin
  #   pmax[i] = dmax
  # }
  # points = round(s)
  # points =rbind(points,as.numeric(pmin),as.numeric(pmax))

  # s = as.matrix(halton(n = n.points,dim=length(design)),ncol=length(design))

  if(!random) {
    s = as.matrix(halton(n = n.points-2,dim=length(design)),ncol=length(design))
    pmin = pmax = c()
    for (i in 1:length(design)) {
      dmin = design[[i]][1]
      dmax = design[[i]][2]
      s[,i] = dmin + s[,i]*(dmax-dmin)
      pmin[i] = dmin
      pmax[i] = dmax
    }
    points = round(s)
    points =rbind(points,as.numeric(pmin),as.numeric(pmax))
}
  if (random) {
    s = as.matrix(sobol(n = n.points,dim=length(design),scrambling=1,seed=seed),ncol=length(design))

    for (i in 1:length(design)) {
      dmin = design[[i]][1]
      dmax = design[[i]][2]
      s[,i] = dmin + s[,i]*(dmax-dmin)
    }
    points = round(s)
  }

colnames(points)=names(design)

return(points)
}


#' Title
#'
#' @param runfun
#' @param dat
#' @param points
#' @param each
#' @param minrun
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
addval = function(runfun,design,dat=NULL,points=NULL,each=1,minrun=F,n.points=10,seed=1) {
  if (is.null(dat)) {
    dat = list()
  }

  if (is.null(points)) {
    points = initpoints(design,n.points,seed=seed)
  }

  xvalues = sapply(dat,function(y) digest(as.numeric(y$x)))
  for (i in 1:nrow(points)) {

    ind = which(xvalues==digest(as.numeric(points[i,])))

    if(length(ind)==0) {
      ind = length(dat)+1
      dat[[ind]] = list()
      dat[[ind]]$x = points[i,]
      resx = c()
    } else {
      resx = dat[[ind]]$y
    }
    a = as.numeric(points[i,])
    # print(a)
    # if(a>1500) browser()
    resx = c(resx,replicate(each,runfun(as.numeric(points[i,]))))

    while(minrun && length(resx)> 1 && var(resx) == 0) {
      resx = c(resx,replicate(1,runfun(as.numeric(points[i,]))))
    }
    dat[[ind]]$y = resx
  }
  return(dat)

}


#
# p_to_power = function(dat,alpha,min.tests=1) {
#   # p_to_power = function(dat,alpha,min.tests=1,bins=FALSE) {
#
#   ns = sort(unique(dat$n))
#
#   power = c()
#   h = c()
#
#   # if (isFALSE(bins)) {
#
#     for (i in ns) {
#       power = c(power,as.numeric(mean(dat$p[dat$n==i]<alpha)))
#       h = c(h,sum(dat$n==i))
#     }
#   # }
#
#   # if (isTRUE(bins)) {
#   #   ns = seq(5,max(ns),5) #center values of the bins
#   #
#   #   for (i in ns) {
#   #     ind = abs(dat$n-i)<=2
#   #     power = c(power,as.numeric(mean(dat$p[ind]<alpha)))
#   #     weight = c(weight,sum(ind))
#   #   }
#   #
#   # }
#
#   sd = sqrt(power*(1-power)*h) / h
#
#   re = data.frame(
#     n=ns,
#     h = h,
#     power = power,
#     sd = sd
#   )
#
#   re=re[re$h>=min.tests&re$sd>0,]
#
#
#   return(re)
# }


# power_plot = function(dat,pred=NA,dat.true=NULL) {
#
#   p = ggplot(data=dat, aes(x=n, y=power)) +
#     geom_point()+ geom_errorbar(aes(ymin = power-sd, ymax = power+sd)) +
#     scale_y_continuous(breaks=seq(0,1,.1))
#
#   if (!is.na(pred)){
#     p = p + geom_vline(xintercept = pred,color="red")
#   }
#   if (!is.null(dat.true)){
#     p = p + geom_line(data=dat.true,aes(x=n, y=true.power),color="blue")
#   }
#   return(p)
# }
#
# quant_plot = function(dat,pred=NA,dat.true=NULL,predfun=NULL) {
#
#   p = ggplot(data=dat, aes(x=n, y=quantiles)) +
#     geom_point() +
#     scale_y_continuous(breaks=seq(0,1,.1))
#   if (!is.na(pred)){
#     p = p + geom_vline(xintercept = pred,color="red")
#   }
#   if (!is.null(dat.true)){
#     p = p + geom_line(data=dat.true,aes(x=n, y=true.quant),color="blue")
#   }
#   if (!is.null(predfun)){
#     p = p + geom_line(data=predfun,aes(x=n, y=fun),color="red")
#   }
#
#   return(p)
# }


