
# if (model.alt@"Call"[[1]]=="mirt"){
#   model = model.alt@Model
#   pars = coef(model.alt,simplify=TRUE)
#   guess = model.alt@Data$N
# }


#   # # two lines added to make it easier for the M2 test, can be deleted later
#   # ind = sample(1:nrow(df),floor(nrow(df)/3))
#   # df[ind,] = 0


# if (model.alt@"Call"[[1]]=="mirt"){
#   model = model.alt@Model
#   pars = coef(model.alt,simplify=TRUE)
#   n.pers = model.alt@Data$N
# }




# functions ---------------------------------------------------------------


#' setup Models
#'
#' Setting up the IRT Model that corresponds to the null hypothesis being tested.
#'
#'
#' @param architecture IRT Model, e.g. Rasch
#' @param n.items Number of items
#' @param pers.dist Distribution of Person Parameters, e.g. normal
#' @param beta Vector of length n.items specifiying item difficulty
#'
#' @return List of model parameters and distribution function
#' @export
#'
#' @examples
setup.model <- function(architecture = "Rasch", n.items= 20, alpha = c(), beta = c(), pers.dist = "normal") {

  if (architecture=="Rasch") {
    items = data.frame(
      alpha = rep(1,n.items),
      beta = beta,
      gamma = rep(0,n.items)
    )

  }

  if (architecture=="2PL") {
    items = data.frame(
      alpha = alpha,
      beta = beta,
      gamma = rep(0,n.items)
    )

  }

  if (pers.dist =="normal") {
    dist.func = rnorm
  }


  re = list(architecture=architecture,n.items=n.items,items = items,dist.func =dist.func)

  return(re)
}




#' Alternative Models
#'
#' Specify alternative models. Instance functions have to be defined to create a model with the desired effect size difference.
#' TODO Anchoring
#'
#'
#' @param class Type of alternative model. 2PL or DIF currently
#' @param diftype Type of DIF: "all" All items have different pars, alternatively an integer specifying the number of items with DIF.
#'
#' @return Function to initialize model with given effect size, function to create dataset from model parameters
#' @export
#'
#' @examples
setup.alternative <- function(class="2PL",diftype="all") {

  if (class=="2PL") {
    instance <- function(model,effect.size) {
      items.true = model$items
      error = rlnorm(nrow(items.true),0,.05)-1

      opt.func <- function(x) {
        temp = items.true
        slopes = items.true$alpha + error*x
        temp$alpha = slopes
        kl(items.true,temp,order=5)-effect.size
      }
      errorfactor = uniroot(opt.func,c(0,100))$root

      items.alt = model$items
      items.alt$alpha = items.alt$alpha+error*errorfactor

      re = items.alt
      return(re)
    }

    df.init = function(model,items.alt,n.pers) {

      initialize(xpl(model$dist.func(n.pers),items.alt$beta,items.alt$alpha,items.alt$gamma))
    }
  }


  if (class=="DIF") {
    instance <- function(model,effect.size) {
      items.true = model$items
      if (diftype == "all") {
        error = rnorm(nrow(items.true), 0, .05)
      } else {
        error = c(rnorm(diftype, 0, .05),rep(0,nrow(items.true)-diftype))
      }

      opt.func <- function(x) {
        temp = items.true
        temp$beta = items.true$beta + error*x
        kl(items.true,temp,order=5)/2-effect.size
      }
      errorfactor = uniroot(opt.func,c(0,1000))$root

      items.alt = model$items
      items.alt$beta = items.alt$beta+error*errorfactor

      re = items.alt
      return(re)
    }

    df.init = function(model,items.alt,n.pers) {
      half = round(n.pers/2)
      a1 = initialize(xpl(model$dist.func(half),model$items$beta,model$items$alpha,model$items$gamma))
      a2 = initialize(xpl(model$dist.func(n.pers-half),items.alt$beta,items.alt$alpha,items.alt$gamma))
      rbind(a1,a2)
    }
  }

  re = list(class=class,instance = instance,df.init=df.init)
  return(re)
}



#' setup Tests
#'
#' Wrapper for Tests
#'
#' @param type Test, currently: AndersenLR, M2
#'
#' @return Function that takes a dataset and returns a p-value
#' @export
#'
#' @examples
setup.test <- function(type = "AndersenLR",waldtype="any") {

  if (type == "AndersenLR") {
    re = function(df) {
      df = clean.data(df)
      rm <- RM(df)
      re1 = LRtest(rm)$pvalue
      return(re1)
    }
  }

  if (type == "M2") {
    re = function(df) {
      df = clean.data(df)
      rm = mirt(as.data.frame(df),1,"Rasch")
      re1 = M2(rm)$p
      return(re1)
    }
  }

  if (type == "Wald") {
    re = function(df) {
      df = clean.data(df)
      #      browser()
      group = c(rep("a", round(nrow(df)/2) ), rep("b", nrow(df) - round(nrow(df)/2)))
      model <- multipleGroup(df, 1, group, SE = TRUE)
      a = DIF(model, 'd',Wald=TRUE,p.adjust = 'fdr')
      if (waldtype =="any") {
        re1 = min(a$adj_pvals)
      } else if (waldtype == "single") {
        re1 = a$adj_pvals[[1]]
      }

      return(re1)
    }
  }

  return(re)
}



#' Power
#'
#' perform Power analysis
#'
#' @import spatstat
#' @import mirt
#' @import eRm
#'
#' @param model model parameters created from the corresponding function
#' @param altmodel altmodel functions created from the corresponding function
#' @param test test functions created from the corresponding function
#' @param effect.size effect size (currently only expected Kullback-Leibler Distance)
#' @param n.pers Number of Persons
#' @param runs Number of Runs
#' @param alpha Alpha Niveau
#'
#' @return Power or 1-Beta
#' @export
#'
#' @examples
power <- function(model = model, altmodel=altmodel, test = test1, effect.size = .01,alpha = .05, n.pers=n.pers,runs=2){


  result <- function(n.pers=n.pers,items.alt,test1) {
    items.alt = altmodel$instance(model=model,effect.size = effect.size)
    df = altmodel$df.init(model,items.alt,n.pers)
    re = test1(df)
    return(re)
  }

  res = c()
  for (i in 1:runs) {
    res[i]=result(n.pers=n.pers,items.alt,test1)
  }
  re = mean(res<alpha)
  return(re)
}



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
svm.pred = function(dat,aggregate=TRUE,use.weight=aggregate, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,...) {

  # Global Settings
  kernel = "radial"
  ctrl = tune.control(sampling ="cross",cross =5)
  tune=FALSE
  widestart=FALSE
  start.pars=list()

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]

  if(use.weight) weight = getweight(dat,weight.type)
  if(!use.weight) weight = NULL

  # Determine if first round
  firstround = is.null(last.out$pars_history)

  if (firstround) {
    pars_history = list()
    pars = start.pars

    # set unspecified start values to defaults
    if (is.null(pars$cost)) {pars$cost = 100}
    if (is.null(pars$epsilon)) {pars$epsilon = .1}
    if (is.null(pars$gamma)) {pars$gamma = .05}
  }

  if (!firstround) {
    pars_history = last.out$pars_history
    pars = pars_history[[length(pars_history)]]
  }

  if (tune) {
    tunepars = data.frame(cost=pars$cost,epsilon=c(.9,1,1.1)*pars$epsilon,gamma=pars$gamma)

    # (optional) Set wider tuning pars
    if(firstround & widestart) {
      tunepars = data.frame(cost=pars$cost,epsilon=seq(.01,.2,length.out = 10),gamma=pars$gamma)
    }
    # Estimate the model
    a = tune.svm(x = datx$n, y = datx$power,kernel=kernel,cost = tunepars$cost,epsilon=tunepars$epsilon,gamma =tunepars$gamma,tune.control=ctrl)

    pars= a$best.parameters
    mod = a$best.model
  }

  if (!tune) {
    mod = wsvm(y ~ .,data = datx, kernel = kernel,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,weight=weight)
  }

  # Save used parameters
  pars_history = list.append(pars_history,as.data.frame(pars))

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}
  if (fun) {return(predfun)}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost,bounds=TRUE)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,pars_history = pars_history,output=output)
  return(re)
}



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
gauss.pred = function(dat,aggregate=TRUE,use.weight=aggregate, weight.type="var",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,factor=1,nugget=FALSE,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type)

  # tune = TRUE

  if (nugget) {
    mod = DiceKriging::km(design=xvars, response=datx$y,control=list(trace=FALSE), nugget = min(weight),nugget.estim	=TRUE)

    predfun = function(x) {
      names(x) = names(xvars)
      DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK",se.compute = FALSE)$mean
    }
  }

  if (!nugget) {
    # repeat{

    mod = DiceKriging::km(design=xvars, response=datx$y,noise.var=weight,control=list(trace=FALSE))

    predfun = function(x) {
      names(x) = names(xvars)
      DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK")$mean
    }

    OK = abs(predfun(200)-predfun(100))>.01

    #   if(OK) break
    #
    #   factor = factor + 1
    #
    # }

  }

  print(factor)
  # abs(predfun(200)-predfun(100))>.01
  # plot(100:250,sapply(100:250,predfun))

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output = output,fun=predfun)

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
gauss.pred = function(dat,aggregate=TRUE,use.weight=aggregate, weight.type="var",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,nugget=FALSE,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  weight = getweight(dat,weight.type)

  if (nugget) {
    mod = DiceKriging::km(design=xvars, response=datx$y,control=list(trace=FALSE), nugget = min(weight),nugget.estim=TRUE)
  }

  if (!nugget) {
    mod = DiceKriging::km(design=xvars, response=datx$y,noise.var=weight,control=list(trace=FALSE))
  }

  predfun = function(x) {
    names(x) = names(xvars)
    DiceKriging::predict.km(mod, newdata=data.frame(t(x)), type="UK")$mean
  }
  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output = output,fun=predfun)

  return(re)

}



#   if (GLM & aggregate) {
#   weight = getweight(dat,weight.type="freq")
#   mod = glm(y~.,datx,weights=weight,family = binomial)
#   }
#
# if (GLM & !aggregate) {
#   mod = glm(y~.,datx,family = binomial)
# }


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
reg.pred2 = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,GLM=FALSE,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)

  if (!GLM) mod = lm(y~.,datx,weights=weight)

  if (GLM) {
    weight = getweight(dat,weight.type="freq")
    mod = glm(y~.,datx,weights=weight,family = binomial)
  }


  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response")}

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response",se.fit = TRUE)}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output=output,fun=predfun)
  return(re)
}



#' Multivariate logistic function
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
nonlin.pred3 = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,...) {
  # generate a prediction for the sample size at the specified power

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)


  # Determine if first round
  firstround = is.null(last.out$pars_history)

  if (firstround) {
    pars_history = list()

    # Find some start Values
    difs = abs(datx$y-.5)
    ind = which(difs==min(difs))[1]
    xmid = xvars[ind,]
    xmid1 = as.numeric(xmid[1])
    xmid2 = as.numeric(xmid[2])
    # par = c(50,10)
    # # fn = function(x) abs(1/(1+exp((xmid-max(datx$V1))/x))-.95)
    # fn = function(x) abs(1/(1+exp((xmid1-max(datx$V1))/x[1]+(xmid2-max(datx$V2))/x[2]))-.95)
    #
    # scal = optim(par, fn,method = "BFGS")$par
    #
    # pars = c(xmid1=xmid1,xmid2=xmid2,scal1=scal[1],scal2=scal[2])


    library(nls.multstart) # from CRAN

    datx$V2 = datx$V1+sample(-5:5,length(datx$V1),replace=T)

    mod = nls_multstart(y ~ 1/(1+exp((xmid1-V1)/scal1+(xmid2-V2)/scal2)),
                        data = datx,
                        iter = 250,
                        start_lower = c(xmid1=xmid1*.8, xmid2=xmid2*.8, scal1=10, scal2=10),
                        start_upper = c(xmid1=xmid1*1.2, xmid2=xmid2*1.2, scal1=100, scal2=100),
                        supp_errors = 'Y',
                        convergence_count = 100,
                        na.action = na.omit,
                        lower = c(xmid1=0, xmid2=0, scal1=1, scal2=1))


    # pars = coef(mod)


    # Find some start Values
    # difs = abs(datx$y-.5)
    # ind = which(difs==min(difs))[1]
    # xmid = datx$V1[ind]
    # par = 100
    # fn = function(x) abs(1/(1+exp((xmid-max(datx$V1))/x))-.95)
    # scal = optim(par, fn,method = "BFGS")$par
    #
    # pars = c(xmid=xmid,scal=scal)

  }

  if (!firstround) {
    pars_history = last.out$pars_history
    pars = pars_history[[length(pars_history)]]
  }

  # mod <- nls(y ~ 1/(1+exp((xmid1-V1)/scal1+(xmid2-V2)/scal2)),data=datx,control=list(warnOnly=T),start=pars)


  # mod <- nls(y ~ 1/(1+exp((xmid-V1)/scal)),data=datx,weights=weight,control=list(warnOnly=F),start=pars)

  # Save used parameters
  pars_history = list.append(pars_history,coef(mod))

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output = output,fun = predfun)

  return(re)
}

# logistic function 2
difs = abs(datx$y-.5)
ind = which(difs==min(difs))[1]
xmid = as.numeric(xvars[ind,])

scal = c(20)
pars = c(scal1=scal[1])

frmla = as.formula(y ~ 1/(1+exp((0-V1)/scal1)))
start_lower = pars * c(.5)
start_upper = pars * c(2)
lower = c(scal1=1)

mod2 = nls_multstart(frmla,data = datx,iter = 250,start_lower = start_lower,
                     start_upper = start_upper,supp_errors = 'Y',convergence_count = 100,na.action = na.omit, lower = lower)

fn2 = function(x) {
  names(x) = names(xvars)
  predict(mod2,newdata=data.frame(t(x)))}





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
reg.pred2 = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)

  mod = glm(y~.,datx,weights=weight,family = binomial)

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response")}

  # predfun = function(x) {
  #   names(x) = names(xvars)
  #   predict(mod,newdata=data.frame(t(x)),type="response",se.fit = TRUE)}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output=output,fun=predfun)
  return(re)
}


#' Title
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
reg.pred = function(dat,alpha =.05,goal=.8,round_res=TRUE,last.out=NULL,weight=TRUE,fun=FALSE,...) {
  # generate a prediction for the sample size at the specified power

  datx = todataframe(dat)

  sample_weight = NULL
  if(weight) {sample_weight = 1/datx$var}

  # fit model
  mod = lm(power~n,datx,weights=sample_weight)

  predfun = function(n) {predict(mod,newdata=data.frame(n=n))}
  if (fun) {return(predfun)}

  # generate prediction
  new.n = optim(par=100,function(n) abs(predfun(n)-goal),method="Brent",lower=min(datx$n)/1000,upper=max(datx$n)*1000)$par

  if (round_res) {new.n = round(new.n)}

  re=list(new.n = new.n)

  return(re)
}






#' GLM Version?
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
reg.pred0 = function(dat,aggregate=FALSE,use.weight=aggregate, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,...) {

  datx = todataframe3(dat,aggregate=aggregate)

  if(use.weight) weight = getweight(dat,weight.type)
  if(!use.weight) weight = NULL

  browser()
  mod <- glm(y ~ n, family = binomial(),data=datx,weights=weight)
  mod
  mod = lm(y~n,datx,weights=weight)
  mod

  # fit model
  mod <- glm(y ~ n + k, family = binomial(),data=datx,weights=weight)

  predfun = function(x) {
    names(x) = names(mod$coefficients)[2:length(mod$coefficients)]
    predict(mod,newdata=data.frame(t(x)))}
  if (fun) {return(predfun)}

  # generate prediction
  startpar = colMeans(xvars)
  fn = function(x) abs(predfun(x)-goal)
  new.n = optim(par=startpar,fn=fn,method="L-BFGS-B")$par

  re=list(new.n = new.n)

  return(re)
}

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
#'
#' @return
#' @export
#'
#' @examples
logi.pred2 = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,alt1=F,alt2=F,alt3=F,alt4=F,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)

  # fam = quasi(link="logit", variance = "mu(1-mu)")
  # mod = glm(y~.,datx,weights=weight,family = fam)

  if (alt1) {
    # Transforming data before and after fitting
    trans = function(x) x/2 +.5
    invtrans = function(y) y*2-1
    datx$y = trans(datx$y)

    fam = quasi(link="logit", variance = "mu(1-mu)")
    mod = glm(y~.,datx,weights=weight,family = fam)
    predfun = function(x) {
      names(x) = names(xvars)
      invtrans(predict(mod,newdata=data.frame(t(x)),type="response"))}
  }


  if (alt2) {
    # Transforming data before and after fitting + Adjusting Variance
    trans = function(x) x/2 +.5
    invtrans = function(y) y*2-1
    datx$y = trans(datx$y)

    fam = quasi(link="logit", variance = "mu(1-mu)")
    fn = function(mu) (invtrans(mu) * (1-invtrans(mu)))
    fam$variance = fn
    mod = glm(y~.,datx,weights=weight,family = fam)
    predfun = function(x) {
      names(x) = names(xvars)
      invtrans(predict(mod,newdata=data.frame(t(x)),type="response"))}
  }

  if (alt3) {
    # Inputting new link function
    trans = function(x) x/2 +.5
    invtrans = function(y) y*2-1

    fam = quasi(link="logit", variance = "mu(1-mu)")
    linkfun = function(mu) log(mu/(1-mu))
    linkfun.new = function(mu) trans(linkfun(mu))
    fam$linkfun = linkfun.new

    linkinv = function(mu) 1/(1+exp(-mu))
    linkinv.new = function(mu) linkinv(invtrans(mu))
    fam$linkinv = linkinv.new
    # linkinv.new(linkfun.new(.7))
    # fn = function(mu) (invtrans(mu) * (1-invtrans(mu)))
    # fam$variance = fn
    fam$mu.eta = function(mu) 2 * exp(mu*2+1) / (exp(mu*2)+exp(1))^2
    mod = glm(y~.,datx,weights=weight,family = fam)

    predfun = function(x) {
      names(x) = names(xvars)
      predict(mod,newdata=data.frame(t(x)),type="response")}
  }

  if (alt4) {
    # Inputting new link function
    trans = function(x) x/2 +.5
    invtrans = function(y) y*2-1

    fam = quasi(link="logit", variance = "mu(1-mu)")
    linkfun = function(mu) log(mu/(1-mu))
    linkfun.new = function(mu) linkfun(trans(mu))
    fam$linkfun = linkfun.new

    linkinv = function(mu) 1/(1+exp(-mu))
    linkinv.new = function(mu) invtrans(linkinv(mu))
    fam$linkinv = linkinv.new
    # linkinv.new(linkfun.new(.7))
    # fn = function(mu) (invtrans(mu) * (1-invtrans(mu)))
    # fam$variance = fn
    fam$mu.eta = function(mu) 2 * exp(-mu) / (exp(-mu)+1)^2
    mod = glm(y~.,datx,weights=weight,family = fam)

    predfun = function(x) {
      names(x) = names(xvars)
      predict(mod,newdata=data.frame(t(x)),type="response")}
  }

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output=output,fun=predfun)
  return(re)
}

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
#'
#' @return
#' @export
#'
#' @examples
logi.pred = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)


  # browser()
  mod = glm(y~.,datx,weights=weight,family = binomial)

  # datx$y = (datx$y+1)/2
  # # datx$y = round(datx$y,2)
  # fam = quasi(link="logit", variance = "mu(1-mu)")
  # mod = glm(y~.,datx,weights=weight,family = fam)
  #

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)),type="response")}

  # predfun = function(x) {
  #   names(x) = names(xvars)
  #   predict(mod,newdata=data.frame(t(x)),type="response",se.fit = TRUE)}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output=output,fun=predfun)
  return(re)
}


# nonlinear Regression ----------------------------------------------------


#' Multivariate logistic function
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
nonlin.pred3 = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,...) {
  # generate a prediction for the sample size at the specified power

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)

  # Determine if first round
  firstround = is.null(last.out$pars_history)

  if (firstround) {
    pars_history = list()

    # Find some start Values
    difs = abs(datx$y-.5)
    ind = which(difs==min(difs))[1]
    xmid = as.numeric(xvars[ind,])

    if (length(xvars) == 2) {
      scal = c(20,20)
      pars = c(xmid1=xmid[1],xmid2=xmid[2],scal1=scal[1],scal2=scal[2])
    }
    if (length(xvars) == 1) {
      scal = c(20)
      pars = c(xmid1=xmid[1],scal1=scal[1])
    }
  }

  if (!firstround) {
    pars_history = last.out$pars_history
    pars = pars_history[[length(pars_history)]]
  }

  if (length(xvars) == 2) {
    frmla = as.formula(y ~ 1/(1+exp((xmid1-V1)/scal1+(xmid2-V2)/scal2)))
    start_lower = pars * c(.8,.8,.5,.5)
    start_upper = pars * c(1.2,1.2,2,2)
    lower = c(xmid1=0, xmid2=0, scal1=1, scal2=1)
  }

  if (length(xvars) == 1) {
    frmla = as.formula(y ~ 1/(1+exp((xmid1-V1)/scal1)))
    start_lower = pars * c(.8,.5)
    start_upper = pars * c(1.2,2)
    lower = c(xmid1=0, scal1=1)
  }

  datx$weight=weight
  mod = nls_multstart(frmla,data = datx,iter = 250,start_lower = start_lower,
                      start_upper = start_upper,supp_errors = 'Y',convergence_count = 100,na.action = na.omit, lower = lower,modelweights=weight)


  # Save used parameters
  pars_history = list.append(pars_history,coef(mod))

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output = output,fun = predfun)

  return(re)
}




#' Title
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
nonlin.pred2 = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE,last.out=NULL,fun=FALSE,...) {
  # generate a prediction for the sample size at the specified power

  datx = todataframe(dat)

  startpars = list(
    list(b=-130,d=0.02),
    list(b=-500,d=0.002),
    list(b=-10,d=0.4),
    list(b=-5,d=0.8),
    list(b=-60,d=0.2)
  )

  if (!is.null(last.out)) {
    startpars = c(list(last.out$coef),startpars)
  }

  for (i in 1:length(startpars)) {
    mod <- try(nls(power~1/(1+exp(-d*(n+b))),start=startpars[[i]],data = datx,weights = datx$h),silent=TRUE)
    if(class(mod)[1]!="try-error") {break}
  }

  predfun = function(n) {predict(mod,newdata=data.frame(n=n))}
  if (fun) {return(predfun)}

  # generate prediction
  new.n = optim(par=100,function(n) abs(predfun(n)-goal),method="Brent",lower=min(datx$n),upper=max(datx$n))$par

  if (round_res) {new.n = round(new.n)}

  re=list(new.n = new.n,coefs=as.list(coef(mod)))

  return(re)
}



#' Title
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
nonlin.pred = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE,last.out=NULL,fun=FALSE,...) {
  # generate a prediction for the sample size at the specified power

  # könnte man die Parameter noch tunen?
  datx = todataframe(dat)

  # fit model
  # ctrl = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024^5,
  #           printEval = FALSE, warnOnly = TRUE)

  mod <- nls(power~n^b+d,start=list(b=0.3,d=0),data = datx,weights = datx$h)


  predfun = function(n) {predict(mod,newdata=data.frame(n=n))}
  if (fun) {return(predfun)}

  # generate prediction
  new.n = optim(par=100,function(n) abs(predfun(n)-goal),method="Brent",lower=min(datx$n),upper=max(datx$n))$par

  if (round_res) {new.n = round(new.n)}

  re=list(new.n = new.n)

  return(re)
}



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
#' @param setsize
#' @param learner
#' @param startset.size
#' @param startbudget
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ss.find = function(learner,runfun,goal,goal.width,alpha=.05,guess,bounds,CI=.95,budget = 10^3,stopearly=FALSE,setsize=50,startset.size=setsize,startbudget=.4*budget,n.startsets=NULL,seed=sample(1:10000000,1),interrupt=FALSE,dat=NULL,...) {

  set.seed(seed) # set seed for multiple methods to have same starting data

  # Initialize dataset using the guess
  if(is.null(n.startsets)) n.startsets = round(startbudget / startset.size)
  dat = addval(val=round(seq(guess[1],guess[2],length.out=n.startsets)),runfun=runfun,N=startset.size,alpha=alpha,CI=CI,dat=dat)

  # update runs
  runs = budget - usedruns(dat)

  new = NULL

  repeat{

    # Increment number of runs
    if(runs<=0){break}

    # Fit Model to generate prediction
    if (!interrupt) {
      new = tryCatch(learner(dat = dat ,alpha=alpha,goal=goal,bounds=bounds,last.out = new,...),error=function(x) return(NULL))
    }
    if (interrupt) {
      new = tryCatch(learner(dat = dat ,alpha=alpha,goal=goal,bounds=bounds,last.out = new,...),error=function(x) return(print(x)))
    }

    if(is.null(new)) {
      browser()
      return(NA)}
    new.n = new$new.n

    # Check success criterion
    if(stopearly && new$overlap>CI ) {break}

    #add value to the dataset
    dat = addval(val=new.n,runfun=runfun,dat=dat,N=setsize,alpha=.05,CI=CI,minrun = TRUE)
    # update runs
    runs = budget - usedruns(dat)

  }

  #Calculate non-rounded value for final output:
  new = tryCatch(learner(dat = dat ,alpha=alpha,goal=goal,bounds=bounds,round_res=FALSE,last.out = new,...),error=function(x) return(NULL))
  if(is.null(new)) {return(NA)}
  new.n = new$new.n

  re = list(new.n,dat)
  if (length(new)>1) {
    re = list(new.n,dat,new[[2]])
  }

  return(re)
}




# Loess Regression ---------------------------------------------------------


#' Title
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
loess.pred2 = function(dat,aggregate=TRUE,use.weight=aggregate, weight.type="inv_var",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,...) {

  datx = todataframe3(dat,aggregate=aggregate)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]

  if(use.weight) weight = getweight(dat,weight.type)
  if(!use.weight) weight = NULL

  mod = loess(y~.,datx,weights=weight)

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost,bounds=TRUE)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,output = output,fun=predfun)

  return(re)
}


#
# loess.pred.fun = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE) {
#   # generate a prediction for the sample size at the specified power
#   # uses loess regression
#
#   # könnte man die Parameter noch tunen?
#   datx = todataframe(dat)
#
#   # fit model
#   mod = loess(power~n,datx,weights=datx$h)
#
#   re = function(n) {predict(mod,newdata=data.frame(n=n))}
#   return(re)
# }



#' #' Title
#' #'
#' #' @param dat
#' #' @param alpha
#' #' @param goal
#' #' @param bounds
#' #' @param round_res
#' #' @param last.out
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' loess.pred = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE,last.out=NULL,...) {
#'   # generate a prediction for the sample size at the specified power
#'   # uses loess regression
#'
#'   # könnte man die Parameter noch tunen?
#'   datx = todataframe(dat)
#'
#'   # fit model
#'   mod = loess(power~n,datx,weights=datx$h)
#'
#'   # generate prediction
#'   #Problem hier: Loess extrapoliert nicht außerhalb der Werte die der Algorithmus kennt. kleinsten / größten Wert als grenze der optim function einsetzen.
#'   new.n = optim(par=100,function(n) abs(predict(mod,newdata=data.frame(n=n))-goal),method="Brent",lower=min(datx$n),upper=max(datx$n))$par
#'
#'   if (round_res) {new.n = round(new.n)}
#'
#'   re=list(new.n = new.n)
#'
#'   return(re)
#' }
#'
#' loess.pred.fun = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE) {
#'   # generate a prediction for the sample size at the specified power
#'   # uses loess regression
#'
#'   # könnte man die Parameter noch tunen?
#'   datx = todataframe(dat)
#'
#'   # fit model
#'   mod = loess(power~n,datx,weights=datx$h)
#'
#'   re = function(n) {predict(mod,newdata=data.frame(n=n))}
#'   return(re)
#' }
#'





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
gauss.pred = function(dat,alpha =.05,goal=.8,goal.width=.1,predtype="UK",bounds,criterium="overlap",round_res=TRUE,last.out=NULL,fun=FALSE,...) {
  # generate a prediction for the sample size at the specified goal power
  # criterium is the unit measuring the distance to the goal power.

  datx = todataframe(dat)

  gp= hush(DiceKriging::km(design=data.frame(n = datx$n), response=datx$power,noise.var=datx$var,...))
  # gp= DiceKriging::km(design=data.frame(n = datx$n), response=datx$power,noise.var=datx$var,...)

  predfun = function(n) {DiceKriging::predict.km(gp, newdata=data.frame(n=n), type=predtype)$mean}
  if (fun) {return(predfun)}


  if (criterium == "overlap") {fn = function(n) {return(-1 * calc_overlap(n,gp,goal=goal,goal.width=goal.width,predtype = predtype))} }

  if (criterium == "abs") {fn = function(x) {
    p <- DiceKriging::predict.km(gp, newdata=data.frame(n=x), type=predtype)
    re = abs(p$mean - goal)
    return(re)
  }}

  new.n = optim(par=100,fn,method="Brent",lower=min(datx$n),upper=max(datx$n))$par

  if (round_res) {new.n = round(new.n)}

  overlap = calc_overlap(new.n,gp,goal=goal,goal.width=goal.width,predtype = predtype)

  re=list(new.n = new.n,overlap=overlap)

  return(re)
}



calc_overlap = function(x,gp,goal,goal.width,predtype="UK") {
  # Goal is that .8 lies within the 95% CI that has a maximum width of .01.
  # Measure percentage of dist that lies in the areal .795-.805.

  # x = data.frame(t(x))
  # names(x) = c("n","k")
  p <- DiceKriging::predict.km(gp, newdata=data.frame(n=x), type=predtype)

  a = pnorm(c(goal-goal.width/2,goal+goal.width/2),mean=p$mean,sd = p$sd)
  re = a[2] - a[1]

  # re = -overlap

  # if (overlap>.95) re = 0 else re = -overlap

  return (re)
}


calc_overlap_multi = function(x,gp,goal,goal.width,predtype="UK") {
  # Goal is that .8 lies within the 95% CI that has a maximum width of .01.
  # Measure percentage of dist that lies in the areal .795-.805.

  x = data.frame(t(x))
  names(x) = c("n","k")
  p <- predict.km(gp, newdata=x, type=predtype)

  a = pnorm(c(goal-goal.width/2,goal+goal.width/2),mean=p$mean,sd = p$sd)
  re = a[2] - a[1]

  # if (overlap>.95) re = 0 else re = -overlap

  return (re)
}

exp_improve_multi = function(x,gp,goal,goal.width) {
  # Goal is that .8 lies within the 95% CI that has a maximum width of .01.
  # Measure percentage of dist that lies in the areal .795-.805.

  x = data.frame(t(x))
  names(x) = c("n","k")
  p <- predict.km(gp, newdata=x, type="SK")

  a = pnorm(c(goal-goal.width/2,goal+goal.width/2),mean=p$mean,sd = p$sd)
  overlap = a[2] - a[1]

  if (overlap>.95) re = 0 else re = -overlap

  return (re)
}




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
svm.pred = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE,last.out=NULL,fun=FALSE,weight=TRUE,tune=TRUE,widestart=FALSE,start.pars=list(),...) {

  # Global Settings
  kernel = "radial"
  ctrl = tune.control(sampling ="cross",cross =5)

  # Convert data
  datx = todataframe(dat,implicitweight=weight)

  # Handling the case of zero variance in the data (causes error)
  if(var(datx$power)==0) {
    browser()
    datx$power = datx$power+rnorm(nrow(datx))/1000 # adding some variance
  }

  # Determine if first round
  firstround = is.null(last.out$pars_history)

  if (firstround) {
    pars_history = list()
    pars = start.pars

    # set unspecified start values to defaults
    if (is.null(pars$cost)) {pars$cost = 100}
    if (is.null(pars$epsilon)) {pars$epsilon = .1}
    if (is.null(pars$gamma)) {pars$gamma = .05}
  }

  if (!firstround) {
    pars_history = last.out$pars_history
    pars = pars_history[[length(pars_history)]]
  }

  if (tune) {
    tunepars = data.frame(cost=pars$cost,epsilon=c(.9,1,1.1)*pars$epsilon,gamma=pars$gamma)

    # (optional) Set wider tuning pars
    if(firstround & widestart) {
      tunepars = data.frame(cost=pars$cost,epsilon=seq(.01,.2,length.out = 10),gamma=pars$gamma)
    }
    # Estimate the model
    a = tune.svm(x = datx$n, y = datx$power,kernel=kernel,cost = tunepars$cost,epsilon=tunepars$epsilon,gamma =tunepars$gamma,tune.control=ctrl)
    a = tune.svm(x = datx$n, y = datx$power,kernel=kernel,cost = 100,epsilon=tunepars$epsilon,gamma =.05,tune.control=ctrl)

    pars= a$best.parameters
    mod = a$best.model
  }

  if (!tune) {
    mod = svm(formula = power ~ n,data = datx, kernel = kernel,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma)
  }

  # Save used parameters
  pars_history = list.append(pars_history,as.data.frame(pars))

  predfun = function(n) {predict(mod,newdata= data.frame(n=n))}
  if (fun) {return(predfun)}

  # generate prediction
  new.n = optim(par=100,function(n) abs(predfun(n)-goal),method="Brent",lower=min(datx$n),upper=max(datx$n))$par

  if (round_res) {new.n = round(new.n)}

  re=list(new.n = new.n,pars_history = pars_history)
  return(re)

}




#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
todataframe = function(dat,binary=FALSE,implicitweight=FALSE) {

  if (binary) {
    temp = lapply(dat,function(x) data.frame(n = rep(x$x,length(x$p)),s = as.numeric(x$p<x$alpha)))
    datx = do.call(rbind,temp)

    return(datx)
  }

  if (implicitweight) {
    # browser()
    datx = data.frame(t(sapply(dat,function(x) c(x$x,x$power,x$var,x$h))))

    a1 = rep(datx[,1],times=datx[,4])
    a2 = rep(datx[,2],times=datx[,4])
    datn = data.frame(n=a1,power=a2)

    # datn = apply(datx,1,function(x) t(sapply(1:x[4],function(y) c(x[1],x[2]))))
    # datn = data.frame(do.call(rbind,datn))
    # names(datn) = c("n","power")

    return(datn)
  }

  datx = data.frame(t(sapply(dat,function(x) c(x$x,x$power,x$var,x$h))))
  names(datx) = c("n","power","var","h")
  return(datx)
}


todataframe2 = function(dat) {

  datx = data.frame(t(sapply(dat,function(x) c(x$x,x$power,x$var,x$h))))
  names(datx) = c(names(dat[[1]]$x),"power","var","h")
  return(datx)
}




#' #' Title
#' #'
#' #' @param dat
#' #' @param weight.type
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' getweight = function(dat,weight.type="freq") {
#'
#'   if(is.null(weight.type)) return(NULL)
#'
#'   if (weight.type == "var") {
#'     fun = function(vec) {
#'       p = mean(vec)
#'       variance = p * (1-p)
#'       re = variance
#'       return(re)
#'     }
#'   }
#'
#'   if (weight.type == "inv_var") {
#'     fun = function(vec) {
#'       p = mean(vec)
#'       variance = p * (1-p)
#'       re = 1/variance
#'       return(re)
#'     }
#'   }
#'
#'   if (weight.type == "sd") {
#'     fun = function(vec) {
#'       p = mean(vec)
#'       variance = p * (1-p)
#'       re = sqrt(variance)
#'       return(re)
#'     }
#'   }
#'
#'   if (weight.type == "inv_sd") {
#'     fun = function(vec) {
#'       p = mean(vec)
#'       variance = p * (1-p)
#'       re = 1/sqrt(variance)
#'       return(re)
#'     }
#'   }
#'
#'   if (weight.type == "freq") {
#'     fun = function(vec) {
#'       re = length(vec)
#'       return(re)
#'     }
#'   }
#'
#'   w = sapply(dat,function(v) fun(v$y))
#'   return(w)
#'   }
#'
#'
#'
#'
#'






#' Title
#'
#' @param val
#' @param runfun
#' @param dat
#' @param N
#' @param minrun
#' @param alpha
#' @param CI
#'
#' @return
#' @export
#'
#' @examples
addval = function(val,runfun,dat=NULL,N=1,minrun=TRUE,alpha=.05,CI=.95) {
  # Performs N runs for the datapoints val.
  # min run =TRUE: If there is no variance, performs until there is.
  # If dat is not given, creates the dat object.

  # N: Number of runs

  # dat sollte eine liste sein mit der condition als erstes element, den p-values, power, anzahl runs, observed und expected var.

  # Initialize dat if it is not given
  if (is.null(dat)) {
    dat = list()
  }

  for (i in 1:length(val)) {
    x = val[[i]]
    # Get index of x in dat
    a = sapply(dat,function(y) digest(as.numeric(y$x)))
    ind = which(a==digest(x))

    # initialize new entry in dat if x is not in there yet
    if (length(ind) == 0) {
      dat = list.append(dat,list(x=x,p=c()))
      ind = length(dat)
    }

    # add values to p
    p = dat[[ind]]$p
    # if(i==2) {browser()}
    p =  c(p,sapply(1:N, function(n) runfun(x))) # generate new values

    if (minrun)  {
      repeat{
        if (length(p)>1 && var(as.numeric(p>alpha))>0) {break}
        p=c(p ,runfun(x)) # generate new values
      }
    }
    dat[[ind]]$p  = p

    # Update the other values of the entry (power,h,var,CI)
    dat[[ind]] = updatestats(dat[[ind]],alpha=alpha,CI = CI)
  }

  return(dat)

}

#  addval.multi = function(val,runfun,dat=NULL,N=1,minrun=TRUE,alpha=.05,CI=.95) {
#   # Performs N runs for the datapoints val.
#   # min run =TRUE: If there is no variance, performs until there is.
#   # If dat is not given, creates the dat object.
#
#   # N: Number of runs
#
#   # dat sollte eine liste sein mit der condition als erstes element, den p-values, power, anzahl runs, observed und expected var.
#
#   # Initialize dat if it is not given
#   if (is.null(dat)) {
#     dat = list()
#   }
#
#   for (i in 1:length(val)) {
#     x = val[i]
#     # Get index of x in dat
#     a = sapply(dat,function(y) digest(as.numeric(y$x)))
#     ind = which(a==digest(x))
#
#     # initialize new entry in dat if x is not in there yet
#     if (length(ind) == 0) {
#       dat = list.append(dat,list(x=x,p=c()))
#       ind = length(dat)
#     }
#
#     # add values to p
#     p = dat[[ind]]$p
#     # if(i==2) {browser()}
#     p =  c(p,sapply(1:N, function(n) runfun(x))) # generate new values
#
#     if (minrun)  {
#       repeat{
#         if (length(p)>1 && var(as.numeric(p>alpha))>0) {break}
#         p=c(p ,runfun(x)) # generate new values
#       }
#     }
#     dat[[ind]]$p  = p
#
#     # Update the other values of the entry (power,h,var,CI)
#     dat[[ind]] = updatestats(dat[[ind]],alpha=alpha,CI = CI)
#   }
#
#   return(dat)
#
# }

addval.multi = function(dat,x,runfun,N=1,minrun=FALSE,alpha=.05) {
  # N: Number of runs
  # minrun = run until there is variance.

  names(x) = c("n","k")
  if (!minrun)  {

    p =  sapply(1:N, function(n) runfun(x)) # generate new value

  } else if (minrun) {
    p = sapply(1:10, function(n) runfun(x))
    while (var(as.numeric(p>alpha))==0) {
      p=c(p,sapply(1:N, function(n) runfun(x)))
    }

  }
  # point is new? index?
  a = sapply(dat,function(x) digest(x$x))
  ind = which(a==digest(x))

  if (length(ind) == 0) {
    dat = list.append(dat,list(x=x,p=p))
    ind = length(dat)
  } else {
    dat[[ind]]$p = append(dat[[ind]]$p,p)
  }

  dat[[ind]] = updatestats(dat[[ind]])

  return(dat)
}





# init = function(runfun,n) {
#   # initialize dataframe with given sample sizes
#   re = data.frame(
#     n = n,
#     p = sapply(n,function(n) runfun(n))
#   )
#   return(re)
# }

# addval = function(runfun,data,n) {
#   # add datapoint
#   re =  rbind(data,c(n,runfun(n)))
#   return(re)
# }

# p_to_quant = function(dat,power) {
#
#   ns = sort(unique(dat$n))
#
#   quants = c()
#   weight = c()
#   for (i in ns) {
#     quants = c(quants,as.numeric(quantile(dat$p[dat$n==i],probs=power)))
#     weight = c(weight,sum(dat$n==i))
#   }
#
#   re = data.frame(
#     n=ns,
#     quantiles = quants,
#     weight = weight
#   )
#   return(re)
# }





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
svm.pred2 = function(dat,aggregate=FALSE, weight.type="inv_sd",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)1,tune=FALSE,mod=F,...) {

  datx = todataframe3(dat,aggregate=aggregate,pseudo=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)

  # Determine if first round
  firstround = is.null(last.out$pars_history)

  if (firstround) {
    pars_history = list()
    pars = list(cost=100,epsilon=.1,gamma=.05)
  }

  if (!firstround) {
    pars_history = last.out$pars_history
    pars = pars_history[[length(pars_history)]]
  }

  if (mod) {
    # pars$epsilon = max(getweight(dat,"sd"))
    # tune=TRUE
    datx = todataframe3(dat,aggregate=aggregate,pseudo=FALSE)
    mod = svm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,probability=T)

  }

  if (tune) {
    tune.seq = seq(.9,1.1,.1)
    tunepars = list(cost=tune.seq*pars$cost,epsilon=tune.seq*pars$epsilon,gamma=tune.seq*pars$gamma)
    a = tune_wsvm(y ~ .,data = datx,weight=weight,ranges=tunepars)
    pars= a$best.parameters
    mod = a$best.model
  }

  if (!tune & !is.null(weight)) {
    mod = wsvm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,weight=weight)
  }

  if (!tune & is.null(weight)) {
    mod = svm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma)
  }

  # Save used parameters
  pars_history = list.append(pars_history,as.data.frame(pars))

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,pars_history = pars_history,output=output,fun=predfun)
  return(re)
}



get.pred =function(xvars,predfun,goal,cost,lastpred,bounds=FALSE,pso=FALSE,retry=FALSE,multi=FALSE,predfun.sd=NULL,type="add") {
  # DIRECT SEARCH

  a = timer()
  # retry is to avoid local minima
  # multi is to choose between multiple candidates
  if (is.null(lastpred)) lastpred = apply(xvars,2,mean)

  # fn = function(x) (predfun(x)-goal)^2*10^20+cost(x)*predfun.sd(x)^(1/2) / (cost(lastpred)*predfun.sd(lastpred))
  # fn = function(x) (predfun(x)-goal)^2*10^20+cost(x)/cost(lastpred)+predfun.sd(x)/predfun.sd(lastpred)*100
  # fn = function(x) (predfun(x)-goal)^2*10^10+cost(x)/cost(lastpred)
  if (type == "add") fn = function(x) abs(predfun(x)-goal)^(2)*10^5+cost(x)/cost(lastpred)
  if (type == "mult") fn = function(x) abs(predfun(x)-goal)^(2)*cost(x)/cost(lastpred)


  datamins = apply(xvars,2,min)
  datamaxs = apply(xvars,2,max)

  boundmins = apply(xvars,2,function(x) max(2,min(x)*.75))
  boundmaxs = apply(xvars,2,max)*1.25

  boundmins = apply(xvars,2,function(x) max(2,min(x)*.0001))
  boundmaxs = apply(xvars,2,max)*3

  bounds =F
  if(bounds) {
    boundmins=datamins
    boundmaxs=datamaxs
  }
  # new.n = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=boundmaxs)$par
  # fn(new.n)

  lastpred = apply(xvars,2,function(x) max(2,min(x)*.0001))
  # new.n =  optim(par=lastpred,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par

  # new.n = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=boundmaxs,control=list(maxit=100))$par

  library(optimr)
  a =  multistart(parmat=xvars,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1,trace=0))
  new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
  # new.n =  optim(par=lastpred,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs)$par
  # if (new.n>200) browser()
  # browser()
  # if (new.n>115) browser()
  # a = data.frame(x =1:300,y=sapply(1:300,predfun))
  # plot(1:300,sapply(1:300,predfun))
  # plot(1:(2*max(xvars)),sapply(1:(2*max(xvars)),fn))
  # plot(1:(2*max(xvars)),sapply(1:(2*max(xvars)),predfun))
  # if (new.n>200) browser()

  toofar = abs(predfun(new.n)-goal)>.01

  trys = 10
  while (toofar & trys>0) {
    trys = trys-1
    startpar=c()
    for (j in 1:length(datamins)) {
      startpar[j] = sample(datamins[j]:datamaxs[j],1)
    }
    new.n =  optim(par=startpar,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
    toofar = abs(predfun(new.n)-goal)>.01
  }

  # new.n =  optim(par=100,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par

  # toofar = abs(predfun(new.n)-goal)>.01
  if (toofar) {
    print("psoptim necessary")
    # browser()
    new.n = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=boundmaxs,control=list(maxit=200))$par
    # diag = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=boundmaxs,control=list(maxit=100,vectorize=T))
    # diag = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=datamaxs,control=list(maxit=100,vectorize=T))

  }
  # print(timer(a))
  return(new.n)
}


# get.pred =function(xvars,predfun,goal,cost,lastpred,bounds=FALSE,pso=FALSE,retry=FALSE,multi=FALSE,predfun.sd=NULL) {
#   # DIRECT SEARCH
#
#   # retry is to avoid local minima
#   # multi is to choose between multiple candidates
#   if (is.null(lastpred)) lastpred = apply(xvars,2,mean)
#
#   # fn = function(x) abs(predfun(x)-goal)*predfun.sd(x)/predfun.sd(lastpred) * cost(x)/cost(lastpred)
#   # fn = function(x) abs(predfun(x)-goal)^(1/2)*predfun.sd(x)/predfun.sd(lastpred) * cost(x)/cost(lastpred)
#   fn = function(x) abs(predfun(x)-goal)^(2)*predfun.sd(x)/predfun.sd(lastpred) * (cost(x)/cost(lastpred))^1
#
#
#   datamins = apply(xvars,2,min)
#   datamaxs = apply(xvars,2,max)
#
#   boundmins = apply(xvars,2,function(x) max(2,min(x)/2))
#   boundmaxs = apply(xvars,2,max)*2
#
#   if(bounds) {
#     boundmins=datamins
#     boundmaxs=datamaxs
#   }
#   # new.n = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=boundmaxs)$par
#   # fn(new.n)
#   new.n =  optim(par=lastpred,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
#   # new.n =  optim(par=lastpred,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs)$par
#
#   toofar = abs(predfun(new.n)-goal)>.01
#
#   trys = 3
#   while (toofar & trys>0) {
#     trys = trys-1
#     startpar=c()
#     for (j in 1:length(datamins)) {
#       startpar[j] = sample(datamins[j]:datamaxs[j],1)
#     }
#     new.n =  optim(par=startpar,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
#     toofar = abs(predfun(new.n)-goal)>.01
#   }
#
#   # new.n =  optim(par=100,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
#
#   # toofar = abs(predfun(new.n)-goal)>.01
#   if (toofar) {
#     print("psoptim necessary")
#     new.n = pso::psoptim(par=lastpred,fn=fn,lower=boundmins,upper=boundmaxs)$par
# }
#
#   return(new.n)
# }


get.pred2 =function(xvars,predfun,goal,cost,lastpred,bounds=FALSE,pso=FALSE,retry=FALSE,multi=FALSE,predfun.sd=NULL) {
  # BEST CANDIDATE

  # retry is to avoid local minima
  # multi is to choose between multiple candidates

  CFACTOR = 1
  # if (is.null(lastpred)) lastpred = apply(xvars,2,mean)
  # cost_normed = function(x) cost(x) / cost(lastpred) / CFACTOR

  # fn = function(x) abs(predfun(x)-goal)*predfun.sd(x)+cost_normed(x)
  fn = function(x) abs(predfun(x)-goal)*predfun.sd(x)

  datamins = apply(xvars,2,min)
  datamaxs = apply(xvars,2,max)

  boundmins = apply(xvars,2,function(x) max(2,min(x)/2))
  boundmaxs = apply(xvars,2,max)*2

  if(bounds) {
    boundmins=datamins
    boundmaxs=datamaxs
  }

  candidate = get.candidate(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar=lastpred,pso,retry)
  if(!multi) new.n = candidate

  if(multi) {
    candidates2 = lapply(list(datamins,datamaxs),function(x) get.candidate(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar=x,pso,retry))
    candidates3=c(list(candidate),candidates2)
    if (any(apply(do.call(rbind,candidates3),2,var)>1)) {
      candidates7 = lapply(1:7,function(x) get.candidate(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar=NULL,pso,retry))
      candidates10 = c(candidates3,candidates7)
      if (!is.null(predfun.sd)) sds = sapply(candidates10,predfun.sd)
      costs = sapply(candidates10,cost)
      rel_sds = sds / min(sds)
      rel_costs = costs / min(costs)
      crit = rel_sds*(rel_costs^CFACTOR)
      new.n = candidates10[[which(crit==min(crit))[1]]]
    }
    else new.n = candidate
  }

  # print(timer(a))

  return(new.n)
}


# get.pred =function(xvars,predfun,goal,cost,lastpred,bounds=FALSE,pso=FALSE,retry=FALSE,multi=FALSE,predfun.sd=NULL) {
#   # retry is to avoid local minima
#   # multi is to choose between multiple candidates
#
#   # print("Stuck in get pred")
#
#   # a = timer()
#
#   CFACTOR = 50
#   if (is.null(lastpred)) lastpred = apply(xvars,2,mean)
#   cost_normed = function(x) cost(x) / cost(lastpred) / CFACTOR
#
#   fn = function(x) abs(predfun(x)-goal)*predfun.sd(x)+cost_normed(x)
#
#   datamins = apply(xvars,2,min)
#   datamaxs = apply(xvars,2,max)
#
#   boundmins = apply(xvars,2,function(x) max(2,min(x)/2))
#   boundmaxs = apply(xvars,2,max)*2
#
#   if(bounds) {
#     boundmins=datamins
#     boundmaxs=datamaxs
#   }
#
#   candidate = get.candidate(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar=lastpred,pso,retry)
#   if(!multi) new.n = candidate
#
#   if(multi) {
#    candidates2 = lapply(list(datamins,datamaxs),function(x) get.candidate(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar=x,pso,retry))
#    candidates3=c(list(candidate),candidates2)
#     if (any(apply(do.call(rbind,candidates3),2,var)>1)) {
#       candidates7 = lapply(1:7,function(x) get.candidate(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar=NULL,pso,retry))
#      candidates10 = c(candidates3,candidates7)
#      if (!is.null(predfun.sd)) sds = sapply(candidates10,predfun.sd)
#     new.n = candidates10[[which(sds==min(sds))[1]]]
#     }
#    else new.n = candidate
#   }
#
#   # print(timer(a))
#
#   return(new.n)
# }


get.candidate = function(fn,predfun,goal,datamins,datamaxs,boundmins,boundmaxs,startpar,pso=FALSE,retry=TRUE) {
  if (is.null(startpar)) {
    startpar=c()
    for (j in 1:length(datamins)) {
      startpar[j] = sample(datamins[j]:datamaxs[j],1)
    }
  }

  if(pso)  new.n = pso::psoptim(par=startpar,fn=fn,lower=boundmins,upper=boundmaxs)$par

  if(!pso) {
    new.n =  optim(par=startpar,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
    # new.n =  optim(par=startpar,fn=fn,method="L-BFGS-B",lower=datamins,upper=datamaxs)$par
  }

  isclose = abs(predfun(new.n)-goal)<.025
  if (!pso & retry & !isclose) {
    i = 0
    repeat{
      i = i+1
      startpar = c()
      for (j in 1:length(datamins)) {
        startpar[j] = sample(datamins[j]:datamaxs[j],1)
      }
      new.n =  optim(par=startpar,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
      isclose = abs(predfun(new.n)-goal)<.025
      if (isclose || i>3) break
    }
  }

  return(new.n)
}




# re1 =  ss.find(reg.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,setsize=setsize,n.startsets=n.startsets,seed=seed,cost=cost)
# re2 =  ss.find(logi.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,setsize=setsize,n.startsets=n.startsets,seed=seed,cost=cost)
# re3 =  ss.find(svm.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,setsize=setsize,n.startsets=n.startsets,seed=seed,cost=cost)
# re4 =  ss.find(gauss.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,setsize=setsize,n.startsets=n.startsets,seed=seed,cost=cost)




# setsize= 30
# n.startsets= 10
# setsize= 50
# n.startsets= 4



if (F) {
  #### PLOT
  rft = runfun.wilson2.true()
  load(file= paste0(folder,"at_4.Rdata"))
  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  xvalues = seq(min(xvars[,1]),max(xvars[,1]),2)

  optpower.x = xvalues
  optpower.y = sapply(optpower.x,function(x) {
    fn = function(y) abs(rft(c(x,y))-.8)
    optim(10,fn,method="Brent",lower=5,upper=40)$par})
  optpower = data.frame(X1=optpower.x,X2=optpower.y)

  optcost.x = xvalues
  optcost.y = sapply(optcost.x,function(x) {
    fn = function(y) abs(cost(c(x,y))-cost(actually_true))
    optim(10,fn,method="Brent",lower=5,upper=40)$par})
  optcost = data.frame(X1=optcost.x,X2=optcost.y)

  modpower.x = xvalues
  modpower.y = sapply(modpower.x,function(x) {
    fn = function(y) abs(pred$fun(c(x,y))-.8)
    optim(10,fn,method="Brent",lower=5,upper=40)$par})
  modpower = data.frame(X1=modpower.x,X2=modpower.y)

  modcost.x = xvalues
  modcost.y = sapply(modcost.x,function(x) {
    fn = function(y) abs(cost(c(x,y))-cost(pred$new.n))
    optim(10,fn,method="Brent",lower=5,upper=40)$par})
  modcost = data.frame(X1=modcost.x,X2=modcost.y)

  a = ggplot(optcost, aes(x=X1, y=X2)) + geom_line() + geom_line(data=optpower,aes(x=X1, y=X2,col="power=.8"))+ geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost")) +
    geom_line(data=modpower,aes(x=X1, y=X2,col="predicted power")) +
    geom_line(data=modcost,aes(x=X1, y=X2,col="predicted cost"))
  print(a)
  #### END PLOT
}




#' Title
#'
#' @param n
#' @param delta
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
runfun.skewed = function(n, delta =.2, alpha =4) {

  runfun= function(n) {

    a1 = skeweddist(n,alpha = alpha)
    a2 = skeweddist(n,alpha = -alpha)+delta
    re = t.test(a1,a2,var.equal=TRUE)$p.value
    return(re)
  }
  return(runfun)
}


# T-TEST runfun
#' Title
#'
#' @param delta
#'
#' @return
#' @export
#'
#' @examples
runfun.ttest2 = function(delta = .2,alpha=.05) {

  runfun = function(n) {
    # fetch a p-value at the specified sample size
    a = rnorm(n,delta)
    re = t.test(a)$p.value<alpha
    return(as.numeric(re))
  }
  re = runfun
  return(re)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
runfun.wilson2 = function() {
  # From wilson Example 4
  # x is a 2 dimensional specification here. (n,clusters)
  h <- c(mu = 0.3, var_t = 1, rho = 0.05) # Hardcoded Hypothesis
  var_t <- h[[2]]; rho <- h[[3]]

  runfun = function(x) {

    n <- x[1]; k <- x[2]; m <- n/k
    sig_c <- sqrt(var_t*rho + var_t/m - var_t*rho/m)
    pow <- power.t.test(n = k, delta = h[[1]], sd = sig_c)$power

    return(runif(1)<pow)
  }
  return(runfun)
}





#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
runfun.wilson2.true = function() {
  # From wilson Example 4
  # x is a 2 dimensional specification here. (n,clusters)
  h <- c(mu = 0.3, var_t = 1, rho = 0.05) # Hardcoded Hypothesis
  var_t <- h[[2]]; rho <- h[[3]]

  runfun = function(x) {

    n <- x[1]; k <- x[2]; m <- n/k
    sig_c <- sqrt(var_t*rho + var_t/m - var_t*rho/m)
    pow <- power.t.test(n = k, delta = h[[1]], sd = sig_c)$power

    return(pow)
  }
  return(runfun)
}


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
svm.pred = function(dat,aggregate=TRUE, weight.type="freq",goal=.8,last.out=NULL,fun=FALSE,cost=function(x)sum(x),tune=TRUE,epsil=FALSE,...) {

  # datx = todataframe(dat,aggregate=TRUE)
  # xvars = datx[,1:(length(datx)-1),drop=FALSE]
  # weight = getweight(dat,weight.type="freq")

  datx = todataframe(dat,aggregate=TRUE,pseudo=FALSE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  if(!aggregate) weight.type=NULL
  weight = getweight(dat,weight.type)

  #Treat the case of zero variance in the data - add some random noise
  if(var(datx[,2])==0) datx[,2] = datx[,2] + rnorm(length(datx[,2]),sd=.00001)

  #Setup Low Anchor
  anchor = c(rep(0,nrow(xvars)),0)
  datx = rbind(datx,anchor)
  weight = c(weight,max(weight))
  xvars = datx[,1:(length(datx)-1),drop=FALSE]

  # Setup High Anchor
  anchor = c(apply(xvars,2,max)*2,1)
  datx = rbind(datx,anchor)
  weight = c(weight,max(weight))
  xvars = datx[,1:(length(datx)-1),drop=FALSE]

  # Determine if first round
  firstround = is.null(last.out$pars_history)

  if (firstround) {
    pars_history = list()
    pars = list(cost=100,epsilon=.1,gamma=.05) # Default Pars
    tunepars = list(cost=10^seq(-1,2),gamma=10^seq(-4,1),epsilon=min(getweight(dat,"sd")))
    if(epsil) tunepars$epsilon = 10^seq(-4,-1)
  }

  if (!firstround) {
    pars_history = last.out$pars_history
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

  if (tune & aggregate) {
    tunepars$epsilon = min(getweight(dat,"sd"))
    ctrl = tune.control(cross=min(10,nrow(datx)))
    a = tune_wsvm(y ~ .,data = datx,weight=weight,ranges=tunepars,tunecontrol=ctrl)
    pars= a$best.parameters
    mod = a$best.model
  }

  if (!tune & aggregate) {
    mod = wsvm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,weight=weight)
  }

  if (tune & !aggregate) {
    a = tune.svm(y ~ .,data = datx,cost=tunepars$cost,epsilon=tunepars$epsilon,gamma=tunepars$gamma)
    pars= a$best.parameters
    mod = a$best.model
  }

  if (!tune & !aggregate) {
    mod = svm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma)
  }

  # Save used parameters
  pars_history = list.append(pars_history,as.data.frame(pars))

  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}

  # freqs = getweight(dat,"freq")
  # predfun.sd = function(x) sum(colMeans((x - xvars)^2*freqs)) # pseudo SD fun

  difs = apply(xvars,1,function(x) predfun(x)-goal)
  isdumb = !any(difs>0) | !any(difs<0)   # Check if it predicts all values to be on one side of the plane
  if (isdumb) {warning("Weird Function")}

  somevals = apply(xvars,1,function(x) predfun(x))
  isplane = summary(lm(y~.,cbind(xvars,y = somevals)))$r.squared>.999 #
  if (isplane) {warning("Plane predicted")}
  # generate prediction
  new.n = get.pred(xvars,predfun,goal,cost,lastpred=last.out$new.n,Ntry=20,design=design)

  #  generate output (closest value and SD)
  output = get.closest(dat,new.n)

  re=list(new.n = new.n,pars_history = pars_history,output=output,fun=predfun)
  return(re)
}





# Bisection Search: Jung 2008--------------------------------------------------


#' Title
#'
#' @param dat
#' @param alpha
#' @param goal
#' @param bounds
#' @param round_res
#'
#' @return
#' @export
#'
#' @examples
bisection.pred = function(dat,alpha =.05,goal=.8,bounds,round_res=TRUE,last.out=NULL,...) {
  # generate a prediction for the sample size at the specified power
  # uses loess regression

  datx = todataframe(dat)


  if (is.null(last.out)) {
    lower = min(datx$n[(length(datx$n)-2):length(datx$n)])
    upper = max(datx$n[(length(datx$n)-2):length(datx$n)])
    mid = median(datx$n[(length(datx$n)-2):length(datx$n)])
  } else {
    lower = last.out$lower
    upper = last.out$upper
    mid = last.out$mid
  }
  # determine new upper and lower
  mid_p = datx[datx$n==mid,"power"]
  lower_p = datx[datx$n==lower,"power"]

  # w = (lower_p - goal) * (mid_p - goal) < 0
  # if(length(w)==0) browser()


  if((lower_p-goal)*(mid_p-goal)<0) {
    upper = mid
  } else {
    lower = mid
  }

  #estimate new middle
  new.n = (upper+lower)/2
  mid = round(new.n)

  if (round_res) {new.n = round(new.n)}

  re=list(new.n = new.n,lower=lower,upper=upper,mid = mid)

  return(re)
}


#' Title
#'
#' @param runfun
#' @param goal
#' @param goal.width
#' @param alpha
#' @param guess
#' @param bounds
#' @param CI
#' @param budget
#'
#' @return
#' @export
#'
#' @examples
bisection.find = function(runfun,goal,goal.width,alpha,guess,bounds,CI=.95,budget = 10^5) {

  lower = guess[1]
  upper = guess[2]

  # first run
  dat = addval(val=c(lower),runfun=runfun,N=100,alpha=.05,CI=CI)

  # update runs
  runs = budget - usedruns(dat)

  #estimate middle
  mid = round((upper+lower)/2)

  while (runs>0)  {

    # add to data
    dat = addval(val=mid,runfun=runfun,dat=dat,N=100,alpha=.05,CI=CI)

    # update runs
    runs = budget - usedruns(dat)

    # determine new upper and lower
    datx = todataframe(dat)
    mid_p = datx[datx$n==mid,"power"]
    lower_p = datx[datx$n==lower,"power"]

    if((lower_p-goal)*(mid_p-goal)<0) {
      upper = mid
    } else {
      lower = mid
    }

    #estimate new middle
    mid = round((upper+lower)/2)

  }

  re = list(mid,dat)
  return(re)
}



bisection = function(runfun,runs=10000,initdata,guess_lower=NULL,guess_upper=NULL,power=.8,alpha=.05) {
  # Implement Bisection Procedure in Jun 2008

  upper = guess_upper
  lower = guess_lower


  upper_p = mean(initdata[initdata$n==upper,2]<alpha)
  lower_p = mean(initdata[initdata$n==lower,2]<alpha)

  while (runs>0) {
    # print(lower);print(upper)
    mid = round((upper+lower)/2)
    mid_res =  sapply(1:1000,function(x) runfun(mid)<alpha)
    mid_p =  mean(mid_res)
    runs = runs - 1000

    if ((lower_p-power)*(mid_p-power)>0 & (upper_p-power)*(mid_p-power)>0) {
      print("perhaps wrong")
      # mid_res = c(mid_res,sapply(1:1000,function(x) runfun(mid)<alpha))
      # mid_p =  mean(mid_res)
      # runs = runs - 1000
    }

    if((lower_p-power)*(mid_p-power)<0) {
      upper = mid
      upper_p = mid_p
    } else {
      lower = mid
      lower_p = mid_p
    }
  }

  re = (upper+lower)/2
  return(re)
}


# Functions to generate a prediciton of the required sample size.
# Taking the data, alpha and power (+hyperparameters)





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
#' @param setsize
#' @param learner
#' @param startset.size
#' @param startbudget
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ss.find = function(learner,runfun,design,goal=.8,goal.ci=NULL,CI=.95,budget = NULL,setsize=if (!is.null(budget)) round(budget*.1/n.startsets)else 50,startset.size=setsize,n.startsets=4*length(design),seed=1,dat=NULL,dat.only=FALSE,...) {

  time_temp = timer()

  set.seed(seed)

  if(is.null(dat)) {
    dat = addval(runfun,design=design,n.points=n.startsets,each=setsize,seed=seed)
  }
  if(dat.only) return (dat)

  pred=NULL

  repeat{
    # generate prediction
    pred = tryCatch(learner(dat = dat,goal=goal,carryover = pred$carryover,design=design,...),error=function(x) {
      print(x)
      return(x)
    })
    # if(is.null(pred$new.n)) {
    #   # list(value=NA,value.sd = NA,data = NA,budget=usedruns(dat),fun=NA,fun.sd =NA)
    #   stop("no predicted value")
    #   # return(pred)
    # }
    if(is.null(pred$new.n)) stop("no predicted value")

    # check CI
    if(!is.null(goal.ci)) {

      # Get SD from GP if not yet there
      if(is.na(pred$new.n.sd)){
        pred2 = tryCatch(gauss.pred(dat = dat,goal=goal,carryover = pred$carryover,design=design,...),error=function(x) {return(x)})
        # if(is.null(pred$new.n))return(pred) # dead code? wird schon darüber gecheckt
        pred$new.n.sd = pred2$fun.sd(pred$new.n)
      }

      if(pred$new.n.sd*qnorm(CI+(1-CI)/2)<goal.ci) break
    }

    # check budget
    used = usedruns(dat)
    if (used>15000) {warning("didn't converge?");break}
    if (!is.null(budget)){
      budget.remaining = budget - used
      if(budget.remaining<=0) break
    }

    # print(pred$new.n)
    # rft = runfun.wilson2.true()
    # a1 = pred$fun(pred$new.n) # predicted power at location
    # a2 = rft(pred$new.n) # true power at location
    # a3 = pred$fun(round(pred$new.n))# predicted power at rounded
    # a4 = rft(round(pred$new.n)) # true power at rounded
    # print(round(c(a1,a2,a3,a4),4))

    #add value to the dataset / random if toofar away
    toofar = abs(pred$new.n.y-goal)>.01
    if (toofar) {
      # a = sapply(design,function(x) sample(x[1]:x[2],1))
      # points = data.frame(t(round(a)))
      # # print(points)
      #
      datx = todataframe(dat)
      xvars = datx[,1:(length(datx)-1),drop=FALSE]
      points = xvars
      dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=round(setsize/nrow(xvars)))

    } else {
      points = data.frame(t(round(pred$new.n)))
      dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=setsize)
    }
  }

  # Get SD from GP if not yet there
  if(is.na(pred$new.n.sd)){
    pred2 = tryCatch(gauss.pred(dat = dat,goal=goal,carryover = pred$carryover,design=design,...),error=function(x) {return(x)})
    # if(is.null(pred$new.n))return(pred)
    if(is.null(pred$new.n))stop("no predicted value")
    pred$new.n.sd = pred2$fun.sd(pred$new.n)
  }

  # Stop the clock
  time_used = timer(time_temp,detailled=T)


  re = list(value=pred$new.n,value.sd = pred$new.n.sd,data = dat,budget=usedruns(dat),fun=pred$fun,fun.sd = pred$fun.sd,time_used=time_used)

  return(re)
}



get.pred =function(xvars,predfun,goal,cost,lastpred,Ntry=20,design=NULL) {

  if (!is.null(design)) {
    lastpred = sapply(design,mean)
  }

  if (is.null(design)) {
    design = list()
    for (j in 1:ncol(xvars)) {
      x= xvars[,j]
      design[[j]] = c(min(x),max(x))
    }
    lastpred = apply(xvars,2,mean)
  }

  #using half of the Ntrys for random values!
  # parmat_last = xvars[nrow(xvars) + 1 - seq(1,min(Ntry,10,nrow(xvars))),,drop=F]
  parmat_last = xvars[nrow(xvars) + 1 - seq(1,min(round(Ntry/2),nrow(xvars))),,drop=F]
  parmat_rand = initpoints(design,max(Ntry-nrow(parmat_last),0),random=TRUE)
  parmat_rand = as.data.frame(parmat_rand)
  names(parmat_rand) = names(parmat_last)

  # if (nrow(xvars)>10) browser()
  # browser()


  parmat = rbind(parmat_last,parmat_rand)
  # print(parmat_last)
  # print("...")
  # parmat = xvars

  # if (nrow(parmat)>Ntry) parmat = parmat[sample(1:nrow(parmat),Ntry),,drop=F]

  # if (nrow(parmat)>30) browser()
  #
  # design = list(n = c(300,1200),k=c(5,30))
  # parmat = initpoints(design,20,2)

  fn = function(x) (predfun(x)-goal)^2*10^5+cost(x)/cost(lastpred)

  boundmins = apply(xvars,2,function(x) max(2,min(x)*.95))
  boundmaxs = apply(xvars,2,max)*1.05

  if (Ntry ==1) new.n = optim(par=lastpred,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par

  if (Ntry >1) {
    a = hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
    new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
    new.n = as.numeric(new.n)
  }
  toofar = abs(predfun(new.n)-goal)>.01
  if (toofar) {warning("No good value found")}
  return(new.n)
}





#' #' Title
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' load.cond = function(fun_nr,task,budget) {
#'
#'   goal = .8
#'   analytical = NA
#'   true_power.fun = NULL
#'   cost=function(x)x #default cost function
#'
#'   if(fun_nr==1) { # T-test 1D
#'     runfun = runfun.ttest(delta = .4)
#'     design = list(n = c(50,200))
#'     true_power.fun = runfun.ttest.true(delta=.4)
#'   }
#'
#'   if(fun_nr==2) { # T-test 2D
#'     runfun = runfun.wilson2()
#'     design = list(n = c(300,1200),k=c(5,30))
#'     cost = function(x) x[1]*5+x[2]*100
#'     true_power.fun = runfun.wilson2.true()
#'   }
#'
#'   if(fun_nr==3) { # T-test Skewed Normal
#'     runfun = runfun.skewed2(delta = .4,alpha =10)
#'     design = list(n = c(50,200))
#'     analytical = power.t.test(delta = .4, sig.level = .05, power = goal,type = "two.sample", alternative = "two.sided")$n
#'   }
#'
#'   if(fun_nr==4) { # IRT
#'     itempars = runfun.irt.itempars(delta=.1,n.items = 20,seed=1)
#'     runfun = runfun.irt(itempars)
#'     design = list(n = c(1000,1500))
#'     # hyp <- setup_hypothesis(type = "1PLvs2PL", altpars = itempars)
#'     # ncps <- calculate_ncps(hyp=hyp,sampling=TRUE,sampling.npers = 10^5,approx.npers=10^5)
#'     # analytical = ssize(hyp=hyp,ncp=ncps["LR"],alpha=.05,power=goal)
#'     analytical = 1336
#'   }
#'
#'   if(fun_nr==5) { # Mixed Models
#'     runfun = runfun.simr()
#'     design = list(n = c(10,20))
#'     # analytical is not available here
#'   }
#'
#'   # Set Budget
#'   if(fun_nr!=2 & budget == "low") budget = 1000
#'   if(fun_nr!=2 & budget == "mid") budget = 2000
#'   if(fun_nr!=2 & budget == "high") budget = 4000
#'   if(fun_nr==2 & budget == "low") budget = 2000
#'   if(fun_nr==2 & budget == "mid") budget = 4000
#'   if(fun_nr==2 & budget == "high") budget = 8000
#'
#'
#'   if(task=="A") {
#'     goal.ci = NULL
#'   }
#'
#'   if(task=="B") {
#'     budget = NULL
#'     goal.ci = .025
#'   }
#'
#'
#'   re = list(fun_nr=fun_nr,task=task,runfun=runfun,design=design,cost=cost,budget=budget,goal.ci=goal.ci,analytical=analytical,goal=goal,true_power.fun= true_power.fun)
#'   return(re)
#' }




get.pred =function(xvars,predfun,goal,cost,Ntry=20,design,fixed_cost) {

  temp = design
  design = NULL
  if (is.null(design)) {
    design = list()
    for (i in 1:ncol(xvars)){
      design[[i]] = c(min(xvars[,i]),max(xvars[,i]))
    }
  }

  midpars = sapply(design,mean)

  #using half of the Ntrys for random values!
  parmat_last = xvars[nrow(xvars) + 1 - seq(1,min(round(Ntry/2),nrow(xvars))),,drop=F]
  parmat_rand = initpoints(design,max(Ntry-nrow(parmat_last),0),random=TRUE)
  parmat_rand = as.data.frame(parmat_rand)
  names(parmat_rand) = names(parmat_last)

  parmat = rbind(parmat_last,parmat_rand)

  # Acquisition Function
  fn = function(x) (predfun(x)-goal)^2*10^5+cost(x)/cost(midpars)

  boundmins = apply(xvars,2,function(x) max(2,min(x)*.95))
  boundmaxs = apply(xvars,2,max)*1.05

  if (Ntry ==1) new.n = optim(par=midpars,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par

  if (Ntry >1) {
    a = hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
    new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
    exact = as.numeric(new.n)
  }

  # Check if a reasonable value has been found
  toofar = abs(predfun(exact)-goal)>.01
  if (toofar) {warning("No good value found")}

  # Serch candidate values for best match
  cands = expand.grid(data.frame(rbind(floor(exact),ceiling(exact))))
  cands = unique(cands)
  aq_vals = apply(cands,1,fn)
  pw_vals = apply(cands,1,function(x) (predfun(x)-goal))
  acceptable = which(pw_vals>0)
  if (length(acceptable)==0) {
    warning("weird local maximum")
    acceptable = 1:length(pw_vals)
  }
  ind = which(aq_vals[acceptable] == min(aq_vals[acceptable]))
  new.n = as.numeric(cands[acceptable[ind],])

  re = list(new.n=new.n, exact=exact,toofar=toofar)

  return(re)
}



# model1 <- lme4::glmer(z ~ x + (1|g), family="poisson", data=simr::simdata)
# tmp = lme4::fixef(model1)
# tmp[2] = -.01
# simr::fixef(model1) <- tmp
# x = c(20,10)
# # x = c(20,20)
# # x = c(30,10)
# # x = c(30,20)
# n <- x[1] # Number of Study Years
# k <- x[2] # Number of Clusters
# model2 <- extend(model1, along="x", n=n)
# model2 <- extend(model2, along="g", n=k)
# re = hush(powerSim(model2,nsim=1,progress=F)$pval<.05)
# a = replicate(100,hush(powerSim(model2,nsim=1,progress=F)$pval<.05))
# mean(a)






# Plots Task A ------------------------------------------------------------

# Additional for functions 1 + 2
# A)Zusätzlich für Conditions mit Known power function•Boxplot true power at the output values•Table Mean, SD, MSE true power at output values•for 2dimensionalen T-Test: Höhenlinien-Plot

# Boxplot cost (i.e. sample size for one dimension)•Boxplot Budget (for task B)•Boxplot Power SD (for task
usetask = "A"
resX = resx[resx$task==usetask,]

# Boxplots
p1 = ggplot(resX, aes(x= learner,y=cost,fill=budget)) +  geom_violin(draw_quantiles = 0.5)+ geom_hline(data= resX, aes(yintercept=opt_cost), linetype="dashed", color = "red") + facet_wrap(~ fun_nr,  scales = "free")

p2 = ggplot(resX, aes(x= learner,y=budget_used)) +  geom_boxplot() + facet_wrap(~ fun_nr,scales = "free_x")

p3 = ggplot(resX, aes(x= learner,y=value.sd,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr,scales = "free_x") +  ylim(0, .03)

p_time = ggplot(resX, aes(x= learner,y=time_used,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")

# Plots True Power
# resy = resX[resX$fun_nr%in% c(1:2),]
resy = resX[!is.na(resX$true_power),]

resy$actual_power = NA
resy$actual_power[as.numeric(resy$fun_nr)==1]=0.8036466
resy$actual_power[as.numeric(resy$fun_nr)==2]=0.8002064

# p4 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resy, aes(yintercept=.8), linetype="dashed", color = "red") + facet_wrap(~ fun_nr,scales = "free_x") +  ylim(0.7, .85)

p4 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resy, aes(yintercept=actual_power), linetype="dashed", color = "red") + facet_wrap(~ fun_nr,scales = "free_x") +  ylim(0.7, .85)


# Höhenlinien Plot
res2 = res[sapply(res,function(x) x[[1]]$fun_nr[1]==2)]

resz = lapply(res2, function(x) {

  cond = x[[1]]
  x1 = load.cond(cond$fun_nr,cond$task,cond$budget,goal.ci=NULL)
  # design=x1$design
  costfun=x1$cost

  value = sapply(2:length(x),function(i) x[[i]]$value)
  value = lapply(value,function(x) if(is.null(x))NA else x)
  value = do.call(rbind,value)
  cost = apply(value,1,costfun) |> as.numeric()

  value.sd  = sapply(2:length(x),function(i) x[[i]]$value.sd)
  value.sd = lapply(value.sd,function(x) if(is.null(x))NA else x) |> as.numeric()
  budget = sapply(2:length(x),function(i) x[[i]]$budget)
  budget = lapply(budget,function(x) if(is.null(x))NA else x) |> as.numeric()

  # actually_true = t(apply(value,1,function(x) at[[cond$fun_nr]]))
  opt_value = at[[cond$fun_nr]] |> as.numeric()
  opt_cost = costfun(opt_value)

  true_power.fun = x1$true_power.fun
  true_power = apply(value,1,true_power.fun)

  re = data.frame(value,cost,value.sd,budget,cond,learner,opt_cost,true_power)
  # re$actually_true = at[[cond$fun_nr]]
  return(re)
})

resz = do.call(rbind,resz)
resz = resz[resz$task==usetask,]
resz = resz[which(!is.na(resz$X1)),]
x1 = load.cond(2,usetask,budget="mid")
true_power.fun = x1$true_power.fun
costfun=x1$cost
actually_true = at[[2]]

#general result - scatter plot on map
# add an optimal cost / optimal power line
optpower.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
optpower.y = sapply(optpower.x,function(x) {
  fn = function(y) abs(true_power.fun(c(x,y))-.8)
  optim(10,fn,method="Brent",lower=1,upper=40)$par})
optpower = data.frame(X1=optpower.x,X2=optpower.y)

optcost.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
optcost.y = sapply(optcost.x,function(x) {
  fn = function(y) abs(costfun(c(x,y))-costfun(actually_true))
  optim(10,fn,method="Brent",lower=5,upper=40)$par})
optcost = data.frame(X1=optcost.x,X2=optcost.y)

resz1 = resz[resz$budget.1=="low",]
p5 = ggplot(resz1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=optpower,aes(x=X1, y=X2,col="power=.8"))+ geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost")) +  xlim(200, 600) +  scale_fill_brewer(palette="Spectral",aesthetics = "col")

resz2 = resz[resz$budget.1=="high",]
p6 = ggplot(resz2, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=optpower,aes(x=X1, y=X2,col="power=.8"))+ geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost")) +  xlim(200, 600) +  scale_fill_brewer(palette="Spectral",aesthetics = "col")


# Export plots

pdf(paste0(folder,"plot1_",usetask,".pdf"),height=9,width=16);p1;dev.off()
pdf(paste0(folder,"plot2_",usetask,".pdf"),height=9,width=16);p2;dev.off()
pdf(paste0(folder,"plot3_",usetask,".pdf"),height=9,width=16);p3;dev.off()
pdf(paste0(folder,"plot4_",usetask,".pdf"),height=6,width=11);p4;dev.off()
pdf(paste0(folder,"plot5_",usetask,".pdf"),height=6,width=11);p5;dev.off()
pdf(paste0(folder,"plot6_",usetask,".pdf"),height=6,width=11);p6;dev.off()
pdf(paste0(folder,"plot_time_",usetask,".pdf"),height=6,width=11);p_time;dev.off()



# task = c("A")
# budget = c("low","mid","high")
# goal.ci = "NULL"
# sim1 = expand.grid(run=1:n.runs,fun_nr=fun_nr,task=task,budget = budget,goal.ci = goal.ci)
# sim1 = sim1  %>% split(., seq(nrow(.))) %>% lapply(.,as.list)





# Adding true power where available


# Preparation for Höhenlinien plot

res2 = res[sapply(res,function(x) x[[1]]$fun_nr[1]==2)]

resz = lapply(res2, function(x) {

  cond = x[[1]]
  x1 = load.cond(cond$fun_nr,cond$task,cond$budget,goal.ci=NULL)
  # design=x1$design
  costfun=x1$cost

  value = sapply(2:length(x),function(i) x[[i]]$value)
  value = lapply(value,function(x) if(is.null(x))NA else x)
  value = do.call(rbind,value)
  cost = apply(value,1,costfun) |> as.numeric()

  value.sd  = sapply(2:length(x),function(i) x[[i]]$value.sd)
  value.sd = lapply(value.sd,function(x) if(is.null(x))NA else x) |> as.numeric()
  budget = sapply(2:length(x),function(i) x[[i]]$budget)
  budget = lapply(budget,function(x) if(is.null(x))NA else x) |> as.numeric()

  # actually_true = t(apply(value,1,function(x) at[[cond$fun_nr]]))
  opt_value = at[[cond$fun_nr]] |> as.numeric()
  opt_cost = costfun(opt_value)

  true_power.fun = x1$true_power.fun
  true_power = apply(value,1,true_power.fun)

  re = data.frame(value,cost,value.sd,budget,cond,learner,opt_cost,true_power)
  # re$actually_true = at[[cond$fun_nr]]
  return(re)
})

resz = do.call(rbind,resz)
resz = resz[resz$task==usetask,]
resz = resz[which(!is.na(resz$X1)),]
x1 = load.cond(2,usetask,budget="mid",goal.ci=NULL)
true_power.fun = x1$true_power.fun
costfun=x1$cost

#general result - scatter plot on map
# add an optimal cost / optimal power line
optpower.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
optpower.y = sapply(optpower.x,function(x) {
  fn = function(y) abs(true_power.fun(c(x,y))-optpow)
  optim(10,fn,method="Brent",lower=1,upper=40)$par})
optpower = data.frame(X1=optpower.x,X2=optpower.y)

optcost.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
optcost.y = sapply(optcost.x,function(x) {
  fn = function(y) abs(costfun(c(x,y))-costfun(actually_true))
  optim(10,fn,method="Brent",lower=5,upper=40)$par})
optcost = data.frame(X1=optcost.x,X2=optcost.y)


eqpower.y = sapply(xvals,function(x) {

  for (y in 1:1000) {
    d = true_power.fun(c(x,y))-actual_power
    if (d>0) break
  }
  return(y)
})

# eqpower.y = sapply(xvals,function(x) {
#
#     for (y in 1:1000) {
#         d = true_power.fun(c(x,y))-actual_power
#         if (d>0) break
#     }
#     return(y)
# })




# Function 6 --------------------------------------------------------------

cost = function(x) x[1]*10+x[2]*5
fixed_cost = 350

# Aufwändige Simulation zur Ermittlung der Power in allen Integer Orten siehe fun6_sim.R
load(file= paste0(folder,"res_fun6sim2.Rdata")) # loads "res"

powers = sapply(res, function(x) x$pow)
costs = sapply(res, function(x) cost(x$val))
vals = lapply(res, function(x) x$val)

# For Task B
cands = powers>.8
optcost = min(costs[cands])
ind = which(costs==optcost&cands)
actually_true = res[[ind]]$val
actual_power = res[[ind]]$pow
actual_cost = cost(actually_true)

# For Task C
cands = costs<=350
optpower = max(powers[cands])
ind = which(powers==optpower&cands)
actually_trueC = res[[ind]]$val
actual_powerC = res[[ind]]$pow
actual_costC = cost(actually_true)

true_power.fun = function() {

  load(file= paste0(folder,"res_fun6sim2.Rdata")) # loads "res"

  powers = sapply(res, function(x) x$pow)
  vals = lapply(res, function(x) x$val)

  fn = function(x) {

    if (is.na(x[1])) return(NA)

    ind = sapply(vals,function(y) all(y==x))
    re = powers[ind]
    return(re)
  }
  return(fn)
}

# rewrite this to fit a GP, then use the prediction as true power.

at6 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,actually_trueC=actually_trueC,actual_powerC=actual_powerC,actual_costC=actual_costC,true_power.fun=true_power.fun)

save(at6,file= paste0(folder,"at_6.Rdata"))



# p2 = ggplot(resX, aes(x= learner,y=budget_used,fill=goal.ci)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")
#
# p3 = ggplot(resX, aes(x= learner,y=value.sd,fill=goal.ci)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")
#
# p_time = ggplot(resX, aes(x= learner,y=time_used,fill=goal.ci)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")

# Plots True Power
# resy = resX[!is.na(resX$true_power),]
#
# resy$actual_power = NA
# resy$actual_power[as.numeric(resy$fun_nr)==1]=0.8036466
# resy$actual_power[as.numeric(resy$fun_nr)==2]=0.8002064
#
# p4 = ggplot(resy, aes(x= learner,y=true_power,fill=goal.ci)) +  geom_boxplot() + geom_hline(data= resy, aes(yintercept=actual_power), linetype="dashed", color = "red") + facet_wrap(~ fun_nr,scales = "free_x") +  ylim(0.75, .85)
#
#
# # Höhenlinien Plot Function 2
# res2 = res[sapply(res,function(x) x[[1]]$fun_nr[1]==2)]
#
# resz = lapply(res2, function(x) {
#
#     cond = x[[1]]
#     x1 = load.cond(cond$fun_nr,cond$task,cond$budget,goal.ci=NULL)
#     # design=x1$design
#     costfun=x1$cost
#
#     value = sapply(2:length(x),function(i) x[[i]]$value)
#     value = lapply(value,function(x) if(is.null(x))NA else x)
#     value = do.call(rbind,value)
#     cost = apply(value,1,costfun) |> as.numeric()
#
#     value.sd  = sapply(2:length(x),function(i) x[[i]]$value.sd)
#     value.sd = lapply(value.sd,function(x) if(is.null(x))NA else x) |> as.numeric()
#     budget = sapply(2:length(x),function(i) x[[i]]$budget)
#     budget = lapply(budget,function(x) if(is.null(x))NA else x) |> as.numeric()
#
#     # actually_true = t(apply(value,1,function(x) at[[cond$fun_nr]]))
#     opt_value = at[[cond$fun_nr]] |> as.numeric()
#     opt_cost = costfun(opt_value)
#
#     true_power.fun = x1$true_power.fun
#     true_power = apply(value,1,true_power.fun)
#
#     re = data.frame(value,cost,value.sd,budget,cond,learner,opt_cost,true_power)
#     # re$actually_true = at[[cond$fun_nr]]
#     return(re)
# })
#
# resz = do.call(rbind,resz)
# resz = resz[resz$task==usetask,]
# resz = resz[which(!is.na(resz$X1)),]
# x1 = load.cond(2,usetask,budget="mid",goal.ci=NULL)
# true_power.fun = x1$true_power.fun
# costfun=x1$cost
# actually_true = at[[2]]
#
# #general result - scatter plot on map
# # add an optimal cost / optimal power line
# optpower.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
# optpower.y = sapply(optpower.x,function(x) {
#     fn = function(y) abs(true_power.fun(c(x,y))-.8)
#     optim(10,fn,method="Brent",lower=1,upper=40)$par})
# optpower = data.frame(X1=optpower.x,X2=optpower.y)
#
# optcost.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
# optcost.y = sapply(optcost.x,function(x) {
#     fn = function(y) abs(costfun(c(x,y))-costfun(actually_true))
#     optim(10,fn,method="Brent",lower=5,upper=40)$par})
# optcost = data.frame(X1=optcost.x,X2=optcost.y)
#
#
# resz1 = resz[resz$goal.ci=="low",]
# p5 = ggplot(resz1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=optpower,aes(x=X1, y=X2,col="power=.8"),size=1)+ geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost"),size=1) +  xlim(250, 600) +  scale_fill_brewer(palette="Set1",aesthetics = "col")
#
# resz2 = resz[resz$goal.ci=="high",]
# p6 = ggplot(resz2, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=optpower,aes(x=X1, y=X2,col="power=.8",size=1))+ geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost"),size=1) +  xlim(250, 600)+  scale_fill_brewer(palette="Set1",aesthetics = "col")
#
#
# # Höhenlinien Plot Function 6
# res2 = res[sapply(res,function(x) x[[1]]$fun_nr[1]==6)]
#
# resz = lapply(res2, function(x) {
#
#     cond = x[[1]]
#     x1 = load.cond(cond$fun_nr,cond$task,cond$budget,goal.ci=NULL)
#     # design=x1$design
#     costfun=x1$cost
#
#     value = sapply(2:length(x),function(i) x[[i]]$value)
#     value = lapply(value,function(x) if(is.null(x))NA else x)
#     value = do.call(rbind,value)
#     cost = apply(value,1,costfun) |> as.numeric()
#
#     value.sd  = sapply(2:length(x),function(i) x[[i]]$value.sd)
#     value.sd = lapply(value.sd,function(x) if(is.null(x))NA else x) |> as.numeric()
#     budget = sapply(2:length(x),function(i) x[[i]]$budget)
#     budget = lapply(budget,function(x) if(is.null(x))NA else x) |> as.numeric()
#
#     # actually_true = t(apply(value,1,function(x) at[[cond$fun_nr]]))
#     opt_value = at[[cond$fun_nr]] |> as.numeric()
#     opt_cost = costfun(opt_value)
#
#     true_power.fun = x1$true_power.fun
#     true_power = apply(value,1,true_power.fun)
#
#     re = data.frame(value,cost,value.sd,budget,cond,learner,opt_cost,true_power)
#     # re$actually_true = at[[cond$fun_nr]]
#     return(re)
# })
#
# resz = do.call(rbind,resz)
# resz = resz[resz$task==usetask,]
# resz = resz[which(!is.na(resz$X1)),]
# x1 = load.cond(6,usetask,budget="mid",goal.ci=NULL)
# # true_power.fun = x1$true_power.fun
# costfun=x1$cost
# actually_true = at[[6]]
#
# optcost.x = seq(min(resz$X1,na.rm=T),max(resz$X1,na.rm=T),2)
# optcost.y = sapply(optcost.x,function(x) {
#     fn = function(y) abs(costfun(c(x,y))-costfun(actually_true))
#     optim(10,fn,method="Brent",lower=5,upper=40)$par})
# optcost = data.frame(X1=optcost.x,X2=optcost.y)
#
# resz1 = resz[resz$goal.ci=="low",]
# p7 = ggplot(resz1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost"),size=1) +  scale_fill_brewer(palette="Set1",aesthetics = "col")
#
# resz2 = resz[resz$goal.ci=="high",]
# p8 = ggplot(resz2, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=optcost,aes(x=X1, y=X2,col="optimal cost"),size=1) +  scale_fill_brewer(palette="Set1",aesthetics = "col")

