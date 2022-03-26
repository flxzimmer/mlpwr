

# T-TEST runfun
#' Title
#'
#' @param delta
#'
#' @return
#' @export
#'
#' @examples
runfun.ttest = function(delta) {

  runfun = function(n) {
    # fetch a p-value at the specified sample size
    a1 = rnorm(n)
    a2 = rnorm(n)+delta
    re = t.test(a1,a2,var.equal=TRUE)$p.value<.05

    return(re)
  }
  re = runfun
  return(re)
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
runfun.ttest.true = function(delta) {

  runfun = function(n) {
    # fetch a p-value at the specified sample size
    re <- power.t.test(n = n, delta = delta)$power
    return(re)
  }
  re = runfun
  return(re)
}



#' Title
#'
#' @param delta
#'
#' @return
#' @export
#'
#' @examples
runfun.anova = function(delta=.16298) {

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    pow = pwr::pwr.anova.test(k, n , f=delta , sig.level = .05)$power
    return(runif(1)<pow)
  }
  return(runfun)
}


#' Title
#'
#' @param delta
#'
#' @return
#' @export
#'
#' @examples
runfun.anova.true = function(delta=.16298) {

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters

    re = pwr::pwr.anova.test(k, n , f=delta , sig.level = .05)$power
    return(re)
  }
  return(runfun)
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
runfun.skewed2 = function(n, delta =.2, alpha =4) {

  runfun= function(n) {

    a1 = skeweddist(n,alpha = alpha)
    a2 = skeweddist(n,alpha = -alpha)+delta
    re = t.test(a1,a2,var.equal=TRUE)$p.value<.05
    return(re)
  }
  return(runfun)
}


#' Title
#'
#' @param delta
#' @param n.items
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
runfun.irt.itempars = function(delta=.1,n.items = 20,seed=1) {
  # delta is the variance of the slope parameters
  set.seed(seed)
  pars <- list(
    a = rlnorm(n.items,sdlog = delta),
    d = rnorm(n.items)
  )
}

#' 1PL vs 2PL
#'
#' @param itempars
#'
#' @return
#' @export
#'
#' @examples
runfun.irt = function(itempars) {

  runfun=function(n) {

    repeat {
      simdat = mirt::simdata(a = itempars$a,d = itempars$d, N=n,itemtype = "2PL")
      mod1 = tryCatch(mirt(simdat,model=1,verbose=FALSE,itemtype="Rasch"),error=function(x) NULL)
      mod2 = tryCatch(mirt(simdat,model=1,verbose=FALSE,itemtype="2PL"),error=function(x) NULL)
      if (!is.null(mod1) & !is.null(mod2)) break
    }
    lr = 2*(mirt::logLik(mod2)-mirt::logLik(mod1))
    pchisq(lr,df=length(itempars[[1]])-1,ncp=0,lower.tail=FALSE)<.05
  }
  return(runfun)
}


#' SIMR runfun
#'
#'
#' @return
#' @export
#'
#' @examples
runfun.simr = function() {
  model1 <- lme4::glmer(z ~ x + (1|g), family="poisson", data=simr::simdata)
  tmp = lme4::fixef(model1)
  tmp[2] = -.05
  simr::fixef(model1) <- tmp

  runfun = function(n) {
    model2 <- extend(model1, along="x", n=n)
    re = hush(powerSim(model2,nsim=1,progress=F)$pval<.05)
    return(re)
  }
  return(runfun)
}


#' SIMR runfun multi
#'
#'
#' @return
#' @export
#'
#' @examples
runfun.simr2 = function() {
  model1 <- lme4::glmer(z ~ x + (1|g), family="poisson", data=simr::simdata)
  tmp = lme4::fixef(model1)
  tmp[2] = -.01
  simr::fixef(model1) <- tmp

  runfun = function(x) {
    n <- x[1] # Number of Study Years
    k <- x[2] # Number of Clusters
    model2 <- extend(model1, along="x", n=n)
    model2 <- extend(model2, along="g", n=k)
    re = hush(powerSim(model2,nsim=1,progress=F)$pval<.05)
    return(re)
  }
  return(runfun)
}

