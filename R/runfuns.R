

# Anova -------------------------------------------------------------------


#' Title
#'
#' @param delta
#' @param within.var
#'
#' @return
#' @export
#'
#' @examples
runfun.anova.long = function(delta=10^(-1),within.var=10) {

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    groupmeans= seq(0,(k-1)*delta,by=delta)

    y <- rnorm(n * k, mean = rep(groupmeans,each=n), sd = sqrt(within.var))
    group <- factor(rep(LETTERS[1:k], each = n))

    dat = data.frame(y=y,group= group)
    fit  <- aov(y ~ group, data = dat)

    p = summary(fit)[[1]][1, "Pr(>F)"]

    return(p<.05)
  }
  return(runfun)
}


#' Title
#'
#' @param delta
#' @param within.var
#'
#' @return
#' @export
#'
#' @examples
runfun.anova.true = function(delta=.16298) {

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    # groupmeans= seq(0,(k-1)*delta,by=delta)

    # For non-integer cluster values:
    # k1 = floor(k)
    # k2 = ceiling(k)
    # var1 = var(seq(0,(k1-1)*delta,by=delta))
    # var2 = var(seq(0,(k2-1)*delta,by=delta))
    # vark = var1 + (k-k1) * (var2-var1)

    # re = power.anova.test(groups=k,n=n,between.var = vark, within.var = within.var)$power
    re = pwr.anova.test(k, n , f=delta , sig.level = .05)$power
    return(re)
  }
  return(runfun)
}

#' Title
#'
#' @param delta
#' @param within.var
#'
#' @return
#' @export
#'
#' @examples
runfun.anova = function(delta=.16298) {

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    pow = pwr.anova.test(k, n , f=delta , sig.level = .05)$power
    return(runif(1)<pow)
  }
  return(runfun)
}

# T-Tests ------------------------------------------------------------------
# Ttest / correlation


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


skeweddist = function(n,alpha = 4) {
  #skeweddist: alpha = 0 entspricht normalverteilung.
  # Wähle alpha = 4 und passe alle anderen Werte an, damit mean~0 und sd~1 gegeben ist.
  delta = alpha/sqrt(1+alpha^2)
  omega = sqrt(1/(1-2*delta^2/pi))
  xi = -omega*delta*sqrt(2/pi)

  skewness = (4-pi)/2*(delta*sqrt(2/pi))^3/(1-2*delta^2/pi)^(3/2)

  return(as.numeric(sn::rsn(n=n, xi=xi, omega=omega, alpha=alpha)))
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
#' @param x
#'
#' @return
#' @export
#'
#' @examples
runfun.wilson2 = function() {
  # From wilson Example 4
  # x is a 2 dimensional specification here. (n,clusters)
  delta = .3
  var_t = 1
  rho = .05

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    m <- n/k # Persons per Cluster
    sd1 <- sqrt(var_t*rho + var_t/m - var_t*rho/m)
    # sd1 <- sqrt(rho+1/m-rho/m)
    pow <- power.t.test(n = k, delta = delta, sd = sd1 )$power

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
  delta = .3
  var_t = 1
  rho = .05

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    m <- n/k # Persons per Cluster
    sd1 <- sqrt(var_t*rho + var_t/m - var_t*rho/m)
    pow <- power.t.test(n = k, delta = delta, sd = sd1 )$power

    return(pow)
  }
  return(runfun)
}



# IRT ---------------------------------------------------------------------


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
#' @param x
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
runfun.12pl.pars = function(delta=.1,n.items = 20,seed=1) {
  # delta is the variance of the slope parameters
  set.seed(seed)
  pars <- list(
    a = rlnorm(n.items,sdlog = delta),
    d = rnorm(n.items)
  )
}

#' 1PL vs 2PL
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
runfun.12pl = function(delta=.1,n.items =20,seed=1) {
  # 1PL vs 2pL with n persons n and pars from runfun.12pl.pars

  # runfun.12pl.pars = function(delta=.1,n.items = 20) {
    # delta is the variance of the slope parameters

  pars = runfun.12pl.pars(delta=delta,n.items = n.items,seed=seed)
  # }
  # print(pars)
  set.seed(Sys.time())

  runfun=function(n) {
  # theta = matrix(rnorm(N/2),ncol=1)
  simdat = mirt::simdata(a = pars$a,d = pars$d, N=n,itemtype = "2PL")
  mod1 = mirt(simdat,model=1,verbose=FALSE,itemtype="Rasch")
  mod2 = mirt(simdat,model=1,verbose=FALSE,itemtype="2PL")

  lr = 2*(mirt::logLik(mod2)-mirt::logLik(mod1))
  pchisq(lr,df=n.items-1,ncp=0,lower.tail=FALSE)
  }
  return(runfun)
}

# library(mirt)
# pars = runfun.12pl.pars(delta=0.2)
# re= replicate(40,runfun.12pl(200,pars))
# mean(re<.05)


# Mixed Models from SIMR --------------------------------------------------


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


#' SIMR runfun multi
#'
#'
#' @return
#' @export
#'
#' @examples
runfun.simr2x = function() {
  model1 <- lme4::glmer(z ~ x + (1|g), family="poisson", data=simr::simdata)
  tmp = lme4::fixef(model1)
  tmp[2] = -.002
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



# Other -------------------------------------------------------------------


# T-TEST runfun
#' Title
#'
#'
#'
#' @return
#' @export
#'
#' @examples
runfun.dummy = function(delta = .2) {

  runfun = function(x) {
    # fetch a p-value at the specified sample size
    a = rnorm(x[1],delta)
    # re = t.test(a)$p.value *(1+x[2]/100) <.05
    re = t.test(a)$p.value <.05
    return(as.numeric(re))
  }
  re = runfun
  return(re)
}


# runfun.ttest = function(n) {
#   # fetch a p-value at the specified sample size
#   a = rnorm(n,.2)
#   re = t.test(a)$p.value
#   return(re)
# }

# Correlation runfun
runfun.cor = function(n) {
  # fetch a p-value at the specified sample size
  a = rnorm_multi(n=n,vars=2,r=.2)
  re = cor.test(a[,1],a[,2])$p.value
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
runfun.wilson1 = function(x) {
  # From wilson Example 4
  # x is a 2 dimensional specification here. (n,clusters)

  h <- c(mu = 0.3, var_t = 1, rho = 0.05) # Hardcoded Hypothesis
  N=1
  # calc_rates4 <- function(x, h, N)
  # {
  n <- x[1]; k <- x[2]; m <- n/k
  var_t <- h[[2]]; rho <- h[[3]]
  sig_c <- sqrt(var_t*rho + var_t/m - var_t*rho/m)
  pow <- power.t.test(n = k, delta = h[[1]], sd = sig_c)$power

  # res <- c(1 - rbinom(1, N, pow)/N, pow*(1-pow)/N) # This is somewhat problematic, as the SD is based on the expected variance, not the observed or an estimation of the expected SD.
  if(runif(1)<pow) re = .01 else re = .5 # Generate a Pseudo-p-value that adheres to the power
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
runfun.wilson1.true = function(x) {
  # From wilson Example 4
  # x is a 2 dimensional specification here. (n,clusters)
  # Gives true power at x

  h <- c(mu = 0.3, var_t = 1, rho = 0.05) # Hardcoded Hypothesis
  N=1
  # calc_rates4 <- function(x, h, N)
  # {
  n <- x[1]; k <- x[2]; m <- n/k
  var_t <- h[[2]]; rho <- h[[3]]
  sig_c <- sqrt(var_t*rho + var_t/m - var_t*rho/m)
  pow <- power.t.test(n = k, delta = h[[1]], sd = sig_c)$power

  return(pow)
}



runfun.wilson3 = function(x) {
  # From wilson Example 3
  # x is a 4 dimensional specification here. (n, therpasists, etc)

  # Hypothesis is hardcoded
  h <- c(r_t=0.05, r_d=0.1, v_w=3.29, cov_res=0.9, cov_w=0.9, p0a=0.1, p1a=0.25, p0b=0.1, p1b=0.25)


  sim_trial3 <- function(x, h)
  {
    n_1 <- x[1]; k <- x[2]; r <- x[3]; j <- x[4]
    n_0 <- round(r*n_1); n_t <- n_0 + n_1

    r_t <- h[1]; r_d <- h[2]; v_w <- h[3]; cov_res <- h[4]; cov_w <- h[5]
    p0a <- h[6]; p1a <- h[7]; p0b <- h[8]; p1b <- h[9]

    # Find the terms for the linear predictor which correspond to the probabilities
    lp0a <- log(p0a/(1-p0a)); lp1a <- log(p1a/(1-p1a)) - lp0a
    lp0b <- log(p0b/(1-p0b)); lp1b <- log(p1b/(1-p1b)) - lp0b

    # Doctor effects
    v_d <- r_d*v_w/(1-r_d)
    doc <- cbind(seq(1,j), rmvnorm(j, c(0,0), matrix(c(v_d, v_d*cov_res, v_d*cov_res, v_d), ncol=2, byrow = 2)))

    # Therapist effects
    v_t <- (r_t*v_d + r_t*v_w)/(1-r_t)
    ther <- cbind(seq(0,k), rbind(c(0, 0), rmvnorm(k, c(0,0), matrix(c(v_t, v_t*cov_res, v_t*cov_res, v_t), ncol=2, byrow = 2))))

    # 1   | 2    | 3   | 4    | 5    | 6     | 7
    # arm | p_id |d_id | d_ef | t_id | t_eff | outcome

    # Treatment group
    trt <- c(rep(0, n_0), rep(1, n_1))
    # Patient ID
    p_id <- seq(1, n_t)
    # Doctor allocation and effect
    d_id <- sample(rep(1:j, ceiling(n_t/j)*j)[1:n_t])
    d_eff_a <- doc[d_id,2]
    d_eff_b <- doc[d_id,3]
    # Therapist allocation and effect
    t_id <- c(rep(0, n_0), rep(1:k, ceiling(n_1/k)*k)[1:n_1])
    t_eff_a <- ther[t_id+1,2]
    t_eff_b <- ther[t_id+1,3]
    # Residual - now bi-variate
    resid_n <- rmvnorm(n_t, c(0,0), matrix(c(v_w, cov_w*v_w, cov_w*v_w, v_w), ncol=2, byrow = 2))

    data_a <- cbind(trt,p_id,d_id,d_eff_a,t_id,t_eff_a,resid_n[,1])
    data_b <- cbind(trt,p_id,d_id,d_eff_b,t_id,t_eff_b,resid_n[,2])

    var(data_a[,7])

    # Outcomes
    data_a <- cbind(data_a, apply(data_a, 1, function(y, lp1, lp0) lp0 + y[1]*lp1 + y[4] + y[6] + y[7], lp1=lp1a, lp0=lp0a))
    data_b <- cbind(data_b, apply(data_b, 1, function(y, lp1, lp0) lp0 + y[1]*lp1 + y[4] + y[6] + y[7], lp1=lp1b, lp0=lp0b))

    # Analyse the data
    df_a <- as.data.frame(data_a)
    names(df_a)[8] <- c("y")
    df_b <- as.data.frame(data_b)
    names(df_b)[8] <- c("y")

    result_a <- tryCatch({
      fit1 <- suppressMessages(lmer(y ~ trt + (0 + trt|t_id) + (1|d_id), REML=F, data=df_a))
      fit2 <- suppressMessages(lmer(y ~ (0 + trt|t_id) + (1|d_id), REML=F, data=df_a))
      p <- anova(fit1, fit2)[2,8]
    }, warning = function(war) {
      # warning handler picks up where error was generated
      #print("warning")
      return(1)
    }, error = function(err) {
      # error handler picks up where error was generated
      print("error")
      return(1)
    }, finally = {

    })

    result_b <- tryCatch({
      fit1 <- suppressMessages(lmer(y ~ trt + (0 + trt|t_id) + (1|d_id), REML=F, data=df_b))
      fit2 <- suppressMessages(lmer(y ~ (0 + trt|t_id) + (1|d_id), REML=F, data=df_b))
      p <- anova(fit1, fit2)[2,8]
    }, warning = function(war) {
      # warning handler picks up where error was generated
      #print("warning")
      return(1)
    }, error = function(err) {
      # error handler picks up where error was generated
      print("error")
      return(1)
    }, finally = {

    })

    return(c(result_a, result_b))
  }

}


