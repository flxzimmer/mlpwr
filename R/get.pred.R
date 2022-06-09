
get.pred =function(fit,dat,power,costfun,max_cost,boundaries,Ntry=20,task) {

  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]

  midpars = sapply(boundaries,mean)

  #using half of the Ntrys for random values!
  parmat_last = xvars[nrow(xvars) + 1 - seq(1,min(round(Ntry/2),nrow(xvars))),,drop=F]
  parmat_rand = initpoints(boundaries,max(Ntry-nrow(parmat_last),0),method="sobol")
  parmat_rand = as.data.frame(parmat_rand)
  names(parmat_rand) = names(parmat_last)

  parmat = rbind(parmat_last,parmat_rand)

  boundmins = sapply(boundaries,function(x) x[1])
  boundmaxs = sapply(boundaries,function(x) x[2])


  ##############################################################################
  if (task == "costthreshold"){ # Cost Threshold Task

    # Acquisition Function
    fn = function(x) ((costfun(x)-max_cost)/max_cost)^2*10^5-fit$fitfun(x)

    if (Ntry >1) {
      a = hush(optimr::multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
    }

    # Check if value is on the edge of the design space
    edgeprediction=FALSE
    if(any(exact==boundmins) | any(exact==boundmaxs)) {
      edgeprediction = TRUE
    }


    # Serch candidate values for best match
    a = floor(exact)
    cands = expand.grid(data.frame(rbind(a-6,a-5,a-4,a-3,a-2,a-1,a,a+1,a+2,a+3,a+4,a+5,a+6,a+7)))
    cands = unique(cands)
    out1 = apply(cands,1,function(x) any(x<boundmins))
    out2 = apply(cands,1,function(x) any(x>boundmaxs))
    cands=cands[!out1&!out2,,drop = F]

    ## find points greedy
    fnvals = apply(cands,1,fn)
    cands2 = cands[order(fnvals)[1:10],]
    fnvals2 = fnvals[order(fnvals)[1:10]]
    sd_vals = apply(cands2,1,function(x) get.sd(dat,x))
    points = cands2[which(min(fnvals2-sd_vals)==(fnvals2-sd_vals)),]

    ## find points not greedy
    out3 = apply(cands,1,function(x) costfun(x)>max_cost)
    cands3=cands[!out3,,drop = F]
    fnvals2 = apply(cands3,1,fn)
    cands4 = cands3[order(fnvals2)[1:10],]
    points.notgreedy = cands4[1,]


    ## final checks

    # Check if a reasonable value has been found
    badprediction = abs((costfun(exact)-max_cost)/max_cost)>.01
  }
  ##############################################################################


  ##############################################################################
  if (task == "desiredpower"){ # Desired Power Task


    # Acquisition Function
    fn = function(x) (fit$fitfun(x)-power)^2*10^5+costfun(x)/costfun(midpars)

    if (Ntry >1) {

      a = hush(optimr::multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
    }

    # Check if value is on the edge of the design space
    edgeprediction=FALSE
    if(any(exact==boundmins) | any(exact==boundmaxs)) {
      edgeprediction = TRUE
    }

    # Serch integer candidate values for best match
    a = floor(exact)
    cands = expand.grid(data.frame(rbind(a-2,a-1,a,a+1,a+2,a+3)))
    cands = unique(cands)
    out1 = apply(cands,1,function(x) any(x<boundmins))
    out2 = apply(cands,1,function(x) any(x>boundmaxs))
    cands=cands[!out1&!out2,,drop = F]

    aq_vals = apply(cands,1,fn)
    pw_vals = apply(cands,1,function(x) (fit$fitfun(x)-power))
    cost_vals = apply(cands,1,costfun)

    ## find points not greedy

    # pick acceptable values (notgreedy)
    acceptable.notgreedy = which(pw_vals>0)

    # if none are acceptable the exact value seems to be a local maximum. treat all as acceptable instead of none
    if (length(acceptable.notgreedy)==0) acceptable.notgreedy = 1:length(pw_vals)

    # pick value from acceptable ones
    ind = which(cost_vals[acceptable.notgreedy] == min(cost_vals[acceptable.notgreedy]))
    acceptable2 = acceptable.notgreedy[ind]
    ind2 = which(aq_vals[acceptable2] == min(aq_vals[acceptable2]))
    acceptable3 = acceptable2[ind2]
    new.n = as.numeric(cands[acceptable3,])

    points.notgreedy = data.frame(t(new.n))


    ## find points greedy

    # pick acceptable values
    sd_vals = apply(cands,1,function(x) get.sd(dat,x))
    if (all(sd_vals>=10)) {
      sd_vals[sd_vals==10]= Inf
    } else {
      sd_vals[sd_vals==10]= min(sd_vals[sd_vals<10])/2
    }

    acceptable = which(pw_vals+sd_vals/2>0)
    # switch to not greedy if no good value was found
    if (length(acceptable)==0 | length(acceptable)==nrow(cands)) acceptable = acceptable.notgreedy

    # if none are acceptable the exact value seems to be a local maximum. treat all as acceptable instead of none
    if (length(acceptable)==0) acceptable = 1:length(pw_vals)

    # pick value from acceptable ones
    ind = which(cost_vals[acceptable] == min(cost_vals[acceptable]))
    acceptable2 = acceptable[ind]
    ind2 = which(aq_vals[acceptable2] == min(aq_vals[acceptable2]))
    acceptable3 = acceptable2[ind2]
    new.n = as.numeric(cands[acceptable3,])

    points = data.frame(t(new.n))


    ## final checks

    # Check if value is too far from goal power
    badprediction = abs(fit$fitfun(exact)-power)>.01 | abs(fit$fitfun(new.n)-power)>.05
  }
  ##############################################################################


  # sample all locations if prediction is bad
  # an edgeprediction should be updated at for falsification, it is not the same as other "bad" predictions (e.g. because of too few data)
  if (badprediction& !edgeprediction) {
    points = datx[,1:(length(datx)-1),drop=FALSE]
  }

  re = list(points=points, exact=exact, badprediction=badprediction,points.notgreedy = points.notgreedy,edgeprediction=edgeprediction)

  return(re)
}
