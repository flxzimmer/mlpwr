
get.pred =function(fit,dat,goal,costfun,max_cost,boundaries,greedy,Ntry=20,task) {

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



  if (task == "costthreshold"){ # Cost Threshold Task

    # Acquisition Function
    fn = function(x) ((costfun(x)-max_cost)/max_cost)^2*10^5-fit$fitfun(x)

    if (Ntry >1) {

      a = hush(optimr::multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
    }

    # Serch candidate values for best match
    a = floor(exact)
    cands = expand.grid(data.frame(rbind(a-6,a-5,a-4,a-3,a-2,a-1,a,a+1,a+2,a+3,a+4,a+5,a+6,a+7)))
    cands = unique(cands)
    out1 = apply(cands,1,function(x) any(x<boundmins))
    out2 = apply(cands,1,function(x) any(x>boundmaxs))
    cands=cands[!out1&!out2,,drop = F]

    fnvals = apply(cands,1,fn)
    cands2 = cands[order(fnvals)[1:10],]
    fnvals2 = fnvals[order(fnvals)[1:10]]


    points = cands2[1,]
    new.n = as.numeric(points)
    print(paste("notgreedy:",new.n))


    if (greedy) {
      sd_vals = apply(cands2,1,function(x) get.sd(dat,x))
      points = cands2[which(min(fnvals2-sd_vals)==(fnvals2-sd_vals)),]
      new.n = as.numeric(points)
    }

    # Check if a reasonable value has been found
    toofar = abs((costfun(exact)-max_cost)/max_cost)>.01

  }


  if (task == "desiredpower"){ # Desired Power Task


    # Acquisition Function
    fn = function(x) (fit$fitfun(x)-goal)^2*10^5+costfun(x)/costfun(midpars)

    if (Ntry >1) {

      a = hush(optimr::multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
    }

    # Serch integer candidate values for best match
    a = floor(exact)
    cands = expand.grid(data.frame(rbind(a-2,a-1,a,a+1,a+2,a+3)))
    cands = unique(cands)
    out1 = apply(cands,1,function(x) any(x<boundmins))
    out2 = apply(cands,1,function(x) any(x>boundmaxs))
    cands=cands[!out1&!out2,,drop = F]

    aq_vals = apply(cands,1,fn)
    pw_vals = apply(cands,1,function(x) (fit$fitfun(x)-goal))
    cost_vals = apply(cands,1,costfun)

    if (greedy) {
      sd_vals = apply(cands,1,function(x) get.sd(dat,x))
      sd_vals[sd_vals==10]= min(sd_vals[sd_vals<10])/2

      acceptable = which(pw_vals+sd_vals/2>0)
      if (length(acceptable)==0 | length(acceptable)==nrow(cands)) greedy=FALSE
    }

    if (!greedy) acceptable = which(pw_vals>0)
    if (length(acceptable)==0) {
      # warning("weird local maximum")
      acceptable = 1:length(pw_vals) # treat all as acceptable instead of none
    }

    ind = which(cost_vals[acceptable] == min(cost_vals[acceptable]))
    acceptable2 = acceptable[ind]
    ind2 = which(aq_vals[acceptable2] == min(aq_vals[acceptable2]))
    acceptable3 = acceptable2[ind2]
    new.n = as.numeric(cands[acceptable3,])

    # Check if a reasonable value has been found
    toofar = abs(fit$fitfun(exact)-goal)>.01 | abs(fit$fitfun(new.n)-goal)>.05

  }

browser()

  # TO DO PRETTIER
  # ADD useprediction in return list
  # useprediction = FALSE
  # if (is.null(fit$new.n)) {
  #   # warning("no predicted value")
  # } else {
    useprediction = !fit$toofar

    # check if fitiction lies within design
    for (i in 1:length(fit$new.n)) {
      if (fit$new.n[i]<design[[i]][1]|fit$new.n[i]>design[[i]][2]) useprediction = FALSE
    }
  }


  # transform this to create the points object, addval happens in main function

  #add value to the dataset if prediction is ok
  if (pred$useprediction) {
    points = data.frame(t(fit$new.n))
    if(!is.null(fixed_cost)) { # add multiple candidates for fixed cost condition
      points = fit$points
    }
    dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=setsize/nrow(points))
  }

  #search at previous values if prediction is bad
  if (!pred$useprediction) {
    datx = todataframe(dat)
    points = datx[,1:(length(datx)-1),drop=FALSE]
    each = max(round(setsize/nrow(datx)),1) # make sure at least one point is added
    dat = addval(runfun=runfun,design=design,dat=dat,points=points,each=each)
  }

# re = list(new.n=new.n, exact=exact,toofar=toofar,points=NULL)
# re = list(new.n=new.n, exact=exact,toofar=toofar,points=points)


  return(re)
}
