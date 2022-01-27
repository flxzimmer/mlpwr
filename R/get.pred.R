

get.pred =function(xvars,predfun,goal=NULL,cost=function(x)sum(x),Ntry=20,design=NULL,fixed_cost=NULL) {

  # browser()
  # temp = design
  # design = NULL
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

  # boundmins = apply(xvars,2,function(x) max(2,min(x)*.95))
  # boundmaxs = apply(xvars,2,max)*1.05
  boundmins = sapply(design,function(x) x[1])
  boundmaxs = sapply(design,function(x) x[2])

  if (!is.null(fixed_cost)){

    # Acquisition Function
    # fn = function(x) ((cost(x)-fixed_cost)/fixed_cost)^2*10^4-predfun(x)
    fn = function(x) ((cost(x)-fixed_cost)/fixed_cost)^2*10^5-predfun(x)


    # print(Ntry)
    #     if (Ntry ==1) {
    #     #   new.n = tryCatch(optim(par=midpars,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par,error=function(x) {
    #     #     return(NA)
    #     #   })
    #     # if (is.na(new.n)) browser()
    #
    #       new.n = optim(par=midpars,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par
    # }

    if (Ntry >1) {

      # a = tryCatch(hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))),error=function(x) {
      #   return(NA)
      # })
      # if (is.na(a)) browser()
      a = hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
      # exact
      # cost(exact)
      # predfun(exact)
    }

    # Check if a reasonable value has been found
    toofar = abs((cost(exact)-fixed_cost)/fixed_cost)>.01
    if (toofar) {warning("No good value found")}

    # Serch candidate values for best match
    a = floor(exact)
    # cands = expand.grid(data.frame(rbind(a-2,a-1,a,a+1,a+2,a+3)))
    cands = expand.grid(data.frame(rbind(a,a+1)))
    cands = unique(cands)
    cost_vals = apply(cands,1,function(x) (cost(x)))
    acceptable = cands[which(cost_vals<=fixed_cost),]
    power = apply(acceptable,1,predfun)

    if (nrow(acceptable)==0) {
      warning("weird local maximum")
      acceptable = cands
      power = apply(acceptable,1,predfun)
    }
    opt = acceptable[rev(order(power)),]
    new.n = as.numeric(opt[1,])
    # points = opt[1:2,]
    points = opt[1,]

    # ind = which(aq_vals[acceptable] == min(aq_vals[acceptable]))
    # new.n = as.numeric(cands[acceptable[ind],])

    # ind = which(aq_vals == min(aq_vals))
    # new.n = as.numeric(cands[ind,])
    re = list(new.n=new.n, exact=exact,toofar=toofar,points=points)
  }


  if (!is.null(goal)){

    # Acquisition Function
    fn = function(x) (predfun(x)-goal)^2*10^5+cost(x)/cost(midpars)

    # boundmins = apply(xvars,2,function(x) max(2,min(x)*.95))
    # boundmaxs = apply(xvars,2,max)*1.05

    # print(Ntry)
    # if (Ntry ==1) new.n = optim(par=midpars,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))$par

    if (Ntry >1) {

      # a = tryCatch(hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1))),error=function(x) {
      #   return(NULL)
      # })
      # if (is.null(a)) browser()

      a = hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
    }

    # Serch integer candidate values for best match
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

    # Check if a reasonable value has been found
    toofar = abs(predfun(exact)-goal)>.01 | abs(predfun(new.n)-goal)>.05
    if (toofar) {warning("No good value found")}

    re = list(new.n=new.n, exact=exact,toofar=toofar,points=NULL)
  }

  # print(new.n);print(exact);print(toofar)
  # browser()

  return(re)
}
