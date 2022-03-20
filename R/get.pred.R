
get.pred =function(xvars,predfun,goal=NULL,cost=function(x)sum(x),Ntry=20,design=NULL,fixed_cost=NULL,greedy=TRUE,dat=NULL) {

  # reconstruct design if missing
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

  boundmins = sapply(design,function(x) x[1])
  boundmaxs = sapply(design,function(x) x[2])


  if (!is.null(fixed_cost)){ # Cost Threshold Task

    # Acquisition Function
    fn = function(x) ((cost(x)-fixed_cost)/fixed_cost)^2*10^5-predfun(x)
    # fn = function(x) (relu(cost(x)-fixed_cost)/fixed_cost)^2*10^5-predfun(x)

    if (Ntry >1) {

      a = hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
      new.n = a[which(a$value==min(a$value))[1],1:ncol(xvars)]
      exact = as.numeric(new.n)
      # print(exact)
    }

    # Check if a reasonable value has been found
    toofar = abs((cost(exact)-fixed_cost)/fixed_cost)>.01
    if (toofar) {warning("No good value found")}

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

    # if (!greedy) {
      points = cands2[1,]
      new.n = as.numeric(points)
      print(paste("notgreedy:",new.n))
    # }

    if (greedy) {
      sd_vals = apply(cands2,1,function(x) get.sd(dat,x))
      # sd_vals[sd_vals==10]= min(sd_vals[sd_vals<10])*10
      # if(all(sd_vals==Inf)) sd_vals = 0
      points = cands2[which(min(fnvals2-sd_vals)==(fnvals2-sd_vals)),]
      new.n = as.numeric(points)
    }


    # if(17 %in% new.n & predfun(new.n)>.6) browser()


    re = list(new.n=new.n, exact=exact,toofar=toofar,points=points)
  }



  if (!is.null(goal)){ # Desired Power Task

    # Acquisition Function
    fn = function(x) (predfun(x)-goal)^2*10^5+cost(x)/cost(midpars)

    if (Ntry >1) {

      a = hush(multistart(parmat=parmat,fn=fn,method="L-BFGS-B",lower=boundmins,upper=boundmaxs,control=list(factr=1)))
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
    pw_vals = apply(cands,1,function(x) (predfun(x)-goal))
    cost_vals = apply(cands,1,cost)

    if (greedy) {
      sd_vals = apply(cands,1,function(x) get.sd(dat,x))
      sd_vals[sd_vals==10]= min(sd_vals[sd_vals<10])/2

      # acceptable = which(pw_vals+sd_vals>0 & sd_vals!=10)
      acceptable = which(pw_vals+sd_vals/2>0)
      if (length(acceptable)==0 | length(acceptable)==nrow(cands)) greedy=FALSE
      }
    if (!greedy) acceptable = which(pw_vals>0)
    if (length(acceptable)==0) {
      warning("weird local maximum")
      acceptable = 1:length(pw_vals) # treat all as acceptable instead of none
    }

    ind = which(cost_vals[acceptable] == min(cost_vals[acceptable]))
    acceptable2 = acceptable[ind]
    ind2 = which(aq_vals[acceptable2] == min(aq_vals[acceptable2]))
    acceptable3 = acceptable2[ind2]
    new.n = as.numeric(cands[acceptable3,])

    # Check if a reasonable value has been found
    toofar = abs(predfun(exact)-goal)>.01 | abs(predfun(new.n)-goal)>.05
    if (toofar) {warning("No good value found")}

    re = list(new.n=new.n, exact=exact,toofar=toofar,points=NULL)
  }

  return(re)
}
