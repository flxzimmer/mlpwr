# stuff -------------------------------------------------------------------


restag = c("48","49","50","51")
restag = 50

folder = "C:/Users/felix/switchdrive/4 irt/paper 2/"
# folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"

rest = list()
for (i in restag) {
  load(file= paste0(folder,"res_",i,".Rdata"))
  rest = c(rest,res)
}
res = rest

seeds.used = sapply(res,function(x)x[[1]]$seed)


# find correct seed


cands = (1643483500):(1643483513+50)


ind = sapply(cands,function(i) {

  # i = 1643483526

  CLUSTERSIZE = 32
  n.runs = 25

  fun_nr = 1:6
  task = c("B")
  budget = c("low","mid","high")
  goal.ci = NA
  sim2 = expand.grid(run=1:n.runs,fun_nr=fun_nr,task=task,budget = budget,goal.ci = goal.ci)
  sim2 = sim2  %>% split(., seq(nrow(.))) %>% lapply(.,as.list)

  fun_nr = c(2,6)
  task = c("C")
  budget = c("low","mid","high")
  goal.ci = NA
  sim3 = expand.grid(run=1:n.runs,fun_nr=fun_nr,task=task,budget = budget,goal.ci = goal.ci)
  sim3 = sim3  %>% split(., seq(nrow(.))) %>% lapply(.,as.list)

  sim = c(sim2,sim3)

  set.seed(i)
  # add seeds
  for (i in 1:length(sim)){
    sim[[i]]$seed = sample(1:10^8, 1)
  }

  #shuffle conditions
  sim = sim[sample(1:length(sim))]

  seeds.cand =sapply(sim,function(x) x$seed)
  all(seeds.cand[1:10] %in% seeds.used)

  # all(sort(seeds.cand)==sort(seeds.used))


})
sum(ind)


# a -----------------------------------------------------------------------


x$seed


seeds.used = sapply(res,function(x)x[[1]]$seed)

ind = seeds.used==x$seed

View(res[[which(ind)]])





# FIXATTEMPT
cands = expand.grid(data.frame(rbind(a-19,a-18,a-17,a-16,a-15,a-14,a-13,a-12,a-11,a-10,a-9,a-8,a-7,a-6,a-5,a-4,a-3,a-2,a-1,a,a+10,a+9,a+8,a+7,a+6,a+5,a+4,a+3,a+2,a+1)))
cands = unique(cands)
out1 = apply(cands,1,function(x) any(x<boundmins))
out2 = apply(cands,1,function(x) any(x>boundmaxs))
cands=cands[!out1&!out2,,drop = F]

res = list()
for (i in 1:nrow(cands)) {
  if (cost(cands[i,])==320) {
    # ind = c(ind,i)
    # print(cands[i,])
    # print(predfun(cands[i,]))
    res = c(res,list(cands[i,]))
  }
}
a = sapply(res,function(x)predfun(as.numeric(x)))
# res[order(a,decreasing=T)]
# which(max(a)==a)
# res[[11]]
new.n = as.numeric(res[[which(max(a)==a)]])
points = res[[which(max(a)==a)]]
print(new.n)

if(17 %in% new.n) browser()


if (F) {

  # try out random pars!
  pars = list(cost=runif(1,0.01,.3),epsilon=runif(1,.001,.02),gamma=runif(1,0.05,.2)) # Default Pars
  mod = wsvm(y ~ .,data = datx,cost=pars$cost,epsilon=pars$epsilon,gamma=pars$gamma,weight=weight)
  predfun = function(x) {
    names(x) = names(xvars)
    predict(mod,newdata=data.frame(t(x)))}
  res = get.pred(xvars,predfun,goal,cost,Ntry=20,design=design,fixed_cost=fixed_cost,dat=dat,greedy=greedy)
  res
  pars

  # $cost
  # [1] 0.2585978
  #
  # $epsilon
  # [1] 0.006396603
  #
  # $gamma
  # [1] 0.1719699
  #
  # ##
  #
  # $cost
  # [1] 0.2269182
  #
  # $epsilon
  # [1] 0.009657583
  #
  # $gamma
  # [1] 0.1324921
  #
  # #
  #
  # $cost
  # [1] 0.2535449
  #
  # $epsilon
  # [1] 0.01443552
  #
  # $gamma
  # [1] 0.1600543
  #
  #
  # #
  #
  # $cost
  # [1] 0.2384068
  #
  # $epsilon
  # [1] 0.004788271
  #
  # $gamma
  # [1] 0.192239

}




if (!is.null(fixed_cost)){

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
  cands = expand.grid(data.frame(rbind(a-2,a-1,a,a+1,a+2,a+3)))
  cands = unique(cands)
  out1 = apply(cands,1,function(x) any(x<boundmins))
  out2 = apply(cands,1,function(x) any(x>boundmaxs))
  cands=cands[!out1&!out2,,drop = F]

  cost_vals = apply(cands,1,function(x) (cost(x)))
  acceptable = cands[which(cost_vals<=fixed_cost),]
  power = apply(acceptable,1,predfun)

  if (nrow(acceptable)==0) {
    warning("weird local maximum")
    acceptable = cands
    power = apply(acceptable,1,predfun)
  }

  if (greedy) {
    sd_vals = apply(acceptable,1,function(x) get.sd(dat,x))
    sd_vals[sd_vals==10]= min(sd_vals[sd_vals<10])*5
    if(any(sd_vals==Inf)) sd_vals = 0

    opt = acceptable[rev(order(power+sd_vals)),]
    new.n = as.numeric(opt[1,])
    points = opt[1,]
  }
  if (!greedy) {
    opt = acceptable[rev(order(power)),]
    new.n = as.numeric(opt[1,])
    points = opt[1,]
  }

  # if(17 %in% new.n & predfun(new.n)>.6) browser()


  re = list(new.n=new.n, exact=exact,toofar=toofar,points=points)
}



