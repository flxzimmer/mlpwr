
# calculating the actual power corresponding to the design parameter sets in the different DGFs

# The results are used in the analysis of the simulation study

# the "actually true" value is the value that implies the lowest cost among values with power > .8

# the actual power is the power of the actually true parameter set

# some DGFs needed larger simulations for this, the files for these are noted there.

library(simpackage)
load.libs()

folder = getwd() # File Location


# Function 1 --------------------------------------------------------------

runfun = runfun.ttest(delta = .4)
true_power.fun = runfun.ttest.true(delta=.4)

actually_true <- power.t.test(delta = .4,power=.8)$n
actually_true = ceiling(actually_true)
actual_cost = actually_true

actual_power = true_power.fun(actually_true)

true_power.fun = runfun.ttest.true(delta=.4)

at1 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,true_power.fun=true_power.fun)

save(at1,file= paste0(folder,"at_1.Rdata"))


# Function 2 --------------------------------------------------------------

# true_power.fun = runfun.wilson2.true()
# design = list(n = c(1,1200),k=c(1,30))
# cost = function(x) x[1]*5+x[2]*100
#
# # For Task B (Minimal Power = .8)
# goal = .8
# cands = list()
# ks = seq(1,30)
# for (i in 1:length(ks)) {
#   k = ks[i]
#   n = 1
#   while (true_power.fun(c(n,k))<goal&n<=2000) n = n+1
#   if (true_power.fun(c(n,k))>goal) cands[[i]] = c(n,k) else cands[[i]] = NA
# }
#
# costs = sapply(cands,cost)
# ind = which(costs == min(costs,na.rm=T))
# actually_true = cands[[ind]]
# actual_power = true_power.fun(actually_true)
# actual_cost = cost(actually_true)
#
# # For Task C (Maximum Costs= 3500)
# cands = list()
# ks = seq(1,30)
# for (i in 1:length(ks)) {
#   k = ks[i]
#   n = 1
#   while (cost(c(n+1,k))<=3500&n<=2000) n = n+1
#   cands[[i]] = c(n,k)
# }
#
# powers = sapply(cands,true_power.fun)
# ind = which(powers == max(powers))
# actually_trueC = cands[[ind]]
# actual_powerC = true_power.fun(actually_trueC)
# actual_costC = cost(actually_trueC)
#
# # true_power.fun = function() {true_power.fun = runfun.wilson2.true()
# # ;return(true_power.fun)}
#
# true_power.fun =  runfun.wilson2.true()
#
# at2 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,true_power.fun=true_power.fun,actually_trueC=actually_trueC,actual_powerC=actual_powerC,actual_costC=actual_costC)
#
# save(at2,file= paste0(folder,"at_2.Rdata"))


# Function 2 (new) --------------------------------------------------------------
true_power.fun = runfun.anova.true()

x1 = load.cond(2,"C",2000,.05)
design=x1$design
cost=x1$cost
fixed_cost=x1$fixed_cost

# cost = function(x) x[1]*1.9+x[2]*6
# fixed_cost = 174


# For Task B (Minimal Power = .8)
goal = .8
cands = list()
ks = seq(design$k[1],design$k[2])
for (i in 1:length(ks)) {
  k = ks[i]
  n = 2
  while (true_power.fun(c(n,k))<goal&n<=design$n[2]) n = n+1
  if (true_power.fun(c(n,k))>goal) cands[[i]] = c(n,k) else cands[[i]] = NA
}

costs = sapply(cands,cost)
ind = which(costs == min(costs,na.rm=T))
actually_true = cands[[ind]]
actual_power = true_power.fun(actually_true)
actual_cost = cost(actually_true)

# For Task C (Maximum Costs= 4800)
cands = list()
ks = seq(design$k[1],design$k[2])
for (i in 1:length(ks)) {
  k = ks[i]
  n = 2
  while (cost(c(n+1,k))<=fixed_cost&n<=design$n[2]) n = n+1
  cands[[i]] = c(n,k)
}

powers = sapply(cands,true_power.fun)
ind = which(powers == max(powers))
actually_trueC = cands[[ind]]
actual_powerC = true_power.fun(actually_trueC)
actual_costC = cost(actually_trueC)


at2 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,true_power.fun=true_power.fun,actually_trueC=actually_trueC,actual_powerC=actual_powerC,actual_costC=actual_costC)

save(at2,file= paste0(folder,"at_2.Rdata"))



# Function 3 --------------------------------------------------------------

  x1 = load.cond(3,"B",2000,.05)
  design=x1$design
  cost=x1$cost

  # Simulation zur Ermittlung der Power in allen Integer Orten siehe fun6_sim.R
  load(file= paste0(folder,"res_fun3sim1.Rdata")) # loads "res"

  # calculate by surrogate model
  dat = res
  dat = lapply(res,function(x) {
    t1 = list()
    t1$x=x$val
    names(t1$x) = c("n")
    t1$y = x$long
    return(t1)
  })

  re = gauss.pred(dat=dat,design=design,cost=cost,goal=.8)
  true_power.fun = re$fun

  # For Task B (Minimal Power = .8)
  goal=.8

  for (i in design[[1]][1]:design[[1]][2]) {
    if(true_power.fun(i)>goal) {
      actually_true = i
      actual_power = true_power.fun(actually_true)
      actual_cost = cost(actually_true)
      break
    }
  }

  at3 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,true_power.fun=true_power.fun)

  save(at3,file= paste0(folder,"at_3.Rdata"))


  # if (FALSE) { # Calculate Actually true value
  #   runfun = runfun.skewed2(delta = .4,alpha =10)
  #   goal = .8
  #   goal.ci = .001
  #   design = list(n = c(50,200))
  #   setsize= 1000
  #   n.startsets= 10
  #
  #   re1 =  ss.find(logi.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,setsize=setsize,n.startsets=n.startsets,seed=1,limitmaxbudget=FALSE)
  #   actually_true = re1$value |> as.numeric()
  #
  #   a_102 = replicate(10^6,runfun(102))
  #   a_103 = replicate(10^6,runfun(103))
  #
  #   at_3_data = list(re1=re1,a_102=a_102,a103=a103)
  #   save(at_3_data,file= paste0(folder,"at_3_data.Rdata"))
  # }
  #
  #   load(file= paste0(folder,"at_3_data.Rdata"))
  #
  #   actually_true = 103
  #   actual_power = mean(at_3_data$a_103)
  #   actual_cost=actually_true
  #
  #   at3 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost)
  #
  #   save(at3,file= paste0(folder,"at_3.Rdata"))
  #


# Function 4 --------------------------------------------------------------


  x1 = load.cond(4,"B",2000,.05)
  design=x1$design
  cost=x1$cost

  # Simulation zur Ermittlung der Power in allen Integer Orten siehe fun6_sim.R
  load(file= paste0(folder,"res_fun4sim1.Rdata")) # loads "res"

  # calculate by surrogate model
  dat = res
  dat = lapply(res,function(x) {
    t1 = list()
    t1$x=x$val
    names(t1$x) = c("n")
    t1$y = x$long
    return(t1)
  })

  re = gauss.pred(dat=dat,design=design,cost=cost,goal=.8)
  true_power.fun = re$fun

  # For Task B (Minimal Power = .8)
  goal=.8

  for (i in design[[1]][1]:design[[1]][2]) {
    if(true_power.fun(i)>goal) {
      actually_true = i
      actual_power = true_power.fun(actually_true)
      actual_cost = cost(actually_true)
      break
    }
  }

  at4 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,true_power.fun=true_power.fun)

  save(at4,file= paste0(folder,"at_4.Rdata"))


  # if (FALSE) { # Calculate Actually true value
  #
  # itempars = runfun.irt.itempars(delta=.1,n.items = 20,seed=1)
  # runfun = runfun.irt(itempars)
  # goal = .8
  # goal.ci = .001
  # design = list(n = c(1398,1402))
  # setsize= 1000
  # n.startsets= 5
  #
  # re1 =  ss.find(logi.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,setsize=setsize,n.startsets=n.startsets,seed=1)
  #
  # at_4_data = list(re1=re1)
  # save(at_4_data,file= paste0(folder,"at_4_data.Rdata"))
  # }
  #
  # load(file= paste0(folder,"at_4_data.Rdata"))

  # actually_true = at_4_data$re1$value
  # actually_true = 1400
  # actually_true = 150
  # # actual_power = at_4_data$re1$value.y
  # actual_power = .8
  # actual_cost=actually_true
#
#   at4 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost)
#
#   save(at4,file= paste0(folder,"at_4.Rdata"))

# Function 5 --------------------------------------------------------------

  x1 = load.cond(5,"B",2000,.05)
  design=x1$design
  cost=x1$cost

  # Simulation zur Ermittlung der Power in allen Integer Orten siehe fun6_sim.R
  load(file= paste0(folder,"res_fun5sim1.Rdata")) # loads "res"

  # calculate by surrogate model
  dat = res
  dat = lapply(res,function(x) {
    t1 = list()
    t1$x=x$val
    names(t1$x) = c("n")
    t1$y = x$long
    return(t1)
  })

  re = gauss.pred(dat=dat,design=design,cost=cost,goal=.8)
  true_power.fun = re$fun

  # For Task B (Minimal Power = .8)
  goal=.8

  for (i in design[[1]][1]:design[[1]][2]) {
    if(true_power.fun(i)>goal) {
      actually_true = i
      actual_power = true_power.fun(actually_true)
      actual_cost = cost(actually_true)
      break
    }
  }

  at5 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,true_power.fun=true_power.fun)

  save(at5,file= paste0(folder,"at_5.Rdata"))


  # if (FALSE) { # Calculate Actually true value
  #
  # runfun = runfun.simr()
  # goal = .8
  # goal.ci = .001
  # design = list(n = c(10,20))
  # setsize= 1000
  # n.startsets= 10
  #
  # re1 =  ss.find(logi.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,setsize=setsize,n.startsets=n.startsets,seed=1)
  #
  # at_5_data = list(re1=re1)
  # save(at_5_data,file= paste0(folder,"at_5_data.Rdata"))
  # }
  #
  # load(file= paste0(folder,"at_5_data.Rdata"))
  #
  # actually_true = at_5_data$re1$value
  # # actually_true = 15.9298
  # actually_true = 16
  # # actual_power = at_5_data$re1$value.y
  # actual_power = .8
  # actual_cost = actually_true
  #
  # at5 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost)
  #
  # save(at5,file= paste0(folder,"at_5.Rdata"))


  # Function 6 --------------------------------------------------------------

  x1 = load.cond(6,"C",2000,.05)
  design=x1$design
  cost=x1$cost
  fixed_cost=x1$fixed_cost
#
#   design = list(n = c(5,50),k=c(3,30))
  # cost = function(x) x[1]*10+x[2]*5
#   cost = function(x) x[1]*x[2]*5+x[2]*10
#   fixed_cost = 1000

  # Simulation zur Ermittlung der Power in allen Integer Orten siehe fun6_sim.R
  load(file= paste0(folder,"res_fun6sim1.Rdata")) # loads "res"
  res1 =res
  load(file= paste0(folder,"res_fun6sim2.Rdata")) # loads "res"
  res2 = res

  # intensivere Simulation bei Kandidatenwerten.
  load(file= paste0(folder,"res_fun6sim_2_2.Rdata")) # loads "res"
  res3 = res

  res = c(res1,res2)


  # integrieren der Ergebnisse der intensiveren Simulation
  for (i in 1:length(res3)) {
    ind = sapply(res,function(x)all(x$val==res3[[i]]$val))
    res[[which(ind)]]$long = c(res[[which(ind)]]$long,res3[[i]]$long)
  }

  test = sapply(res,function(x)length(x$long))
  table(test)

# calculate by surrogate model
  dat = res
  dat = lapply(res,function(x) {
    t1 = list()
    t1$x=x$val
    names(t1$x) = c("n","k")
    t1$y = x$long
    return(t1)
  })
  # design = list(n = c(5,30),k=c(5,30))
  re = gauss.pred(dat=dat,design=design,cost=cost,goal=.8)
  true_power.fun = re$fun

  # For Task B (Minimal Power = .8)
  goal=.8
  cands = list()
  ks = seq(design$k[1],design$k[2])
  for (i in 1:length(ks)) {
    k = ks[i]
    n = 1
    while (true_power.fun(c(n,k))<goal&n<=design$n[2]) n = n+1
    if (true_power.fun(c(n,k))>goal) cands[[i]] = c(n,k) else cands[[i]] = NA
  }

  costs = sapply(cands,cost)
  ind = which(costs == min(costs,na.rm=T))
  cands2 = cands[ind]
  powers2 = sapply(cands2,true_power.fun)
  ind2 = which(powers2 == max(powers2,na.rm=T))

  actually_true = cands2[[ind2]]
  actual_power = true_power.fun(actually_true)
  actual_cost = cost(actually_true)

  # For Task C
  cands = list()
  ks = seq(design$k[1],design$k[2])
  for (i in 1:length(ks)) {
    k = ks[i]
    n = 1
    while (cost(c(n+1,k))<=fixed_cost&n<=design$n[2]) n = n+1
    cands[[i]] = c(n,k)
  }

  powers = sapply(cands,true_power.fun)
  ind = which(powers == max(powers))
  actually_trueC = cands[[ind]]
  actual_powerC = true_power.fun(actually_trueC)
  actual_costC = cost(actually_trueC)


  at6 = list(actually_true=actually_true,actual_power = actual_power,actual_cost=actual_cost,actually_trueC=actually_trueC,actual_powerC=actual_powerC,actual_costC=actual_costC,true_power.fun=true_power.fun)

  save(at6,file= paste0(folder,"at_6.Rdata"))



# # calculate with actual data
#
#   powers = sapply(res, function(x) x$pow)
#   costs = sapply(res, function(x) cost(x$val))
#   vals = lapply(res, function(x) x$val)
#
#   # For Task B
#   cands = powers>.8
#   optcost = min(costs[cands])
#   ind = which(costs==optcost&cands)
#   actually_true = res[[ind]]$val
#   actual_power = res[[ind]]$pow
#   actual_cost = cost(actually_true)
# #
# #   # For Task C
#   cands = costs<=350
#   optpower = max(powers[cands])
#   ind = which(powers==optpower&cands)
#   actually_trueC = res[[ind]]$val
#   actual_powerC = res[[ind]]$pow
#   actual_costC = cost(actually_trueC)
#
#   true_power.fun = function() {
#
#     load(file= paste0(folder,"res_fun6sim2.Rdata")) # loads "res"
#
#     powers = sapply(res, function(x) x$pow)
#     vals = lapply(res, function(x) x$val)
#
#     fn = function(x) {
#
#     if (is.na(x[1])) return(NA)
#
#     ind = sapply(vals,function(y) all(y==x))
#     re = powers[ind]
#     return(re)
#     }
#     return(fn)
#   }


# Save all in one file ----------------------------------------------------

  # at3 = at5= "a"

for (i in 1:6) {
  load(file= paste0(folder,"at_",i,".Rdata"))
}
at = list(at1,at2,at3,at4,at5,at6)

save(at,file= paste0(folder,"at.Rdata"))



