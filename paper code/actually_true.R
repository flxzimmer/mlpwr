
# calculating the actual power corresponding to the design parameter sets in the different DGFs

# The results are used in the analysis of the simulation study

# the "actually true" value is the value that implies the lowest cost among values with power > .8

# the actual power is the power of the actually true parameter set

# some DGFs needed larger simulations for this, the files for these are respectively referred to.

install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)
library(simpackage)
load.libs()
folder = paste0(getwd(),"/results data/") # File Location of Results

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
true_power.fun = runfun.anova.true()

x1 = load.cond(2,"C",2000,.05)
design=x1$design
cost=x1$cost
fixed_cost=x1$fixed_cost


# For Task B (Minimal Power)
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


# For Task C (Maximum Costs)
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

  # Larger Simulation, see actually_true_fun3_sim.R
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


# Function 4 --------------------------------------------------------------


  x1 = load.cond(4,"B",2000,.05)
  design=x1$design
  cost=x1$cost

  # Larger Simulation, see actually_true_fun4_sim.R
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


# Function 5 --------------------------------------------------------------

  x1 = load.cond(5,"B",2000,.05)
  design=x1$design
  cost=x1$cost

  # Larger Simulation, see actually_true_fun5_sim.R
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


  # Function 6 --------------------------------------------------------------

  x1 = load.cond(6,"C",2000,.05)
  design=x1$design
  cost=x1$cost
  fixed_cost=x1$fixed_cost

  # Larger Simulation in two parts, see actually_true_fun6_sim.R
  load(file= paste0(folder,"res_fun6sim1.Rdata")) # loads "res"
  res1 = res

  # more intensive simulation at candidate values.
  load(file= paste0(folder,"res_fun6sim2.Rdata")) # loads "res"
  res2 = res

  res = res1


  # integrate the results of the more intensive simulation
  for (i in 1:length(res2)) {
    ind = sapply(res,function(x)all(x$val==res2[[i]]$val))
    res[[which(ind)]]$long = c(res[[which(ind)]]$long,res2[[i]]$long)
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



# Save all in one file ----------------------------------------------------

for (i in 1:6) {
  load(file= paste0(folder,"at_",i,".Rdata"))
}
at = list(at1,at2,at3,at4,at5,at6)

save(at,file= paste0(folder,"at.Rdata"))



