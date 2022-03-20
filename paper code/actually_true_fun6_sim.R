
install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)

library(simpackage)
load.libs()

CLUSTERSIZE = 120

runseach= 10^4

runfun = runfun.simr2()
x1 = load.cond(6,"C",2000,.05)
design=x1$design


a = expand.grid(design[[1]][1]:design[[1]][2],design[[2]][1]:design[[2]][2])
sim = list()
for (i in 1:nrow(a)) {
  re = list()
  re$val = as.numeric(a[i,])
  sim[[i]] = re
}

for (i in 1:length(sim)){
  sim[[i]]$seed = i
}

# load data and delete what has already been calculated
load(file= "res_fun6sim2.Rdata") # loads "res"

vals = lapply(res,function(x) x$val)

  use=c()
  for (i in 1:length(sim)) {
    x = sim[[i]]$val
    ind = sapply(vals,function(y) all(y==x))
    use = c(use,!any(ind))
  }
  sim = sim[use]


# Run on the server -------------------------------------------------------

cl <- makeCluster(CLUSTERSIZE)

clusterExport(cl, objects(), envir=environment())

res =  parLapply(cl,X = sim,fun = function(x){

  library(simpackage)
  load.libs()

  set.seed(x$seed)

  res = replicate(runseach,runfun(x$val))

  re = list(long=res,val=x$val,seed=x$seed,runs=length(res),pow=mean(res))

  return(re)

})

stopCluster(cl)


res_string=paste0("res_fun6sim")
i = 1
file_string =paste0(res_string,i,".Rdata")
while(file.exists(file_string)) {
  i=i+1
  file_string =paste0(res_string,i,".Rdata")
}

save(res,file= file_string)



# additional --------------------------------------------------------------


# additional simulation to identify candidates more thoroughly!

# running additional analysis for values in the range .75 - .85

# which are in the range .75 - .85? and have good cost?
if (F) {

  load(file= paste0(folder,"at.Rdata"))
  at = at[[6]]
  true_power.fun = at$true_power.fun
  costfun=x1$cost
  x1 = load.cond(6,"B",1234124,NA)
  design=x1$design
  design
  cands = list()
  for (k in 3:30) {
    for (n in 5:50) {
      val = true_power.fun(c(n,k))
      cost = costfun(c(n,k))
      if (val<.85 & val>.75 & cost <350) cands = c(cands,list(c(n,k)))
    }
  }

  save(cands,file="fun6_sim2_cands.Rdata")

}

# load the close candidates to a power of .8 and low cost!
load(file="fun6_sim2_cands.Rdata")

install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)

library(simpackage)
load.libs()

CLUSTERSIZE = 120
runfun = runfun.simr2()

runseach= 10^5
runspernode = 10^4
multiply = runseach/runspernode

sim = lapply(cands,function(x) {
  re=list()
  re$val = as.numeric(x)
  return(re)
})

sim = rep(sim,each=multiply)

for (i in 1:length(sim)){
  sim[[i]]$seed = 2000+ i # increased seed to avoid overlaps with original (fun6sim ends with seed 1288)
}

print(length(sim))


# Run on the server -------------------------------------------------------

cl <- makeCluster(CLUSTERSIZE)

clusterExport(cl, objects(), envir=environment())


res =  parLapplyLB(cl,X = sim,chunk.size =1,fun = function(x){

  library(simpackage)
  load.libs()
  set.seed(x$seed)

  res = replicate(runspernode,runfun(x$val))

  re = list(long=res,val=x$val,seed=x$seed,runs=length(res),pow=mean(res))

  return(re)

})

stopCluster(cl)


#save file with new label
res_string=paste0("res_fun6sim_2_")
i = 1
file_string =paste0(res_string,i,".Rdata")
while(file.exists(file_string)) {
  i=i+1
  file_string =paste0(res_string,i,".Rdata")
}

save(res,file= file_string)



