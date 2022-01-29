

# additional simulation to clear up candidates!

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

# (pilot)
# runseach= 10
# runspernode = 10
# multiply = 1

sim = lapply(cands,function(x) {
  re=list()
  re$val = as.numeric(x)
  return(re)
  })

sim = rep(sim,each=multiply)

for (i in 1:length(sim)){
  sim[[i]]$seed = 2000+ i # erhöhter Seed damit keine Überschneidungen mit original entstehen können(fun6sim endet mit seed 1288)
}

print(length(sim))



# Run on the server -------------------------------------------------------

cl <- makeCluster(CLUSTERSIZE)

clusterExport(cl, objects(), envir=environment())


res =  parLapplyLB(cl,X = sim,chunk.size =1,fun = function(x){

  library(simpackage)
  load.libs()

  # x = sim[[1]]
  set.seed(x$seed)

  res = replicate(runspernode,runfun(x$val))

  re = list(long=res,val=x$val,seed=x$seed,runs=length(res),pow=mean(res))

  return(re)

})

stopCluster(cl)


res_string=paste0("res_fun6sim_2_")
i = 1
file_string =paste0(res_string,i,".Rdata")
while(file.exists(file_string)) {
  i=i+1
  file_string =paste0(res_string,i,".Rdata")
}

save(res,file= file_string)


# merge existing files ----------------------------------------------------


