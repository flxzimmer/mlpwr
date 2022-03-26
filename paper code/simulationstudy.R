
install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)
library(simpackage)
load.libs()
folder = paste0(getwd(),"/results data/") # File Location of Results

CLUSTERSIZE = 32
n.runs = 25 # runs per cluster (4 clusters were used for a total of 100 runs per condition)

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


# set random seed (systime)
# systime = Sys.time()
# file_string = paste0("time_",systime,".Rdata")
# save(systime,file= file_string)
# set.seed(as.integer(systime))


# actually used seeds
# seed = 1643483479 # f1
# seed = 1643483500 # f2
# seed = 1643483526 # f3
# seed = 1643483513 # f4

set.seed(seed)

# add seeds
for (i in 1:length(sim)){
  sim[[i]]$seed = sample(1:10^8, 1)
}

#shuffle conditions
sim = sim[sample(1:length(sim))]


# Run on the server -------------------------------------------------------

cl <- makeCluster(CLUSTERSIZE)

clusterExport(cl, objects(), envir=environment())

res =  parLapplyLB(cl,X = sim,chunk.size =1,fun = function(x){

  library(simpackage)
  load.libs()

  fun_nr = x$fun_nr
  task = x$task
  budget = x$budget
  goal.ci = x$goal.ci
  seed = x$seed
  x1 = load.cond(fun_nr,task,budget,goal.ci)

  runfun=x1$runfun
  design=x1$design
  cost=x1$cost
  budget=x1$budget
  goal.ci=x1$goal.ci
  analytical=x1$analytical
  goal=x1$goal
  fixed_cost=x1$fixed_cost

  # run ---------------------------------------------------------------------

  re0 = list(value=analytical,value.sd = NA,data = NA,budget=NA,fun=NA,fun.sd =NA)

  startdat= ss.find(reg.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,seed=seed,cost=cost,dat.only=T)

  re1=re2=NULL
if (!fun_nr%in%c(2,6)) {
  re1 =  ss.find(reg.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,seed=seed,cost=cost,dat=startdat)
  re2 =  ss.find(logi.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat)
}
  re3 =  ss.find(svm.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat,fixed_cost=fixed_cost)
  re4 =  ss.find(gauss.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat,fixed_cost=fixed_cost)

  re = list(x,re0,re1,re2,re3,re4)

  # Delete funs (smaller file)
  re = lapply(re,function(y) return(y[names(y)!="fun.sd"]))
  re = lapply(re,function(y) return(y[names(y)!="fun"]))

  file_string=paste0("seeds/seed_",seed,"_fun_",fun_nr,"_task",task,".Rdata")
  save(re,file= file_string)

  return(re)

})

stopCluster(cl)

res_string=paste0("res_")
i = 24
file_string =paste0(res_string,i,".Rdata")
while(file.exists(file_string)) {
  i=i+1
  file_string =paste0(res_string,i,".Rdata")
}

save(res,file= file_string)


