
install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)

library(simpackage)
load.libs()

CLUSTERSIZE = 120
n.runs = 100

fun_nr = c(2)
task = c("B")
# budget = "NULL"
# goal.ci = c("low","mid","high")
budget = c("low","mid","high")
goal.ci = NA
sim2 = expand.grid(run=1:n.runs,fun_nr=fun_nr,task=task,budget = budget,goal.ci = goal.ci)
sim2 = sim2  %>% split(., seq(nrow(.))) %>% lapply(.,as.list)

fun_nr = c(2)
task = c("C")
# budget = "NULL"
# goal.ci = c("low","mid","high")
budget = c("low","mid","high")
goal.ci = NA
sim3 = expand.grid(run=1:n.runs,fun_nr=fun_nr,task=task,budget = budget,goal.ci = goal.ci)
sim3 = sim3  %>% split(., seq(nrow(.))) %>% lapply(.,as.list)

sim = c(sim2,sim3)

for (i in 1:length(sim)){
  sim[[i]]$seed = i
}

set.seed(1)
pilotseeds = sample(1:length(sim),120)

notpilot = !(1:length(sim) %in% pilotseeds)
# sim =sim[pilotseeds]
sim =sim[notpilot]

# Run on the server -------------------------------------------------------

cl <- makeCluster(CLUSTERSIZE)

clusterExport(cl, objects(), envir=environment())

res =  parLapplyLB(cl,X = sim,chunk.size =1,fun = function(x){

  library(simpackage)
  load.libs()

  # x = sim[[120]]

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



# diagnostics -------------------------------------------------------------
if (F) {

  # 1D Plot
  plot.res = function(re,fun=NULL) {
    dat = re$data
    if(is.null(fn)) fun = re$fun
    dat.obs = todataframe(dat)
    ns = seq(min(dat.obs[,1]/2),max(dat.obs[,1])*1.5)
    p2 = data.frame(n =ns,y=sapply(ns,fun))
    pl1 = ggplot(p2, aes(n,y)) +
      geom_line() +
      geom_point(data=dat.obs,aes(x =V1,y = y))
    pl1
  }
  plot.res.sd = function(re,fun=NULL,predfun=NULL) {
    dat = re$data
    if(is.null(fn)) fun = re$fun.sd
    dat.obs = todataframe(dat)
    dat.pred = sapply(dat.obs$V1,predfun)
    err = abs(dat.pred-dat.obs$y)
    dat.obs$error = err
    ns = seq(min(dat.obs[,1]/2),max(dat.obs[,1])*1.5)
    p2 = data.frame(n =ns,y=sapply(ns,fun))
    pl1 = ggplot(p2, aes(n,y)) +
      geom_line()+
      geom_point(data=dat.obs,aes(x=V1,y = err))
    pl1
  }

  re =  res[[868]][[4]]
  fun = logi.pred(re$data,goal=.8)$fun
  sefun = logi.pred(re$data,goal=.8)$fun.sd

  plot.res(re,fun)
  plot.res.sd(re,sefun,fun)




  re = re3
  dat = re$data
  datx = todataframe(dat)
  fun = re$fun

  re1 = svm.pred(dat,goal=goal)
  re2= gauss.pred(dat,goal=goal)
  fun = re1$fun


  fig1 = plot_ly(x=datx$V1, y=datx$V2, z=datx$y,type='scatter3d')
  ns = seq(min(datx$V1),max(datx$V1),10)
  ks = seq(min(datx$V2),max(datx$V2),4)
  x = expand.grid(ns,ks)
  preddat = matrix(apply(x,1,fun),ncol=length(ns),byrow=TRUE)
  fig2 <- add_surface(fig1,x=ns, y=ks, z=preddat)
  fig2



  re1$value;re2$value;re3$value;re4$value
  set.seed(seed)
  # dat = addval(runfun,design=design,n.points=n.startsets,each=setsize)
  # dat = re2$data
  # fn2 = re4$fun
  dat = startdat
  # dat=re3$data
  dat = res[[62]][[6]]$data

  re1 = svm.pred(dat,goal=goal)
  re2 = gauss.pred(dat,goal=goal)
  # re1 = logi.pred(dat,goal=goal)
  # re1 = reg.pred(dat,goal=goal)


  fn1 = re1$fun
  fn2 = re2$fun
  # fn2 = re$fun



  # Observed data
  dat.obs = todataframe(dat)
  # n Values
  ns = seq(min(dat.obs[,1]/2),max(dat.obs[,1])*1.5)
  # Predictions
  p1 = data.frame(n =ns,y=sapply(ns,fn1),type="1")
  p2 = data.frame(n =ns,y=sapply(ns,fn2),type="2")
  # p3 = data.frame(n =ns,y=sapply(ns,fn3),type="3")

  dat1 = rbind(p1,p2)
  dat.obs$type=NA

  pl1 = ggplot(dat1, aes(n,y, group=type)) +
    geom_line(aes(color=type)) +
    geom_point(data=dat.obs,aes(x =V1,y = y))
  pl1

}

# collect seeds and save (If errors) -------------------------------------------------

if (F) {

files=list.files("Y:/seeds/")

res =list()
#Save run"Y:"
for (i in 1:length(files)) {
load(file= paste0("Y:/seeds/",files[i]))
res[[i]] = re
}

#Save result in new file
# res_string=paste0("Y:/res_",simcond,"_")
# i = 1
# file_string =paste0(res_string,i,".Rdata")
# while(file.exists(file_string)) {
#   i=i+1
#   file_string =paste0(res_string,i,".Rdata")
# }
# save(res,file= file_string)

res_string=paste0("Y:/res_")
i = 24
file_string =paste0(res_string,i,".Rdata")
while(file.exists(file_string)) {
  i=i+1
  file_string =paste0(res_string,i,".Rdata")
}

save(res,file= file_string)

}



