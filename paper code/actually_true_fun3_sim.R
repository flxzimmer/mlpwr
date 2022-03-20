
install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)

library(simpackage)
load.libs()

CLUSTERSIZE = 60

runseach= 10^4

fun_nr = 3

x1 = load.cond(fun_nr,"B",2000,.05)
runfun = x1$runfun
design=x1$design


a = design[[1]][1]:design[[1]][2]
sim = list()
for (i in 1:length(a)) {
  re = list()
  re$val = as.numeric(a[i])
  sim[[i]] = re
}

for (i in 1:length(sim)){
  sim[[i]]$seed = i
}


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


res_string=paste0("res_fun",fun_nr,"sim")
i = 1
file_string =paste0(res_string,i,".Rdata")
while(file.exists(file_string)) {
  i=i+1
  file_string =paste0(res_string,i,".Rdata")
}

save(res,file= file_string)



