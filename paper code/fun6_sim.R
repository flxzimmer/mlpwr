
install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)

library(simpackage)
load.libs()

CLUSTERSIZE = 120

runseach= 10^4
# runseach= 10

# variance at p=.8 is:

v11=.8*(1-.8)/runseach
.8023547 - sqrt(v11)


runfun = runfun.simr2()
# design = list(n = c(5,30),k=c(5,30))
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


  print(length(sim))

# Run on the server -------------------------------------------------------

cl <- makeCluster(CLUSTERSIZE)

clusterExport(cl, objects(), envir=environment())

res =  parLapply(cl,X = sim,fun = function(x){

  library(simpackage)
  load.libs()

  # x = sim[[1]]
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


# merge existing files ----------------------------------------------------


