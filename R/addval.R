


addval = function(simfun,dat=list(),points=NULL,each=1,minrun=F,autosave_dir=NULL) {

  xvalues = sapply(dat,function(y) digest::digest(as.numeric(y$x)))

  for (i in 1:nrow(points)) {

    ind = which(xvalues==digest::digest(as.numeric(points[i,])))

    if(length(ind)==0) {
      ind = length(dat)+1
      dat[[ind]] = list()
      dat[[ind]]$x = points[i,]
      resx = c()
    } else {
      resx = dat[[ind]]$y
    }
    a = as.numeric(points[i,])

    resx = c(resx,replicate(each,suppressMessages(hush(simfun(as.numeric(points[i,]))))))

    while(minrun && length(resx)> 1 && stats::var(resx) == 0) {
      resx = c(resx,replicate(1,suppressMessages(hush(simfun(as.numeric(points[i,]))))))
    }
    dat[[ind]]$y = resx
  }

  if(!is.null(autosave_dir)) save(dat,file=paste0(autosave_dir,"dat_autosave.RData"))

  return(dat)

}




