

timer = function(obj=NULL,printit = FALSE) {
  # usage:
  # a = timer()
  # timer(a)

  if (is.null(obj)) {
    obj <- proc.time()
    return(obj)
  }

  if (!is.null(obj)) {

    if (printit){
      timex = proc.time() - obj
      t = timex[3]
      a = paste(round(t/60/60,2),"Hours |",round(t/60,2),"Minutes |",round(t,2),"Seconds")
      print(a)
    }


    timex = proc.time() - obj
    return(timex[3])

    }
}



hush=function(code){

  os = Sys.info()['sysname']
  st = "/dev/null"
  if (os =="Windows") st  = "NUL"

  sink(st) # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}


usedruns = function(dat) {
  return(sum(sapply(dat, function(x) length(x$y))))
}



todataframe = function(dat,aggregate=TRUE,pseudo=FALSE) {

  dim.design=length(dat[[1]]$x)

  if (aggregate) {
  temp = t(sapply(dat,function(v) c(as.numeric(v$x),mean(as.numeric(v$y)))))
  }

  if (!aggregate & !pseudo) {
    temp = lapply(dat, function(v) {
      tempx = data.frame(y=v$y)
      for (i in 1:dim.design) {
        tempx[,i+1]=v$x[i]
      }
      return(tempx)
    })
    temp = do.call(rbind,temp)
    temp = temp[,c(2:length(temp),1)]
  }


  if (!aggregate & pseudo) {
    temp = lapply(dat, function(v) {
      tempx = data.frame(y=rep(mean(v$y),length(v$y)))
      for (i in 1:dim.design) {
        tempx[,i+1]=v$x[i]
      }
      return(tempx)
    })
    temp = do.call(rbind,temp)
    temp = temp[,c(2:length(temp),1)]
  }

  temp = apply(temp,2,as.numeric)
  temp = as.data.frame(temp)
  names(temp) = c(paste0("V",1:dim.design),"y")

  return(temp)
}




get.sd = function(dat,value) {

  ind = which(sapply(dat,function(ele) all(ele$x==value)))
  if (length(ind)==0) return(10)
  else {
    return(getweight(dat,"sd")[ind])
  }
}



getweight = function(dat,weight.type="freq",correct_zero=T) {

  # correct_zero Adds one number to the data if it consists only of 0s or only of 1s. Variance can only then be calculated.

  if(is.null(weight.type)) return(NULL)

  fun = function(vec,weight.type) {
    vec = as.numeric(vec)
    p = mean(vec)
    n = length(vec)
      if (correct_zero) {
        if (is.na(p==0)) browser()
        if (p==0) vec = c(vec,1)
        if (p==1) vec = c(vec,0)
        p = mean(vec)
    }
    variance = p * (1-p) / n

    if (weight.type == "freq") return(n)
    if (weight.type == "var") return(variance)
    if (weight.type == "inv_var") return(1/variance)
    if (weight.type == "sd") return(sqrt(variance))
    if (weight.type == "inv_sd") return(1/sqrt(variance))
  }

  w = sapply(dat,function(v) fun(v$y,weight.type))
  return(w)
}


print.progress = function(n_updates,runs_used,time_used) {
  # cat('\r',paste(c("Updates","Runs","Time"),c(n_updates,runs_used,round(time_used,1)),sep=": ",collapse=", "),"\n")
  cat('\r',paste(c("Updates","Runs","Time"),c(n_updates,runs_used,round(time_used,1)),sep=": ",collapse=", "))
  flush.console()
}


#
# while (1) {
#   cat('\r',format(Sys.time(),'%H:%M:%S'))
#   flush.console()
# }









