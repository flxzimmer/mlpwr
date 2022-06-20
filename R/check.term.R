

check.term = function(runs,ci,time,dat,time_temp,fit,pred,ci_perc){

  re = FALSE

  # check termination (runs)
  if (!is.null(runs)) {
    used = usedruns(dat)
    runs.remaining = runs - used
    if(runs.remaining<=0) re=TRUE
  }

  # check termination (ci)
  if (!is.null(ci)) {

    if (is.null(fit$fitfun.sd)) fit$fitfun.sd = fit.surrogate(dat = dat,surrogate="gpr")$fitfun.sd

    sdval = fit$fitfun.sd(as.numeric(pred$points.notgreedy))
    interval = sdval*qnorm(ci_perc+(1-ci_perc)/2)
    if(interval<ci) re=TRUE
  }

  # check termination (time)
  if (!is.null(time)) {
    time_used = as.numeric(timer(time_temp))
    if(time_used>time) re=TRUE
  }


return(re)

}
