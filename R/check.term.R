

check.term = function(runs,ci,time,dat,time_temp){

  re = FALSE

  # check termination (runs)
  if (!is.null(runs)) {
    used = usedruns(dat)
    runs.remaining = runs - used
    if(runs.remaining<=0) re=TRUE
    print(runs.remaining)
  }

  # check termination (CI)
  if (!is.null(ci)) {

    if(!is.na(goal.ci)&& n.iter>0&useprediction) {

      if (use_data_sd) new.n.sd = get.sd(dat,fit$new.n)

      if (!use_data_sd) { #Use SD from the learner
        if(is.na(fit$new.n.sd)){ # Generate from GP if missing
          fit2 = tryCatch(gauss.fit(dat = dat,goal=goal,carryover = fit$carryover,design=design,fixed_cost=fixed_cost,cost=cost,...),error=function(x) {return(x)})
          new.n.sd = fit2$fun.sd(fit$new.n)
        } else {
          new.n.sd = fit$new.n.sd
        }
      }

      if(new.n.sd*qnorm(CI+(1-CI)/2)<goal.ci) re=TRUE
    }
  }


  # check termination (time)
  if (!is.null(time)) {
    time_used = timer(time_temp,detailled=F)
    if(time_used>time) re=TRUE
  }


return(re)


}
