#' Title
#'
#' @return
#' @export
#'
#' @examples
check.term = function(terminate,...){

re =FALSE

  # check termination (runs)
  if ("runs" %in% terminate) {
    used = usedruns(dat)

    if (limitmaxruns& used>15000) {warning("max runs used, didn't converge?");re=TRUE}
    if (!is.na(runs)){
      runs.remaining = runs - used
      if(runs.remaining<=0) re=TRUE
    }
  }

  # check termination (CI)
  if ("ci" %in% terminate) {

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
  if ("time" %in% terminate) {
    time_used = timer(time_temp,detailled=F)
    if(time_used>time) re=TRUE
  }


return(re)


}
