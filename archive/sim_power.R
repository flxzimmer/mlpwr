#' Title
#'
#'  Basic function to estimate power at a sample size.
#'
#' @param model
#' @param runfun
#' @param n.pers
#' @param max.runs
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
sim_power = function(model=NULL,runfun, n.pers,max.runs = 10,alpha = .05,showprogress =FALSE) {

  #generate some data with the parameters from the alternative model and perform the test


  if (!is.null(model)) {
    runfunc  = function(n) runfun(n=n,model=model)<.05
  } else {
    runfunc = function(n) runfun(n=n)<.05
  }

  res = c()

  # progress bar
  if (showprogress) { pb <- txtProgressBar(min = 0, max = max.runs, style = 3)}

  for (i in 1:max.runs) {
    res[i] = runfunc(n.pers)
    if (showprogress) {setTxtProgressBar(pb, i)}
  }
  if (showprogress) {close(pb)}

  p = mean(res)

  #Confidence Interval
  q = 1-p
  ci = sqrt(max.runs*p*q)/max.runs * qnorm(.975)
  ci = p + c(-1,1) * ci
  ci[ci<0] = 0
  ci[ci>1] = 1

  re = list(power=p,ci=ci)

  return(re)

}




