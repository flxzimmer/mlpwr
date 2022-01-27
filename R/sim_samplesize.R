


# find_ss = function() {
#
#   fit.model() # Function to fit the model from the data
#   prediction() # Function to generate a prediction from the fitted model
#
#   # finding loop:
#   # Check if the created dataset fullfills the condition
#     # yes: break and report the result + Number of evaluations
#     # no: Fit Model,generate prediction,Evaluate next datapoint
#
#
#   return(re)
# }



#' Title
#'
#' Wrapper function for different methods to approximate necessary sample size.
#'
#' @param model
#' @param runfun
#' @param max.runs
#' @param tol
#' @param power
#' @param guess
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
sim_samplesize = function(model=NULL,runfun,max.runs = 500, tol = 0, power=.8,guess = 500,alpha=.05,showprogress =FALSE) {

  # uses fit of exponential cdf to find optimal sample size

  pwfunc = function(l,x) 1-exp(-l*x)
  persfunc = function(l,p) -1/l*log(1-p)
  ratefunc = function(x,p) -1/x*log(1-p)

  if (!is.null(model)) {
  runfunc  = function(n) runfun(n=n,model=model)<.05
  } else {
  runfunc = function(n) runfun(n=n)<.05
}

  pers = c() # Vector of all sample sizes
  sig = c() # Vector of all results

  width = 10^20 # Initialize width of the samplesizeCI
  i = 1 # Initialize run index
  rateCI = 1 # Initialize Rate Confidence Interval

  # progress bar
  if (showprogress) { pb <- txtProgressBar(min = 0, max = max.runs, style = 3)}

  while (i <= max.runs & width > tol) {
    # loop until widht<tol or max.runs is reached

    # Add prediction, perform next run
    pers = c(guess,pers)
    sig = c(runfunc(pers[1]),sig)

    #use fitting as soon as there are both 1s and 0s
    if (i>1 & var(sig) != 0) {

      # fit an exponential distribution and predict sample size for the desired power
      # startvalue sollte so sein, dass er bei der start sample size .8 ergibt - Nach dem ersten Durchgang identisch zur Rate
      startval = ratefunc(x = pers[1],p = power)
      fit <- nls(sig ~ pwfunc(l,pers), start = list(l = startval))
      rate = as.numeric(coef(fit))
      guess = persfunc(l=rate,p=power)

      #Calculate width of the power CI
      se = summary(fit)$coefficients[2]
      rateCI = c(rate-1.96*se,rate+1.96*se)
      samplesizeCI = sort(persfunc(l=rateCI,p=power))
      width = diff(samplesizeCI)
      # Print some progress
      # if (i - 10 * floor(i/10)==0) {
      #   samplesizeCI = persfunc(l=rateCI,p=power)
      #   re = list(samplesize = pers[1],samplesizeCI = samplesizeCI,powerCI=powerCI)
      #   print(re)
      # }
    }
    if (guess>10^5) {return("Error, sample size probably very large (>100.000)")}
    i = i+1
    if (showprogress) {setTxtProgressBar(pb, i) }
  }

  if (showprogress) { close(pb) }

  powerCI = sort(pwfunc(l = rateCI,x=guess))

  data = data.frame(pers = pers,sig=sig)

  re = list(samplesize = pers[1],samplesizeCI = samplesizeCI,powerCI = powerCI,data = data,rate = rate)

  return(re)

}

