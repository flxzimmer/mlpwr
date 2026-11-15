#' Display info on the generated data
#'
#' Output a data frame that, for each set of design parameters, includes the cost, the estimated power and SE, the surrogate model estimated power and SE, and the number of performed evaluations.
#'
#' The raw power is estimated using the ratio of significant hypothesis tests among the performed evaluations (or, more generally, the `aggregate_fun` of the search). The raw SE is estimated using \eqn{p(1-p)/n} with \eqn{p} being the raw power and \eqn{n} being the number of performed evaluations; if the search used a `noise_fun` function, the raw SE is the square root of its variance instead.
#'
#' For the estimation of SE using a surrogate model, GPR is employed because it is capable of variance estimation, unlike the other surrogate models. The surrogate SE is `NA` if no GPR SD is available (e.g. with `use_noise = FALSE`).
#'
#' @param ds Object of class designresult as created by the find.design function
#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' # Load a simulation function
#' simfun <- example.simfun('ttest')
#' # Perform the search
#' ds <- find.design(simfun = simfun, boundaries = c(100,300), power = .95)
#' # Display more info on the collected data
#' simulations_data(ds)
#'
simulations_data <- function(ds) {

  dat = ds$dat
  costfun = ds$costfun
  fitfun = ds$fit$fitfun
  fitfun.sd = ds$fit$fitfun.sd

  aggregate_fun = ds$aggregate_fun
  if (is.null(aggregate_fun)) aggregate_fun = mean

  datx = todataframe(dat, aggregate_fun = aggregate_fun)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
  # use user-defined design parameter names
  names(xvars) = names(ds$boundaries)

  # design pars, cost, power_surrogate, SE_surrogate, power, SE
  re = list()
  re$cost = apply(xvars,1,costfun)
  re$power_surrogate = apply(xvars,1,fitfun)
  # NA if the surrogate has no SD function (e.g. use_noise = FALSE)
  re$SE_surrogate = if (is.null(fitfun.sd)) NA_real_ else apply(xvars,1,fitfun.sd)
  re$power = datx$y
  # with a noise_fun function, the SE is the square root of its variance
  # (as used by the search); otherwise the binomial SE of power
  noise_fun = ds$settings$noise_fun
  re$SE = if (is.function(noise_fun)) sqrt(vapply(dat, noise_fun, numeric(1))) else
    getweight(dat, weight.type = "sd")
  re$Evaluations = getweight(dat, weight.type = "freq")

  re = as.data.frame(re)
  re = cbind(xvars,re)

  # kick cost if redundant
  if (ncol(xvars) == 1 && all(re$cost==re[,1])) re = re[,-2]

  # sort
  re = re[order(re$power),]
  # re = re[do.call(order, xvars[colnames(xvars)]),, drop = FALSE]
  rownames(re) <- seq(nrow(re))

  return(re)
}


