#' Display info on the generated data
#'
#' Output a data frame that, for each set of design parameters, includes the cost, the estimated power and SE, the surrogate model estimated power and SE, and the number of performed evaluations.
#'
#' The raw power is estimated using the ratio of significant hypothesis tests among the performed evaluations. The raw SE is estimated using \eqn{p(1-p)/n} with \eqn{p} being the raw power and \eqn{n} being the number of performed evaluations.
#'
#' For the estimation of SE using a surrogate model, GPR is employed because it is capable of variance estimation, unlike the other surrogate models.
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

  datx = todataframe(dat)
  xvars <- datx[, 1:(length(datx) - 1), drop = FALSE]
  # use user-defined design parameter names
  names(xvars) = names(ds$boundaries)

  # design pars, cost, power_surrogate, SE_surrogate, power, SE
  re = list()
  re$cost = apply(xvars,1,costfun)
  re$power_surrogate = apply(xvars,1,fitfun)
  re$SE_surrogate = apply(xvars,1,fitfun.sd)
  re$power = datx$y
  re$SE = getweight(dat, weight.type = "sd")
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


