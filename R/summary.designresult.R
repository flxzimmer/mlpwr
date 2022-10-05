#' Summary of the search result
#'
#' Produce summary statistics of the results and the algorithm run.
#'
#' @param object Object of class designresult as created by the find.design function
#' @param ... additional arguments to be passed
#'
#' @return An object of class summary.designresult
#' @export
#'
#' @examples #Load a simulation function
#' simfun = example.simfun("ttest")
#' # Perform the search
#' ds = find.design(simfun = simfun, boundaries = c(100,300), power = .95)
#' # Output the results
#' summary(ds)
summary.designresult = function(object, ...) {

  ds = object
  class(ds)="summary.designresult"

  return(ds)
}




#' Print Summary of the search result
#'
#' @param x Object of class designresult as created by the find.design function
#' @param ... additional arguments to be passed
#'
#' @return An object of class summary.designresult
#' @export
#'
#' @examples #Load a simulation function
#' simfun = example.simfun("ttest")
#' # Perform the search
#' ds = find.design(simfun = simfun, boundaries = c(100,300), power = .95)
#' # Output the results
#' summary(ds)
print.summary.designresult = function(x, ...) {

  ds = x
  design = ds$final$design
  power = round(ds$final$power,5)
  se = round(ds$final$se,5)
  cost = round(ds$final$cost,2)
  evaluations = ds$evaluations_used
  timex = round(ds$time_used,2)
  updates = ds$n_updates
  surrogate = ds$surrogate

  surrogate = switch(surrogate,
         reg = "Linear regression",
         logreg = "Logistic regression",
         svr = "Support vector regression",
         gpr = "Gaussian process regression"
  )


  cat("\nCall:\n", paste(deparse(ds$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")

  cat("\nDesign: ", paste(names(design),design, sep = " = ", collapse = ", "),
      "\n",sep = "")

  cat("\nPower: ", paste(power, sep = "\n", collapse = "\n"),
       sep = "")

  cat(",  SE: ", paste(se, sep = "\n", collapse = "\n"),
      sep = "")

  if (length(ds$boundaries)>1) cat(",  Cost: ", paste(cost, sep = "\n", collapse = "\n"),
       sep = "")

  cat("\nEvaluations: ", paste(evaluations, sep = "\n", collapse = "\n"),
     sep = "")

  cat(",  Time: ", paste(timex, sep = "\n", collapse = "\n"),
      sep = "")

  cat(",  Updates: ", paste(updates, sep = "\n", collapse = "\n"),
     sep = "")

  cat("\nSurrogate: ", paste(surrogate, sep = "\n", collapse = "\n"),
      sep = "")


  cat("\n")
  cat("\n")

  invisible(ds)
}

