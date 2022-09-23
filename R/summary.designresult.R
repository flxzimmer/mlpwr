#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
summary.designresult = function(object, ...) {

  ds = object
  # remove unneeded list elements here

  class(ds)="summary.designresult"

  return(ds)
}


#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
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

