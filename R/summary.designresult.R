#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
summary.designresult = function(ds) {

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
print.summary.designresult = function(ds) {

  design = ds$final$design
  power = round(ds$final$power,5)
  se = round(ds$final$se,5)
  cost = round(ds$final$cost,2)
  runs = ds$runs_used
  timex = round(ds$time_used,2)
  updates = ds$n_updates


  cat("\nCall:\n", paste(deparse(ds$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")

  cat("\nDesign: ", paste(names(design),design, sep = " = ", collapse = ", "),
      "\n",sep = "")

  cat("\nPower: ", paste(power, sep = "\n", collapse = "\n"),
       sep = "")

  cat("\nSE: ", paste(se, sep = "\n", collapse = "\n"),
      sep = "")

  cat("\nCost: ", paste(cost, sep = "\n", collapse = "\n"),
       sep = "")

  cat("\nRuns: ", paste(runs, sep = "\n", collapse = "\n"),
     sep = "")

  cat("\nTime: ", paste(timex, sep = "\n", collapse = "\n"),
      sep = "")

  cat("\nUpdates: ", paste(updates, sep = "\n", collapse = "\n"),
     sep = "")

  cat("\n")
  cat("\n")

  invisible(ds)
}
