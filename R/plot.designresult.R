
#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
plot.designresult = function(ds,design=NULL,adderrorbars=NULL,addribbon=NULL) {

  # choose 1D or 2D plot
  dims = length(ds$final$design)
  if(dims==2&is.null(design)) {
    pl = plot2d(ds)
  } else {
    pl = plot1d(ds,design,adderrorbars,addribbon)
  }

  print(pl)
  invisible(pl)

}


