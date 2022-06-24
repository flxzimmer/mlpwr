
#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
plot.designresult = function(x,design=NULL,adderrorbars=NULL,addribbon=NULL,...) {

  # choose 1D or 2D plot
  dims = length(x$final$design)
  if(dims==2&is.null(design)) {
    pl = plot2d(x)
  } else {
    pl = plot1d(x,design=design,adderrorbars=adderrorbars,addribbon=addribbon)
  }

  print(pl)
  invisible(pl)

}


