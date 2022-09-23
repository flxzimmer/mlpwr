
#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
plot.designresult = function(x,design=NULL,adderrorbars=NULL,addribbon=NULL,trim=TRUE,type="heat",...) {

  # choose 1D or 2D plot
  dims = length(x$final$design)
  if(dims==2&is.null(design)) {

    if (type=="hline")pl = plot2d_line(x,trim)
    if (type=="heat")pl = plot2d_heat(x)

  } else {
    pl = plot1d(x,design=design,adderrorbars=adderrorbars,addribbon=addribbon)
  }

  print(pl)
  invisible(pl)

}


