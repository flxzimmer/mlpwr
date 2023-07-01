
#' Plot search result
#'
#' Plot a one- or two-dimensional graph of the result.
#'
#' @param design Specify a design as a list. Can be used to make a 1D plot for a two-dimensional simfun. Set NA for the dimension that should be plotted and set a value for all others. For example: design=list(n=NA,k=9)
#' @param adderrorbars logical. Plots errorbars in the 1D plot if TRUE. Default is FALSE (also if specified as NULL).
#' @param addribbon logical. Adds ribbon in the 1D plot if TRUE. Default is TRUE (also if specified as NULL).
#' @param trim logical. Option to trim the plotting area for the 2D line plot. The trimmed area is the area where the line is plotted. Default is TRUE.
#' @param type character indicating the type of the 2D plot. Can be 'heat'(default) or 'line'.
#' @param color.gradient character indicating whether the 2D plot should have a "diverging" color gradient (white-blue-white, default) or a "linear" color gradient (blue-red)
#' @param color.width numeric. Option for the diverging colormap in the 2D plot. Width of the blue-white colorband.
#' @param x Object of class designresult as created by the find.design function.
#' @param ... additional arguments to be passed.
#'
#' @return A ggplot object
#' @export
#'
#' @examples #Load a simulation function
#' simfun = example.simfun('ttest')
#' # Perform the search
#' ds = find.design(simfun = simfun, boundaries = c(100,300), power = .95)
#' # Plot results
#' plot(ds)
plot.designresult <- function(x, design = NULL, adderrorbars = NULL,
    addribbon = NULL, trim = TRUE, type = "heat", color.width = .15, color.gradient = "diverging", ...) {

    # choose 1D or 2D plot
    dims <- length(x$final$design)

    if (dims > 2) stop("Plots for designs with more than two dimensions are currently not supported.")

    if (dims == 2 & is.null(design)) {

        if (type == "hline")
            pl <- plot2d_line(x, trim)
        if (type == "heat")
            pl <- plot2d_heat(x,color.width,color.gradient)

    } else {
        pl <- plot1d(x, design = design, adderrorbars = adderrorbars,
            addribbon = addribbon)
    }

    print(pl)
    invisible(pl)

}


