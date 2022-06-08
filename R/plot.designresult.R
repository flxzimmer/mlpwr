
#' Title
#'
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
plot.designresult = function(ds,adderrorbars=NULL,addribbon=NULL,...) {


  dat = ds$dat
  fit = ds$fit

  boundaries = eval(ds$call$boundaries)

  # convert boundaries to list
  if (!is.list(boundaries)) boundaries=list(n=boundaries)

  # treat 2D dgfun differntly
  if(length(boundaries)>1) {

    namesx = names(boundaries)
    specified = sapply(namesx,exists)
    conditions = sapply(names,)

    eval(parse(text=""))
    if(exists(namesx))

  }

  ns = seq(boundaries[1],boundaries[2])

  xlab = names(ds$final$design)

  # Prediction
  dat_pred = data.frame(n =ns,y=sapply(ns,fit$fitfun),type="Prediction")

  # Actual SD
  dat_obs = todataframe(dat)
  dat_obs$sd = getweight(dat,"sd")

  # Estimated SD
  dat_sd = data.frame(n =ns,pred=dat_pred$y,sd=sapply(ns,fit$fitfun.sd))


  ## choose plot

  # set default options if arguments aren't specified
  if (is.null(addribbon)& is.null(adderrorbars)) {

    if (!is.null(fit$fitfun.sd)) {
    addribbon = TRUE
    adderrorbars = FALSE
    } else {
      addribbon = FALSE
      adderrorbars = TRUE
    }
  }
  if (is.null(addribbon)) addribbon=FALSE
  if (is.null(adderrorbars)) adderrorbars=FALSE


  ## build plot

  pl2 = ggplot2::ggplot()

  if (addribbon) pl2 = pl2 +
    ggplot2::geom_ribbon(data=dat_sd,ggplot2::aes(ymin = pred - sd, ymax = pred + sd,y=1,x=ns), fill = "grey70")

  if (adderrorbars) pl2 = pl2 +
    ggplot2::geom_errorbar(data=dat_obs,ggplot2::aes(ymin = y - sd, ymax = y + sd,y=1,x=V1))


    pl2 = pl2 +
    ggplot2::geom_line(data = dat_pred,ggplot2::aes(x=n,y=y)) +
    ggplot2::geom_point(data=dat_obs,ggplot2::aes(x =V1,y = y))+
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(palette="Set1") +
    ggplot2::theme(legend.title = ggplot2::element_blank())+ ggplot2::xlab(xlab) + ggplot2::ylab("power") +
    ggplot2::theme(legend.position="bottom")




  print(pl2)
  invisible(pl2)

}
