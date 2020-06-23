

# functions ---------------------------------------------------------------


#' setup Models
#'
#' Setting up the IRT Model that corresponds to the null hypothesis being tested.
#'
#'
#' @param architecture IRT Model, e.g. Rasch
#' @param n.items Number of items
#' @param pers.dist Distribution of Person Parameters, e.g. normal
#' @param beta Vector of length n.items specifiying item difficulty
#'
#' @return List of model parameters and distribution function
#' @export
#'
#' @examples
setup.model <- function(architecture = "Rasch", n.items= 20, alpha = c(), beta = c(), pers.dist = "normal") {

  if (architecture=="Rasch") {
    items = data.frame(
      alpha = rep(1,n.items),
      beta = beta,
      gamma = rep(0,n.items)
    )

  }

  if (architecture=="2PL") {
    items = data.frame(
      alpha = alpha,
      beta = beta,
      gamma = rep(0,n.items)
    )

  }

  if (pers.dist =="normal") {
    dist.func = rnorm
  }


  re = list(architecture=architecture,n.items=n.items,items = items,dist.func =dist.func)

  return(re)
}




#' Alternative Models
#'
#' Specify alternative models. Instance functions have to be defined to create a model with the desired effect size difference.
#'
#'
#'
#' @param class Type of alternative model. 2PL or DIF currently
#' @param diftype Type of DIF: "all" All items have different pars, alternatively an integer specifying the number of items with DIF.
#'
#' @return Function to initialize model with given effect size, function to create dataset from model parameters
#' @export
#'
#' @examples
setup.alternative <- function(class="2PL",diftype="all") {

  if (class=="2PL") {
    instance <- function(model,effect.size) {
      items.true = model$items
      error = rlnorm(nrow(items.true),0,.05)-1

      opt.func <- function(x) {
        temp = items.true
        slopes = items.true$alpha + error*x
        temp$alpha = slopes
        kl(items.true,temp,order=5)-effect.size
      }
      errorfactor = uniroot(opt.func,c(0,100))$root

      items.alt = model$items
      items.alt$alpha = items.alt$alpha+error*errorfactor

      re = items.alt
      return(re)
    }

    df.init = function(model,items.alt,n.pers) {

      initialize(xpl(model$dist.func(n.pers),items.alt$beta,items.alt$alpha,items.alt$gamma))
    }
  }


  if (class=="DIF") {
    instance <- function(model,effect.size) {
      items.true = model$items
      if (diftype == "all") {
        error = rnorm(nrow(items.true), 0, .05)
      } else {
        error = c(rnorm(diftype, 0, .05),rep(0,nrow(items.true)-diftype))
      }

      opt.func <- function(x) {
        temp = items.true
        temp$beta = items.true$beta + error*x
        kl(items.true,temp,order=5)/2-effect.size
      }
      errorfactor = uniroot(opt.func,c(0,1000))$root

      items.alt = model$items
      items.alt$beta = items.alt$beta+error*errorfactor

      re = items.alt
      return(re)
    }

    df.init = function(model,items.alt,n.pers) {
      half = round(n.pers/2)
      a1 = initialize(xpl(model$dist.func(half),model$items$beta,model$items$alpha,model$items$gamma))
      a2 = initialize(xpl(model$dist.func(n.pers-half),items.alt$beta,items.alt$alpha,items.alt$gamma))
      rbind(a1,a2)
    }
  }

  re = list(class=class,instance = instance,df.init=df.init)
  return(re)
}



#' setup Tests
#'
#' Wrapper for Tests
#'
#' @param type Test, currently: AndersenLR, M2
#'
#' @return Function that takes a dataset and returns a p-value
#' @export
#'
#' @examples
setup.test <- function(type = "AndersenLR",waldtype="any") {

  if (type == "AndersenLR") {
  re = function(df) {
    df = clean.data(df)
    rm <- RM(df)
    re1 = LRtest(rm)$pvalue
    return(re1)
  }
  }

  if (type == "M2") {
    re = function(df) {
      df = clean.data(df)
      rm = mirt(as.data.frame(df),1,"Rasch")
      re1 = M2(rm)$p
      return(re1)
    }
  }

  if (type == "Wald") {
    re = function(df) {
      df = clean.data(df)
#      browser()
      group = c(rep("a", round(nrow(df)/2) ), rep("b", nrow(df) - round(nrow(df)/2)))
      model <- multipleGroup(df, 1, group, SE = TRUE)
      a = DIF(model, 'd',Wald=TRUE,p.adjust = 'fdr')
      if (waldtype =="any") {
        re1 = min(a$adj_pvals)
      } else if (waldtype == "single") {
        re1 = a$adj_pvals[[1]]
      }

      return(re1)
    }
  }

  return(re)
}



#' Power
#'
#' perform Power analysis
#'
#' @import spatstat
#' @import mirt
#' @import eRm
#'
#' @param model model parameters created from the corresponding function
#' @param altmodel altmodel functions created from the corresponding function
#' @param test test functions created from the corresponding function
#' @param effect.size effect size (currently only expected Kullback-Leibler Distance)
#' @param n.pers Number of Persons
#' @param runs Number of Runs
#' @param alpha Alpha Niveau
#'
#' @return Power or 1-Beta
#' @export
#'
#' @examples
power <- function(model = model, altmodel=altmodel, test = test1, effect.size = .01,alpha = .05, n.pers=n.pers,runs=2){


  result <- function(n.pers=n.pers,items.alt,test1) {
    items.alt = altmodel$instance(model=model,effect.size = effect.size)
    df = altmodel$df.init(model,items.alt,n.pers)
    re = test1(df)
    return(re)
  }

  res = c()
  for (i in 1:runs) {
    res[i]=result(n.pers=n.pers,items.alt,test1)
  }
  re = mean(res<alpha)
  return(re)
  }



