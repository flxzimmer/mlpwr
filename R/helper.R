


# helper ------------------------------------------------------------------

kl = function(items.true,items.alt,order=30) {
  # mean expected kullback-leibler distance across items
  # (by convention, p is true, q is est)
  res = c()
  for (i in 1:nrow(items.true)) {
    px = function(x) {xpl(x,items.true$beta[i],items.true$alpha[i],items.true$gamma[i])}
    qx = function(x) {xpl(x,items.alt$beta[i],items.alt$alpha[i],items.alt$gamma[i])}
    kl = function(x) {px(x)*log(px(x)/qx(x))+(1-px(x))*log((1-px(x))/(1-qx(x))) }
    res[i] = gauss.hermite(kl,order=order)
  }
  return(mean(res))
}


kl2 = function(items.true,items.alt,order=30) {
  # expected kullback-leibler distance for whole data
  # (by convention, p is true, q is est)
  res = c()
  px = function(x) {xpl(x,items.true$beta,items.true$alpha,items.true$gamma)}
  qx = function(x) {xpl(x,items.alt$beta,items.alt$alpha,items.alt$gamma)}
  kl = function(x) {px(x)*log(px(x)/qx(x))+(1-px(x))*log((1-px(x))/(1-qx(x))) }
  res[i] = gauss.hermite(kl,order=order)

  return(mean(res))
}


xpl = function (pers, items, slopes=NA, guessing=NA) {
  #Generate pmat from 1PL / 2PL / 3PL Model

  if (is.na(slopes[1])) {slopes = rep(1,length(items))}
  if (is.na(guessing[1])) {guessing = rep(0,length(items))}
  matrix = matrix(NA, length(pers), length(items))
  for (i in 1:length(pers)) {
    for (j in 1:length(items)) {
      theta = pers[i]
      beta = items[j]
      alpha = slopes[j]
      gamma = guessing [j]
      a = exp(alpha * (theta - beta))
      matrix[i, j] = gamma + (1-gamma)* a / (1 + a)
    }
  }
  return(matrix)
}

initialize = function (m) {
  #Generate 0/1 Matrix from Probabilites-Matrix
  m = apply(m,1:2,function(x) {x>=runif(1)})
  m = apply(m,2,as.numeric)
  return(m)
}

clean.data =  function(df,ind = FALSE) {
  #removes cols and rows without variance
  #return excluded indices if ind =TRUE

  df  = as.data.frame(df)
  colnames(df) = as.character(1:ncol(df))

  exclude = (rowSums(df) %in% c(0, ncol(df)))
  exclude2 = (colSums(df) %in% c(0, nrow(df)))
  while (any(c(exclude, exclude2))) {
    df = df[!exclude, !exclude2]
    exclude = (rowSums(df) %in% c(0, ncol(df)))
    exclude2 = (colSums(df) %in% c(0, nrow(df)))
  }
  if (ind==FALSE) {return(as.matrix(df))} else {
    pers.ind = as.numeric(rownames(df))
    item.ind = as.numeric(colnames(df))
    return(list(pers.ind,item.ind))
  }
}

