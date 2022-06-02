

get.closest = function(dat,new.n,predfun.sd=NULL) {

  datx = todataframe(dat,aggregate=TRUE)
  xvars = datx[,1:(length(datx)-1),drop=FALSE]
  xvars.z= scale(xvars)
  new.n.z = matrix(new.n,nrow=1)
  new.n.z = scale(new.n.z,center=attributes(xvars.z)$`scaled:center`,scale = attributes(xvars.z)$`scaled:scale`)
  dist = apply(xvars.z,1,function(x) sum((x-new.n.z)^2))
  ind = which(dist==min(dist))
  closest = as.numeric(xvars[ind,])
  if (!is.null(predfun.sd)) closest.sd = predfun.sd(closest)
  if (is.null(predfun.sd)) closest.sd = getweight(dat,"sd")[ind]
  re = list(value=closest,y = datx[ind,ncol(datx)],sd =closest.sd)
  return(re)
}
