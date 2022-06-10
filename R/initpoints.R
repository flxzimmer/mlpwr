

initpoints = function(boundaries,n.points,method="halton") {

  if(method=="halton") {
    s = as.matrix(randtoolbox::halton(n = n.points-2,dim=length(boundaries)),ncol=length(boundaries))
    pmin = pmax = c()
    for (i in 1:length(boundaries)) {
      dmin = boundaries[[i]][1]
      dmax = boundaries[[i]][2]
      s[,i] = dmin + s[,i]*(dmax-dmin)
      pmin[i] = dmin
      pmax[i] = dmax
    }
    points = round(s)
    points =rbind(points,as.numeric(pmin),as.numeric(pmax))
  }

  if (method=="sobol") {
    s = as.matrix(randtoolbox::sobol(n = n.points,dim=length(boundaries),scrambling=0),ncol=length(boundaries))

    for (i in 1:length(boundaries)) {
      dmin = boundaries[[i]][1]
      dmax = boundaries[[i]][2]
      s[,i] = dmin + s[,i]*(dmax-dmin)
    }
    points = round(s)
  }


  if (method=="random") {

    s = matrix(runif(n.points*length(boundaries)),ncol=length(boundaries))

    for (i in 1:length(boundaries)) {
      dmin = boundaries[[i]][1]
      dmax = boundaries[[i]][2]
      s[,i] = dmin + s[,i]*(dmax-dmin)
    }
    points = round(s)
  }


  colnames(points)=names(boundaries)

  return(points)
}


