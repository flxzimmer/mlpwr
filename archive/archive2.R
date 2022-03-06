#
# p_to_power = function(dat,alpha,min.tests=1) {
#   # p_to_power = function(dat,alpha,min.tests=1,bins=FALSE) {
#
#   ns = sort(unique(dat$n))
#
#   power = c()
#   h = c()
#
#   # if (isFALSE(bins)) {
#
#     for (i in ns) {
#       power = c(power,as.numeric(mean(dat$p[dat$n==i]<alpha)))
#       h = c(h,sum(dat$n==i))
#     }
#   # }
#
#   # if (isTRUE(bins)) {
#   #   ns = seq(5,max(ns),5) #center values of the bins
#   #
#   #   for (i in ns) {
#   #     ind = abs(dat$n-i)<=2
#   #     power = c(power,as.numeric(mean(dat$p[ind]<alpha)))
#   #     weight = c(weight,sum(ind))
#   #   }
#   #
#   # }
#
#   sd = sqrt(power*(1-power)*h) / h
#
#   re = data.frame(
#     n=ns,
#     h = h,
#     power = power,
#     sd = sd
#   )
#
#   re=re[re$h>=min.tests&re$sd>0,]
#
#
#   return(re)
# }


# power_plot = function(dat,pred=NA,dat.true=NULL) {
#
#   p = ggplot(data=dat, aes(x=n, y=power)) +
#     geom_point()+ geom_errorbar(aes(ymin = power-sd, ymax = power+sd)) +
#     scale_y_continuous(breaks=seq(0,1,.1))
#
#   if (!is.na(pred)){
#     p = p + geom_vline(xintercept = pred,color="red")
#   }
#   if (!is.null(dat.true)){
#     p = p + geom_line(data=dat.true,aes(x=n, y=true.power),color="blue")
#   }
#   return(p)
# }
#
# quant_plot = function(dat,pred=NA,dat.true=NULL,predfun=NULL) {
#
#   p = ggplot(data=dat, aes(x=n, y=quantiles)) +
#     geom_point() +
#     scale_y_continuous(breaks=seq(0,1,.1))
#   if (!is.na(pred)){
#     p = p + geom_vline(xintercept = pred,color="red")
#   }
#   if (!is.null(dat.true)){
#     p = p + geom_line(data=dat.true,aes(x=n, y=true.quant),color="blue")
#   }
#   if (!is.null(predfun)){
#     p = p + geom_line(data=predfun,aes(x=n, y=fun),color="red")
#   }
#
#   return(p)
# }
