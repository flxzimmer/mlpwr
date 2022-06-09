plot2d = function(ds) {


  usetask = "B"
  resX = resx[resx$task==usetask,]
  resX = resX[!is.na(resX$cost),]

  # import analytical results for DGFs 3 and 4 ("analytical")
  # Calculate corresponding actual power for these values ("analytical_power")
  resX$analytical = NA
  resX$analytical[as.numeric(resX$fun_nr)==3] = an[[3]]$analytical
  resX$analytical[as.numeric(resX$fun_nr)==4] = an[[4]]$analytical
  resX$analytical_power = NA
  resX$analytical_power[as.numeric(resX$fun_nr)==3] = at[[3]]$true_power.fun(an[[3]]$analytical)
  resX$analytical_power[as.numeric(resX$fun_nr)==4] = at[[4]]$true_power.fun(an[[4]]$analytical)

  resX = resX[resX$learner!="Analytical",]

  fn_nr = 2
  resX1 = resX[resX$fun_nr=="2 ANOVA",]

  atx = at[[fn_nr]]
  true_power.fun = atx$true_power.fun
  actual_power = atx$actual_power
  actual_cost = atx$actual_cost
  x1 = load.cond(fn_nr,usetask,budget=NA,goal.ci=NA)
  costfun=x1$cost
  design=x1$design

  xvals = seq(min(resX1$X1,na.rm=T),max(resX1$X1,na.rm=T),.5)
  eqpower.y = sapply(xvals,function(x) {
    fn = function(y) abs(true_power.fun(c(x,y))-actual_power)
    a = optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])
    legit = a$value<.001
    if(legit) return(a$par) else return(NA)
  })
  eqpower = data.frame(X1=xvals,X2=eqpower.y)

  eqcost.y = sapply(xvals,function(x) {
    fn = function(y) abs(costfun(c(x,y))-actual_cost)
    optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])$par})
  eqcost = data.frame(X1=xvals,X2=eqcost.y)

  correct = sapply(1:nrow(eqcost),function(i) {
    costfun(eqcost[i,])==actual_cost
  })
  eqcost = eqcost[correct,]


  dat.at = data.frame(t(atx$actually_true))


  eqpower = eqpower[!is.na(eqpower$X2),]

  p7 = ggplot() + theme_bw()  + xlim(40, 80) + ylim(5,16) +
    scale_colour_manual(values = c("#B2182B", "#2166AC","black"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid","blank"),shape = c(NA, NA,3)))) +
    xlab("Participants per Cluster") +
    ylab("Number of Clusters") +
    geom_ribbon(data=eqpower,aes(x=X1,ymin = X2, ymax = 16), fill = "#92C5DE") +
    geom_line(data=eqpower,aes(x=X1, y=X2,col="Minimal power"),size=1)+
    geom_line(data=eqcost,aes(x=X1, y=X2,col="Maximal cost"),size=1)+
    geom_point(data=dat.at, aes(x=X1, y=X2,col="Optimal set"),shape=3,size=5) +
    annotate(geom="text", x=52, y=14, label="Desired Power", color="#2166AC",size = 5,hjust = "left") +
    annotate(geom="text", x=67, y=7, label="Optimal Cost", color="#B2182B",size = 5,hjust = "right") +
    annotate(geom="text", x=58, y=10, label="Optimal Set", color="black",size = 5, hjust = "right") +
    theme(legend.position="none",plot.title = element_text(hjust = 0.5)) + ggtitle("Desired Power Task")



  p8 = ggplot() + theme_bw()  + xlim(40, 80) + ylim(5,16) +
    scale_colour_manual(values = c("#B2182B", "#2166AC","black"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid","blank"),shape = c(NA, NA,3)))) +
    xlab("Participants per Cluster") +
    ylab("Number of Clusters") +
    geom_ribbon(data=eqcost,aes(x=X1,ymin = 5, ymax = X2), fill = "#F4A582") +
    geom_line(data=eqpower,aes(x=X1, y=X2,col="Minimal power"),size=1)+
    geom_line(data=eqcost,aes(x=X1, y=X2,col="Maximal cost"),size=1)+
    geom_point(data=dat.at, aes(x=X1, y=X2,col="Optimal set"),shape=3,size=5) +
    annotate(geom="text", x=52, y=14, label="Optimal Power", color="#2166AC",size = 5,hjust = "left") +
    annotate(geom="text", x=67, y=7, label="Cost Threshold", color="#B2182B",size = 5,hjust = "right") +
    annotate(geom="text", x=58, y=10, label="Optimal Set", color="black",size = 5, hjust = "right") +
    theme(legend.position="none",plot.title = element_text(hjust = 0.5)) + ggtitle("Cost Threshold Task")


  g1 = grid.arrange(p7,p8, ncol=2)

  pdf(paste0(folder,"multi_true_2D.pdf"),height=4,width=7);grid.draw(g1);dev.off()


}
