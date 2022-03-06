
devtools::load_all(".")
load.libs()

# Load Results ------------------------------------------------------------

# restag = c("48","49","50","51")
restag = c("52","53","54","55")


folder = "C:/Users/felix/switchdrive/4 irt/paper 2/"
# folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"

rest = list()
for (i in restag) {
  load(file= paste0(folder,"res_",i,".Rdata"))
  rest = c(rest,res)
}
res = rest


load(file= paste0(folder,"at.Rdata"))


# Manual input ------------------------------------------------------------

learner.names = c("Analytical","Linreg","Logreg","SVR","GP")
learner = factor(learner.names,levels=learner.names,ordered=T)

funlabels = c("1 t-test","2 ANOVA","3 Skewed t-test","4 IRT","5 Mixed Model","6 2D Mixed Model")
# funlabels = c("4 IRT","5 Mixed Model","6 2D Mixed Model")
# funlabels = c("2 2D-t-test","6 2D Mixed Model")
# funlabels = c("4 IRT")

# resx$cost[resx$learner=="Analytical"]=150


# Long Result Table -------------------------------------------------------

resx = lapply(res, function(x) {

    cond = x[[1]]
    task = cond$task
    seed = cond$seed
    budget=cond$budget
    goal.ci = cond$goal.ci
    fun_nr = cond$fun_nr
    x1 = load.cond(fun_nr,task,budget=budget,goal.ci=goal.ci)
    costfun=x1$cost
    if (is.null(goal.ci))goal.ci ="NULL"
    if (is.null(budget))budget ="NULL"

    atx = at[[fun_nr]]
    true_power.fun = atx$true_power.fun

    if (task != "C") {
        actual_power = atx$actual_power
        actually_true = atx$actually_true
        actual_cost = atx$actual_cost
    }
    if (task == "C") {
        actual_power = atx$actual_powerC
        actually_true = atx$actually_trueC
        actual_cost = atx$actual_costC
    }

    value = sapply(2:length(x),function(i) x[[i]]$value)
    value = lapply(value,function(x) if(is.null(x))NA else x)
    t1 = do.call(rbind,value)
    X1 = t1[,1]
    X2 = tryCatch(t1[,2],error=function(e)NA)
    value = list()
    for (i in 1:nrow(t1)) {
        value[[i]] = t1[i,]
    }
    cost = sapply(value,costfun) |> as.numeric()
    # if(task=="C"&fun_nr==6&&cost[4]!=320) browser()

    value.sd  = sapply(2:length(x),function(i) x[[i]]$value.sd)
    value.sd = lapply(value.sd,function(x) if(is.null(x))NA else x) |> as.numeric()
    budget_used = sapply(2:length(x),function(i) x[[i]]$budget)
    budget_used = lapply(budget_used,function(x) if(is.null(x))NA else x) |> as.numeric()

    time_used   = sapply(2:length(x),function(i) x[[i]]$time_used[1])
    time_used = lapply(time_used,function(x) if(is.null(x))NA else x) |> as.numeric()

    if (!is.null(true_power.fun)) {
    true_power = sapply(value,function(x){
        if(is.na(x[1])) return(NA)
        else return(true_power.fun(x))
        })
    } else {true_power= NA}


    # if(seed == 300)browser()

    re = data.frame(cost,value.sd,budget_used,learner,actual_cost,actual_power,true_power,time_used,budget,goal.ci,fun_nr,task,seed,I(value),X1,X2)
    return(re)
})

# save(resx,file= paste0(folder,"resx_temp.Rdata"))
# load(file= paste0(folder,"resx_temp.Rdata"))

resx = do.call(rbind,resx)

resx$budget = as.factor(resx$budget)
resx$goal.ci = as.factor(resx$goal.ci)

resx$fun_nr = factor(resx$fun_nr, labels=funlabels[which(1:6 %in% resx$fun_nr)])

resx = resx[!is.na(resx$fun_nr),]

resx$value.sd[resx$value.sd ==10] = NA


save(resx, file = paste0(folder,"tempres.Rdata"))


# Load Preprocessed -------------------------------------------------------

devtools::load_all(".")
load.libs()

load(file = paste0(folder,"tempres.Rdata")) #resx
load(file = paste0(folder,"at.Rdata")) # at


# Task B ------------------------------------------------------------

usetask = "B"
resX = resx[resx$task==usetask,]
resX = resX[!is.na(resX$cost),]

# insert analytical results (wo kommen die her? am anfang importieren?)
resX$analytical = NA
resX$analytical[as.numeric(resX$fun_nr)==3] = 99.0805650087345
resX$analytical[as.numeric(resX$fun_nr)==4] = 150
resX$analytical_power = NA
resX$analytical_power[as.numeric(resX$fun_nr)==3] = 0.786687772969949
resX$analytical_power[as.numeric(resX$fun_nr)==4] = 0.787123625772207

resX = resX[resX$learner!="Analytical",]

# Power Plot
resy = resX[!is.na(resX$true_power),]
p1 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  theme_bw() +
  geom_hline(data= resX, aes(yintercept=actual_power, linetype="Optimal power"), color = "#2166AC") +
  geom_hline(data= resX, aes(yintercept=analytical_power, linetype="Analytical power"), color = "black") +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~ fun_nr,scales = "free_x") +
  scale_linetype_manual(name = "", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("black","#2166AC")))) +
  ylab("Power")+ xlab("Surrogate Model") +
  labs(fill='Budget')+theme(legend.position="bottom")

pdf(paste0(folder,restag,"_plot1_",usetask,".pdf"),height=6,width=9.2);p1;dev.off()


# Power Plot / removed outliers
sum(resy$true_power<.5) # how many?
resy = resy[resy$true_power>=.5,]
p11 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  theme_bw() +
  geom_hline(data= resX, aes(yintercept=actual_power, linetype="Optimal power"), color = "#2166AC") +
  geom_hline(data= resX, aes(yintercept=analytical_power, linetype="Analytical power"), color = "black") +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~ fun_nr,scales = "free_x") +
  scale_linetype_manual(name = "", values = c(1, 1),guide = guide_legend(override.aes = list(color = c("black","#2166AC")))) +
  ylab("Power")+ xlab("Surrogate Model") +
  labs(fill='Budget')+theme(legend.position="bottom")

pdf(paste0(folder,restag,"_plot11_",usetask,".pdf"),height=6,width=9.2);p11;dev.off()


# Cost Plot
resy = resX[as.numeric(resX$fun_nr)%in%c(2,6),]
p2 = ggplot(resy, aes(x= learner,y=cost,fill=budget)) + theme_bw() +
  geom_hline(data= resy, aes(yintercept=actual_cost, linetype="Optimal cost"), color = "#B2182B") +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_brewer(palette="Reds")+
  facet_wrap(~ fun_nr,  scales = "free") + scale_linetype_manual(name = "", values = c(1),guide = guide_legend(override.aes = list(color = c("#B2182B")))) + ylab("Cost")+ xlab("Surrogate Model")+
  labs(fill='Budget')+theme(legend.position="bottom")

pdf(paste0(folder,restag,"_plot2_",usetask,".pdf"),height=4 ,width=7);p2;dev.off()



# Höhenlinien Plot Function 2
fn_nr = 2
resX1 = resX[resX$fun_nr=="2 ANOVA",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_power
actual_cost = atx$actual_cost
dat.at = data.frame(t(atx$actually_true))

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
# ignore some wrong values
correct = sapply(1:nrow(eqcost),function(i) {
  costfun(eqcost[i,])==actual_cost
})
eqcost = eqcost[correct,]

eqpower = eqpower[eqpower$X2<=max(resX1$X2),]
eqcost = eqcost[eqcost$X2<=max(resX1$X2),]

resX1$budget = recode(resX1$budget, "low" = 'Budget = 1000', "mid" = 'Budget = 3000', "high" =  'Budget = 9000' )


p3 = ggplot(resX1, aes(x=X1, y=X2)) + theme_bw() +
  scale_colour_manual(values = c("#2166AC","#B2182B"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid"),shape = c(NA, NA)))) +
  geom_ribbon(data=eqpower,aes(x=X1,ymin = X2, ymax = max(resX1$X2)), fill = "#92C5DE") +
  geom_line(data=eqpower,aes(x=X1, y=X2,col="Desired power"),size=1)+
  geom_line(data=eqcost,aes(x=X1, y=X2,col="Optimal cost"),size=1)+
  geom_point(aes(shape = learner),size=2) +
  geom_point(data=dat.at, aes(x=X1, y=X2,shape="Optimal set"),size=3,color="purple") +
  xlab("Participants per group") +
  ylab("Number of groups")+
  facet_wrap(~ budget,scales = "fixed") +
  scale_shape_manual(values = c(1,16,4), guide = guide_legend(title="",override.aes = list(shape = c(1,16,4),color=c("black","purple","black"),size=c(4,4,4))))+theme(legend.position="bottom")

pdf(paste0(folder,restag,"_plot3_",usetask,".pdf"),height=6,width=9.2);p3;dev.off()


# Höhenlinien Plot Function 6
fn_nr = 6
resX1 = resX[resX$fun_nr=="6 2D Mixed Model",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_power
actual_cost = atx$actual_cost
dat.at = data.frame(t(atx$actually_true))

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
# ignore some wrong values
correct = sapply(1:nrow(eqcost),function(i) {
  costfun(eqcost[i,])==actual_cost
})
eqcost = eqcost[correct,]

eqpower = eqpower[eqpower$X2<=max(resX1$X2),]
eqcost = eqcost[eqcost$X2<=max(resX1$X2),]

resX1$budget = recode(resX1$budget, "low" = 'Budget = 1000', "mid" = 'Budget = 3000', "high" =  'Budget = 9000' )


p4 = ggplot(resX1, aes(x=X1, y=X2)) + theme_bw() +
  scale_colour_manual(values = c("#2166AC","#B2182B"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid"),shape = c(NA, NA)))) +
  geom_ribbon(data=eqpower,aes(x=X1,ymin = X2, ymax = max(resX1$X2)), fill = "#92C5DE") +
  geom_line(data=eqpower,aes(x=X1, y=X2,col="Desired power"),size=1)+
  geom_line(data=eqcost,aes(x=X1, y=X2,col="Optimal cost"),size=1)+
  geom_point(aes(shape = learner),size=2) +
  geom_point(data=dat.at, aes(x=X1, y=X2,shape="Optimal set"),size=3,color="purple") +
  xlab("Participants per cluster") + ylab("Number of clusters")+
  facet_wrap(~ budget,scales = "fixed") +
  scale_shape_manual(values = c(1,16,4), guide = guide_legend(title="",override.aes = list(shape = c(1,16,4),color=c("black","purple","black"),size=c(4,4,4))))+theme(legend.position="bottom")

pdf(paste0(folder,restag,"_plot4_",usetask,".pdf"),height=6,width=9.2);p4;dev.off()


# Boxplot Zeit
p_time = ggplot(resX, aes(x= learner,y=time_used,fill=budget)) + theme_bw() +
  geom_violin(draw_quantiles = 0.5) +scale_fill_brewer(palette="Greys")+ facet_wrap(~ fun_nr, scales = "free_x")+theme(legend.position="bottom") + labs(x="Surrogate Model",y="Time used",fill="Budget")


pdf(paste0(folder,restag,"_plot_time_",usetask,".pdf"),height=4, width=7);p_time;dev.off()



# Task C -----------------------------------------------------------------

usetask = "C"
resX = resx[resx$task==usetask,]
resX = resX[!is.na(resX$cost),]

# Power Plot
resy = resX[!is.na(resX$true_power),]
p1 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  theme_bw() +
  geom_hline(data= resX, aes(yintercept=actual_power, linetype="Optimal power"), color = "#2166AC") +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~ fun_nr,scales = "free_x") +
  scale_linetype_manual(name = "", values = c(1),guide = guide_legend(override.aes = list(color = c("#2166AC")))) +
  ylab("Power")+ xlab("Surrogate Model") +
  labs(fill='Budget')+theme(legend.position="bottom")


pdf(paste0(folder,restag,"_plot1_",usetask,".pdf"),height=6,width=9.2);p1;dev.off()


# Power Plot / removed outliers
sum(resy$true_power<.7) # how many?
resy = resy[resy$true_power>=.7,]
p11 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  theme_bw() +
  geom_hline(data= resX, aes(yintercept=actual_power, linetype="Optimal power"), color = "#2166AC") +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~ fun_nr,scales = "free_x") +
  scale_linetype_manual(name = "", values = c(1),guide = guide_legend(override.aes = list(color = c("#2166AC")))) +
  ylab("Power")+ xlab("Surrogate Model") +
  labs(fill='Budget')+theme(legend.position="bottom")

pdf(paste0(folder,restag,"_plot11_",usetask,".pdf"),height=6,width=9.2);p11;dev.off()


# Höhenlinien Plot Function 2

fn_nr = 2
resX1 = resX[resX$fun_nr=="2 ANOVA",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_power
actual_cost = atx$actual_cost
dat.at = data.frame(t(atx$actually_true))

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

eqpower = eqpower[eqpower$X2<=max(resX1$X2),]
eqcost = eqcost[eqcost$X2<=max(resX1$X2),]

resX1$budget = recode(resX1$budget, "low" = 'Budget = 1000', "mid" = 'Budget = 3000', "high" =  'Budget = 9000' )


p3 = ggplot(resX1, aes(x=X1, y=X2)) + theme_bw() +
  scale_colour_manual(values = c("#B2182B","#2166AC"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid"),shape = c(NA, NA)))) +
  geom_ribbon(data=eqcost,aes(x=X1,ymin = 5, ymax = X2), fill = "#F4A582") +
  geom_line(data=eqpower,aes(x=X1, y=X2,col="Optimal power"),size=1)+
  geom_line(data=eqcost,aes(x=X1, y=X2,col="Cost Threshold"),size=1)+
  geom_point(aes(shape = learner),size=2) +
  geom_point(data=dat.at, aes(x=X1, y=X2,shape="Optimal set"),size=3,color="purple") +
  xlab("Participants per group") +
  ylab("Number of groups")+
  facet_wrap(~ budget,scales = "fixed") +
  scale_shape_manual(values = c(1,16,4), guide = guide_legend(title="",override.aes = list(shape = c(1,16,4),color=c("black","purple","black"),size=c(4,4,4))))+theme(legend.position="bottom")


pdf(paste0(folder,restag,"_plot3_",usetask,".pdf"),height=6,width=9.2);p3;dev.off()


# Höhenlinien Plot Function 6

fn_nr = 6
resX1 = resX[resX$fun_nr=="6 2D Mixed Model",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_power
actual_cost = atx$actual_cost
dat.at = data.frame(t(atx$actually_true))

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

eqpower = eqpower[eqpower$X2<=max(resX1$X2),]
eqcost = eqcost[eqcost$X2<=max(resX1$X2),]

resX1$budget = recode(resX1$budget, "low" = 'Budget = 1000', "mid" = 'Budget = 3000', "high" =  'Budget = 9000' )


p4 = ggplot(resX1, aes(x=X1, y=X2)) + theme_bw() +
  scale_colour_manual(values = c("#B2182B","#2166AC"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid"),shape = c(NA, NA)))) +
  geom_ribbon(data=eqcost,aes(x=X1,ymin = 5, ymax = X2), fill = "#F4A582") +
  geom_line(data=eqpower,aes(x=X1, y=X2,col="Optimal power"),size=1)+
  geom_line(data=eqcost,aes(x=X1, y=X2,col="Cost Threshold"),size=1)+
  geom_point(aes(shape = learner),size=2) +
  geom_point(data=dat.at, aes(x=X1, y=X2,shape="Optimal set"),size=3,color="purple") +
  xlab("Participants per cluster") + ylab("Number of clusters")+
  facet_wrap(~ budget,scales = "fixed") +
  scale_shape_manual(values = c(1,16,4), guide = guide_legend(title="",override.aes = list(shape = c(1,16,4),color=c("black","purple","black"),size=c(4,4,4))))+theme(legend.position="bottom")


pdf(paste0(folder,restag,"_plot4_",usetask,".pdf"),height=6,width=9.2);p4;dev.off()


# Plot Time
p_time = ggplot(resX, aes(x= learner,y=time_used,fill=budget)) + theme_bw() +
  geom_violin(draw_quantiles = 0.5) +scale_fill_brewer(palette="Greys")+ facet_wrap(~ fun_nr, scales = "free_x")+theme(legend.position="bottom")+ labs(x="Surrogate Model",y="Time used",fill="Budget")

pdf(paste0(folder,restag,"_plot_time_",usetask,".pdf"),height=4, width=7);p_time;dev.off()




# Höhenlinienplot for introduction----------------------------------
# http://www.sthda.com/english/wiki/colors-in-r


usetask = "B"
resX = resx[resx$task==usetask,]
resX = resX[!is.na(resX$cost),]

# convert analytical to column
resX$analytical = NA
resX$analytical[as.numeric(resX$fun_nr)==3] = 99.0805650087345
resX$analytical[as.numeric(resX$fun_nr)==4] = 150
resX$analytical_power = NA
resX$analytical_power[as.numeric(resX$fun_nr)==3] = 0.786687772969949
resX$analytical_power[as.numeric(resX$fun_nr)==4] = 0.787123625772207

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
# ignore some wrong values
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



# comparison simr ----------------------------------------------------------

# relevant runs
ind = sapply(res,function(x)
  x[[1]]$fun_nr==5 & x[[1]]$budget=="mid"
  )
rel = res[ind]

# sampled @n=16
n = sapply(rel,function(x) {
  a=x[[4]]
  ind = sapply(a$data,function(y) y$x==16)
  length(a$data[ind][[1]]$y)
})
mean(n)




a = resx[as.numeric(resx$fun_nr)==5&resx$learner=="Logreg"&resx$budget=="mid",]


p + qnorm(.975)*sd
var = p*(1-p)/n
sd = sqrt(var)
n=1000
p = .816





# Additional Stuff --------------------------------------------------------


# Diagnostics ------------------------------------------------------------
# how many NA's?

sapply(unique(resx$learner), function(i)  mean(is.na(resx[resx$learner==i,1])))

# a = resx[is.na(resx$cost),]

#remove NAs
resx = resx[!is.na(resx$cost),]



# Trace something ---------------------------------------------------------

# missing seeds
seeds = sapply(res,function(x) x[[1]]$seed)
max(seeds)
is.missing = function(x) return(! x %in% seeds)
missing = which(sapply(1:max(seeds),is.missing))
missing
save(missing,file="missing.Rdata")


n = which(resx$budget_used>10000&resx$task=="B"&resx$fun_nr=="5 Mixed Model")

n = which(resx$task=="B"&as.numeric(resx$fun_nr)==6)


View(resx[n,])

n = which(resx$value.sd*1.96>.03&resx$learner=="GP"&resx$task=="B"&resx$fun_nr==5)
resx$seed[n]

n = which(resx$value.sd<.001&resx$learner=="Linreg"&resx$task=="B"&resx$fun_nr==4)
resx$seed[n]

n = which(resx$budget_used==200&resx$goal.ci==0.015&learner=="Linreg")
resx$seed[n]

n = which(resx$cost>440&as.numeric(resx$fun_nr)==6)
n = which(resx$budget_used<700&as.numeric(resx$fun_nr)==6)



n = which(resx$cost>19&as.numeric(resx$fun_nr)==2)
seeds = resx[n,]$seed

for (i in seeds) {
    print(length(res[[i]][[5]]$data))
}


View(resx[n,])


View(res[[395]][[5]])
res[[395]][[5]]$data[[4]]$y |> mean()
startdat = res[[395]][[5]]$data
design = list(n = c(10,20))
t1 = svm.pred(startdat,goal=.8,design=design)
re3 =  ss.find(svm.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat,fixed_cost=fixed_cost)

fn = t1$fun
a = sapply(10:40,fn)
names(a) = 10:40
a
todataframe(startdat)

